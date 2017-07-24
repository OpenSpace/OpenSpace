/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
 *                                                                                       *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
 * software and associated documentation files (the "Software"), to deal in the Software *
 * without restriction, including without limitation the rights to use, copy, modify,    *
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
 * permit persons to whom the Software is furnished to do so, subject to the following   *
 * conditions:                                                                           *
 *                                                                                       *
 * The above copyright notice and this permission notice shall be included in all copies *
 * or substantial portions of the Software.                                              *
 *                                                                                       *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
 ****************************************************************************************/

#include <modules/base/rendering/renderablesphere.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/powerscaledsphere.h>
#include <openspace/util/updatestructures.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/opengl/programobject.h>

#define _USE_MATH_DEFINES
#include <math.h>

namespace {
    enum Orientation {
        Outside = 1,
        Inside = 2
    };

    static const openspace::properties::Property::PropertyInfo TextureInfo = {
        "Texture",
        "Texture",
        "This value specifies an image that is loaded from disk and is used as a texture "
        "that is applied to this sphere. This image is expected to be an equirectangular "
        "projection."
    };

    static const openspace::properties::Property::PropertyInfo OrientationInfo = {
        "Orientation",
        "Orientation",
        "Specifies whether the texture is applied to the inside of the sphere, the "
        "outside of the sphere, or both."
    };

    static const openspace::properties::Property::PropertyInfo SegmentsInfo = {
        "Segments",
        "Number of Segments",
        "This value specifies the number of segments that the sphere is separated in."
    };

    static const openspace::properties::Property::PropertyInfo SizeInfo = {
        "Size",
        "Size (in meters)",
        "This value specifies the radius of the sphere in meters."
    };

    static const openspace::properties::Property::PropertyInfo TransparencyInfo = {
        "Alpha",
        "Transparency",
        "This value determines the transparency of the sphere. If this value is set to "
        "1, the sphere is completely opaque. At 0, the sphere is completely transparent."
    };
} // namespace

namespace openspace {

documentation::Documentation RenderableSphere::Documentation() {
    using namespace documentation;
    return {
        "RenderableSphere",
        "base_renderable_sphere",
        {
            {
                SizeInfo.identifier,
                new DoubleVerifier,
                SizeInfo.description,
                Optional::No
            },
            {
                SegmentsInfo.identifier,
                new IntVerifier,
                SegmentsInfo.description,
                Optional::No
            },
            {
                TextureInfo.identifier,
                new StringVerifier,
                TextureInfo.description,
                Optional::No
            },
            {
                OrientationInfo.identifier,
                new StringInListVerifier({ "Inside", "Outside", "Inside/Outside" }),
                OrientationInfo.description,
                Optional::Yes
            },
            {
                TransparencyInfo.identifier,
                new DoubleInRangeVerifier(0.0, 1.0),
                TransparencyInfo.description,
                Optional::Yes
            }
        }
    };
}


RenderableSphere::RenderableSphere(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _texturePath(TextureInfo)
    , _orientation(OrientationInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _size(SizeInfo, 1.f, 0.f, 1e35)
    , _segments(SegmentsInfo, 8, 4, 1000)
    , _transparency(TransparencyInfo, 1.f, 0.f, 1.f)
    , _shader(nullptr)
    , _texture(nullptr)
    , _sphere(nullptr)
    , _sphereIsDirty(false)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderableSphere"
    );

    _size = static_cast<float>(dictionary.value<double>(SizeInfo.identifier));
    _segments = static_cast<int>(dictionary.value<double>(SegmentsInfo.identifier));
    _texturePath = absPath(dictionary.value<std::string>(TextureInfo.identifier));

    _orientation.addOptions({
        { Outside, "Outside" },
        { Inside, "Inside" },
        { Outside | Inside, "Inside/Outside" }
    });

    if (dictionary.hasKey(OrientationInfo.identifier)) {
        const std::string v = dictionary.value<std::string>(OrientationInfo.identifier);
        if (v == "Inside") {
            _orientation = Inside;
        }
        else if (v == "Outside") {
            _orientation = Outside;
        }
        else if (v == "Inside/Outside") {
            _orientation = Outside | Inside;
        }
        else {
            throw ghoul::MissingCaseException();
        }
    }
    else {
        _orientation = Outside;
    }
    addProperty(_orientation);

    addProperty(_size);
    _size.onChange([this](){ _sphereIsDirty = true; });

    addProperty(_segments);
    _segments.onChange([this](){ _sphereIsDirty = true; });

    _transparency.onChange([this](){
        if (_transparency > 0.f && _transparency < 1.f) {
            setRenderBin(Renderable::RenderBin::Transparent);
        }
        else {
            setRenderBin(Renderable::RenderBin::Opaque);
        }
    });
    if (dictionary.hasKey(TransparencyInfo.identifier)) {
        _transparency = dictionary.value<double>(TransparencyInfo.identifier);
    }
    addProperty(_transparency);

    addProperty(_texturePath);
    _texturePath.onChange([this]() { loadTexture(); });

}

bool RenderableSphere::isReady() const {
    return _shader && _texture;
}

bool RenderableSphere::initialize() {
    _sphere = std::make_unique<PowerScaledSphere>(
        PowerScaledScalar::CreatePSS(_size), _segments
    );
    _sphere->initialize();

    // pscstandard
    _shader = OsEng.renderEngine().buildRenderProgram("Sphere",
        "${MODULE_BASE}/shaders/sphere_vs.glsl",
        "${MODULE_BASE}/shaders/sphere_fs.glsl");

    loadTexture();

    return isReady();
}

bool RenderableSphere::deinitialize() {
    _texture = nullptr;

    if (_shader) {
        OsEng.renderEngine().removeRenderProgram(_shader);
        _shader = nullptr;
    }

    return true;
}

void RenderableSphere::render(const RenderData& data, RendererTasks&) {
    glm::mat4 transform = glm::mat4(1.0);

    transform = glm::rotate(transform, static_cast<float>(M_PI_2), glm::vec3(1, 0, 0));

    // Activate shader
    using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
    _shader->activate();
    _shader->setIgnoreUniformLocationError(IgnoreError::Yes);

    _shader->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
    _shader->setUniform("ModelTransform", transform);

    setPscUniforms(*_shader.get(), data.camera, data.position);
    _shader->setUniform("alpha", _transparency);

    ghoul::opengl::TextureUnit unit;
    unit.activate();
    _texture->bind();
    _shader->setUniform("texture1", unit);

    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);
    
    bool usingFramebufferRenderer =
        OsEng.renderEngine().rendererImplementation() == RenderEngine::RendererImplementation::Framebuffer;

    bool usingABufferRenderer =
        OsEng.renderEngine().rendererImplementation() == RenderEngine::RendererImplementation::ABuffer;

    if (usingABufferRenderer) {
        _shader->setUniform("additiveBlending", true);
    }

    if (usingFramebufferRenderer) {
        glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    }

    _sphere->render();

    if (usingFramebufferRenderer) {
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    }

    _shader->setIgnoreUniformLocationError(IgnoreError::No);
    _shader->deactivate();
}

void RenderableSphere::update(const UpdateData&) {
    if (_shader->isDirty()) {
        _shader->rebuildFromFile();
    }

    if (_sphereIsDirty) {
        _sphere = std::make_unique<PowerScaledSphere>(
            PowerScaledScalar::CreatePSS(_size), _segments
        );
        _sphere->initialize();
        _sphereIsDirty = false;
    }
}

void RenderableSphere::loadTexture() {
    if (_texturePath.value() != "") {
        std::unique_ptr<ghoul::opengl::Texture> texture = ghoul::io::TextureReader::ref().loadTexture(_texturePath);
        if (texture) {
            LDEBUGC(
                "RenderableSphere",
                "Loaded texture from '" << absPath(_texturePath) << "'"
            );
            texture->uploadTexture();

            // Textures of planets looks much smoother with AnisotropicMipMap rather than linear
            // TODO: AnisotropicMipMap crashes on ATI cards ---abock
            //texture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
            texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);

            _texture = std::move(texture);
        }
    }
}

} // namespace openspace
