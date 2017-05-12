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
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/filesystem/filesystem.h>

#define _USE_MATH_DEFINES
#include <math.h>

namespace {
    const char* KeySize = "Size";
    const char* KeySegments = "Segments";
    const char* KeyTexture = "Texture";
    const char* KeyOrientation = "Orientation";



    enum Orientation {
        Outside = 1,
        Inside = 2
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
                KeySize,
                new DoubleVerifier,
                "Specifies the radius of the sphere in meters.",
                Optional::No
            },
            {
                KeySegments,
                new IntVerifier,
                "Specifies the number of segments the sphere is separated in.",
                Optional::No
            },
            {
                KeyTexture,
                new StringVerifier,
                "Specifies the texture that is applied to the sphere.",
                Optional::No
            },
            {
                KeyOrientation,
                new StringInListVerifier({ "Inside", "Outside", "Inside/Outside" }),
                "Specifies whether the texture is applied to the inside of the sphere, "
                "the outside of the sphere, or both. The default value is 'Outside'.",
                Optional::Yes
            }
        }
    };
}


RenderableSphere::RenderableSphere(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _texturePath("texture", "Texture")
    , _orientation("orientation", "Orientation")
    , _size("size", "Size", 1.f, 0.f, std::pow(10.f, 45))
    , _segments("segments", "Segments", 8, 4, 100)
    , _transparency("transparency", "Transparency", 1.f, 0.f, 1.f)
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

    _size = dictionary.value<double>(KeySize);
    _segments = static_cast<int>(dictionary.value<double>(KeySegments));
    _texturePath = absPath(dictionary.value<std::string>(KeyTexture));

    _orientation.addOption(Outside, "Outside");
    _orientation.addOption(Inside, "Inside");
    _orientation.addOption(Outside | Inside, "Inside/Outside");

    if (dictionary.hasKey(KeyOrientation)) {
        const std::string v = dictionary.value<std::string>(KeyOrientation);
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
            ghoul_assert(false, "Missing 'case' label");
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

    addProperty(_transparency);
    addProperty(_texturePath);
    _texturePath.onChange([this]() {loadTexture(); });


    setRenderBin(Renderable::RenderBin::Transparent);
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

void RenderableSphere::render(const RenderData& data) {
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
