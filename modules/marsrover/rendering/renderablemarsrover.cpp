/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <modules/marsrover/rendering/renderablemarsrover.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/powerscaledsphere.h>
#include <openspace/util/updatestructures.h>

#include <ghoul/glm.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/opengl/programobject.h>

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

    static const openspace::properties::Property::PropertyInfo FadeOutThreshouldInfo = {
        "FadeOutThreshould",
        "Fade-Out Threshould",
        "This value determines percentage of the sphere is visible before starting "
        "fading-out it."
    };

    static const openspace::properties::Property::PropertyInfo FadeInThreshouldInfo = {
        "FadeInThreshould",
        "Fade-In Threshould",
        "Distance from center of MilkyWay from where the astronomical object starts to "
        "fade in."
    };

    static const openspace::properties::Property::PropertyInfo DisableFadeInOuInfo = {
        "DisableFadeInOu",
        "Disable Fade-In/Fade-Out effects",
        "Enables/Disables the Fade-In/Out effects."
    };

} // namespace

namespace openspace {

documentation::Documentation RenderableMarsrover::Documentation() {
    using namespace documentation;
    return {
        "RenderableMarsrover",
        "marsrover_renderable_marsrover",

        {
            {
                SizeInfo.identifier,
                new DoubleVerifier,
                Optional::No,
                SizeInfo.description
            },
            {
                SegmentsInfo.identifier,
                new IntVerifier,
                Optional::No,
                SegmentsInfo.description
            },
            {
                TextureInfo.identifier,
                new StringVerifier,
                Optional::No,
                TextureInfo.description
            },
            {
                OrientationInfo.identifier,
                new StringInListVerifier({ "Inside", "Outside", "Inside/Outside" }),
                Optional::Yes,
                OrientationInfo.description
            },
            {
                TransparencyInfo.identifier,
                new DoubleInRangeVerifier(0.0, 1.0),
                Optional::Yes,
                TransparencyInfo.description
            },
            {
                FadeOutThreshouldInfo.identifier,
                new DoubleInRangeVerifier(0.0, 1.0),
                Optional::Yes,
                FadeOutThreshouldInfo.description
            },
            {
                FadeInThreshouldInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                FadeInThreshouldInfo.description
            },
            {
                DisableFadeInOuInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                DisableFadeInOuInfo.description
            },
        }

    };
}


RenderableMarsrover::RenderableMarsrover(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _texturePath(TextureInfo)
    , _orientation(OrientationInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _size(SizeInfo, 1.f, 0.f, 1e35f)
    , _segments(SegmentsInfo, 8, 4, 1000)
    , _transparency(TransparencyInfo, 1.f, 0.f, 1.f)
    , _disableFadeInDistance(DisableFadeInOuInfo, true)
    , _fadeOutThreshold(-1.0)
    , _fadeInThreshold(0.0)
    , _shader1(nullptr)
    , _texture1(nullptr)
    , _marsrover(nullptr)
    , _sphereIsDirty(false)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderableMarsrover"
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
        _transparency = static_cast<float>(
            dictionary.value<double>(TransparencyInfo.identifier)
        );
    }
    addProperty(_transparency);

    addProperty(_texturePath);
    _texturePath.onChange([this]() { loadTexture(); });

    if (dictionary.hasKey(FadeOutThreshouldInfo.identifier)) {
        _fadeOutThreshold = static_cast<float>(
            dictionary.value<double>(FadeOutThreshouldInfo.identifier)
        );
    }

    if (dictionary.hasKey(FadeInThreshouldInfo.identifier)) {
        _fadeInThreshold = static_cast<float>(
            dictionary.value<double>(FadeInThreshouldInfo.identifier)
        );
    }

    if (dictionary.hasKey(FadeOutThreshouldInfo.identifier) ||
        dictionary.hasKey(FadeInThreshouldInfo.identifier)) {
        _disableFadeInDistance.set(false);
        addProperty(_disableFadeInDistance);
    }

}

bool RenderableMarsrover::isReady() const {
    return _shader1 && _texture1;
}

void RenderableMarsrover::initializeGL() {
    _marsrover = std::make_unique<PowerScaledSphere>(
        PowerScaledScalar::CreatePSS(_size), _segments
    );
    _marsrover->initialize();

    // pscstandard
    _shader1 = OsEng.renderEngine().buildRenderProgram(
        "Marsrover",
        absPath("${MODULE_MARSROVER}/shaders/sphere_vs.glsl"),
        absPath("${MODULE_MARSROVER}/shaders/sphere_fs.glsl")
    );

    loadTexture();
}

void RenderableMarsrover::deinitializeGL() {
    _texture1 = nullptr;

    if (_shader1) {
        OsEng.renderEngine().removeRenderProgram(_shader1);
        _shader1 = nullptr;
    }
}

void RenderableMarsrover::render(const RenderData& data, RendererTasks&) {
    glm::mat4 transform = glm::mat4(1.0);

    transform = glm::rotate(transform, glm::half_pi<float>(), glm::vec3(1, 0, 0));

    // Activate shader
    using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
    _shader1->activate();
    _shader1->setIgnoreUniformLocationError(IgnoreError::Yes);

    _shader1->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
    _shader1->setUniform("ModelTransform", transform);

    setPscUniforms(*_shader1.get(), data.camera, data.position);

    float adjustedTransparency = _transparency;

    if (_fadeInThreshold > 0.0) {
        float distCamera = glm::length(data.camera.positionVec3());
        float funcValue = static_cast<float>(
            (1.0 / double(_fadeInThreshold/1E24))*(distCamera / 1E24)
        );

        adjustedTransparency *= funcValue > 1.0 ? 1.0 : funcValue;
    }

    if (_fadeOutThreshold > -1.0) {
        float distCamera = glm::distance(
            data.camera.positionVec3(),
            data.position.dvec3()
        );
        double term = std::exp(
            (-distCamera + _size * _fadeOutThreshold) / (_size * _fadeOutThreshold)
        );

        adjustedTransparency *= static_cast<float>(term / (term + 1.0));
    }

    // Performance wise
    if (adjustedTransparency < 0.01) {
        return;
    }

    _shader1->setUniform("alpha", adjustedTransparency);

    ghoul::opengl::TextureUnit unit;
    unit.activate();
    _texture1->bind();
    _shader1->setUniform("texture1", unit);

    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);

    bool usingFramebufferRenderer =
        OsEng.renderEngine().rendererImplementation() ==
        RenderEngine::RendererImplementation::Framebuffer;

    bool usingABufferRenderer =
        OsEng.renderEngine().rendererImplementation() ==
        RenderEngine::RendererImplementation::ABuffer;

    if (usingABufferRenderer) {
        _shader1->setUniform("additiveBlending", true);
    }

    if (usingFramebufferRenderer) {
        glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    }

    _marsrover->render();

    if (usingFramebufferRenderer) {
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    }

    _shader1->setIgnoreUniformLocationError(IgnoreError::No);
    _shader1->deactivate();
}

void RenderableMarsrover::update(const UpdateData&) {
    if (_shader1->isDirty()) {
        _shader1->rebuildFromFile();
    }

    if (_sphereIsDirty) {
        _marsrover = std::make_unique<PowerScaledSphere>(
            PowerScaledScalar::CreatePSS(_size), _segments
        );
        _marsrover->initialize();
        _sphereIsDirty = false;
    }
}

void RenderableMarsrover::loadTexture() {
    if (_texturePath.value() != "") {
        using TR = ghoul::io::TextureReader;
        std::unique_ptr<ghoul::opengl::Texture> texture = TR::ref().loadTexture(
            _texturePath
        );
        if (texture) {
            LDEBUGC(
                "RenderableSphere",
                fmt::format("Loaded texture from '{}'", absPath(_texturePath))
            );
            texture->uploadTexture();

            // Textures of planets looks much smoother with AnisotropicMipMap rather than
            // linear
            // TODO: AnisotropicMipMap crashes on ATI cards ---abock
            //texture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
            texture->setFilter(ghoul::opengl::Texture::FilterMode::LinearMipMap);

            _texture1 = std::move(texture);
        }
    }
}

} // namespace openspace
