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

#include <modules/base/rendering/renderablesphere.h>

#include <modules/base/basemodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/powerscaledscalar.h>
#include <openspace/util/powerscaledsphere.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/glm.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/opengl/programobject.h>

namespace {
    constexpr const char* ProgramName = "Sphere";

    constexpr const std::array<const char*, 4> UniformNames = {
        "opacity", "ViewProjection", "ModelTransform", "texture1"
    };

    enum Orientation {
        Outside = 1,
        Inside = 2
    };

    constexpr openspace::properties::Property::PropertyInfo TextureInfo = {
        "Texture",
        "Texture",
        "This value specifies an image that is loaded from disk and is used as a texture "
        "that is applied to this sphere. This image is expected to be an equirectangular "
        "projection."
    };

    constexpr openspace::properties::Property::PropertyInfo OrientationInfo = {
        "Orientation",
        "Orientation",
        "Specifies whether the texture is applied to the inside of the sphere, the "
        "outside of the sphere, or both."
    };

    constexpr openspace::properties::Property::PropertyInfo SegmentsInfo = {
        "Segments",
        "Number of Segments",
        "This value specifies the number of segments that the sphere is separated in."
    };

    constexpr openspace::properties::Property::PropertyInfo SizeInfo = {
        "Size",
        "Size (in meters)",
        "This value specifies the radius of the sphere in meters."
    };

    constexpr openspace::properties::Property::PropertyInfo FadeOutThreshouldInfo = {
        "FadeOutThreshould",
        "Fade-Out Threshould",
        "This value determines percentage of the sphere is visible before starting "
        "fading-out it."
    };

    constexpr openspace::properties::Property::PropertyInfo FadeInThreshouldInfo = {
        "FadeInThreshould",
        "Fade-In Threshould",
        "Distance from center of MilkyWay from where the astronomical object starts to "
        "fade in."
    };

    constexpr openspace::properties::Property::PropertyInfo DisableFadeInOuInfo = {
        "DisableFadeInOu",
        "Disable Fade-In/Fade-Out effects",
        "Enables/Disables the Fade-In/Out effects."
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


RenderableSphere::RenderableSphere(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _texturePath(TextureInfo)
    , _orientation(OrientationInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _size(SizeInfo, 1.f, 0.f, 1e35f)
    , _segments(SegmentsInfo, 8, 4, 1000)
    , _disableFadeInDistance(DisableFadeInOuInfo, true)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderableSphere"
    );

    addProperty(_opacity);
    registerUpdateRenderBinFromOpacity();

    _size = static_cast<float>(dictionary.value<double>(SizeInfo.identifier));
    _segments = static_cast<int>(dictionary.value<double>(SegmentsInfo.identifier));
    _texturePath = absPath(dictionary.value<std::string>(TextureInfo.identifier));

    _orientation.addOptions({
        { Outside, "Outside" },
        { Inside, "Inside" },
        { Outside | Inside, "Inside/Outside" }
    });

    if (dictionary.hasKey(OrientationInfo.identifier)) {
        const std::string& v = dictionary.value<std::string>(OrientationInfo.identifier);
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
    _size.onChange([this]() { _sphereIsDirty = true; });

    addProperty(_segments);
    _segments.onChange([this]() { _sphereIsDirty = true; });

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

bool RenderableSphere::isReady() const {
    return _shader && _texture;
}

void RenderableSphere::initializeGL() {
    _sphere = std::make_unique<PowerScaledSphere>(
        PowerScaledScalar::CreatePSS(_size),
        _segments
    );
    _sphere->initialize();

    _shader = BaseModule::ProgramObjectManager.request(
        ProgramName,
        []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            return global::renderEngine.buildRenderProgram(
                ProgramName,
                absPath("${MODULE_BASE}/shaders/sphere_vs.glsl"),
                absPath("${MODULE_BASE}/shaders/sphere_fs.glsl")
            );
        }
    );

    ghoul::opengl::updateUniformLocations(*_shader, _uniformCache, UniformNames);

    loadTexture();
}

void RenderableSphere::deinitializeGL() {
    _texture = nullptr;

    BaseModule::ProgramObjectManager.release(
        ProgramName,
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine.removeRenderProgram(p);
        }
    );
    _shader = nullptr;
}

void RenderableSphere::render(const RenderData& data, RendererTasks&) {
    glm::mat4 transform = glm::mat4(1.0);

    transform = glm::rotate(transform, glm::half_pi<float>(), glm::vec3(1, 0, 0));

    // Activate shader
    using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
    _shader->activate();
    _shader->setIgnoreUniformLocationError(IgnoreError::Yes);

    _shader->setUniform(_uniformCache.viewProjection, data.camera.viewProjectionMatrix());
    _shader->setUniform(_uniformCache.modelTransform, transform);

    setPscUniforms(*_shader, data.camera, data.position);

    float adjustedTransparency = _opacity;

    if (_fadeInThreshold > 0.0) {
        const double distCamera = glm::length(data.camera.positionVec3());
        const float funcValue = static_cast<float>(
            (1.0 / double(_fadeInThreshold / 1E24)) * (distCamera / 1E24)
        );

        adjustedTransparency *= (funcValue > 1.f) ? 1.f : funcValue;
    }

    if (_fadeOutThreshold > -1.0) {
        const double distCamera = glm::distance(
            data.camera.positionVec3(),
            data.position.dvec3()
        );
        const double term = std::exp(
            (-distCamera + _size * _fadeOutThreshold) / (_size * _fadeOutThreshold)
        );

        adjustedTransparency *= static_cast<float>(term / (term + 1.0));
    }

    // Performance wise
    if (adjustedTransparency < 0.01f) {
        return;
    }

    _shader->setUniform(_uniformCache.opacity, _opacity);

    ghoul::opengl::TextureUnit unit;
    unit.activate();
    _texture->bind();
    _shader->setUniform(_uniformCache.texture, unit);

    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);

    bool usingFramebufferRenderer = global::renderEngine.rendererImplementation() ==
                                    RenderEngine::RendererImplementation::Framebuffer;

    bool usingABufferRenderer = global::renderEngine.rendererImplementation() ==
                                RenderEngine::RendererImplementation::ABuffer;

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
        ghoul::opengl::updateUniformLocations(*_shader, _uniformCache, UniformNames);
    }

    if (_sphereIsDirty) {
        _sphere = std::make_unique<PowerScaledSphere>(
            PowerScaledScalar::CreatePSS(_size),
            _segments
        );
        _sphere->initialize();
        _sphereIsDirty = false;
    }
}

void RenderableSphere::loadTexture() {
    if (!_texturePath.value().empty()) {
        std::unique_ptr<ghoul::opengl::Texture> texture =
            ghoul::io::TextureReader::ref().loadTexture(_texturePath);

        if (texture) {
            LDEBUGC(
                "RenderableSphere",
                fmt::format("Loaded texture from '{}'", absPath(_texturePath))
            );
            texture->uploadTexture();
            texture->setFilter(ghoul::opengl::Texture::FilterMode::LinearMipMap);
            _texture = std::move(texture);
        }
    }
}

} // namespace openspace
