/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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
#include <openspace/util/sphere.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/glm.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/opengl/programobject.h>
#include <optional>

namespace {
    constexpr const char* ProgramName = "Sphere";

    constexpr const std::array<const char*, 5> UniformNames = {
        "opacity", "modelViewProjection", "modelViewRotation", "colorTexture",
        "mirrorTexture"
    };

    enum class Orientation : int {
        Outside = 0,
        Inside,
        Both
    };

    constexpr openspace::properties::Property::PropertyInfo TextureInfo = {
        "Texture",
        "Texture",
        "This value specifies an image that is loaded from disk and is used as a texture "
        "that is applied to this sphere. This image is expected to be an equirectangular "
        "projection."
    };

    constexpr openspace::properties::Property::PropertyInfo MirrorTextureInfo = {
        "MirrorTexture",
        "Mirror Texture",
        "Mirror the texture along the x-axis."
    };

    constexpr openspace::properties::Property::PropertyInfo OrientationInfo = {
        "Orientation",
        "Orientation",
        "Specifies whether the texture is applied to the inside of the sphere, the "
        "outside of the sphere, or both."
    };

    constexpr openspace::properties::Property::PropertyInfo UseAdditiveBlendingInfo = {
        "UseAdditiveBlending",
        "Use Additive Blending",
        "Render the object using additive blending."
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

    constexpr openspace::properties::Property::PropertyInfo FadeOutThresholdInfo = {
        "FadeOutThreshold",
        "Fade-Out Threshold",
        "This value determines percentage of the sphere is visible before starting "
        "fading-out it."
    };

    constexpr openspace::properties::Property::PropertyInfo FadeInThresholdInfo = {
        "FadeInThreshold",
        "Fade-In Threshold",
        "Distance from center of MilkyWay from where the astronomical object starts to "
        "fade in."
    };

    constexpr openspace::properties::Property::PropertyInfo DisableFadeInOutInfo = {
        "DisableFadeInOut",
        "Disable Fade-In/Fade-Out effects",
        "Enables/Disables the Fade-In/Out effects."
    };

    constexpr openspace::properties::Property::PropertyInfo BackgroundInfo = {
        "Background",
        "Sets the current sphere rendering as a background rendering type",
        "Enables/Disables background rendering."
    };

    struct [[codegen::Dictionary(RenderableSphere)]] Parameters {
        // [[codegen::verbatim(SizeInfo.description)]]
        float size;

        // [[codegen::verbatim(SegmentsInfo.description)]]
        int segments;

        // [[codegen::verbatim(TextureInfo.description)]]
        std::string texture;

        enum class Orientation {
            Outside,
            Inside,
            Both
        };

        // [[codegen::verbatim(OrientationInfo.description)]]
        std::optional<Orientation> orientation;

        // [[codegen::verbatim(UseAdditiveBlendingInfo.description)]]
        std::optional<bool> useAdditiveBlending;

        // [[codegen::verbatim(MirrorTextureInfo.description)]]
        std::optional<bool> mirrorTexture;

        // [[codegen::verbatim(FadeOutThresholdInfo.description)]]
        std::optional<float> fadeOutThreshold [[codegen::inrange(0.0, 1.0)]];

        // [[codegen::verbatim(FadeInThresholdInfo.description)]]
        std::optional<float> fadeInThreshold;

        // [[codegen::verbatim(DisableFadeInOutInfo.description)]]
        std::optional<bool> disableFadeInOut;

        // [[codegen::verbatim(BackgroundInfo.description)]]
        std::optional<bool> background;
    };
#include "renderablesphere_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableSphere::Documentation() {
    documentation::Documentation doc = codegen::doc<Parameters>();
    doc.id = "base_renderable_sphere";
    return doc;
}

RenderableSphere::RenderableSphere(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _texturePath(TextureInfo)
    , _orientation(OrientationInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _size(SizeInfo, 1.f, 0.f, 1e35f)
    , _segments(SegmentsInfo, 8, 4, 1000)
    , _mirrorTexture(MirrorTextureInfo, false)
    , _useAdditiveBlending(UseAdditiveBlendingInfo, false)
    , _disableFadeInDistance(DisableFadeInOutInfo, true)
    , _backgroundRendering(BackgroundInfo, false)
    , _fadeInThreshold(FadeInThresholdInfo, -1.f, 0.f, 1.f)
    , _fadeOutThreshold(FadeOutThresholdInfo, -1.f, 0.f, 1.f)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    addProperty(_opacity);
    registerUpdateRenderBinFromOpacity();

    _size = p.size;
    _segments = p.segments;
    _texturePath = p.texture;

    _orientation.addOptions({
        { static_cast<int>(Orientation::Outside), "Outside" },
        { static_cast<int>(Orientation::Inside), "Inside" },
        { static_cast<int>(Orientation::Both), "Both" }
    });

    if (p.orientation.has_value()) {
        switch (*p.orientation) {
            case Parameters::Orientation::Inside:
                _orientation = static_cast<int>(Orientation::Inside);
                break;
            case Parameters::Orientation::Outside:
                _orientation = static_cast<int>(Orientation::Outside);
                break;
            case Parameters::Orientation::Both:
                _orientation = static_cast<int>(Orientation::Both);
                break;
            default:
                throw ghoul::MissingCaseException();
        }
    }
    else {
        _orientation = static_cast<int>(Orientation::Outside);
    }
    addProperty(_orientation);

    addProperty(_size);
    _size.onChange([this]() { _sphereIsDirty = true; });

    addProperty(_segments);
    _segments.onChange([this]() { _sphereIsDirty = true; });

    addProperty(_texturePath);
    _texturePath.onChange([this]() { loadTexture(); });

    addProperty(_mirrorTexture);
    addProperty(_useAdditiveBlending);

    _mirrorTexture = p.mirrorTexture.value_or(_mirrorTexture);
    _useAdditiveBlending = p.useAdditiveBlending.value_or(_useAdditiveBlending);

    if (_useAdditiveBlending) {
        setRenderBin(Renderable::RenderBin::PreDeferredTransparent);
    }

    bool hasGivenFadeOut = p.fadeOutThreshold.has_value();
    if (hasGivenFadeOut) {
        _fadeOutThreshold = *p.fadeOutThreshold;
        addProperty(_fadeOutThreshold);
    }

    bool hasGivenFadeIn = p.fadeInThreshold.has_value();
    if (hasGivenFadeIn) {
        _fadeInThreshold = *p.fadeInThreshold;
        addProperty(_fadeInThreshold);
    }

    if (hasGivenFadeIn || hasGivenFadeOut) {
        _disableFadeInDistance = false;
        addProperty(_disableFadeInDistance);
    }

    _backgroundRendering = p.background.value_or(_backgroundRendering);

    if (_backgroundRendering) {
        setRenderBin(Renderable::RenderBin::Background);
    }

    setRenderBinFromOpacity();
}

bool RenderableSphere::isReady() const {
    return _shader && _texture;
}

void RenderableSphere::initializeGL() {
    _sphere = std::make_unique<Sphere>(_size, _segments);
    _sphere->initialize();

    _shader = BaseModule::ProgramObjectManager.request(
        ProgramName,
        []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            return global::renderEngine->buildRenderProgram(
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
            global::renderEngine->removeRenderProgram(p);
        }
    );
    _shader = nullptr;
}

void RenderableSphere::render(const RenderData& data, RendererTasks&) {
    Orientation orientation = static_cast<Orientation>(_orientation.value());

    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        glm::dmat4(data.modelTransform.rotation) *
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));

    glm::dmat3 modelRotation =
        glm::dmat3(data.modelTransform.rotation);

    // Activate shader
    using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
    _shader->activate();
    _shader->setIgnoreUniformLocationError(IgnoreError::Yes);

    glm::mat4 modelViewProjection = data.camera.projectionMatrix() *
                             glm::mat4(data.camera.combinedViewMatrix() * modelTransform);
    _shader->setUniform(_uniformCache.modelViewProjection, modelViewProjection);

    glm::mat3 modelViewRotation = glm::mat3(
        glm::dmat3(data.camera.viewRotationMatrix()) * modelRotation
    );
    _shader->setUniform(_uniformCache.modelViewRotation, modelViewRotation);

    float adjustedOpacity = _opacity;

    if (!_disableFadeInDistance) {
        if (_fadeInThreshold > -1.0) {
            const float logDistCamera = glm::log(static_cast<float>(
                glm::distance(data.camera.positionVec3(), data.modelTransform.translation)
                ));
            const float startLogFadeDistance = glm::log(_size * _fadeInThreshold);
            const float stopLogFadeDistance = startLogFadeDistance + 1.f;

            if (logDistCamera > startLogFadeDistance && logDistCamera <
                stopLogFadeDistance)
            {
                const float fadeFactor = glm::clamp(
                    (logDistCamera - startLogFadeDistance) /
                    (stopLogFadeDistance - startLogFadeDistance),
                    0.f,
                    1.f
                );
                adjustedOpacity *= fadeFactor;
            }
            else if (logDistCamera <= startLogFadeDistance) {
                adjustedOpacity = 0.f;
            }
        }

        if (_fadeOutThreshold > -1.0) {
            const float logDistCamera = glm::log(static_cast<float>(
                glm::distance(data.camera.positionVec3(), data.modelTransform.translation)
                ));
            const float startLogFadeDistance = glm::log(_size * _fadeOutThreshold);
            const float stopLogFadeDistance = startLogFadeDistance + 1.f;

            if (logDistCamera > startLogFadeDistance && logDistCamera <
                stopLogFadeDistance)
            {
                const float fadeFactor = glm::clamp(
                    (logDistCamera - startLogFadeDistance) /
                    (stopLogFadeDistance - startLogFadeDistance),
                    0.f,
                    1.f
                );
                adjustedOpacity *= (1.f - fadeFactor);
            }
            else if (logDistCamera >= stopLogFadeDistance) {
                adjustedOpacity = 0.f;
            }
        }
    }
    // Performance wise
    if (adjustedOpacity < 0.01f) {
        return;
    }

    _shader->setUniform(_uniformCache.opacity, adjustedOpacity);
    _shader->setUniform(_uniformCache._mirrorTexture, _mirrorTexture.value());

    ghoul::opengl::TextureUnit unit;
    unit.activate();
    _texture->bind();
    _shader->setUniform(_uniformCache.colorTexture, unit);

    // Setting these states should not be necessary,
    // since they are the default state in OpenSpace.
    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);

    if (orientation == Orientation::Inside) {
        glCullFace(GL_FRONT);
    }
    else if (orientation == Orientation::Both) {
        glDisable(GL_CULL_FACE);
    }

    bool usingFramebufferRenderer = global::renderEngine->rendererImplementation() ==
                                    RenderEngine::RendererImplementation::Framebuffer;

    bool usingABufferRenderer = global::renderEngine->rendererImplementation() ==
                                RenderEngine::RendererImplementation::ABuffer;

    if (usingABufferRenderer && _useAdditiveBlending) {
        _shader->setUniform("additiveBlending", true);
    }

    if (usingFramebufferRenderer && _useAdditiveBlending) {
        glBlendFunc(GL_SRC_ALPHA, GL_ONE);
        glDepthMask(false);
    }

    _sphere->render();

    if (usingFramebufferRenderer && _useAdditiveBlending) {
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        glDepthMask(true);
    }

    _shader->setIgnoreUniformLocationError(IgnoreError::No);
    _shader->deactivate();

    if (orientation == Orientation::Inside) {
        glCullFace(GL_BACK);
    }
    else if (orientation == Orientation::Both) {
        glEnable(GL_CULL_FACE);
    }
}

void RenderableSphere::update(const UpdateData&) {
    if (_shader->isDirty()) {
        _shader->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(*_shader, _uniformCache, UniformNames);
    }

    if (_sphereIsDirty) {
        _sphere = std::make_unique<Sphere>(_size, _segments);
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
            texture->purgeFromRAM();
            _texture = std::move(texture);
        }
    }
}

} // namespace openspace
