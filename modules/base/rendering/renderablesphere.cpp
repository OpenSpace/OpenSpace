/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/glm.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>
#include <optional>

namespace {
    constexpr openspace::properties::Property::PropertyInfo SizeInfo = {
        "Size",
        "Size (in meters)",
        "The radius of the sphere in meters.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo SegmentsInfo = {
        "Segments",
        "Number of Segments",
        "The number of segments that the sphere is split into.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    enum class Orientation : int {
        Outside = 0,
        Inside,
        Both
    };

    constexpr openspace::properties::Property::PropertyInfo OrientationInfo = {
        "Orientation",
        "Orientation",
        "Specifies whether the texture is applied to the inside of the sphere, the "
        "outside of the sphere, or both.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo MirrorTextureInfo = {
        "MirrorTexture",
        "Mirror Texture",
        "If true, mirror the texture along the x-axis.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo DisableFadeInOutInfo = {
        "DisableFadeInOut",
        "Disable Fade-In/Fade-Out effects",
        "Enables/Disables the fade in and out effects.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo FadeInThresholdInfo = {
        "FadeInThreshold",
        "Fade-In Threshold",
        "The distance from the center of the Milky Way at which the sphere should start "
        "to fade in, given as a percentage of the size of the object. A value of zero "
        "means that no fading in will happen.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo FadeOutThresholdInfo = {
        "FadeOutThreshold",
        "Fade-Out Threshold",
        "A threshold for when the sphere should start fading out, given as a percentage "
        "of how much of the sphere that is visible before the fading should start. A "
        "value of zero means that no fading out will happen.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(RenderableSphere)]] Parameters {
        // [[codegen::verbatim(SizeInfo.description)]]
        float size;

        // [[codegen::verbatim(SegmentsInfo.description)]]
        int segments;

        enum class [[codegen::map(Orientation)]] Orientation {
            Outside,
            Inside,
            Both
        };

        // [[codegen::verbatim(OrientationInfo.description)]]
        std::optional<Orientation> orientation;

        // [[codegen::verbatim(MirrorTextureInfo.description)]]
        std::optional<bool> mirrorTexture;

        // [[codegen::verbatim(DisableFadeInOutInfo.description)]]
        std::optional<bool> disableFadeInOut;

        // [[codegen::verbatim(FadeInThresholdInfo.description)]]
        std::optional<float> fadeInThreshold;

        // [[codegen::verbatim(FadeOutThresholdInfo.description)]]
        std::optional<float> fadeOutThreshold [[codegen::inrange(0.0, 1.0)]];
    };
#include "renderablesphere_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableSphere::Documentation() {
    return codegen::doc<Parameters>("base_renderable_sphere");
}

RenderableSphere::RenderableSphere(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _size(SizeInfo, 1.f, 0.f, 1e25f)
    , _segments(SegmentsInfo, 8, 4, 1000)
    , _orientation(OrientationInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _mirrorTexture(MirrorTextureInfo, false)
    , _disableFadeInDistance(DisableFadeInOutInfo, false)
    , _fadeInThreshold(FadeInThresholdInfo, 0.f, 0.f, 1.f, 0.001f)
    , _fadeOutThreshold(FadeOutThresholdInfo, 0.f, 0.f, 1.f, 0.001f)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    addProperty(Fadeable::_opacity);

    _size = p.size;
    _size.setExponent(15.f);
    _size.onChange([this]() {
        setBoundingSphere(_size);
        _sphereIsDirty = true;
    });
    addProperty(_size);

    _segments = p.segments;
    _segments.onChange([this]() {
        _sphereIsDirty = true;
    });
    addProperty(_segments);

    _orientation.addOptions({
        { static_cast<int>(Orientation::Outside), "Outside" },
        { static_cast<int>(Orientation::Inside), "Inside" },
        { static_cast<int>(Orientation::Both), "Both" }
    });
    _orientation = p.orientation.has_value() ?
        static_cast<int>(codegen::map<Orientation>(*p.orientation)):
        static_cast<int>(Orientation::Outside);
    addProperty(_orientation);

    _mirrorTexture = p.mirrorTexture.value_or(_mirrorTexture);
    addProperty(_mirrorTexture);

    _disableFadeInDistance = p.disableFadeInOut.value_or(_disableFadeInDistance);
    addProperty(_disableFadeInDistance);

    _fadeInThreshold = p.fadeInThreshold.value_or(_fadeInThreshold);
    addProperty(_fadeInThreshold);

    _fadeOutThreshold = p.fadeOutThreshold.value_or(_fadeOutThreshold);
    addProperty(_fadeOutThreshold);

    setBoundingSphere(_size);
}

bool RenderableSphere::isReady() const {
    return _shader != nullptr;
}

void RenderableSphere::initializeGL() {
    _sphere = std::make_unique<Sphere>(_size, _segments);
    _sphere->initialize();

    _shader = BaseModule::ProgramObjectManager.request(
        "Sphere",
        []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            return global::renderEngine->buildRenderProgram(
                "Sphere",
                absPath("${MODULE_BASE}/shaders/sphere_vs.glsl"),
                absPath("${MODULE_BASE}/shaders/sphere_fs.glsl")
            );
        }
    );

    ghoul::opengl::updateUniformLocations(*_shader, _uniformCache);
}

void RenderableSphere::deinitializeGL() {
    _sphere = nullptr;

    BaseModule::ProgramObjectManager.release(
        "Sphere",
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine->removeRenderProgram(p);
        }
    );

    _shader = nullptr;
}

void RenderableSphere::render(const RenderData& data, RendererTasks&) {
    const Orientation orientation = static_cast<Orientation>(_orientation.value());

    // Activate shader
    using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
    _shader->activate();
    _shader->setIgnoreUniformLocationError(IgnoreError::Yes);

    auto [modelTransform, modelViewTransform, modelViewProjectionTransform] =
        calcAllTransforms(data);
    const glm::dmat3 modelRotation = glm::dmat3(data.modelTransform.rotation);

    _shader->setUniform(_uniformCache.modelViewTransform, glm::mat4(modelViewTransform));
    _shader->setUniform(
        _uniformCache.modelViewProjection,
        glm::mat4(modelViewProjectionTransform)
    );

    const glm::mat3 modelViewRotation = glm::mat3(
        glm::dmat3(data.camera.viewRotationMatrix()) * modelRotation
    );
    _shader->setUniform(_uniformCache.modelViewRotation, modelViewRotation);

    float adjustedOpacity = opacity();

    if (!_disableFadeInDistance) {
        if (_fadeInThreshold > 0.f) {
            const double d = glm::distance(
                data.camera.positionVec3(),
                data.modelTransform.translation
            );
            const float logDist =
                d > 0.0 ?
                std::log(static_cast<float>(d)) :
                -std::numeric_limits<float>::max();

            const float startLogFadeDistance = glm::log(_size * _fadeInThreshold);
            const float stopLogFadeDistance = startLogFadeDistance + 1.f;

            if (logDist > startLogFadeDistance && logDist < stopLogFadeDistance) {
                const float fadeFactor = glm::clamp(
                    (logDist - startLogFadeDistance) /
                    (stopLogFadeDistance - startLogFadeDistance),
                    0.f,
                    1.f
                );
                adjustedOpacity *= fadeFactor;
            }
            else if (logDist <= startLogFadeDistance) {
                adjustedOpacity = 0.f;
            }
        }

        if (_fadeOutThreshold > 0.f) {
            const double d = glm::distance(
                data.camera.positionVec3(),
                data.modelTransform.translation
            );
            const float logDist =
                d > 0.0 ?
                std::log(static_cast<float>(d)) :
                -std::numeric_limits<float>::max();
            const float startLogFadeDistance = glm::log(_size * _fadeOutThreshold);
            const float stopLogFadeDistance = startLogFadeDistance + 1.f;

            if (logDist > startLogFadeDistance && logDist < stopLogFadeDistance) {
                const float fadeFactor = glm::clamp(
                    (logDist - startLogFadeDistance) /
                        (stopLogFadeDistance - startLogFadeDistance),
                    0.f,
                    1.f
                );
                adjustedOpacity *= (1.f - fadeFactor);
            }
            else if (logDist >= stopLogFadeDistance) {
                adjustedOpacity = 0.f;
            }
        }
    }
    // Performance wise
    if (adjustedOpacity < 0.01f) {
        return;
    }

    _shader->setUniform(_uniformCache.opacity, adjustedOpacity);
    _shader->setUniform(_uniformCache.mirrorTexture, _mirrorTexture.value());

    ghoul::opengl::TextureUnit unit;
    unit.activate();
    bindTexture();
    defer{ unbindTexture(); };
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

    if (renderBin() == Renderable::RenderBin::PreDeferredTransparent) {
        glBlendFunc(GL_SRC_ALPHA, GL_ONE);
        glDepthMask(false);
    }

    _sphere->render();

    _shader->setIgnoreUniformLocationError(IgnoreError::No);
    _shader->deactivate();

    // Reset
    global::renderEngine->openglStateCache().resetBlendState();
    global::renderEngine->openglStateCache().resetDepthState();
    global::renderEngine->openglStateCache().resetPolygonAndClippingState();
    unbindTexture();

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
        ghoul::opengl::updateUniformLocations(*_shader, _uniformCache);
    }

    if (_sphereIsDirty) {
        _sphere = std::make_unique<Sphere>(_size, _segments);
        _sphere->initialize();
        _sphereIsDirty = false;
    }
}

void RenderableSphere::unbindTexture() {
    glBindTexture(GL_TEXTURE_2D, 0);
}

} // namespace openspace
