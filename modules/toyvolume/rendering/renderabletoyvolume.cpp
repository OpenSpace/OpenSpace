/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <modules/toyvolume/rendering/renderabletoyvolume.h>

#include <modules/toyvolume/rendering/toyvolumeraycaster.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/raycastermanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo SizeInfo = {
        "Size",
        "Size",
        "", // @TODO Missing documentation
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ScalingExponentInfo = {
        "ScalingExponent",
        "Scaling Exponent",
        "", // @TODO Missing documentation
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo StepSizeInfo = {
        "StepSize",
        "Step Size",
        "", // @TODO Missing documentation
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo TranslationInfo = {
        "Translation",
        "Translation",
        "", // @TODO Missing documentation
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo RotationInfo = {
        "Rotation",
        "Euler rotation",
        "", // @TODO Missing documentation
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ColorInfo = {
        "Color",
        "Color",
        "", // @TODO Missing documentation
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo DownscaleVolumeRenderingInfo =
    {
        "Downscale",
        "Downscale Factor Volume Rendering",
        "The downscaling factor used when rendering the current volume.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(RenderableToyVolume)]] Parameters {
        // [[codegen::verbatim(ScalingExponentInfo.description)]]
        std::optional<int> scalingExponent;

        // [[codegen::verbatim(SizeInfo.description)]]
        std::optional<glm::vec3> size;

        // [[codegen::verbatim(TranslationInfo.description)]]
        std::optional<glm::vec3> translation;

        // [[codegen::verbatim(RotationInfo.description)]]
        std::optional<glm::vec3> rotation;

        // [[codegen::verbatim(ColorInfo.description)]]
        std::optional<glm::vec3> color [[codegen::color()]];

        // [[codegen::verbatim(StepSizeInfo.description)]]
        std::optional<float> stepSize;

        // Raycast steps
        std::optional<int> steps;

        // [[codegen::verbatim(DownscaleVolumeRenderingInfo.description)]]
        std::optional<float> downscale;
    };
#include "renderabletoyvolume_codegen.cpp"
} // namespace

namespace openspace {

RenderableToyVolume::RenderableToyVolume(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _size(SizeInfo, glm::vec3(1.f, 1.f, 1.f), glm::vec3(0.f), glm::vec3(10.f))
    , _scalingExponent(ScalingExponentInfo, 1, -10, 20)
    , _stepSize(StepSizeInfo, 0.02f, 0.01f, 1.f)
    , _translation(TranslationInfo, glm::vec3(0.f), glm::vec3(0.f), glm::vec3(10.f))
    , _rotation(
        RotationInfo,
        glm::vec3(0.f),
        glm::vec3(0.f),
        glm::vec3(glm::two_pi<float>())
    )
    , _color(ColorInfo, glm::vec3(1.f, 0.f, 0.f), glm::vec3(0.f), glm::vec3(1.f))
    , _downScaleVolumeRendering(DownscaleVolumeRenderingInfo, 1.f, 0.1f, 1.f)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _scalingExponent = p.scalingExponent.value_or(_scalingExponent);
    _size = p.size.value_or(_size);
    _translation = p.translation.value_or(_translation);
    _rotation = p.rotation.value_or(_rotation);
    _color = p.color.value_or(_color);
    _stepSize = p.stepSize.value_or(_stepSize);
    _rayCastSteps = p.steps.value_or(_rayCastSteps);

    _downScaleVolumeRendering.setVisibility(properties::Property::Visibility::Developer);
    _downScaleVolumeRendering = p.downscale.value_or(_downScaleVolumeRendering);
}

RenderableToyVolume::~RenderableToyVolume() {}

void RenderableToyVolume::initializeGL() {
    glm::vec4 color = glm::vec4(glm::vec3(_color), opacity());
    _raycaster = std::make_unique<ToyVolumeRaycaster>(std::move(color));
    _raycaster->initialize();

    global::raycasterManager->attachRaycaster(*_raycaster);

    auto onChange = [this](bool enabled) {
        if (enabled) {
            global::raycasterManager->attachRaycaster(*_raycaster);
        }
        else {
            global::raycasterManager->detachRaycaster(*_raycaster);
        }
    };

    onEnabledChange(onChange);

    addProperty(_size);
    addProperty(_scalingExponent);
    addProperty(_stepSize);
    addProperty(_translation);
    addProperty(_rotation);
    addProperty(_color);
    addProperty(Fadeable::_opacity);
    addProperty(_downScaleVolumeRendering);
}

void RenderableToyVolume::deinitializeGL() {
    if (_raycaster) {
        global::raycasterManager->detachRaycaster(*_raycaster);
        _raycaster = nullptr;
    }
}

bool RenderableToyVolume::isReady() const {
    // @TODO isReady function needs to be filled
    return true;
}

void RenderableToyVolume::update(const UpdateData& data) {
    if (!_raycaster) {
        return;
    }

    glm::mat4 transform = glm::translate(
        glm::mat4(1.f),
        static_cast<glm::vec3>(_translation) *
            std::pow(10.f, static_cast<float>(_scalingExponent))
    );
    const glm::vec3 eulerRotation = _rotation;
    transform = glm::rotate(transform, eulerRotation.x, glm::vec3(1.f, 0.f, 0.f));
    transform = glm::rotate(transform, eulerRotation.y, glm::vec3(0.f, 1.f, 0.f));
    transform = glm::rotate(transform, eulerRotation.z,  glm::vec3(0.f, 0.f, 1.f));

    transform = glm::scale(
        transform,
        static_cast<glm::vec3>(_size) *
            std::pow(10.f, static_cast<float>(_scalingExponent))
    );

    const glm::vec4 color = glm::vec4(glm::vec3(_color), opacity());

    _raycaster->setColor(color);
    _raycaster->setStepSize(_stepSize);
    _raycaster->setModelTransform(transform);
    _raycaster->setTime(data.time.j2000Seconds());
    _raycaster->setDownscaleRender(_downScaleVolumeRendering);
    _raycaster->setMaxSteps(_rayCastSteps);
}

void RenderableToyVolume::render(const RenderData& data, RendererTasks& tasks) {
    RaycasterTask task { _raycaster.get(), data };
    tasks.raycasterTasks.push_back(std::move(task));
}

} // namespace openspace
