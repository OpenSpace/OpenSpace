/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/raycastermanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const char* _loggerCat = "Renderable ToyVolume";
    constexpr openspace::properties::Property::PropertyInfo SizeInfo = {
        "Size",
        "Size",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo ScalingExponentInfo = {
        "ScalingExponent",
        "Scaling Exponent",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo StepSizeInfo = {
        "StepSize",
        "Step Size",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo TranslationInfo = {
        "Translation",
        "Translation",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo RotationInfo = {
        "Rotation",
        "Euler rotation",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo ColorInfo = {
        "Color",
        "Color",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo DownscaleVolumeRenderingInfo =
    {
        "Downscale",
        "Downscale Factor Volume Rendering",
        "This value set the downscaling factor"
        " when rendering the current volume."
    };
} // namespace

namespace openspace {

RenderableToyVolume::RenderableToyVolume(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _size(SizeInfo, glm::vec3(1.f, 1.f, 1.f), glm::vec3(0.f), glm::vec3(10.f))
    , _scalingExponent(ScalingExponentInfo, 1, -10, 20)
    , _stepSize(StepSizeInfo, 0.02f, 0.01f, 1.f )
    , _translation(TranslationInfo, glm::vec3(0.f), glm::vec3(0.f), glm::vec3(10.f))
    , _rotation(
        RotationInfo,
        glm::vec3(0.f),
        glm::vec3(0.f),
        glm::vec3(glm::two_pi<float>())
    )
    , _color(ColorInfo, glm::vec4(1.f, 0.f, 0.f, 0.1f), glm::vec4(0.f), glm::vec4(1.f))
    , _downScaleVolumeRendering(DownscaleVolumeRenderingInfo, 1.f, 0.1f, 1.f)
{
    if (dictionary.hasKeyAndValue<double>(ScalingExponentInfo.identifier)) {
        _scalingExponent = static_cast<int>(
            dictionary.value<double>(ScalingExponentInfo.identifier)
        );
    }

    if (dictionary.hasKeyAndValue<glm::vec3>(SizeInfo.identifier)) {
        _size = dictionary.value<glm::vec3>(SizeInfo.identifier);
    }

    if (dictionary.hasKeyAndValue<glm::vec3>(TranslationInfo.identifier)) {
        _translation = dictionary.value<glm::vec3>(TranslationInfo.identifier);
    }

    if (dictionary.hasKeyAndValue<glm::vec3>(RotationInfo.identifier)) {
        _rotation = dictionary.value<glm::vec3>(RotationInfo.identifier);
    }

    if (dictionary.hasKeyAndValue<glm::vec4>(ColorInfo.identifier)) {
        _color = dictionary.value<glm::vec4>(ColorInfo.identifier);
    }

    if (dictionary.hasKeyAndValue<double>(StepSizeInfo.identifier)) {
        _stepSize = static_cast<float>(dictionary.value<double>(StepSizeInfo.identifier));
    }

    _downScaleVolumeRendering.setVisibility(
        openspace::properties::Property::Visibility::Developer
    );
    if (dictionary.hasKey("Downscale")) {
        _downScaleVolumeRendering = dictionary.value<float>("Downscale");
    }

    if (dictionary.hasKey("Steps")) {
        _rayCastSteps = static_cast<int>(dictionary.value<float>("Steps"));
    }
    else {
        LINFO("Number of raycasting steps not specified for ToyVolume."
            " Using default value.");
    }

}

RenderableToyVolume::~RenderableToyVolume() {}

void RenderableToyVolume::initializeGL() {
    _raycaster = std::make_unique<ToyVolumeRaycaster>(_color);
    _raycaster->initialize();

    global::raycasterManager.attachRaycaster(*_raycaster.get());

    std::function<void(bool)> onChange = [&](bool enabled) {
        if (enabled) {
            global::raycasterManager.attachRaycaster(*_raycaster.get());
        }
        else {
            global::raycasterManager.detachRaycaster(*_raycaster.get());
        }
    };

    onEnabledChange(onChange);

    addProperty(_size);
    addProperty(_scalingExponent);
    addProperty(_stepSize);
    addProperty(_translation);
    addProperty(_rotation);
    addProperty(_color);
    addProperty(_downScaleVolumeRendering);
}

void RenderableToyVolume::deinitializeGL() {
    if (_raycaster) {
        global::raycasterManager.detachRaycaster(*_raycaster.get());
        _raycaster = nullptr;
    }
}

bool RenderableToyVolume::isReady() const {
    // @TODO isReady function needs to be filled
    return true;
}

void RenderableToyVolume::update(const UpdateData& data) {
    if (_raycaster) {
        glm::mat4 transform = glm::translate(
            glm::mat4(1.f),
            static_cast<glm::vec3>(_translation) *
                std::pow(10.f, static_cast<float>(_scalingExponent))
        );
        glm::vec3 eulerRotation = static_cast<glm::vec3>(_rotation);
        transform = glm::rotate(transform, eulerRotation.x, glm::vec3(1.f, 0.f, 0.f));
        transform = glm::rotate(transform, eulerRotation.y, glm::vec3(0.f, 1.f, 0.f));
        transform = glm::rotate(transform, eulerRotation.z,  glm::vec3(0.f, 0.f, 1.f));

        transform = glm::scale(
            transform,
            static_cast<glm::vec3>(_size) *
                std::pow(10.f, static_cast<float>(_scalingExponent))
        );

        _raycaster->setColor(_color);
        _raycaster->setStepSize(_stepSize);
        _raycaster->setModelTransform(transform);
        _raycaster->setTime(data.time.j2000Seconds());
        _raycaster->setDownscaleRender(_downScaleVolumeRendering);
        _raycaster->setMaxSteps(_rayCastSteps);
    }
}

void RenderableToyVolume::render(const RenderData& data, RendererTasks& tasks) {
    RaycasterTask task { _raycaster.get(), data };
    tasks.raycasterTasks.push_back(task);
}

} // namespace openspace
