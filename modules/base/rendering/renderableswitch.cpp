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

#include <modules/base/rendering/renderableswitch.h>

#include <modules/base/basemodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/crc32.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/opengl/texture.h>
#include <fstream>
#include <optional>

namespace {
    constexpr openspace::properties::Property::PropertyInfo DistanceThresholdInfo = {
        "DistanceThreshold",
        "Distance Threshold",
        "Threshold in meters for when the switch happens between the two renderables.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    // A RenderableSwitch can be used to render one of two renderables depending on the
    // distance between the camera and the object's position.
    //
    // The two renderables are specified separately: `RenderableNear` and `RenderableFar`.
    // These can be any renderable types.
    //
    // The `DistanceThreshold` property determines which renderable will be shown.
    // If the camera is closer to the object than the threshold, `RenderableNear` is used,
    // otherwise, `RenderableFar` is rendered.
    struct [[codegen::Dictionary(RenderableSwitch)]] Parameters {
        // The renderable to show when the camera is closer to the object than the
        // threshold.
        std::optional<ghoul::Dictionary>
            renderableNear [[codegen::reference("renderable")]];

        // The renderable to show when the camera is further away than the threshold.
        std::optional<ghoul::Dictionary>
            renderableFar [[codegen::reference("renderable")]];

        // [[codegen::verbatim(DistanceThresholdInfo.description)]]
        std::optional<double> distanceThreshold [[codegen::greater(0.f)]];
    };
#include "renderableswitch_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableSwitch::Documentation() {
    return codegen::doc<Parameters>(
        "base_renderable_switch"
    );
}

RenderableSwitch::RenderableSwitch(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _distanceThreshold(DistanceThresholdInfo, 1.f, 0.f, 1e25f)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    if (!p.renderableNear.has_value() && !p.renderableFar.has_value()) {
        throw ghoul::RuntimeError(
            "Either a RenderableNear or a RenderableFar (or both) has to be provided, "
            "but omitting both is invalid."
        );
    }

    if (p.renderableNear.has_value()) {
        _renderableNear = createFromDictionary(*p.renderableNear);
        _renderableNear->setIdentifier("RenderableNear");
        _renderableNear->setGuiName("Renderable Near");
        addPropertySubOwner(_renderableNear.get());
    }

    if (p.renderableFar.has_value()) {
        _renderableFar = createFromDictionary(*p.renderableFar);
        _renderableFar->setIdentifier("RenderableFar");
        _renderableFar->setGuiName("Renderable Far");
        addPropertySubOwner(_renderableFar.get());
    }

    _distanceThreshold = p.distanceThreshold.value_or(_distanceThreshold);
    addProperty(_distanceThreshold);
}

void RenderableSwitch::initialize() {
    ghoul_assert(_renderableNear || _renderableFar, "No renderable");

    if (_renderableNear) {
        _renderableNear->initialize();
    }

    if (_renderableFar) {
        _renderableFar->initialize();
    }
}

void RenderableSwitch::deinitialize() {
    ghoul_assert(_renderableNear || _renderableFar, "No renderable");

    if (_renderableNear) {
        _renderableNear->deinitialize();
    }

    if (_renderableFar) {
        _renderableFar->deinitialize();
    }
}

void RenderableSwitch::initializeGL() {
    ghoul_assert(_renderableNear || _renderableFar, "No renderable");

    if (_renderableNear) {
        _renderableNear->initializeGL();
    }

    if (_renderableFar) {
        _renderableFar->initializeGL();
    }
}

void RenderableSwitch::deinitializeGL() {
    ghoul_assert(_renderableNear || _renderableFar, "No renderable");

    if (_renderableNear) {
        _renderableNear->deinitializeGL();
    }

    if (_renderableFar) {
        _renderableFar->deinitializeGL();
    }
}

bool RenderableSwitch::isReady() const {
    const bool near = _renderableNear ? _renderableNear->isReady() : true;
    const bool far = _renderableFar ? _renderableFar->isReady() : true;
    return near && far;
}

void RenderableSwitch::update(const UpdateData& data) {
    ghoul_assert(_renderableNear || _renderableFar, "No renderable");

    if (_renderableNear) {
        _renderableNear->update(data);
    }

    if (_renderableFar) {
        _renderableFar->update(data);
    }
}

void RenderableSwitch::render(const RenderData& data, RendererTasks& tasks) {
    glm::dvec3 cameraPosition = data.camera.positionVec3();
    glm::dvec3 modelPosition = data.modelTransform.translation;

    if (glm::distance(cameraPosition, modelPosition) < _distanceThreshold) {
        if (_renderableNear && _renderableNear->isEnabled()) {
            _renderableNear->render(data, tasks);
        }
    }
    else {
        if (_renderableFar && _renderableFar->isEnabled()) {
            _renderableFar->render(data, tasks);
        }
    }
}

} // namespace openspace
