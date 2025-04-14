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
        "Threshold in meters for when the switch happens between the two renderables."
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
        ghoul::Dictionary renderableNear;
        ghoul::Dictionary renderableFar;

        std::optional<double> distanceThreshold;
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
    : Renderable(dictionary),
    _distanceThreshold(DistanceThresholdInfo, 1.f, 0.f, 1e25f)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _renderableNear = createFromDictionary(p.renderableNear);
    _renderableNear->setIdentifier("RenderableNear");
    _renderableNear->setGuiName("Renderable Near");
    addPropertySubOwner(_renderableNear.get());

    _renderableFar = createFromDictionary(p.renderableFar);
    _renderableFar->setIdentifier("RenderableFar");
    _renderableFar->setGuiName("Renderable Far");
    addPropertySubOwner(_renderableFar.get());

    _distanceThreshold = p.distanceThreshold.value_or(_distanceThreshold);
    addProperty(_distanceThreshold);

}

void RenderableSwitch::initializeGL() {
    ghoul_assert(_renderableNear, "No renderableNear");
    ghoul_assert(_renderableFar, "No renderableFar");
        
    _renderableNear->initializeGL();
    _renderableFar->initializeGL();
}

void RenderableSwitch::deinitializeGL() {
    ghoul_assert(_renderableNear, "No renderableNear");
    ghoul_assert(_renderableFar, "No renderableFar");

    _renderableNear->deinitializeGL();
    _renderableFar->deinitializeGL();
}

bool RenderableSwitch::isReady() const {
    return (_renderableNear->isReady() && _renderableFar->isReady());
}

void RenderableSwitch::update(const UpdateData& data) {
    _renderableNear->update(data);
    _renderableFar->update(data);
}

void RenderableSwitch::render(const RenderData& data, RendererTasks& tasks) {
    glm::dvec3 cameraPosition = data.camera.positionVec3();
    glm::dvec3 modelPosition = data.modelTransform.translation;

    if (glm::distance(cameraPosition, modelPosition) < _distanceThreshold) {
        _renderableNear->render(data, tasks);
    }
    else {
        _renderableFar->render(data, tasks);
    }
}
} // namespace openspace
