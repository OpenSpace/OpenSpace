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

#include <openspace/interaction/interactionmode.h>
#include <openspace/interaction/interactionhandler.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scene.h>
#include <openspace/util/time.h>
#include <openspace/util/keys.h>

#include <ghoul/logging/logmanager.h>

#include <glm/gtx/quaternion.hpp>
#include <glm/gtx/vector_angle.hpp>

namespace {
    const std::string _loggerCat = "InteractionMode";
}

namespace openspace {
namespace interaction {

InteractionMode::InteractionMode() {
    // The transfer function is used here to get a different interpolation than the one
    // obtained from newValue = lerp(0, currentValue, dt). That one will result in an
    // exponentially decreasing value but we want to be able to control it. Either as
    // a linear interpolation or a smooth step interpolation. Therefore we use
    // newValue = lerp(0, currentValue * f(t) * dt) where f(t) is the transfer function
    // and lerp is a linear iterpolation
    // lerp(endValue, startValue, interpolationParameter).
    //
    // The transfer functions are derived from:
    // f(t) = d/dt ( ln(1 / f_orig(t)) ) where f_orig is the transfer function that would
    // be used if the interpolation was sinply linear between a start value and an end
    // value instead of current value and end value (0) as we use it when inerpoláting.
    // As an example f_orig(t) = 1 - t yields f(t) = 1 / (1 - t) which results in a linear
    // interpolation from 1 to 0.

    auto smoothStepDerivedTranferFunction = 
        [](double t) {
            return (6 * (t + t*t) / (1 - 3 * t*t + 2 * t*t*t));
        };
    auto linearDerivedTranferFunction = 
        [](double t) {
            return 1 / (1 - t);
        };
    _rotateToFocusNodeInterpolator.setTransferFunction(smoothStepDerivedTranferFunction);
}

InteractionMode::~InteractionMode() {

}

void InteractionMode::setFocusNode(SceneGraphNode* focusNode) {
    _focusNode = focusNode;

    if (_focusNode != nullptr) {
        _previousFocusNodePosition = _focusNode->worldPosition();
        _previousFocusNodeRotation = glm::quat_cast(_focusNode->worldRotationMatrix());
    }
}

SceneGraphNode* InteractionMode::focusNode() {
    return _focusNode;
}

void InteractionMode::startInterpolateCameraDirection(const Camera& camera) {
    glm::dvec3 camPos = camera.positionVec3();
    glm::dvec3 camDir = glm::normalize(camera.rotationQuaternion() * glm::dvec3(0, 0, -1));
    glm::dvec3 centerPos = _focusNode->worldPosition();
    glm::dvec3 directionToCenter = glm::normalize(centerPos - camPos);

    double angle = glm::angle(camDir, directionToCenter);

    _rotateToFocusNodeInterpolator.start(glm::max(angle * 4.0, 2.0));
}

} // namespace interaction
} // namespace openspace
