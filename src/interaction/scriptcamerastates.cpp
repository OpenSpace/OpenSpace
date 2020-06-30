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

#include <openspace/interaction/scriptcamerastates.h>

#include <openspace/interaction/inputstate.h>

namespace {
    const double SENSITIVITY_ADJUSTMENT_INCREASE = 8.0;
    const double SENSITIVITY_ADJUSTMENT_DECREASE = 0.5;
}

namespace openspace::interaction {

ScriptCameraStates::ScriptCameraStates() : CameraInteractionStates(1.0, 1.0) {}

void ScriptCameraStates::updateStateFromInput(const InputState&, double deltaTime) {
    if (_localRotation != glm::dvec2(0.0)) {
        _localRotationState.velocity.set(
            _localRotation * _sensitivity,
            deltaTime
        );
        _localRotation = glm::dvec2(0.0);
    }
    else {
        _localRotationState.velocity.decelerate(deltaTime);
    }

    if (_globalRotation != glm::dvec2(0.0)) {
        _globalRotationState.velocity.set(
            _globalRotation * _sensitivity,
            deltaTime
        );
        _globalRotation = glm::dvec2(0.0);
    }
    else {
        _globalRotationState.velocity.decelerate(deltaTime);
    }

    if (_truckMovement != glm::dvec2(0.0)) {
        _truckMovementState.velocity.set(
            _truckMovement * _sensitivity,
            deltaTime
        );
        _truckMovement = glm::dvec2(0.0);
    }
    else {
        _truckMovementState.velocity.decelerate(deltaTime);
    }

    if (_localRoll != glm::dvec2(0.0)) {
        _localRollState.velocity.set(
            _localRoll * _sensitivity,
            deltaTime
        );
        _localRoll = glm::dvec2(0.0);
    }
    else {
        _localRollState.velocity.decelerate(deltaTime);
    }

    if (_globalRoll != glm::dvec2(0.0)) {
        _globalRollState.velocity.set(
            _globalRoll * _sensitivity,
            deltaTime
        );
        _globalRoll = glm::dvec2(0.0);
    }
    else {
        _globalRollState.velocity.decelerate(deltaTime);
    }
}

void ScriptCameraStates::addLocalRotation(const glm::dvec2& delta) {
    _localRotation += delta;
}

void ScriptCameraStates::addGlobalRotation(const glm::dvec2& delta) {
    _globalRotation += delta;
}

void ScriptCameraStates::addTruckMovement(const glm::dvec2& delta) {
    _truckMovement += delta;
}

void ScriptCameraStates::addLocalRoll(const glm::dvec2& delta) {
    _localRoll += delta;
}

void ScriptCameraStates::addGlobalRoll(const glm::dvec2& delta) {
    _globalRoll += delta;
}

} // namespace openspace::interaction
