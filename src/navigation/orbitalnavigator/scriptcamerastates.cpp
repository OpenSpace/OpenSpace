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

#include <openspace/navigation/orbitalnavigator/scriptcamerastates.h>

namespace openspace::interaction {

ScriptCameraStates::ScriptCameraStates() : OrbitalCameraStates(1.0, 1.0) {}

void ScriptCameraStates::updateVelocitiesFromInput(double deltaTime) {
    if (_localRotation != glm::dvec2(0.0)) {
        _localRotationVelocity.set(
            _localRotation * _sensitivity,
            deltaTime
        );
        _localRotation = glm::dvec2(0.0);
    }
    else {
        _localRotationVelocity.decelerate(deltaTime);
    }

    if (_globalRotation != glm::dvec2(0.0)) {
        _globalRotationVelocity.set(
            _globalRotation * _sensitivity,
            deltaTime
        );
        _globalRotation = glm::dvec2(0.0);
    }
    else {
        _globalRotationVelocity.decelerate(deltaTime);
    }

    if (_truckMovement != 0.0) {
        _truckMovementVelocity.set(
            _truckMovement * _sensitivity,
            deltaTime
        );
        _truckMovement = 0.0;
    }
    else {
        _truckMovementVelocity.decelerate(deltaTime);
    }

    if (_localRoll != 0.0) {
        _localRollVelocity.set(
            _localRoll * _sensitivity,
            deltaTime
        );
        _localRoll = 0.0;
    }
    else {
        _localRollVelocity.decelerate(deltaTime);
    }

    if (_globalRoll != 0.0) {
        _globalRollVelocity.set(
            _globalRoll * _sensitivity,
            deltaTime
        );
        _globalRoll = 0.0;
    }
    else {
        _globalRollVelocity.decelerate(deltaTime);
    }
}

void ScriptCameraStates::addLocalRotation(const glm::dvec2& delta) {
    _localRotation += delta;
}

void ScriptCameraStates::addGlobalRotation(const glm::dvec2& delta) {
    _globalRotation += delta;
}

void ScriptCameraStates::addTruckMovement(double delta) {
    _truckMovement += delta;
}

void ScriptCameraStates::addLocalRoll(double delta) {
    _localRoll += delta;
}

void ScriptCameraStates::addGlobalRoll(double delta) {
    _globalRoll += delta;
}

} // namespace openspace::interaction
