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

#include <openspace/interaction/joystickstate.h>

#include <openspace/interaction/inputstate.h>
#include <ghoul/logging/logmanager.h>

namespace openspace::interaction {

JoystickState::JoystickState(double scaleFactor)
    : velocity(scaleFactor, 1)
{}

void JoystickState::setFriction(double friction) {
    velocity.setFriction(friction);
}

void JoystickState::setVelocityScaleFactor(double scaleFactor) {
    velocity.setScaleFactor(scaleFactor);
}

JoystickStates::JoystickStates(double sensitivity, double velocityScaleFactor)
    : _sensitivity(sensitivity)
    , _globalRotationJoystickState(velocityScaleFactor)
    , _localRotationJoystickState(velocityScaleFactor)
    , _truckMovementJoystickState(velocityScaleFactor)
    , _localRollJoystickState(velocityScaleFactor)
    , _globalRollJoystickState(velocityScaleFactor)
{}

void JoystickStates::updateJoystickStatesFromInput(const InputState& inputState,
                                                   double deltaTime)
{
    // Rolling
    _globalRotationJoystickState.velocity.set(
        glm::dvec2(
            inputState.joystickAxis(0) * _sensitivity,
            inputState.joystickAxis(1) * _sensitivity
        ),
        deltaTime
    );

    // Zooming
    float zoomOut = (inputState.joystickAxis(4) + 1.f) / 2.f;
    float zoomIn = (inputState.joystickAxis(5) + 1.f) / 2.f;
    float zoom = -zoomOut + zoomIn;

    _truckMovementJoystickState.velocity.set(
        glm::dvec2(
            zoom * _sensitivity
        ),
        deltaTime
    );

    if (inputState.joystickButton(_rollToggleButton)) {
        _isInRollMode = !_isInRollMode;
    }

    if (_isInRollMode) {
        _globalRollJoystickState.velocity.set(
            glm::dvec2(
                -inputState.joystickAxis(2) * _sensitivity,
                inputState.joystickAxis(3) * _sensitivity
            ),
            deltaTime
        );

        _localRotationJoystickState.velocity.decelerate(deltaTime);
    }
    else {
        // Panning
        // The x-axis in panning is inverted
        _localRotationJoystickState.velocity.set(
            glm::dvec2(
                -inputState.joystickAxis(2) * _sensitivity,
                inputState.joystickAxis(3) * _sensitivity
            ),
            deltaTime
        );

        _globalRollJoystickState.velocity.decelerate(deltaTime);
    }
}

void JoystickStates::setRotationalFriction(double friction) {
    _localRotationJoystickState.setFriction(friction);
    _localRollJoystickState.setFriction(friction);
    _globalRollJoystickState.setFriction(friction);
}

void JoystickStates::setHorizontalFriction(double friction) {
    _globalRotationJoystickState.setFriction(friction);
}

void JoystickStates::setVerticalFriction(double friction) {
    _truckMovementJoystickState.setFriction(friction);
}

void JoystickStates::setSensitivity(double sensitivity) {
    _sensitivity = sensitivity;
}

void JoystickStates::setVelocityScaleFactor(double scaleFactor) {
    _globalRotationJoystickState.setVelocityScaleFactor(scaleFactor);
    _localRotationJoystickState.setVelocityScaleFactor(scaleFactor);
    _truckMovementJoystickState.setVelocityScaleFactor(scaleFactor);
    _localRollJoystickState.setVelocityScaleFactor(scaleFactor);
    _globalRollJoystickState.setVelocityScaleFactor(scaleFactor);
}

glm::dvec2 JoystickStates::globalRotationJoystickVelocity() const{
    return _globalRotationJoystickState.velocity.get();
}

glm::dvec2 JoystickStates::localRotationJoystickVelocity() const{
    return _localRotationJoystickState.velocity.get();
}

glm::dvec2 JoystickStates::truckMovementJoystickVelocity() const{
    return _truckMovementJoystickState.velocity.get();
}

glm::dvec2 JoystickStates::localRollJoystickVelocity() const{
    return _localRollJoystickState.velocity.get();
}

glm::dvec2 JoystickStates::globalRollJoystickVelocity() const{
    return _globalRollJoystickState.velocity.get();
}

} // namespace openspace::interaction
