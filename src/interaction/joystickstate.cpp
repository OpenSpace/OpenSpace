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

namespace openspace::interaction {

JoystickStates::JoystickStates(double sensitivity, double velocityScaleFactor)
    : InputDeviceStates(sensitivity, velocityScaleFactor)
{}

void JoystickStates::updateStateFromInput(const InputState& inputState, double deltaTime)
{
    // Rolling
    _globalRotationState.velocity.set(
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

    _truckMovementState.velocity.set(
        glm::dvec2(
            zoom * _sensitivity
        ),
        deltaTime
    );

    if (inputState.joystickButton(_rollToggleButton)) {
        _isInRollMode = !_isInRollMode;
    }

    if (_isInRollMode) {
        _globalRollState.velocity.set(
            glm::dvec2(
                -inputState.joystickAxis(2) * _sensitivity,
                inputState.joystickAxis(3) * _sensitivity
            ),
            deltaTime
        );

        _localRotationState.velocity.decelerate(deltaTime);
    }
    else {
        // Panning
        // The x-axis in panning is inverted
        _localRotationState.velocity.set(
            glm::dvec2(
                -inputState.joystickAxis(2) * _sensitivity,
                inputState.joystickAxis(3) * _sensitivity
            ),
            deltaTime
        );

        _globalRollState.velocity.decelerate(deltaTime);
    }
}

} // namespace openspace::interaction
