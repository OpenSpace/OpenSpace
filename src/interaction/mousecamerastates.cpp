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

#include <openspace/interaction/mousecamerastates.h>

#include <openspace/interaction/inputstate.h>

namespace {
    const double SENSITIVITY_ADJUSTMENT_INCREASE = 8.0;
    const double SENSITIVITY_ADJUSTMENT_DECREASE = 0.5;
}

namespace openspace::interaction {

MouseCameraStates::MouseCameraStates(double sensitivity, double velocityScaleFactor)
    : CameraInteractionStates(sensitivity, velocityScaleFactor)
{}

void MouseCameraStates::updateStateFromInput(const InputState& inputState,
                                             double deltaTime)
{
    MouseButton primary =
        _isMouseButtonInverted ? MouseButton::Button2 : MouseButton::Button1;
    MouseButton secondary =
        _isMouseButtonInverted ? MouseButton::Button1 : MouseButton::Button2;

    glm::dvec2 mousePosition = inputState.mousePosition();

    bool primaryPressed = inputState.isMouseButtonPressed(primary);
    bool secondaryPressed = inputState.isMouseButtonPressed(secondary);
    bool button3Pressed = inputState.isMouseButtonPressed(MouseButton::Button3);
    bool keyCtrlPressed = inputState.isKeyPressed(Key::LeftControl) |
                          inputState.isKeyPressed(Key::RightControl);
    bool keyShiftPressed = inputState.isKeyPressed(Key::LeftShift) |
                           inputState.isKeyPressed(Key::RightShift);
    bool keyAltPressed = inputState.isKeyPressed(Key::LeftAlt) |
                         inputState.isKeyPressed(Key::RightAlt);

    // Update the mouse states
    if (primaryPressed && !keyShiftPressed && !keyAltPressed) {
        if (keyCtrlPressed) {
            glm::dvec2 mousePositionDelta = _localRotationState.previousPosition -
                                             mousePosition;
            _localRotationState.velocity.set(
                mousePositionDelta * _sensitivity,
                deltaTime
            );

            _globalRotationState.previousPosition = mousePosition;
            _globalRotationState.velocity.decelerate(deltaTime);
        }
        else {
            glm::dvec2 mousePositionDelta = _globalRotationState.previousPosition -
                                            mousePosition;
            _globalRotationState.velocity.set(
                mousePositionDelta * _sensitivity,
                deltaTime
            );

            _localRotationState.previousPosition = mousePosition;
            _localRotationState.velocity.decelerate(deltaTime);
        }
    }
    else { // !button1Pressed
        _localRotationState.previousPosition = mousePosition;
        _localRotationState.velocity.decelerate(deltaTime);

        _globalRotationState.previousPosition = mousePosition;
        _globalRotationState.velocity.decelerate(deltaTime);
    }
    if (secondaryPressed || (keyAltPressed && primaryPressed)) {
        glm::dvec2 mousePositionDelta = _truckMovementState.previousPosition -
                                        mousePosition;

        double sensitivity = _sensitivity;
        if (inputState.isKeyPressed(Key::Z)) {
            sensitivity *= SENSITIVITY_ADJUSTMENT_INCREASE;
        }
        else if (inputState.isKeyPressed(Key::X)) {
            sensitivity *= SENSITIVITY_ADJUSTMENT_DECREASE;
        }

        _truckMovementState.velocity.set(
            mousePositionDelta * sensitivity,
            deltaTime
        );
    }
    else { // !button2Pressed
        _truckMovementState.previousPosition = mousePosition;
        _truckMovementState.velocity.decelerate(deltaTime);
    }
    if (button3Pressed || (keyShiftPressed && primaryPressed)) {
        if (keyCtrlPressed) {
            glm::dvec2 mousePositionDelta = _localRollState.previousPosition -
                                            mousePosition;
            _localRollState.velocity.set(
                mousePositionDelta * _sensitivity,
                deltaTime
            );

            _globalRollState.previousPosition = mousePosition;
            _globalRollState.velocity.decelerate(deltaTime);
        }
        else {
            glm::dvec2 mousePositionDelta = _globalRollState.previousPosition -
                                            mousePosition;
            _globalRollState.velocity.set(
                mousePositionDelta * _sensitivity,
                deltaTime
            );

            _localRollState.previousPosition = mousePosition;
            _localRollState.velocity.decelerate(deltaTime);
        }
    }
    else { // !button3Pressed
        _globalRollState.previousPosition = mousePosition;
        _globalRollState.velocity.decelerate(deltaTime);

        _localRollState.previousPosition = mousePosition;
        _localRollState.velocity.decelerate(deltaTime);
    }
}

void MouseCameraStates::setInvertMouseButton(bool value) {
    _isMouseButtonInverted = value;
}

} // namespace openspace::interaction
