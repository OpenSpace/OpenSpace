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

#include <openspace/interaction/mousecamerastates.h>

#include <openspace/interaction/mouseinputstate.h>
#include <openspace/interaction/keyboardinputstate.h>

namespace {
    constexpr double SensitivityAdjustmentIncrease = 8.0;
    constexpr double SensitivityAdjustmentDecrease = 0.5;
} // namespace

namespace openspace::interaction {

MouseCameraStates::MouseCameraStates(double sensitivity, double velocityScaleFactor)
    : CameraInteractionStates(sensitivity, velocityScaleFactor)
{}

void MouseCameraStates::updateStateFromInput(const MouseInputState& mouseState,
                                             const KeyboardInputState& keyboardState,
                                             double deltaTime)
{
    const MouseButton primary =
        _isMouseButtonInverted ? MouseButton::Button2 : MouseButton::Button1;
    const MouseButton secondary =
        _isMouseButtonInverted ? MouseButton::Button1 : MouseButton::Button2;

    const glm::dvec2 mousePosition = mouseState.mousePosition();

    const bool primaryPressed = mouseState.isMouseButtonPressed(primary);
    const bool secondaryPressed = mouseState.isMouseButtonPressed(secondary);
    const bool button3Pressed = mouseState.isMouseButtonPressed(MouseButton::Button3);
    const bool keyCtrlPressed =
        keyboardState.isKeyPressed(Key::LeftControl) ||
        keyboardState.isKeyPressed(Key::RightControl);
    const bool keyShiftPressed =
        keyboardState.isKeyPressed(Key::LeftShift) ||
        keyboardState.isKeyPressed(Key::RightShift);
    const bool keyAltPressed =
        keyboardState.isKeyPressed(Key::LeftAlt) ||
        keyboardState.isKeyPressed(Key::RightAlt);


    if (keyboardState.isKeyPressed(Key::Z)) {
        _currentSensitivityRamp += deltaTime;
    }

    // The reverse for the X key
    if (keyboardState.isKeyPressed(Key::X)) {
        _currentSensitivityRamp -= deltaTime;
    }

    if (!keyboardState.isKeyPressed(Key::Z) && !keyboardState.isKeyPressed(Key::X) &&
        _currentSensitivityRamp != 0.0)
    {
        // If neither key is pressed, the sensitivity ramp falls off by 90% every frame
        // when letting go of the key
        _currentSensitivityRamp = _currentSensitivityRamp * 0.9;
        if (std::abs(_currentSensitivityRamp) < 0.01) {
            _currentSensitivityRamp = 0.0;
        }
    }

    _currentSensitivityRamp = std::clamp(_currentSensitivityRamp, -1.0, 1.0);
    const double totalSensitivity =
        _currentSensitivityRamp < 0.0 ?
        _currentSensitivityRamp * SensitivityAdjustmentDecrease :
        _currentSensitivityRamp * SensitivityAdjustmentIncrease;

    // Update the mouse states
    if (primaryPressed && !keyShiftPressed && !keyAltPressed) {
        if (keyCtrlPressed) {
            const glm::dvec2 mousePositionDelta =
                _localRotationState.previousPosition - mousePosition;
            _localRotationState.velocity.set(
                mousePositionDelta * _sensitivity,
                deltaTime
            );

            _globalRotationState.previousPosition = mousePosition;
            _globalRotationState.velocity.decelerate(deltaTime);
        }
        else {
            const glm::dvec2 mousePositionDelta =
                _globalRotationState.previousPosition - mousePosition;
            _globalRotationState.velocity.set(
                mousePositionDelta * (_sensitivity + _sensitivity * totalSensitivity / 5),
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
        const glm::dvec2 mousePosDelta =
            _truckMovementState.previousPosition - mousePosition;

        _truckMovementState.velocity.set(
            mousePosDelta * (_sensitivity + _sensitivity * totalSensitivity),
            deltaTime
        );
    }
    else { // !button2Pressed
        _truckMovementState.previousPosition = mousePosition;
        _truckMovementState.velocity.decelerate(deltaTime);
    }
    if (button3Pressed || (keyShiftPressed && primaryPressed)) {
        if (keyCtrlPressed) {
            const glm::dvec2 mousePosDelta = _localRollState.previousPosition -
                                            mousePosition;
            _localRollState.velocity.set(
                mousePosDelta * _sensitivity,
                deltaTime
            );

            _globalRollState.previousPosition = mousePosition;
            _globalRollState.velocity.decelerate(deltaTime);
        }
        else {
            const glm::dvec2 mousePosDelta = _globalRollState.previousPosition -
                                            mousePosition;
            _globalRollState.velocity.set(
                mousePosDelta * _sensitivity,
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
