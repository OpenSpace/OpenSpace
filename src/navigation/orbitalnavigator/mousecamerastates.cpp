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

#include <openspace/navigation/orbitalnavigator/mousecamerastates.h>

#include <openspace/interaction/mouseinputstate.h>
#include <openspace/interaction/keyboardinputstate.h>
#include <openspace/util/keys.h>
#include <openspace/util/mouse.h>
#include <algorithm>
#include <cstdlib>

namespace {
    constexpr double SensitivityAdjustmentIncrease = 8.0;
    constexpr double SensitivityAdjustmentDecrease = 0.5;
} // namespace

namespace openspace::interaction {

MouseCameraStates::MouseCameraStates(double sensitivity, double velocityScaleFactor)
    : OrbitalCameraStates(sensitivity, velocityScaleFactor)
{}

void MouseCameraStates::updateVelocitiesFromInput(const MouseInputState& mouseState,
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

    // @TODO (2025-12-19, emmbr) make this into a keybinding somehow? Or even break
    // out into the orbital navigator settings so that it can be applied to other input methods
    if (keyboardState.isKeyPressed(Key::Z)) {
        // Increase sensitivity ramp when Z is pressed
        _currentSensitivityRamp += deltaTime;
    }
    else if (keyboardState.isKeyPressed(Key::X)) {
        // The reverse for the X key
        _currentSensitivityRamp -= deltaTime;
    }
    else if (_currentSensitivityRamp != 0.0) {
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

    UpdateStates updateStates;

    // Update the mouse states
    if (primaryPressed && !keyShiftPressed && !keyAltPressed) {
        const glm::dvec2 mousePosDelta = _prevMousePos.primary - mousePosition;
        if (keyCtrlPressed) {
            updateStates.localRotation = mousePosDelta * _sensitivity;
        }
        else {
            updateStates.globalRotation = mousePosDelta *
                (_sensitivity + _sensitivity * totalSensitivity / 5.0);
        }
    }
    else {
        _prevMousePos.primary = mousePosition;
    }

    if (secondaryPressed || (keyAltPressed && primaryPressed)) {
        const glm::dvec2 mousePosDelta = _prevMousePos.secondary - mousePosition;
        updateStates.zoom = mousePosDelta.y * (_sensitivity + _sensitivity * totalSensitivity);
    }
    else {
        _prevMousePos.secondary = mousePosition;
    }

    if (button3Pressed || (keyShiftPressed && primaryPressed)) {
        const glm::dvec2 mousePosDelta = _prevMousePos.button3 - mousePosition;

        if (keyCtrlPressed) {
            updateStates.localRoll = mousePosDelta.x * _sensitivity;
        }
        else {
            updateStates.globalRoll = mousePosDelta.x * _sensitivity;
        }
    }
    else {
        _prevMousePos.button3 = mousePosition;
    }

    updateVelocities(updateStates, deltaTime);
}

void MouseCameraStates::setInvertMouseButton(bool value) {
    _isMouseButtonInverted = value;
}

} // namespace openspace::interaction
