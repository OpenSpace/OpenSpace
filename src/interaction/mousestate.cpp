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

#include <openspace/interaction/mousestate.h>

namespace openspace::interaction {

MouseState::MouseState(double scaleFactor)
    : velocity(scaleFactor, 1)
    , previousPosition(0.0, 0.0)
{ }

void MouseState::setFriction(double friction) {
    velocity.setFriction(friction);
}

void MouseState::setVelocityScaleFactor(double scaleFactor) {
    velocity.setScaleFactor(scaleFactor);
}

MouseStates::MouseStates(double sensitivity, double velocityScaleFactor)
    : _sensitivity(sensitivity)
    , _globalRotationMouseState(velocityScaleFactor)
    , _localRotationMouseState(velocityScaleFactor)
    , _truckMovementMouseState(velocityScaleFactor)
    , _localRollMouseState(velocityScaleFactor)
    , _globalRollMouseState(velocityScaleFactor)
{ }

void MouseStates::updateMouseStatesFromInput(const InputState& inputState,
                                             double deltaTime)
{
    glm::dvec2 mousePosition = inputState.getMousePosition();

    bool button1Pressed = inputState.isMouseButtonPressed(MouseButton::Button1);
    bool button2Pressed = inputState.isMouseButtonPressed(MouseButton::Button2);
    bool button3Pressed = inputState.isMouseButtonPressed(MouseButton::Button3);
    bool keyCtrlPressed = inputState.isKeyPressed(Key::LeftControl);
    bool keyShiftPressed = inputState.isKeyPressed(Key::LeftShift);
    
    // Update the mouse states
    if (button1Pressed && !keyShiftPressed) {
        if (keyCtrlPressed) {
            glm::dvec2 mousePositionDelta =
                _localRotationMouseState.previousPosition - mousePosition;
            _localRotationMouseState.velocity.set(
                mousePositionDelta * _sensitivity, deltaTime);

            _globalRotationMouseState.previousPosition = mousePosition;
            _globalRotationMouseState.velocity.decelerate(deltaTime);
        }
        else {
            glm::dvec2 mousePositionDelta =
                _globalRotationMouseState.previousPosition - mousePosition;
            _globalRotationMouseState.velocity.set(
                mousePositionDelta * _sensitivity, deltaTime);

            _localRotationMouseState.previousPosition = mousePosition;
            _localRotationMouseState.velocity.decelerate(deltaTime);
        }
    }
    else { // !button1Pressed
        _localRotationMouseState.previousPosition = mousePosition;
        _localRotationMouseState.velocity.decelerate(deltaTime);

        _globalRotationMouseState.previousPosition = mousePosition;
        _globalRotationMouseState.velocity.decelerate(deltaTime);
    }
    if (button2Pressed) {
        glm::dvec2 mousePositionDelta =
            _truckMovementMouseState.previousPosition - mousePosition;
        _truckMovementMouseState.velocity.set(
            mousePositionDelta * _sensitivity, deltaTime);
    }
    else { // !button2Pressed
        _truckMovementMouseState.previousPosition = mousePosition;
        _truckMovementMouseState.velocity.decelerate(deltaTime);
    }
    if (button3Pressed || (keyShiftPressed && button1Pressed)) {
        if (keyCtrlPressed) {
            glm::dvec2 mousePositionDelta =
                _localRollMouseState.previousPosition - mousePosition;
            _localRollMouseState.velocity.set(
                mousePositionDelta * _sensitivity, deltaTime);

            _globalRollMouseState.previousPosition = mousePosition;
            _globalRollMouseState.velocity.decelerate(deltaTime);
        }
        else {
            glm::dvec2 mousePositionDelta =
                _globalRollMouseState.previousPosition - mousePosition;
            _globalRollMouseState.velocity.set(
                mousePositionDelta * _sensitivity, deltaTime);

            _localRollMouseState.previousPosition = mousePosition;
            _localRollMouseState.velocity.decelerate(deltaTime);
        }
    }
    else { // !button3Pressed
        _globalRollMouseState.previousPosition = mousePosition;
        _globalRollMouseState.velocity.decelerate(deltaTime);

        _localRollMouseState.previousPosition = mousePosition;
        _localRollMouseState.velocity.decelerate(deltaTime);
    }
}

void MouseStates::setRotationalFriction(double friction) {
    _localRotationMouseState.setFriction(friction);
    _localRollMouseState.setFriction(friction);
    _globalRollMouseState.setFriction(friction);
}

void MouseStates::setHorizontalFriction(double friction) {
    _globalRotationMouseState.setFriction(friction);
}

void MouseStates::setVerticalFriction(double friction) {
    _truckMovementMouseState.setFriction(friction);
}

void MouseStates::setSensitivity(double sensitivity) {
    _sensitivity = sensitivity;
}

void MouseStates::setVelocityScaleFactor(double scaleFactor) {
    _globalRotationMouseState.setVelocityScaleFactor(scaleFactor);
    _localRotationMouseState.setVelocityScaleFactor(scaleFactor);
    _truckMovementMouseState.setVelocityScaleFactor(scaleFactor);
    _localRollMouseState.setVelocityScaleFactor(scaleFactor);
    _globalRollMouseState.setVelocityScaleFactor(scaleFactor);
}

glm::dvec2 MouseStates::globalRotationMouseVelocity() const{
    return _globalRotationMouseState.velocity.get();
}

glm::dvec2 MouseStates::localRotationMouseVelocity() const{
    return _localRotationMouseState.velocity.get();
}

glm::dvec2 MouseStates::truckMovementMouseVelocity() const{
    return _truckMovementMouseState.velocity.get();
}

glm::dvec2 MouseStates::localRollMouseVelocity() const{
    return _localRollMouseState.velocity.get();
}

glm::dvec2 MouseStates::globalRollMouseVelocity() const{
    return _globalRollMouseState.velocity.get();
}

} // namespace openspace::interaction
