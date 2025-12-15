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

#include <openspace/navigation/orbitalinputhandler.h>

#include <openspace/engine/globals.h>

namespace {
    constexpr std::string_view _loggerCat = "OrbitalInputHandler";

    constexpr openspace::properties::Property::PropertyInfo MouseSensitivityInfo = {
        "MouseSensitivity",
        "Mouse sensitivity",
        "Determines the sensitivity of the camera motion thorugh the mouse. The lower "
        "the sensitivity is the less impact a mouse motion will have.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo JoystickSensitivityInfo = {
        "JoystickSensitivity",
        "Joystick sensitivity",
        "Determines the sensitivity of the camera motion thorugh a joystick. The lower "
        "the sensitivity is the less impact a joystick motion will have.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo WebsocketSensitivityInfo = {
        "WebsocketSensitivity",
        "Websocket Sensitivity",
        "Determines the sensitivity of the camera motion thorugh a websocket. The lower "
        "the sensitivity is the less impact a webstick motion will have.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo InvertMouseButtons = {
        "InvertMouseButtons",
        "Invert left and right mouse buttons",
        "If this value is 'false', the left mouse button causes the camera to rotate "
        "around the object and the right mouse button causes the zooming motion. If this "
        "value is 'true', these two functionalities are reversed.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    double velocityScaleFromFriction(double friction) {
        return 1.0 / (friction + 0.0000001);
    }
} // namespace

namespace openspace::interaction {

OrbitalInputHandler::OrbitalInputHandler(double friction)
    : properties::PropertyOwner({"Input", "Input"})
    , _mouseSensitivity(MouseSensitivityInfo, 15.f, 1.f, 50.f)
    , _joystickSensitivity(JoystickSensitivityInfo, 10.f, 1.f, 50.f)
    , _websocketSensitivity(WebsocketSensitivityInfo, 5.f, 1.f, 50.f)
    , _invertMouseButtons(InvertMouseButtons, false)
    , _mouseStates(_mouseSensitivity * 0.0001, velocityScaleFromFriction(friction))
    , _joystickStates(_joystickSensitivity * 0.1, velocityScaleFromFriction(friction))
    , _websocketStates(_websocketSensitivity, velocityScaleFromFriction(friction))
{
    _mouseSensitivity.onChange([this]() {
        _mouseStates.setSensitivity(_mouseSensitivity * pow(10.0, -4));
    });
    _joystickSensitivity.onChange([this]() {
        _joystickStates.setSensitivity(_joystickSensitivity * 0.1);
    });
    _websocketSensitivity.onChange([this]() {
        _websocketStates.setSensitivity(_websocketSensitivity);
    });

    _invertMouseButtons.onChange([this]() {
        _mouseStates.setInvertMouseButton(_invertMouseButtons);
    });
    addProperty(_invertMouseButtons);

    addProperty(_mouseSensitivity);
    addProperty(_joystickSensitivity);
    addProperty(_websocketSensitivity);
}

JoystickCameraStates& OrbitalInputHandler::joystickStates() {
    return _joystickStates;
}

const JoystickCameraStates& OrbitalInputHandler::joystickStates() const {
    return _joystickStates;
}

WebsocketCameraStates& OrbitalInputHandler::websocketStates() {
    return _websocketStates;
}

const WebsocketCameraStates& OrbitalInputHandler::websocketStates() const {
    return _websocketStates;
}

ScriptCameraStates& OrbitalInputHandler::scriptStates() {
    return _scriptStates;
}

const ScriptCameraStates& OrbitalInputHandler::scriptStates() const {
    return _scriptStates;
}

bool OrbitalInputHandler::hasNonZeroVelocity() const {
    return _mouseStates.hasNonZeroVelocities() ||
        _joystickStates.hasNonZeroVelocities() ||
        _websocketStates.hasNonZeroVelocities() ||
        _scriptStates.hasNonZeroVelocities();
}

bool OrbitalInputHandler::hasTranslationalVelocity() const {
    return _mouseStates.hasNonZeroVelocities(true) ||
        _joystickStates.hasNonZeroVelocities(true) ||
        _websocketStates.hasNonZeroVelocities(true) ||
        _scriptStates.hasNonZeroVelocities(true);
}

void OrbitalInputHandler::resetVelocities() {
    _mouseStates.resetVelocities();
    _joystickStates.resetVelocities();
    _websocketStates.resetVelocities();
    _scriptStates.resetVelocities();
}

double OrbitalInputHandler::localRollVelocity() const {
    return _mouseStates.localRollVelocity() +
        _joystickStates.localRollVelocity() +
        _websocketStates.localRollVelocity() +
        _scriptStates.localRollVelocity();
}

double OrbitalInputHandler::globalRollVelocity() const {
    return _mouseStates.globalRollVelocity() +
        _joystickStates.globalRollVelocity() +
        _websocketStates.globalRollVelocity() +
        _scriptStates.globalRollVelocity();
}

glm::dvec2 OrbitalInputHandler::localRotationVelocity() const {
    return _mouseStates.localRotationVelocity() +
        _joystickStates.localRotationVelocity() +
        _websocketStates.localRotationVelocity() +
        _scriptStates.localRotationVelocity();
}

glm::dvec2 OrbitalInputHandler::globalRotationVelocity() const {
    return _mouseStates.globalRotationVelocity() +
        _joystickStates.globalRotationVelocity() +
        _websocketStates.globalRotationVelocity() +
        _scriptStates.globalRotationVelocity();
}

double OrbitalInputHandler::truckMovementVelocity() const {
    return _mouseStates.truckMovementVelocity() +
        _joystickStates.truckMovementVelocity() +
        _websocketStates.truckMovementVelocity() +
        _scriptStates.truckMovementVelocity();
}

// @TODO: Move mouse and keyboard input state handling out of here
void OrbitalInputHandler::updateStatesFromInput(const MouseInputState& mouseInputState,
                                             const KeyboardInputState& keyboardInputState,
                                                                         double deltaTime)
{
    _mouseStates.updateStateFromInput(mouseInputState, keyboardInputState, deltaTime);
    _joystickStates.updateStateFromInput(*global::joystickInputStates, deltaTime);
    _websocketStates.updateStateFromInput(*global::websocketInputStates, deltaTime);
    _scriptStates.updateStateFromInput(deltaTime);
}

void OrbitalInputHandler::updateFrictionFactor(double friction) {
    double scaleFactor = velocityScaleFromFriction(friction);
    _mouseStates.setVelocityScaleFactor(scaleFactor);
    _joystickStates.setVelocityScaleFactor(scaleFactor);
    _websocketStates.setVelocityScaleFactor(scaleFactor);
}

void OrbitalInputHandler::setRollFrictionEnabled(bool enabled) {
    _mouseStates.setRotationalFriction(enabled);
    _joystickStates.setRotationalFriction(enabled);
    _websocketStates.setRotationalFriction(enabled);
}

void OrbitalInputHandler::setRotationalFrictionEnabled(bool enabled) {
    _mouseStates.setHorizontalFriction(enabled);
    _joystickStates.setHorizontalFriction(enabled);
    _websocketStates.setHorizontalFriction(enabled);
}

void OrbitalInputHandler::setZoomFrictionEnabled(bool enabled) {
    _mouseStates.setVerticalFriction(enabled);
    _joystickStates.setVerticalFriction(enabled);
    _websocketStates.setVerticalFriction(enabled);
}

} // namespace openspace::interaction
