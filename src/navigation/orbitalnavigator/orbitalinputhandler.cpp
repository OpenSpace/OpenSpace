/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <openspace/navigation/orbitalnavigator/orbitalinputhandler.h>

#include <openspace/engine/globals.h>
#include <openspace/interaction/interactionhandler.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo MouseSensitivityInfo = {
        "MouseSensitivity",
        "Mouse sensitivity",
        "Determines the sensitivity of the camera motion through the mouse. The lower "
        "the sensitivity is the less impact a mouse motion will have.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo JoystickSensitivityInfo = {
        "JoystickSensitivity",
        "Joystick sensitivity",
        "Determines the sensitivity of the camera motion through a joystick. The lower "
        "the sensitivity is the less impact a joystick motion will have.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo WebsocketSensitivityInfo = {
        "WebsocketSensitivity",
        "Websocket sensitivity",
        "Determines the sensitivity of the camera motion through a websocket. The lower "
        "the sensitivity is the less impact a webstick motion will have.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo TouchSensitivityInfo = {
        "TouchSensitivity",
        "Touch sensitivity",
        "Determines the sensitivity of the camera motion through touch interaction. The "
        "lower the sensitivity is the less the impact from touch motion will be.",
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
    , _touchSensitivity(TouchSensitivityInfo, 1.f, 1.f, 50.f)
    , _mouseStates(_mouseSensitivity * 0.0001, velocityScaleFromFriction(friction))
    , _joystickStates(_joystickSensitivity * 0.1, velocityScaleFromFriction(friction))
    , _websocketStates(_websocketSensitivity, velocityScaleFromFriction(friction))
    , _touchStates(_touchSensitivity, velocityScaleFromFriction(friction))
{
    _mouseSensitivity.onChange([this]() {
        _mouseStates.setSensitivity(_mouseSensitivity * pow(10.0, -4));
    });
    addProperty(_mouseSensitivity);

    _joystickSensitivity.onChange([this]() {
        _joystickStates.setSensitivity(_joystickSensitivity * 0.1);
    });
    addProperty(_joystickSensitivity);

    _websocketSensitivity.onChange([this]() {
        _websocketStates.setSensitivity(_websocketSensitivity);
    });
    addProperty(_websocketSensitivity);

    _touchSensitivity.onChange([this]() {
        _touchStates.setSensitivity(_touchSensitivity);
    });
    addProperty(_touchSensitivity);
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

TouchCameraStates& OrbitalInputHandler::touchStates() {
    return _touchStates;
}

const TouchCameraStates& OrbitalInputHandler::touchStates() const {
    return _touchStates;
}

bool OrbitalInputHandler::hasNonZeroVelocity() const {
    return _mouseStates.hasNonZeroVelocities() ||
        _joystickStates.hasNonZeroVelocities() ||
        _websocketStates.hasNonZeroVelocities() ||
        _scriptStates.hasNonZeroVelocities() ||
        _touchStates.hasNonZeroVelocities();
}

bool OrbitalInputHandler::hasTranslationalVelocity() const {
    return _mouseStates.hasNonZeroVelocities(true) ||
        _joystickStates.hasNonZeroVelocities(true) ||
        _websocketStates.hasNonZeroVelocities(true) ||
        _scriptStates.hasNonZeroVelocities(true) ||
        _touchStates.hasNonZeroVelocities(true);
}

void OrbitalInputHandler::resetVelocities() {
    _mouseStates.resetVelocities();
    _joystickStates.resetVelocities();
    _websocketStates.resetVelocities();
    _scriptStates.resetVelocities();
    _touchStates.resetVelocities();
}

double OrbitalInputHandler::localRollVelocity() const {
    return _mouseStates.localRollVelocity() +
        _joystickStates.localRollVelocity() +
        _websocketStates.localRollVelocity() +
        _scriptStates.localRollVelocity() +
        _touchStates.localRollVelocity();
}

double OrbitalInputHandler::globalRollVelocity() const {
    return _mouseStates.globalRollVelocity() +
        _joystickStates.globalRollVelocity() +
        _websocketStates.globalRollVelocity() +
        _scriptStates.globalRollVelocity() +
        _touchStates.globalRollVelocity();
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
        _scriptStates.globalRotationVelocity() +
        _touchStates.globalRotationVelocity();
}

double OrbitalInputHandler::truckMovementVelocity() const {
    return _mouseStates.truckMovementVelocity() +
        _joystickStates.truckMovementVelocity() +
        _websocketStates.truckMovementVelocity() +
        _scriptStates.truckMovementVelocity() +
        _touchStates.truckMovementVelocity();
}

void OrbitalInputHandler::updateStatesFromInput(double deltaTime) {
    _touchStates.updateVelocitiesFromInput(
        global::interactionHandler->touchInputState(),
        deltaTime
    );

    // If touch input was detected, do not process mouse input. This avoids conflicts
    // between touch and mouse input on devices that support both (where a touch is
    // often also translated into a mouse input)
    if (global::interactionHandler->touchInputState().touchPoints().empty()) {
        _mouseStates.updateVelocitiesFromInput(
            global::interactionHandler->mouseInputState(),
            global::interactionHandler->keyboardInputState(),
            deltaTime
        );
    }

    _joystickStates.updateVelocitiesFromInput(
        global::interactionHandler->joystickInputStates(),
        deltaTime
    );

    _websocketStates.updateVelocitiesFromInput(
        global::interactionHandler->websocketInputStates(),
        deltaTime
    );

    _scriptStates.updateVelocitiesFromInput(deltaTime);
}

void OrbitalInputHandler::updateFrictionFactor(double friction) {
    double scaleFactor = velocityScaleFromFriction(friction);
    _mouseStates.setVelocityScaleFactor(scaleFactor);
    _joystickStates.setVelocityScaleFactor(scaleFactor);
    _websocketStates.setVelocityScaleFactor(scaleFactor);
    _touchStates.setVelocityScaleFactor(scaleFactor);
}

void OrbitalInputHandler::setRollFrictionEnabled(bool enabled) {
    _mouseStates.setRotationalFriction(enabled);
    _joystickStates.setRotationalFriction(enabled);
    _websocketStates.setRotationalFriction(enabled);
    _touchStates.setRotationalFriction(enabled);
}

void OrbitalInputHandler::setRotationalFrictionEnabled(bool enabled) {
    _mouseStates.setHorizontalFriction(enabled);
    _joystickStates.setHorizontalFriction(enabled);
    _websocketStates.setHorizontalFriction(enabled);
    _touchStates.setHorizontalFriction(enabled);
}

void OrbitalInputHandler::setZoomFrictionEnabled(bool enabled) {
    _mouseStates.setVerticalFriction(enabled);
    _joystickStates.setVerticalFriction(enabled);
    _websocketStates.setVerticalFriction(enabled);
    _touchStates.setVerticalFriction(enabled);
}

} // namespace openspace::interaction
