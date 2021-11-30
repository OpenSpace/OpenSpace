/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <openspace/interaction/joystickcamerastates.h>

#include <openspace/engine/globals.h>
#include <openspace/interaction/inputstate.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/misc/exception.h>
#include <cmath>
#include <utility>

namespace openspace::interaction {

JoystickCameraStates::JoystickCameraStates(double sensitivity, double velocityScaleFactor)
    : CameraInteractionStates(sensitivity, velocityScaleFactor)
{}

void JoystickCameraStates::updateStateFromInput(const InputState& inputState,
                                                double deltaTime)
{
    std::pair<bool, glm::dvec2> globalRotation = { false, glm::dvec2(0.0) };
    std::pair<bool, double> zoom = { false, 0.0 };
    std::pair<bool, glm::dvec2> localRoll = { false, glm::dvec2(0.0) };
    std::pair<bool, glm::dvec2> globalRoll = { false, glm::dvec2(0.0) };
    std::pair<bool, glm::dvec2> localRotation = { false, glm::dvec2(0.0) };

    for (int i = 0; i < JoystickInputState::MaxAxes; ++i) {
        AxisInformation t = _axisMapping[i];
        if (t.type == AxisType::None) {
            continue;
        }

        float rawValue = inputState.joystickAxis(i);
        float value = rawValue;

        if (t.isSticky) {
            value = rawValue - _prevAxisValues[i];
            _prevAxisValues[i] = rawValue;
        }

        if (std::fabs(value) <= t.deadzone) {
            continue;
        }

        if (t.normalize) {
            value = (value + 1.f) / 2.f;
        }

        if (t.invert) {
            value *= -1.f;
        }

        if (std::abs(t.sensitivity) > std::numeric_limits<double>::epsilon()) {
            value = static_cast<float>(value * t.sensitivity * _sensitivity);
        }
        else {
            value = static_cast<float>(value * _sensitivity);
        }

        switch (t.type) {
            case AxisType::None:
                break;
            case AxisType::OrbitX:
                globalRotation.first = true;
                globalRotation.second.x = value;
                break;
            case AxisType::OrbitY:
                globalRotation.first = true;
                globalRotation.second.y = value;
                break;
            case AxisType::Zoom:
            case AxisType::ZoomIn:
                zoom.first = true;
                zoom.second += value;
                break;
            case AxisType::ZoomOut:
                zoom.first = true;
                zoom.second -= value;
                break;
            case AxisType::LocalRollX:
                localRoll.first = true;
                localRoll.second.x = value;
                break;
            case AxisType::LocalRollY:
                localRoll.first = true;
                localRoll.second.y = value;
                break;
            case AxisType::GlobalRollX:
                globalRoll.first = true;
                globalRoll.second.x = value;
                break;
            case AxisType::GlobalRollY:
                globalRoll.first = true;
                globalRoll.second.y = value;
                break;
            case AxisType::PanX:
                localRotation.first = true;
                localRotation.second.x = value;
                break;
            case AxisType::PanY:
                localRotation.first = true;
                localRotation.second.y = value;
                break;
        }
    }

    if (globalRotation.first) {
        _globalRotationState.velocity.set(globalRotation.second, deltaTime);
    }
    else {
        _globalRotationState.velocity.decelerate(deltaTime);
    }

    if (zoom.first) {
        _truckMovementState.velocity.set(glm::dvec2(zoom.second), deltaTime);
    }
    else {
        _truckMovementState.velocity.decelerate(deltaTime);
    }

    if (localRoll.first) {
        _localRollState.velocity.set(localRoll.second, deltaTime);
    }
    else {
        _localRollState.velocity.decelerate(deltaTime);
    }

    if (globalRoll.first) {
        _globalRollState.velocity.set(globalRoll.second, deltaTime);
    }
    else {
        _globalRollState.velocity.decelerate(deltaTime);
    }

    if (localRotation.first) {
        _localRotationState.velocity.set(localRotation.second, deltaTime);
    }
    else {
        _localRotationState.velocity.decelerate(deltaTime);
    }

    for (int i = 0; i < JoystickInputState::MaxButtons; ++i) {
        auto itRange = _buttonMapping.equal_range(i);
        for (auto it = itRange.first; it != itRange.second; ++it) {
            bool active = global::joystickInputStates->button(i, it->second.action);

            if (active) {
                global::scriptEngine->queueScript(
                    it->second.command,
                    scripting::ScriptEngine::RemoteScripting(it->second.synchronization)
                );
            }
        }
    }
}

void JoystickCameraStates::setAxisMapping(int axis, AxisType mapping,
                                          AxisInvert shouldInvert,
                                          AxisNormalize shouldNormalize,
                                          bool isSticky,
                                          double sensitivity)
{
    ghoul_assert(axis < JoystickInputState::MaxAxes, "axis must be < MaxAxes");

    _axisMapping[axis].type = mapping;
    _axisMapping[axis].invert = shouldInvert;
    _axisMapping[axis].normalize = shouldNormalize;
    _axisMapping[axis].isSticky = isSticky;
    _axisMapping[axis].sensitivity = sensitivity;

    if (isSticky) {
        global::joystickInputStates->at(axis).isSticky = true;
    }

    _prevAxisValues[axis] = global::joystickInputStates->axis(axis);
}

JoystickCameraStates::AxisInformation JoystickCameraStates::axisMapping(int axis) const {
    return _axisMapping[axis];
}

void JoystickCameraStates::setDeadzone(int axis, float deadzone) {
    _axisMapping[axis].deadzone = deadzone;
}

float JoystickCameraStates::deadzone(int axis) const {
    return _axisMapping[axis].deadzone;
}

void JoystickCameraStates::bindButtonCommand(int button, std::string command,
                                             JoystickAction action,
                                             ButtonCommandRemote remote,
                                             std::string documentation)
{
    _buttonMapping.insert({
        button,
        { std::move(command), action, remote, std::move(documentation) }
    });
}

void JoystickCameraStates::clearButtonCommand(int button) {
    for (auto it = _buttonMapping.begin(); it != _buttonMapping.end();) {
        // If the current iterator is the button that we are looking for, delete it
        // (std::multimap::erase will return the iterator to the next element for us)
        if (it->first == button) {
            it = _buttonMapping.erase(it);
        }
        else {
            ++it;
        }
    }
}

std::vector<std::string> JoystickCameraStates::buttonCommand(int button) const {
    std::vector<std::string> result;
    auto itRange = _buttonMapping.equal_range(button);
    for (auto it = itRange.first; it != itRange.second; ++it) {
        result.push_back(it->second.command);
    }
    return result;
}


} // namespace openspace::interaction
