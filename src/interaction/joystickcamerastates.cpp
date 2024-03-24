/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
#include <openspace/engine/openspaceengine.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/exception.h>
#include <algorithm>
#include <cmath>
#include <utility>

namespace {
    constexpr std::string_view _loggerCat = "JoystickCameraStates";
} // namespace

namespace openspace::interaction {

JoystickCameraStates::JoystickCameraStates(double sensitivity, double velocityScaleFactor)
    : CameraInteractionStates(sensitivity, velocityScaleFactor)
{}

void JoystickCameraStates::updateStateFromInput(
                                           const JoystickInputStates& joystickInputStates,
                                                double deltaTime)
{
    const OpenSpaceEngine::Mode mode = global::openSpaceEngine->currentMode();
    if (mode == OpenSpaceEngine::Mode::CameraPath ||
        mode == OpenSpaceEngine::Mode::SessionRecordingPlayback)
    {
        return;
    }

    std::pair<bool, glm::dvec2> globalRotation = std::pair(false, glm::dvec2(0.0));
    std::pair<bool, double> zoom = std::pair(false, 0.0);
    std::pair<bool, double> localRoll = std::pair(false, 0.0);
    std::pair<bool, double> globalRoll = std::pair(false, 0.0);
    std::pair<bool, glm::dvec2> localRotation = std::pair(false, glm::dvec2(0.0));

    for (const JoystickInputState& joystickInputState : joystickInputStates) {
        if (joystickInputState.name.empty()) {
            continue;
        }

        JoystickCameraState* joystick = joystickCameraState(joystickInputState.name);

        if (!joystick) {
            continue;
        }

        const int nAxes = joystickInputStates.numAxes(joystickInputState.name);
        for (int i = 0;
             i < std::min(nAxes, static_cast<int>(joystick->axisMapping.size()));
             i++)
        {
            AxisInformation t = joystick->axisMapping[i];
            if (t.type == AxisType::None) {
                continue;
            }

            const float rawValue = joystickInputStates.axis(joystickInputState.name, i);
            float value = rawValue;

            if (t.isSticky) {
                value = rawValue - joystick->prevAxisValues[i];
                joystick->prevAxisValues[i] = rawValue;
            }

            if ((t.joystickType == JoystickType::JoystickLike &&
                 std::abs(value) <= t.deadzone) ||
                (
                    t.joystickType == JoystickType::TriggerLike &&
                    value <= -1.f + t.deadzone
                ))
            {
                continue;
            }

            if (t.invert) {
                value *= -1.f;
            }

            if (t.joystickType == JoystickType::TriggerLike ||
                t.type == AxisType::Property)
            {
                value = (value + 1.f) / 2.f;
            }

            if (t.type == AxisType::Property) {
                value = value * (t.maxValue - t.minValue)  + t.minValue;
            }
            else {
                if (std::abs(t.sensitivity) > std::numeric_limits<double>::epsilon()) {
                    value = static_cast<float>(value * t.sensitivity * _sensitivity);
                }
                else {
                    value = static_cast<float>(value * _sensitivity);
                }
            }

            if (t.flip) {
                value = -value;
            }

            switch (t.type) {
                case AxisType::None:
                    break;
                case AxisType::OrbitX:
                    globalRotation.first = true;
                    globalRotation.second.x += value;
                    break;
                case AxisType::OrbitY:
                    globalRotation.first = true;
                    globalRotation.second.y += value;
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
                case AxisType::LocalRoll:
                    localRoll.first = true;
                    localRoll.second+= value;
                    break;
                case AxisType::GlobalRoll:
                    globalRoll.first = true;
                    globalRoll.second += value;
                    break;
                case AxisType::PanX:
                    localRotation.first = true;
                    localRotation.second.x += value;
                    break;
                case AxisType::PanY:
                    localRotation.first = true;
                    localRotation.second.y += value;
                    break;
                case AxisType::Property:
                    const std::string script = std::format(
                        "openspace.setPropertyValue('{}', {});",
                        t.propertyUri, value
                    );

                    global::scriptEngine->queueScript(
                        script,
                        scripting::ScriptEngine::ShouldBeSynchronized(t.isRemote),
                        scripting::ScriptEngine::ShouldSendToRemote(t.isRemote)
                    );
                    break;
            }
        }

        const int nButtons = joystickInputStates.numButtons(joystickInputState.name);
        for (int i = 0; i < nButtons; i++) {
            auto itRange = joystick->buttonMapping.equal_range(i);
            for (auto it = itRange.first; it != itRange.second; it++) {
                const bool active = global::joystickInputStates->button(
                    joystickInputState.name,
                    i,
                    it->second.action
                );

                if (active) {
                    global::scriptEngine->queueScript(
                        it->second.command,
                        scripting::ScriptEngine::ShouldBeSynchronized(
                            it->second.synchronization
                        ),
                        scripting::ScriptEngine::ShouldSendToRemote(
                            it->second.synchronization
                        )
                    );
                }
            }
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
        _localRollState.velocity.set(glm::dvec2(localRoll.second), deltaTime);
    }
    else {
        _localRollState.velocity.decelerate(deltaTime);
    }

    if (globalRoll.first) {
        _globalRollState.velocity.set(glm::dvec2(globalRoll.second), deltaTime);
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
}

void JoystickCameraStates::setAxisMapping(const std::string& joystickName,
                                          int axis, AxisType mapping,
                                          AxisInvert shouldInvert,
                                          JoystickType joystickType,
                                          bool isSticky,
                                          AxisFlip shouldFlip,
                                          double sensitivity)
{
    JoystickCameraState* joystickCameraState = findOrAddJoystickCameraState(joystickName);
    if (!joystickCameraState) {
        return;
    }

    // If the axis index is too big for the vector then resize it to have room
    if (axis >= static_cast<int>(joystickCameraState->axisMapping.size())) {
        joystickCameraState->axisMapping.resize(axis + 1);
        joystickCameraState->prevAxisValues.resize(axis + 1);
    }

    joystickCameraState->axisMapping[axis].type = mapping;
    joystickCameraState->axisMapping[axis].invert = shouldInvert;
    joystickCameraState->axisMapping[axis].joystickType = joystickType;
    joystickCameraState->axisMapping[axis].isSticky = isSticky;
    joystickCameraState->axisMapping[axis].flip = shouldFlip;
    joystickCameraState->axisMapping[axis].sensitivity = sensitivity;

    joystickCameraState->prevAxisValues[axis] =
        global::joystickInputStates->axis(joystickName, axis);
}

void JoystickCameraStates::setAxisMappingProperty(const std::string& joystickName,
                                                  int axis,
                                                  std::string propertyUri,
                                                  float min, float max,
                                                  AxisInvert shouldInvert,
                                                  bool isRemote)
{
    JoystickCameraState* joystickCameraState = findOrAddJoystickCameraState(joystickName);
    if (!joystickCameraState) {
        return;
    }

    // If the axis index is too big for the vector then resize it to have room
    if (axis >= static_cast<int>(joystickCameraState->axisMapping.size())) {
        joystickCameraState->axisMapping.resize(axis + 1);
        joystickCameraState->prevAxisValues.resize(axis + 1);
    }

    joystickCameraState->axisMapping[axis].type = AxisType::Property;
    joystickCameraState->axisMapping[axis].invert = shouldInvert;
    joystickCameraState->axisMapping[axis].propertyUri = std::move(propertyUri);
    joystickCameraState->axisMapping[axis].minValue = min;
    joystickCameraState->axisMapping[axis].maxValue = max;
    joystickCameraState->axisMapping[axis].isRemote = isRemote;

    joystickCameraState->prevAxisValues[axis] =
        global::joystickInputStates->axis(joystickName, axis);
}

JoystickCameraStates::AxisInformation JoystickCameraStates::axisMapping(
                                                          const std::string& joystickName,
                                                                        int axis) const
{
    const JoystickCameraState* joystick = joystickCameraState(joystickName);
    if (!joystick) {
        JoystickCameraStates::AxisInformation dummy;
        return dummy;
    }

    if (axis >= static_cast<int>(joystick->axisMapping.size())) {
        JoystickCameraStates::AxisInformation dummy;
        return dummy;
    }

    return joystick->axisMapping[axis];
}

void JoystickCameraStates::setDeadzone(const std::string& joystickName, int axis,
                                       float deadzone)
{
    JoystickCameraState* joystickCameraState = findOrAddJoystickCameraState(joystickName);
    if (!joystickCameraState) {
        return;
    }

    // If the axis index is too big for the vector then resize it to have room
    if (axis >= static_cast<int>(joystickCameraState->axisMapping.size())) {
        joystickCameraState->axisMapping.resize(axis + 1);
        joystickCameraState->prevAxisValues.resize(axis + 1);
    }

    joystickCameraState->axisMapping[axis].deadzone = deadzone;
}

float JoystickCameraStates::deadzone(const std::string& joystickName, int axis) const {
    const JoystickCameraState* joystick = joystickCameraState(joystickName);
    if (!joystick) {
        return 0.f;
    }

    if (axis >= static_cast<int>(joystick->axisMapping.size())) {
        return 0.f;
    }

    return joystick->axisMapping[axis].deadzone;
}

void JoystickCameraStates::bindButtonCommand(const std::string& joystickName,
                                             int button, std::string command,
                                             JoystickAction action,
                                             ButtonCommandRemote remote,
                                             std::string documentation)
{
    JoystickCameraState* joystickCameraState = findOrAddJoystickCameraState(joystickName);
    if (!joystickCameraState) {
        return;
    }

    joystickCameraState->buttonMapping.insert({
        button,
        { std::move(command), action, remote, std::move(documentation) }
    });
}

void JoystickCameraStates::clearButtonCommand(const std::string& joystickName,
                                              int button)
{
    JoystickCameraState* joystick = joystickCameraState(joystickName);
    if (!joystick) {
        return;
    }

    for (auto it = joystick->buttonMapping.begin();
         it != joystick->buttonMapping.end();)
    {
        // If the current iterator is the button that we are looking for, delete it
        // (std::multimap::erase will return the iterator to the next element for us)
        if (it->first == button) {
            it = joystick->buttonMapping.erase(it);
        }
        else {
            it++;
        }
    }
}

std::vector<std::string> JoystickCameraStates::buttonCommand(
                                                          const std::string& joystickName,
                                                             int button) const
{
    std::vector<std::string> result;
    const JoystickCameraState* joystick = joystickCameraState(joystickName);
    if (!joystick) {
        return result;
    }

    auto itRange = joystick->buttonMapping.equal_range(button);
    for (auto it = itRange.first; it != itRange.second; it++) {
        result.push_back(it->second.command);
    }
    return result;
}

JoystickCameraStates::JoystickCameraState* JoystickCameraStates::joystickCameraState(
                                                          const std::string& joystickName)
{
    for (JoystickCameraState& joystickCameraState : _joystickCameraStates) {
        if (joystickCameraState.joystickName == joystickName) {
            return &joystickCameraState;
        }
    }

    return nullptr;
}

const JoystickCameraStates::JoystickCameraState*
JoystickCameraStates::joystickCameraState(const std::string& joystickName) const
{
    for (const JoystickCameraState& joystickCameraState : _joystickCameraStates) {
        if (joystickCameraState.joystickName == joystickName) {
            return &joystickCameraState;
        }
    }

    LWARNING(std::format("Cannot find JoystickCameraState with name '{}'", joystickName));
    return nullptr;
}

JoystickCameraStates::JoystickCameraState*
JoystickCameraStates::findOrAddJoystickCameraState(const std::string& joystickName)
{
    JoystickCameraState* joystick = joystickCameraState(joystickName);
    if (!joystick) {
        if (_joystickCameraStates.size() < JoystickInputStates::MaxNumJoysticks) {
            _joystickCameraStates.emplace_back();
            joystick = &_joystickCameraStates.back();
            joystick->joystickName = joystickName;
        }
        else {
            LWARNING(std::format(
                "Cannot add more joysticks, only {} joysticks are supported",
                JoystickInputStates::MaxNumJoysticks
            ));
            return nullptr;
        }
    }
    return joystick;
}


} // namespace openspace::interaction
