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

#include <openspace/navigation/orbitalnavigator/joystickcamerastates.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/logging/logmanager.h>
#include <algorithm>
#include <cstdlib>
#include <limits>
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

    // Pair of "was changed" and new value. @TODO: Bake into generic function
    std::pair<bool, glm::dvec2> globalRotation = std::pair(false, glm::dvec2(0.0));
    std::pair<bool, double> zoom = std::pair(false, 0.0);
    std::pair<bool, double> localRoll = std::pair(false, 0.0);
    std::pair<bool, double> globalRoll = std::pair(false, 0.0);
    std::pair<bool, glm::dvec2> localRotation = std::pair(false, glm::dvec2(0.0));

    for (const JoystickInputState& joystickInputState : joystickInputStates) {
        if (joystickInputState.name.empty()) {
            continue;
        }

        Joystick* joystick = joystickMapping(joystickInputState.name);

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

                    using Script = scripting::ScriptEngine::Script;
                    global::scriptEngine->queueScript({
                        .code = script,
                        .synchronized = Script::ShouldBeSynchronized(t.isRemote),
                        .sendToRemote = Script::ShouldSendToRemote(t.isRemote)
                    });
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
                    using Script = scripting::ScriptEngine::Script;
                    global::scriptEngine->queueScript({
                        .code = it->second.command,
                        .synchronized = Script::ShouldBeSynchronized(
                            it->second.synchronization
                        ),
                        .sendToRemote = Script::ShouldSendToRemote(
                            it->second.synchronization
                        )
                    });
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
        _truckMovementState.velocity.set(zoom.second, deltaTime);
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
}

void JoystickCameraStates::setAxisMapping(const std::string& joystickName,
                                          int axis, AxisType mapping,
                                          AxisInvert shouldInvert,
                                          JoystickType joystickType,
                                          bool isSticky,
                                          AxisFlip shouldFlip,
                                          double sensitivity)
{
    Joystick* joystickMapping = findOrAddJoystickMapping(joystickName);
    if (!joystickMapping) {
        return;
    }

    // If the axis index is too big for the vector then resize it to have room
    if (axis >= static_cast<int>(joystickMapping->axisMapping.size())) {
        joystickMapping->axisMapping.resize(axis + 1);
        joystickMapping->prevAxisValues.resize(axis + 1);
    }

    joystickMapping->axisMapping[axis].type = mapping;
    joystickMapping->axisMapping[axis].invert = shouldInvert;
    joystickMapping->axisMapping[axis].joystickType = joystickType;
    joystickMapping->axisMapping[axis].isSticky = isSticky;
    joystickMapping->axisMapping[axis].flip = shouldFlip;
    joystickMapping->axisMapping[axis].sensitivity = sensitivity;

    joystickMapping->prevAxisValues[axis] =
        global::joystickInputStates->axis(joystickName, axis);
}

void JoystickCameraStates::setAxisMappingProperty(const std::string& joystickName,
                                                  int axis,
                                                  std::string propertyUri,
                                                  float min, float max,
                                                  AxisInvert shouldInvert,
                                                  bool isRemote)
{
    Joystick* joystickMapping = findOrAddJoystickMapping(joystickName);
    if (!joystickMapping) {
        return;
    }

    // If the axis index is too big for the vector then resize it to have room
    if (axis >= static_cast<int>(joystickMapping->axisMapping.size())) {
        joystickMapping->axisMapping.resize(axis + 1);
        joystickMapping->prevAxisValues.resize(axis + 1);
    }

    joystickMapping->axisMapping[axis].type = AxisType::Property;
    joystickMapping->axisMapping[axis].invert = shouldInvert;
    joystickMapping->axisMapping[axis].propertyUri = std::move(propertyUri);
    joystickMapping->axisMapping[axis].minValue = min;
    joystickMapping->axisMapping[axis].maxValue = max;
    joystickMapping->axisMapping[axis].isRemote = isRemote;

    joystickMapping->prevAxisValues[axis] =
        global::joystickInputStates->axis(joystickName, axis);
}

JoystickCameraStates::AxisInformation JoystickCameraStates::axisMapping(
                                                          const std::string& joystickName,
                                                                        int axis) const
{
    const Joystick* joystick = joystickMapping(joystickName);
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
    Joystick* joystickMapping = findOrAddJoystickMapping(joystickName);
    if (!joystickMapping) {
        return;
    }

    // If the axis index is too big for the vector then resize it to have room
    if (axis >= static_cast<int>(joystickMapping->axisMapping.size())) {
        joystickMapping->axisMapping.resize(axis + 1);
        joystickMapping->prevAxisValues.resize(axis + 1);
    }

    joystickMapping->axisMapping[axis].deadzone = deadzone;
}

float JoystickCameraStates::deadzone(const std::string& joystickName, int axis) const {
    const Joystick* joystick = joystickMapping(joystickName);
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
    Joystick* joystickMapping = findOrAddJoystickMapping(joystickName);
    if (!joystickMapping) {
        return;
    }

    joystickMapping->buttonMapping.insert({
        button,
        { std::move(command), action, remote, std::move(documentation) }
    });
}

void JoystickCameraStates::clearButtonCommand(const std::string& joystickName,
                                              int button)
{
    Joystick* joystick = joystickMapping(joystickName);
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
    const Joystick* joystick = joystickMapping(joystickName);
    if (!joystick) {
        return result;
    }

    auto itRange = joystick->buttonMapping.equal_range(button);
    for (auto it = itRange.first; it != itRange.second; it++) {
        result.push_back(it->second.command);
    }
    return result;
}

JoystickCameraStates::Joystick* JoystickCameraStates::joystickMapping(
                                                          const std::string& joystickName)
{
    for (Joystick& j : _joysticks) {
        if (j.name == joystickName) {
            return &j;
        }
    }
    return nullptr;
}

const JoystickCameraStates::Joystick*
JoystickCameraStates::joystickMapping(const std::string& joystickName) const
{
    for (const Joystick& j : _joysticks) {
        if (j.name == joystickName) {
            return &j;
        }
    }

    LWARNING(std::format("Cannot find Joystick with name '{}'", joystickName));
    return nullptr;
}

JoystickCameraStates::Joystick*
JoystickCameraStates::findOrAddJoystickMapping(const std::string& joystickName)
{
    Joystick* joystick = joystickMapping(joystickName);
    if (!joystick) {
        if (_joysticks.size() < JoystickInputStates::MaxNumJoysticks) {
            _joysticks.emplace_back();
            joystick = &_joysticks.back();
            joystick->name = joystickName;
        }
        else {
            // @TODO (emmbr, 2025-12-16): It's super weird that we add a joystick mapping
            // the JoystickCameraStates, but check the maximum number of joysticks in the
            // JoystickInputStates. This should be refactored to be more intuitive.
            //
            // Maybe we should have some type of joystick handler? That handles all added
            // joysticks, and each joystick can be of a given type (e.g. orbital camera,
            // as in this case, or maybe something else like "free flight" in the future).
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
