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

#include <openspace/navigation/orbitalnavigator/joystickcamerastates.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/interaction/interactionhandler.h>
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
    : OrbitalCameraStates(sensitivity, velocityScaleFactor)
{}

void JoystickCameraStates::updateVelocitiesFromInput(
                                           const JoystickInputStates& joystickInputStates,
                                                double deltaTime)
{
    // TODO: Move this. The joystick camera state thing should not have to care about this mode
    const OpenSpaceEngine::Mode mode = global::openSpaceEngine->currentMode();
    if (mode == OpenSpaceEngine::Mode::CameraPath ||
        mode == OpenSpaceEngine::Mode::SessionRecordingPlayback)
    {
        return;
    }

    UpdateStates deltaStates;

    for (const JoystickInputState& joystickInputState : joystickInputStates) {
        if (joystickInputState.name.empty()) {
            continue;
        }

        JoystickMapping* joystick = joystickMapping(joystickInputState.name);

        if (!joystick) {
            continue;
        }

        const int nAxes = joystickInputStates.numAxes(joystickInputState.name);
        for (int i = 0;
             i < std::min(nAxes, static_cast<int>(joystick->axisMapping.size()));
             i++)
        {
            AxisInformation axis = joystick->axisMapping[i];
            if (axis.type == AxisType::None) {
                continue;
            }

            const float rawValue = joystickInputStates.axis(joystickInputState.name, i);
            float value = rawValue;

            if (axis.isSticky) {
                value = rawValue - joystick->prevAxisValues[i];
                joystick->prevAxisValues[i] = rawValue;
            }

            if ((axis.joystickType == JoystickType::JoystickLike &&
                 std::abs(value) <= axis.deadzone) ||
                (
                    axis.joystickType == JoystickType::TriggerLike &&
                    value <= -1.f + axis.deadzone
                ))
            {
                continue;
            }

            if (axis.invert) {
                value *= -1.f;
            }

            if (axis.joystickType == JoystickType::TriggerLike ||
                axis.type == AxisType::Property)
            {
                value = (value + 1.f) / 2.f;
            }

            if (axis.type == AxisType::Property) {
                value = value * (axis.maxValue - axis.minValue)  + axis.minValue;
            }
            else {
                value = static_cast<float>(value * _sensitivity);
                if (std::abs(axis.sensitivity) > std::numeric_limits<double>::epsilon()) {
                    value *= static_cast<float>(axis.sensitivity);
                }
            }

            if (axis.flip) {
                value = -value;
            }

            switch (axis.type) {
                case AxisType::None:
                    break;
                case AxisType::OrbitX:
                    if (!deltaStates.globalRotation.has_value()) {
                        deltaStates.globalRotation = glm::dvec2(0.0, 0.0);
                    }
                    (*deltaStates.globalRotation).x += value;
                    break;
                case AxisType::OrbitY:
                    if (!deltaStates.globalRotation.has_value()) {
                        deltaStates.globalRotation = glm::dvec2(0.0, 0.0);
                    }
                    (*deltaStates.globalRotation).y += value;
                    break;
                case AxisType::Zoom:
                case AxisType::ZoomIn:
                    if (!deltaStates.zoom.has_value()) {
                        deltaStates.zoom = 0.0;
                    }
                    (*deltaStates.zoom) += value;
                    break;
                case AxisType::ZoomOut:
                    if (!deltaStates.zoom.has_value()) {
                        deltaStates.zoom = 0.0;
                    }
                    (*deltaStates.zoom) -= value;
                    break;
                case AxisType::LocalRoll:
                    if (!deltaStates.localRoll.has_value()) {
                        deltaStates.localRoll = 0.0;
                    }
                    (*deltaStates.localRoll) += value;
                    break;
                case AxisType::GlobalRoll:
                    if (!deltaStates.globalRoll.has_value()) {
                        deltaStates.globalRoll = 0.0;
                    }
                    (*deltaStates.globalRoll) += value;
                    break;
                case AxisType::PanX:
                    if (!deltaStates.localRotation.has_value()) {
                        deltaStates.localRotation = glm::dvec2(0.0, 0.0);
                    }
                    (*deltaStates.localRotation).x += value;
                    break;
                case AxisType::PanY:
                    if (!deltaStates.localRotation.has_value()) {
                        deltaStates.localRotation = glm::dvec2(0.0, 0.0);
                    }
                    (*deltaStates.localRotation).y += value;
                    break;
                case AxisType::Property:
                    const std::string script = std::format(
                        "openspace.setPropertyValue('{}', {});",
                        axis.propertyUri, value
                    );

                    using Script = scripting::ScriptEngine::Script;
                    global::scriptEngine->queueScript({
                        .code = script,
                        .synchronized = Script::ShouldBeSynchronized(axis.isRemote),
                        .sendToRemote = Script::ShouldSendToRemote(axis.isRemote)
                    });
                    break;
            }
        }

        const int nButtons = joystickInputStates.numButtons(joystickInputState.name);
        for (int i = 0; i < nButtons; i++) {
            auto itRange = joystick->buttonMapping.equal_range(i);
            for (auto it = itRange.first; it != itRange.second; it++) {
                const bool active = global::interactionHandler->joystickInputStates().button(
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

    updateVelocities(deltaStates, deltaTime);
}

void JoystickCameraStates::setAxisMapping(const std::string& joystickName,
                                          int axis, AxisType mapping,
                                          AxisInvert shouldInvert,
                                          JoystickType joystickType,
                                          bool isSticky,
                                          AxisFlip shouldFlip,
                                          double sensitivity)
{
    JoystickMapping* joystickMapping = findOrAddJoystickMapping(joystickName);
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
        global::interactionHandler->joystickInputStates().axis(joystickName, axis);
}

void JoystickCameraStates::setAxisMappingProperty(const std::string& joystickName,
                                                  int axis,
                                                  std::string propertyUri,
                                                  float min, float max,
                                                  AxisInvert shouldInvert,
                                                  bool isRemote)
{
    JoystickMapping* joystickMapping = findOrAddJoystickMapping(joystickName);
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
        global::interactionHandler->joystickInputStates().axis(joystickName, axis);
}

JoystickCameraStates::AxisInformation JoystickCameraStates::axisMapping(
                                                          const std::string& joystickName,
                                                                        int axis) const
{
    const JoystickMapping* joystick = joystickMapping(joystickName);

    if (!joystick || axis >= static_cast<int>(joystick->axisMapping.size())) {
        JoystickCameraStates::AxisInformation dummy;
        return dummy;
    }

    return joystick->axisMapping[axis];
}

void JoystickCameraStates::setDeadzone(const std::string& joystickName, int axis,
                                       float deadzone)
{
    JoystickMapping* joystick = findOrAddJoystickMapping(joystickName);
    if (!joystick) {
        return;
    }

    // If the axis index is too big for the vector then resize it to have room
    if (axis >= static_cast<int>(joystick->axisMapping.size())) {
        joystick->axisMapping.resize(axis + 1);
        joystick->prevAxisValues.resize(axis + 1);
    }

    joystick->axisMapping[axis].deadzone = deadzone;
}

float JoystickCameraStates::deadzone(const std::string& joystickName, int axis) const {
    const JoystickMapping* joystick = joystickMapping(joystickName);
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
    JoystickMapping* joystickMapping = findOrAddJoystickMapping(joystickName);
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
    JoystickMapping* joystick = joystickMapping(joystickName);
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
    const JoystickMapping* joystick = joystickMapping(joystickName);
    if (!joystick) {
        return result;
    }

    auto itRange = joystick->buttonMapping.equal_range(button);
    for (auto it = itRange.first; it != itRange.second; it++) {
        result.push_back(it->second.command);
    }
    return result;
}

JoystickCameraStates::JoystickMapping* JoystickCameraStates::joystickMapping(
                                                          const std::string& joystickName)
{
    for (JoystickMapping& j : _joysticks) {
        if (j.name == joystickName) {
            return &j;
        }
    }
    return nullptr;
}

const JoystickCameraStates::JoystickMapping*
JoystickCameraStates::joystickMapping(const std::string& joystickName) const
{
    for (const JoystickMapping& j : _joysticks) {
        if (j.name == joystickName) {
            return &j;
        }
    }

    LWARNING(std::format("Cannot find Joystick with name '{}'", joystickName));
    return nullptr;
}

JoystickCameraStates::JoystickMapping*
JoystickCameraStates::findOrAddJoystickMapping(const std::string& joystickName)
{
    JoystickMapping* joystick = joystickMapping(joystickName);
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
