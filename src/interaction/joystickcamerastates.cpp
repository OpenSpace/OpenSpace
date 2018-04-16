/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <openspace/engine/openspaceengine.h>
#include <openspace/interaction/inputstate.h>
#include <openspace/scripting/scriptengine.h>

namespace openspace::interaction {

JoystickCameraStates::JoystickCameraStates(double sensitivity, double velocityScaleFactor)
    : CameraInteractionStates(sensitivity, velocityScaleFactor)
{}

void JoystickCameraStates::updateStateFromInput(const InputState& inputState,
                                                double deltaTime)
{
    glm::dvec2 globalRotation;
    double zoom = 0.0;
    //glm::dvec2 truckMovement;
    glm::dvec2 localRoll;
    glm::dvec2 globalRoll;
    glm::dvec2 localRotation;

    for (int i = 0; i < JoystickInputState::MaxAxes; ++i) {
        AxisInformation t = _axisMapping[i];
        if (t.type == AxisType::None) {
            continue;
        }

        float value = inputState.joystickAxis(i);
        
        if (t.normalize) {
            value = (value + 1.f) / 2.f;
        }

        if (t.invert) {
            value *= -1.f;
        }

        value *= _sensitivity;

        switch (t.type) {
            case AxisType::None:
                break;
            case AxisType::OrbitX:
                globalRotation.x = value;
                break;
            case AxisType::OrbitY:
                globalRotation.y = value;
                break;
            case AxisType::ZoomIn:
                zoom += value;
                break;
            case AxisType::ZoomOut:
                zoom -= value;
                break;
            case AxisType::LocalRollX:
                localRoll.x = value;
                break;
            case AxisType::LocalRollY:
                localRoll.y = value;
                break;
            case AxisType::GlobalRollX:
                globalRoll.x = value;
                break;
            case AxisType::GlobalRollY:
                globalRoll.y = value;
                break;
            case AxisType::PanX:
                localRotation.x = value;
                break;
            case AxisType::PanY:
                localRotation.y = value;
                break;
        }
    }

    _globalRotationState.velocity.set(globalRotation, deltaTime);
    _truckMovementState.velocity.set(glm::dvec2(zoom), deltaTime);
    _localRollState.velocity.set(localRoll, deltaTime);
    _globalRollState.velocity.set(globalRoll, deltaTime);
    _localRotationState.velocity.set(localRotation, deltaTime);

    for (int i = 0; i < JoystickInputState::MaxButtons; ++i) {
        auto itRange = _buttonMapping.equal_range(i);
        for (auto it = itRange.first; it != itRange.second; ++it) {
            bool active = inputState.joystickInputStates().button(i, it->second.action);

            if (active) {
                OsEng.scriptEngine().queueScript(
                    it->second.command,
                    scripting::ScriptEngine::RemoteScripting(it->second.synchronization)
                );
            }
        }
    }
}

void JoystickCameraStates::setAxisMapping(int axis, AxisType mapping,
                                          AxisInvert shouldInvert,
                                          AxisNormalize shouldNormalize)
{
    ghoul_assert(axis < JoystickInputState::MaxAxes, "axis must be < MaxAxes");

    _axisMapping[axis] = { mapping, shouldInvert, shouldNormalize };
}

JoystickCameraStates::AxisInformation JoystickCameraStates::axisMapping(int axis) const {
    return _axisMapping[axis];
}

void JoystickCameraStates::bindButtonCommand(int button, std::string command,
                                             JoystickAction action,
                                             ButtonCommandRemote remote)
{
    _buttonMapping.insert({
        button,
        { std::move(command), action, remote }
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

namespace std {

std::string to_string(const openspace::interaction::JoystickCameraStates::AxisType& type)
{
    using T = openspace::interaction::JoystickCameraStates::AxisType;
    switch (type) {
        case T::None:        return "None";
        case T::OrbitX:      return "Orbit X";
        case T::OrbitY:      return "Orbit Y";
        case T::ZoomIn:      return "Zoom In";
        case T::ZoomOut:     return "Zoom Out";
        case T::LocalRollX:  return "LocalRoll X";
        case T::LocalRollY:  return "LocalRoll Y";
        case T::GlobalRollX: return "GlobalRoll X";
        case T::GlobalRollY: return "GlobalRoll Y";
        case T::PanX:        return "Pan X";
        case T::PanY:        return "Pan Y";
        default:             return "";
    }
}

} // namespace std

namespace ghoul {

template <>
openspace::interaction::JoystickCameraStates::AxisType from_string(
                                                                const std::string& string)
{
    using T = openspace::interaction::JoystickCameraStates::AxisType;
    
    static const std::map<std::string, T> Map = {
        { "None",         T::None },
        { "Orbit X",      T::OrbitX },
        { "Orbit Y",      T::OrbitY },
        { "Zoom In",      T::ZoomIn },
        { "Zoom Out",     T::ZoomOut },
        { "LocalRoll X",  T::LocalRollX },
        { "LocalRoll Y",  T::LocalRollY },
        { "GlobalRoll X", T::GlobalRollX },
        { "GlobalRoll Y", T::GlobalRollY },
        { "Pan X",        T::PanX },
        { "Pan Y",        T::PanY }
    };

    return Map.at(string);
}

} // namespace ghoul
