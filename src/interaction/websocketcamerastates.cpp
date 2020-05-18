/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#include <openspace/interaction/websocketcamerastates.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/interaction/inputstate.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/misc/stringconversion.h>
#include <utility>

namespace openspace::interaction {

WebsocketCameraStates::WebsocketCameraStates(double sensitivity,
                                             double velocityScaleFactor)
    : CameraInteractionStates(sensitivity, velocityScaleFactor)
{}

void WebsocketCameraStates::updateStateFromInput(const InputState& inputState,
                                                 double deltaTime)
{
    std::pair<bool, glm::dvec2> globalRotation = { false, glm::dvec2(0.0) };
    std::pair<bool, double> zoom = { false, 0.0 };
    std::pair<bool, glm::dvec2> localRoll = { false, glm::dvec2(0.0) };
    std::pair<bool, glm::dvec2> globalRoll = { false, glm::dvec2(0.0) };
    std::pair<bool, glm::dvec2> localRotation = { false, glm::dvec2(0.0) };

    if (inputState.hasWebsocketStates()) {
        for (int i = 0; i < WebsocketInputState::MaxAxes; ++i) {
            AxisInformation t = _axisMapping[i];
            if (t.type == AxisType::None) {
                continue;
            }

            float value = inputState.websocketAxis(i);
            bool hasValue = abs(value) > t.deadzone;

            if (!hasValue) {
                value = 0.f;
            }

            if (t.normalize) {
                value = (value + 1.f) / 2.f;
            }

            if (t.invert) {
                value *= -1.f;
            }

            value = static_cast<float>(value * _sensitivity);

            switch (t.type) {
                case AxisType::None:
                    break;
                case AxisType::OrbitX:
                    globalRotation.first = hasValue || globalRotation.first;
                    globalRotation.second.x = value;
                    break;
                case AxisType::OrbitY:
                    globalRotation.first = hasValue || globalRotation.first;
                    globalRotation.second.y = value;
                    break;
                case AxisType::ZoomIn:
                    zoom.first = hasValue || zoom.first;
                    zoom.second += value;
                    break;
                case AxisType::ZoomOut:
                    zoom.first = hasValue || zoom.first;
                    zoom.second -= value;
                    break;
                case AxisType::LocalRollX:
                    localRoll.first = hasValue || localRoll.first;
                    localRoll.second.x = value;
                    break;
                case AxisType::LocalRollY:
                    localRoll.first = hasValue || localRoll.first;
                    localRoll.second.y = value;
                    break;
                case AxisType::GlobalRollX:
                    globalRoll.first = hasValue || globalRoll.first;
                    globalRoll.second.x = value;
                    break;
                case AxisType::GlobalRollY:
                    globalRoll.first = hasValue || globalRoll.first;
                    globalRoll.second.y = value;
                    break;
                case AxisType::PanX:
                    localRotation.first = hasValue || localRotation.first;
                    localRotation.second.x = value;
                    break;
                case AxisType::PanY:
                    localRotation.first = hasValue || localRotation.first;
                    localRotation.second.y = value;
                    break;
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

void WebsocketCameraStates::setAxisMapping(int axis, AxisType mapping,
                                          AxisInvert shouldInvert,
                                          AxisNormalize shouldNormalize)
{
    ghoul_assert(axis < WebsocketInputState::MaxAxes, "axis must be < MaxAxes");

    _axisMapping[axis].type = mapping;
    _axisMapping[axis].invert = shouldInvert;
    _axisMapping[axis].normalize = shouldNormalize;
}

WebsocketCameraStates::AxisInformation WebsocketCameraStates::axisMapping(int axis) const
{
    return _axisMapping[axis];
}

void WebsocketCameraStates::setDeadzone(int axis, float deadzone) {
    _axisMapping[axis].deadzone = deadzone;
}

float WebsocketCameraStates::deadzone(int axis) const {
    return _axisMapping[axis].deadzone;
}

void WebsocketCameraStates::bindButtonCommand(int button, std::string command,
                                             WebsocketAction action,
                                             ButtonCommandRemote remote,
                                             std::string documentation)
{
    _buttonMapping.insert({
        button,
        { std::move(command), action, remote, std::move(documentation) }
    });
}

void WebsocketCameraStates::clearButtonCommand(int button) {
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

std::vector<std::string> WebsocketCameraStates::buttonCommand(int button) const {
    std::vector<std::string> result;
    auto itRange = _buttonMapping.equal_range(button);
    for (auto it = itRange.first; it != itRange.second; ++it) {
        result.push_back(it->second.command);
    }
    return result;
}


} // namespace openspace::interaction

namespace ghoul {

template <>
std::string to_string(const openspace::interaction::WebsocketCameraStates::AxisType& type)
{
    using T = openspace::interaction::WebsocketCameraStates::AxisType;
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

template <>
openspace::interaction::WebsocketCameraStates::AxisType from_string(
                                                                const std::string& string)
{
    using T = openspace::interaction::WebsocketCameraStates::AxisType;

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
