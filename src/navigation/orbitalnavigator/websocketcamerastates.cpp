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

#include <openspace/navigation/orbitalnavigator/websocketcamerastates.h>

#include <ghoul/misc/assert.h>
#include <cmath>
#include <utility>

namespace openspace::interaction {

WebsocketCameraStates::WebsocketCameraStates(double sensitivity,
                                             double velocityScaleFactor)
    : OrbitalCameraStates(sensitivity, velocityScaleFactor)
{}

void WebsocketCameraStates::updateVelocitiesFromInput(
                                         const WebsocketInputStates& websocketInputStates,
                                                 double deltaTime)
{
    UpdateStates updateStates;

    if (!websocketInputStates.empty()) {
        for (int i = 0; i < WebsocketInputState::MaxAxes; i++) {
            const AxisInformation axis = _axisMapping[i];
            if (axis.type == AxisType::None) {
                continue;
            }

            float value = websocketInputStates.axis(i);
            const bool hasValue = std::fabs(value) > axis.deadzone;

            if (!hasValue) {
                continue;
            }

            if (axis.normalize) {
                value = (value + 1.f) / 2.f;
            }

            if (axis.invert) {
                value *= -1.f;
            }

            value = static_cast<float>(value * _sensitivity);

            switch (axis.type) {
                case AxisType::None:
                    break;
                case AxisType::OrbitX:
                    if (!updateStates.globalRotation.has_value()) {
                        updateStates.globalRotation = glm::dvec2(0.0, 0.0);
                    }
                    (*updateStates.globalRotation).x = value;
                    break;
                case AxisType::OrbitY:
                    if (!updateStates.globalRotation.has_value()) {
                        updateStates.globalRotation = glm::dvec2(0.0, 0.0);
                    }
                    (*updateStates.globalRotation).y = value;
                    break;
                case AxisType::ZoomIn:
                    if (!updateStates.zoom.has_value()) {
                        updateStates.zoom = 0.0;
                    }
                    (*updateStates.zoom) += value;
                    break;
                case AxisType::ZoomOut:
                    if (!updateStates.zoom.has_value()) {
                        updateStates.zoom = 0.0;
                    }
                    (*updateStates.zoom) -= value;
                    break;
                case AxisType::LocalRollX:
                case AxisType::LocalRollY:
                    if (!updateStates.localRoll.has_value()) {
                        updateStates.localRoll = 0.0;
                    }
                    (*updateStates.localRoll) = value;
                    break;
                case AxisType::GlobalRollX:
                case AxisType::GlobalRollY:
                    if (!updateStates.globalRoll.has_value()) {
                        updateStates.globalRoll = 0.0;
                    }
                    (*updateStates.globalRoll) = value;
                    break;
                case AxisType::PanX:
                    if (!updateStates.localRotation.has_value()) {
                        updateStates.localRotation = glm::dvec2(0.0, 0.0);
                    }
                    (*updateStates.localRotation).x = value;
                    break;
                case AxisType::PanY:
                    if (!updateStates.localRotation.has_value()) {
                        updateStates.localRotation = glm::dvec2(0.0, 0.0);
                    }
                    (*updateStates.localRotation).y = value;
                    break;
            }
        }
    }

    updateVelocities(updateStates, deltaTime);
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
            it++;
        }
    }
}

std::vector<std::string> WebsocketCameraStates::buttonCommand(int button) const {
    std::vector<std::string> result;
    auto itRange = _buttonMapping.equal_range(button);
    for (auto it = itRange.first; it != itRange.second; it++) {
        result.push_back(it->second.command);
    }
    return result;
}

} // namespace openspace::interaction
