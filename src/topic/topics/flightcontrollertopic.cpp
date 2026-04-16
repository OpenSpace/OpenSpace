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

#include <openspace/topic/topics/flightcontrollertopic.h>

#include <openspace/documentation/schema.h>
#include <openspace/engine/globals.h>
#include <openspace/interaction/interactionhandler.h>
#include <openspace/interaction/websocketinputstate.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/orbitalnavigator/orbitalnavigator.h>
#include <openspace/navigation/orbitalnavigator/websocketcamerastates.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scene.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/topic/connection.h>
#include <openspace/topic/jsonconverters.h>
#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/misc/assert.h>
#include <algorithm>
#include <iterator>
#include <string_view>
#include <unordered_map>
#include <vector>

using nlohmann::json;

namespace {
    using namespace openspace;

    enum class Command {
        Disconnect,
        InputState,
    };

    using AxisType = WebsocketCameraStates::AxisType;

    constexpr std::string_view _loggerCat = "FlightControllerTopic";

    const std::unordered_map<std::string, AxisType> AxisIndexMap ({
        { "orbitX", AxisType::OrbitX},
        { "orbitY", AxisType::OrbitY},
        { "zoomIn", AxisType::ZoomIn},
        { "zoomOut", AxisType::ZoomOut},
        { "localRollX", AxisType::LocalRollX},
        { "localRollY", AxisType::LocalRollY},
        { "globalRollX", AxisType::GlobalRollX},
        { "globalRollY", AxisType::GlobalRollY},
        { "panX", AxisType::PanX},
        { "panY", AxisType::PanY}
    });

    //const std::unordered_map<std::string, Command> CommandMap ({
    //    { Disconnect, Command::Disconnect },
    //    { InputState, Command::InputState },
    //});
} // namespace

namespace openspace {

FlightControllerTopic::FlightControllerTopic() {
    for (auto it = AxisIndexMap.begin(); it != AxisIndexMap.end(); it++) {
        global::navigationHandler->orbitalNavigator().websocketStates().setAxisMapping(
            static_cast<int>(std::distance(AxisIndexMap.begin(), it)),
            it->second
        );
    }

    // Add WebsocketInputState to global states
    global::interactionHandler->websocketInputStates()[_topicId] = &_inputState;
    std::fill(_inputState.axes.begin(), _inputState.axes.end(), 0.f);

}

FlightControllerTopic::~FlightControllerTopic() {
    // Reset global websocketInputStates
    global::interactionHandler->websocketInputStates().erase(_topicId);
}

bool FlightControllerTopic::isDone() const {
    return _isDone;
}

void FlightControllerTopic::handleJson(const nlohmann::json& json) {
    const std::string& event = json.at("event").get<std::string>();

    if (event == "disconnect") {
        disconnect();
    }
    else if (event == "inputState") {
        processInputState(json);
    }
}

void FlightControllerTopic::disconnect() {
    // Reset global websocketInputStates
    global::interactionHandler->websocketInputStates().erase(_topicId);
    // @TODO (emmbr, 2026-04-02): This assumes only one state. Remove?
    //global::interactionHandler->websocketInputStates() = interaction::WebsocketInputStates();
    _isDone = true;
}

Schema FlightControllerTopic::Schema() {
    nlohmann::json schema = nlohmann::json::parse(R"(
        {
          "$defs": {
            "AxisValues": {
              "type": "object",
              "properties": {
                "orbitX": { "type": "number" },
                "orbitY": { "type": "number" },
                "zoomIn": { "type": "number" },
                "zoomOut": { "type": "number" },
                "localRollX": { "type": "number" },
                "localRollY": { "type": "number" },
                "globalRollX": { "type": "number" },
                "globalRollY": { "type": "number" },
                "panX": { "type": "number" },
                "panY": { "type": "number" }
              },
              "additionalProperties": false
            },
            "FlightControllerDisconnectCommand": {
              "type": "object",
              "properties": {
                "type": { "const": "disconnect" }
              },
              "additionalProperties": false,
              "required": ["type"]
            },
            "FlightControllerInputStateCommand": {
              "type": "object",
              "properties": {
                "type": { "const": "inputState" },
                "inputState": { "$ref": "#/$defs/AxisValues" },
                  "additionalProperties": false,
                  "required": ["values"]
                }
              },
              "additionalProperties": false,
              "required": ["type", "inputState"]
            },
            "FlightControllerCommand": {
              "oneOf": [
                { "$ref": "#/$defs/FlightControllerDisconnectCommand" },
                { "$ref": "#/$defs/FlightControllerInputStateCommand" }
              ]
            }
          },
          "title": "FlightControllerTopic",
          "type": "object",
          "properties": {
            "topicId": { "const": "flightcontroller" },
            "topicPayload": { "$ref": "#/$defs/FlightControllerCommand" },
            "data": false
          },
          "additionalProperties": false,
          "required": ["topicId", "topicPayload", "data"]
        }
    )");

    return { "flightcontrollertopic", schema };
}

void FlightControllerTopic::processInputState(const nlohmann::json& json) {
    std::fill(_inputState.axes.begin(), _inputState.axes.end(), 0.f);
    _inputState.isConnected = true;

    nlohmann::json inputState = json.at("inputState");

    for (auto it = inputState.begin(); it != inputState.end(); it++) {
        const auto mapIt = AxisIndexMap.find(it.key());
        if (mapIt == AxisIndexMap.end() || !it.value().is_number()) {
            LWARNING(std::format("No axis, button, or command named '{}'", it.key()));
            continue;
        }
        float axisValue = it.value().get<float>();
        _inputState.axes[std::distance(AxisIndexMap.begin(), mapIt)] = axisValue;
    }
}

} // namespace openspace
