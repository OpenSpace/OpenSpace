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
        Connect = 0,
        Disconnect,
        InputState,
        UpdateView,
        Autopilot,
        Friction,
        Lua
    };

    using AxisType = WebsocketCameraStates::AxisType;

    constexpr std::string_view _loggerCat = "FlightControllerTopic";

    constexpr const char* TypeKey = "type";
    constexpr const char* ValuesKey = "values";

    // Disconnect JSON keys
    constexpr const char* SuccessKey = "success";

    // Connect JSON keys
    constexpr const char* FocusNodesKey = "focusNodes";
    constexpr const char* AllNodesKey = "allNodes";
    constexpr const char* InterestingTimesKey = "interestingTimes";

    // Change focus JSON keys
    constexpr const char* FocusKey = "focus";
    constexpr const char* AnchorKey = "anchor";
    constexpr const char* AimKey = "aim";
    constexpr const char* ResetVelocitiesKey = "resetVelocities";
    constexpr const char* RetargetAnchorKey = "retargetAnchor";
    constexpr const char* RetargetAimKey = "retargetAim";
    constexpr const char* SceneNodeName = "identifier";
    constexpr const char* SceneNodeEnabled = "enabled";

    constexpr const char* RenderableKey = "renderable";
    constexpr const char* RenderableEnabled = "Enabled";

    // Autopilot JSON keys
    constexpr const char* AutopilotEngagedKey = "engaged";
    constexpr const char* AutopilotInputKey = "autopilotInput";

    // Friction JSON Keys
    constexpr const char* FrictionRotationKey = "rotation";
    constexpr const char* FrictionZoomKey = "zoom";
    constexpr const char* FrictionRollKey = "roll";
    constexpr const char* RotationalFriction = "Friction.RotationalFriction";
    constexpr const char* ZoomFriction = "Friction.ZoomFriction";
    constexpr const char* RollFriction = "Friction.RollFriction";

    // Friction Lua Keys
    constexpr const char* LuaScript = "script";

    constexpr const char* OrbitX = "orbitX";
    constexpr const char* OrbitY = "orbitY";
    constexpr const char* ZoomIn = "zoomIn";
    constexpr const char* ZoomOut = "zoomOut";
    constexpr const char* LocalRollX = "localRollX";
    constexpr const char* LocalRollY = "localRollY";
    constexpr const char* GlobalRollX = "globalRollX";
    constexpr const char* GlobalRollY = "globalRollY";
    constexpr const char* PanX = "panX";
    constexpr const char* PanY = "panY";

    constexpr const char* Connect = "connect";
    constexpr const char* Disconnect = "disconnect";
    constexpr const char* InputState = "inputState";
    constexpr const char* UpdateView = "updateView";
    constexpr const char* Autopilot = "autopilot";
    constexpr const char* Friction = "friction";
    constexpr const char* Lua = "lua";

    const std::unordered_map<std::string, AxisType> AxisIndexMap ({
        { OrbitX, AxisType::OrbitX },
        { OrbitY, AxisType::OrbitY },
        { ZoomIn, AxisType::ZoomIn },
        { ZoomOut, AxisType::ZoomOut },
        { LocalRollX, AxisType::LocalRollX },
        { LocalRollY, AxisType::LocalRollY },
        { GlobalRollX, AxisType::GlobalRollX },
        { GlobalRollY, AxisType::GlobalRollY },
        { PanX, AxisType::PanX },
        { PanY, AxisType::PanY }
    });

    const std::unordered_map<std::string, Command> CommandMap ({
        { Connect, Command::Connect },
        { Disconnect, Command::Disconnect },
        { InputState, Command::InputState },
        { UpdateView, Command::UpdateView },
        { Autopilot, Command::Autopilot },
        { Friction, Command::Friction },
        { Lua, Command::Lua }
    });
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
}

FlightControllerTopic::~FlightControllerTopic() {
    // Reset global websocketInputStates
    global::interactionHandler->websocketInputStates().erase(_topicId);
}

bool FlightControllerTopic::isDone() const {
    if (_isDone) {
        disengageAutopilot();
    }
    return _isDone;
}

void FlightControllerTopic::handleJson(const nlohmann::json& json) {
    auto it = CommandMap.find(json["type"].get<std::string>());
    if (it == CommandMap.end()) {
        LWARNING(
            std::format("Malformed JSON command: no '{}' in payload", TypeKey)
        );
        return;
    }

    switch (it->second) {
        case Command::Connect:
            connect();
            break;
        case Command::Disconnect:
            disconnect();
            break;
        case Command::InputState:
            processInputState(json);
            break;
        case Command::UpdateView:
            updateView(json);
            break;
        case Command::Autopilot:
            handleAutopilot(json[Autopilot]);
            break;
        case Command::Friction:
            setFriction(json[Friction]);
            break;
        case Command::Lua:
            processLua(json[Lua]);
            break;
        default:
            LWARNING(std::format("Unrecognized action: {}", it->first));
            break;
    }
}

void FlightControllerTopic::connect() {
    _isDone = false;
    std::fill(_inputState.axes.begin(), _inputState.axes.end(), 0.f);
    _payload[TypeKey] = Connect;
    setFocusNodes();
    _payload[Connect][FocusNodesKey] = _focusNodes;
    _payload[Connect][AllNodesKey] = _allNodes;
    _payload[Connect][InterestingTimesKey] = _interestingTimes;
    _connection->sendJson(wrappedPayload(_payload));
}

void FlightControllerTopic::setFocusNodes() {
    // Get all scene nodes
    std::vector<SceneGraphNode*> nodes =
        global::renderEngine->scene()->allSceneGraphNodes();

    // Remove all nodes with no renderable
    nodes.erase(
        std::remove_if(
            nodes.begin(),
           nodes.end(),
           [](SceneGraphNode* node) { return node->renderable() == nullptr; }
        ),
        nodes.end()
    );

    // Sort them alphabetically
    std::sort(
        nodes.begin(),
        nodes.end(),
        [](SceneGraphNode* lhs, SceneGraphNode* rhs) {
            return lhs->guiName() < rhs->guiName();
        }
    );

    // Add to interesting nodes list and all nodes list
    for (SceneGraphNode* n : nodes) {
        // Set whether it's enabled
        const std::vector<std::string>& tags = n->tags();
        const auto it = std::find(tags.begin(), tags.end(), "GUI.Interesting");
        if (it != tags.end()) {
            _focusNodes[n->guiName()][SceneNodeName] = n->identifier();
            _focusNodes[n->guiName()][SceneNodeEnabled] = n->renderable()->isEnabled();
        }
        _allNodes[n->guiName()][SceneNodeName] = n->identifier();
        _allNodes[n->guiName()][SceneNodeEnabled] = n->renderable()->isEnabled();
    }
}

void FlightControllerTopic::updateView(const nlohmann::json& json) const {
    if (json.find(RenderableKey) != json.end()) {
        setRenderableEnabled(json);
    }
    else {
        changeFocus(json);
    }
}

void FlightControllerTopic::changeFocus(const nlohmann::json& json) const {
    if (json.find(FocusKey) == json.end()) {
        const std::string j = json.get<std::string>();
        LWARNING(
            std::format("Could not find '{}' key in JSON. JSON was:\n{}", FocusKey, j)
        );
        if (json.find(AimKey) == json.end()) {
            LWARNING(
                std::format("Could not find '{}' key in JSON. JSON was:\n{}", AimKey, j)
            );
            if (json.find(AnchorKey) == json.end()) {
                LWARNING(std::format(
                    "Could not find '{}' key in JSON. JSON was:\n{}", AnchorKey, j
                ));
                return;
            }
        }
    }

    const std::string focus =
        json.find(FocusKey) != json.end() ? json[FocusKey].get<std::string>() : "";
    const std::string aim =
        json.find(AimKey) != json.end() ? json[AimKey].get<std::string>() : "";
    const std::string anchor =
        json.find(AnchorKey) != json.end() ? json[AnchorKey].get<std::string>() : "";

    const bool resetVelocities = json[ResetVelocitiesKey].get<bool>();
    const bool retargetAnchor = json[RetargetAnchorKey].get<bool>();
    const bool retargetAim = json[RetargetAimKey].get<bool>();

    Scene* scene = global::renderEngine->scene();
    const SceneGraphNode* focusNode = scene->sceneGraphNode(focus);
    const SceneGraphNode* aimNode = scene->sceneGraphNode(aim);
    const SceneGraphNode* anchorNode = scene->sceneGraphNode(anchor);
    if (focusNode) {
        global::navigationHandler->orbitalNavigator().setFocusNode(
            focusNode,
            resetVelocities
        );
    }
    else {
        if (aimNode) {
            global::navigationHandler->orbitalNavigator().setAimNode(aim);
        }
        if (anchorNode) {
            global::navigationHandler->orbitalNavigator().setAnchorNode(anchor);
        }
    }

    if (retargetAnchor) {
        global::navigationHandler->orbitalNavigator().startRetargetAnchor();
    }
    if (retargetAim) {
        global::navigationHandler->orbitalNavigator().startRetargetAim();
    }
}

void FlightControllerTopic::setRenderableEnabled(const nlohmann::json& json) const {
    if (json[RenderableKey].find(SceneNodeName) == json[RenderableKey].end()) {
        const std::string j = json.get<std::string>();
        LWARNING(std::format(
            "Could not find '{}' key in JSON. JSON was:\n{}",
            SceneNodeName, j
        ));
        return;
    }

    const std::string name = json[RenderableKey][SceneNodeName].get<std::string>();
    const bool enabled = json[RenderableKey][SceneNodeEnabled].get<bool>();

    const SceneGraphNode* node = global::renderEngine->scene()->sceneGraphNode(name);
    if (node && node->renderable() != nullptr) {
        Property* prop = node->renderable()->property(RenderableEnabled);
        BoolProperty* boolProp = dynamic_cast<BoolProperty*>(prop);
        ghoul_assert(boolProp, "Enabled is not a boolean property");
        *boolProp = enabled;
    }
}

void FlightControllerTopic::disconnect() {
    // Reset global websocketInputStates
    global::interactionHandler->websocketInputStates().erase(_topicId);
    // @TODO (emmbr, 2026-04-02): This assumes only one state. Remove?
    //global::interactionHandler->websocketInputStates() = interaction::WebsocketInputStates();

    // Update FlightController
    nlohmann::json j;
    j[TypeKey] = Disconnect;
    j[Disconnect][SuccessKey] = true;
    _connection->sendJson(wrappedPayload(j));

    _isDone = true;
}

void FlightControllerTopic::setFriction(bool all) const {
    setFriction(all, all, all);
}

// @TODO (emmbr, 2025-12-17) Consider removing this function from the topic. It is just
// setting the properties in orbitalnavigator
void FlightControllerTopic::setFriction(bool roll, bool rotation, bool zoom) const {
    const OrbitalNavigator& navigator = global::navigationHandler->orbitalNavigator();

    Property* rollProp = navigator.property(RollFriction);
    BoolProperty* rollBoolProp = dynamic_cast<BoolProperty*>(rollProp);
    ghoul_assert(rollBoolProp, "RollFriction is not a boolean property");
    *rollBoolProp = roll;

    Property* rotProp = navigator.property(RotationalFriction);
    BoolProperty* rotBoolProp = dynamic_cast<BoolProperty*>(rotProp);
    ghoul_assert(rotBoolProp, "RotationFriction is not a boolean property");
    *rotBoolProp = rotation;

    Property* zoomProp = navigator.property(ZoomFriction);
    BoolProperty* zoomBoolProp = dynamic_cast<BoolProperty*>(zoomProp);
    ghoul_assert(zoomBoolProp, "ZoomFriction is not a boolean property");
    *zoomBoolProp = zoom;

    // Update FlightController
    nlohmann::json j;
    j[TypeKey] = Friction;
    j[Friction][FrictionRollKey] = roll;
    j[Friction][FrictionRotationKey] = rotation;
    j[Friction][FrictionZoomKey] = zoom;
    _connection->sendJson(wrappedPayload(j));
}

void FlightControllerTopic::setFriction(const nlohmann::json& json) const {
    setFriction(
        json[FrictionRollKey].get<bool>(),
        json[FrictionRotationKey].get<bool>(),
        json[FrictionZoomKey].get<bool>()
    );
}

void FlightControllerTopic::disengageAutopilot() const {
    setFriction(true);
}

void FlightControllerTopic::engageAutopilot(const nlohmann::json &json) {
    // Disable/enable friction
    setFriction(false);
    auto input = json[AutopilotInputKey][ValuesKey];

    std::fill(_inputState.axes.begin(), _inputState.axes.end(), 0.f);
    _inputState.isConnected = true;

    for (auto it = input.begin(); it != input.end(); it++) {
        const auto mapIt = AxisIndexMap.find(it.key());
        if (mapIt == AxisIndexMap.end()) {
            if (it.key() != TypeKey ||
                CommandMap.find(it->get<std::string>()) == CommandMap.end())
            {
                LWARNING(std::format(
                    "No axis, button, or command named '{}' (value: {})",
                    it.key(), static_cast<int>(it.value())
                ));
            }
            continue;
        }
        _inputState.axes[std::distance(AxisIndexMap.begin(), mapIt)] = float(it.value());
    }
}

void FlightControllerTopic::handleAutopilot(const nlohmann::json &json) {
    const bool engaged = json[AutopilotEngagedKey].get<bool>();

    if (engaged) {
        engageAutopilot(json);
    }
    else {
        disengageAutopilot();
    }
    _autopilotEngaged = engaged;

    nlohmann::json j;
    j[TypeKey] = Autopilot;
    j[Autopilot][AutopilotEngagedKey] = _autopilotEngaged;

    _connection->sendJson(wrappedPayload(j));
}

Schema FlightControllerTopic::Schema() {
    nlohmann::json schema = nlohmann::json::parse(R"(
        {
          "$defs": {
            "SceneNodeMap": {
              "type": "object",
              "additionalProperties": {
                "type": "object",
                "properties": {
                  "identifier": { "type": "string" },
                  "enabled": { "type": "boolean" }
                },
                "additionalProperties": false,
                "required": ["identifier", "enabled"]
              }
            },
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
            "FrictionPayload": {
              "type": "object",
              "properties": {
                "rotation": { "type": "boolean" },
                "zoom": { "type": "boolean" },
                "roll": { "type": "boolean" }
              },
              "additionalProperties": false,
              "required": ["rotation", "zoom", "roll"]
            },
            "FlightControllerConnectCommand": {
              "type": "object",
              "properties": {
                "type": { "const": "connect" }
              },
              "additionalProperties": false,
              "required": ["type"]
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
                "inputState": {
                  "type": "object",
                  "properties": {
                    "values": { "$ref": "#/$defs/AxisValues" }
                  },
                  "additionalProperties": false,
                  "required": ["values"]
                }
              },
              "additionalProperties": false,
              "required": ["type", "inputState"]
            },
            "FlightControllerUpdateViewCommand": {
              "type": "object",
              "anyOf": [
                {
                  "type": "object",
                  "properties": {
                    "type": { "const": "updateView" },
                    "renderable": {
                      "type": "object",
                      "properties": {
                        "identifier": { "type": "string" },
                        "enabled": { "type": "boolean" }
                      },
                      "additionalProperties": false,
                      "required": ["identifier", "enabled"]
                    }
                  },
                  "additionalProperties": false,
                  "required": ["type", "renderable"]
                },
                {
                  "type": "object",
                  "properties": {
                    "type": { "const": "updateView" },
                    "focus": { "type": "string" },
                    "anchor": { "type": "string" },
                    "aim": { "type": "string" },
                    "resetVelocities": { "type": "boolean" },
                    "retargetAnchor": { "type": "boolean" },
                    "retargetAim": { "type": "boolean" }
                  },
                  "additionalProperties": false,
                  "required": [
                    "type",
                    "resetVelocities",
                    "retargetAnchor",
                    "retargetAim"
                  ]
                }
              ]
            },
            "FlightControllerAutopilotCommand": {
              "type": "object",
              "properties": {
                "type": { "const": "autopilot" },
                "autopilot": {
                  "type": "object",
                  "properties": {
                    "engaged": { "type": "boolean" },
                    "autopilotInput": {
                      "type": "object",
                      "properties": {
                        "values": { "$ref": "#/$defs/AxisValues" }
                      },
                      "additionalProperties": false,
                      "required": ["values"]
                    }
                  },
                  "additionalProperties": false,
                  "required": ["engaged"]
                }
              },
              "additionalProperties": false,
              "required": ["type", "autopilot"]
            },
            "FlightControllerFrictionCommand": {
              "type": "object",
              "properties": {
                "type": { "const": "friction" },
                "friction": { "$ref": "#/$defs/FrictionPayload" }
              },
              "additionalProperties": false,
              "required": ["type", "friction"]
            },
            "FlightControllerLuaCommand": {
              "type": "object",
              "properties": {
                "type": { "const": "lua" },
                "lua": {
                  "type": "object",
                  "properties": {
                    "script": { "type": "string" }
                  },
                  "additionalProperties": false,
                  "required": ["script"]
                }
              },
              "additionalProperties": false,
              "required": ["type", "lua"]
            },
            "FlightControllerCommand": {
              "oneOf": [
                { "$ref": "#/$defs/FlightControllerConnectCommand" },
                { "$ref": "#/$defs/FlightControllerDisconnectCommand" },
                { "$ref": "#/$defs/FlightControllerInputStateCommand" },
                { "$ref": "#/$defs/FlightControllerUpdateViewCommand" },
                { "$ref": "#/$defs/FlightControllerAutopilotCommand" },
                { "$ref": "#/$defs/FlightControllerFrictionCommand" },
                { "$ref": "#/$defs/FlightControllerLuaCommand" }
              ]
            },
            "FlightControllerConnectData": {
              "type": "object",
              "properties": {
                "type": { "const": "connect" },
                "connect": {
                  "type": "object",
                  "properties": {
                    "focusNodes": { "$ref": "#/$defs/SceneNodeMap" },
                    "allNodes": { "$ref": "#/$defs/SceneNodeMap" },
                    "interestingTimes": {}
                  },
                  "additionalProperties": false,
                  "required": ["focusNodes", "allNodes", "interestingTimes"]
                }
              },
              "additionalProperties": false,
              "required": ["type", "connect"]
            },
            "FlightControllerDisconnectData": {
              "type": "object",
              "properties": {
                "type": { "const": "disconnect" },
                "disconnect": {
                  "type": "object",
                  "properties": {
                    "success": { "type": "boolean" }
                  },
                  "additionalProperties": false,
                  "required": ["success"]
                }
              },
              "additionalProperties": false,
              "required": ["type", "disconnect"]
            },
            "FlightControllerAutopilotData": {
              "type": "object",
              "properties": {
                "type": { "const": "autopilot" },
                "autopilot": {
                  "type": "object",
                  "properties": {
                    "engaged": { "type": "boolean" }
                  },
                  "additionalProperties": false,
                  "required": ["engaged"]
                }
              },
              "additionalProperties": false,
              "required": ["type", "autopilot"]
            },
            "FlightControllerFrictionData": {
              "type": "object",
              "properties": {
                "type": { "const": "friction" },
                "friction": { "$ref": "#/$defs/FrictionPayload" }
              },
              "additionalProperties": false,
              "required": ["type", "friction"]
            },
            "FlightControllerData": {
              "oneOf": [
                { "$ref": "#/$defs/FlightControllerConnectData" },
                { "$ref": "#/$defs/FlightControllerDisconnectData" },
                { "$ref": "#/$defs/FlightControllerAutopilotData" },
                { "$ref": "#/$defs/FlightControllerFrictionData" }
              ]
            }
          },
          "title": "FlightControllerTopic",
          "type": "object",
          "properties": {
            "topicId": { "const": "flightcontroller" },
            "topicPayload": { "$ref": "#/$defs/FlightControllerCommand" },
            "data": { "$ref": "#/$defs/FlightControllerData" }
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

    // Get "inputState" object from "payload"
    auto input = json[InputState][ValuesKey];

    for (auto it = input.begin(); it != input.end(); it++) {
        const auto mapIt = AxisIndexMap.find(it.key());
        if (mapIt == AxisIndexMap.end()) {
            if (it.key() != TypeKey ||
                CommandMap.find(it->get<std::string>()) == CommandMap.end())
            {
                LWARNING(std::format(
                    "No axis, button, or command named '{}' (value: {})",
                    it.key() , static_cast<int>(it.value())
                ));
            }
            continue;
        }
        _inputState.axes[std::distance(AxisIndexMap.begin(), mapIt)] = float(it.value());
    }
}

void FlightControllerTopic::processLua(const nlohmann::json &json) {
    const std::string script = json[LuaScript].get<std::string>();
    global::scriptEngine->queueScript(script);
}

} // namespace openspace
