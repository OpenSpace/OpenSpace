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

#include <modules/server/include/topics/flightcontrollertopic.h>

#include <modules/server/include/connection.h>
#include <modules/server/include/jsonconverters.h>
#include <openspace/engine/globals.h>
#include <openspace/interaction/websocketcamerastates.h>
#include <openspace/interaction/websocketinputstate.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/orbitalnavigator.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scene.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/timemanager.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/fmt.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/lua/ghoul_lua.h>
#include <iterator>
#include <unordered_map>

namespace {
    enum class Command {
        Connect = 0,
        Disconnect,
        InputState,
        UpdateView,
        Autopilot,
        Friction,
        Lua
    };

    using AxisType = openspace::interaction::WebsocketCameraStates::AxisType;

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

using nlohmann::json;

namespace openspace {

FlightControllerTopic::FlightControllerTopic() {
    for (auto it = AxisIndexMap.begin(); it != AxisIndexMap.end(); it++) {
        global::navigationHandler->setWebsocketAxisMapping(
            int(std::distance(AxisIndexMap.begin(), it)),
            it->second
        );
    }

    // Add WebsocketInputState to global states
    (*global::websocketInputStates)[_topicId] = &_inputState;
}

FlightControllerTopic::~FlightControllerTopic() {
    // Reset global websocketInputStates
    global::websocketInputStates->erase(_topicId);
    *global::websocketInputStates = interaction::WebsocketInputStates();
}

bool FlightControllerTopic::isDone() const {
    if (_isDone) {
        disengageAutopilot();
    }
    return _isDone;
}

void FlightControllerTopic::handleJson(const nlohmann::json& json) {
    auto it = CommandMap.find(json[TypeKey]);
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
        const std::string j = json;
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

    const std::string focus = json.find(FocusKey) != json.end() ? json[FocusKey] : "";
    const std::string aim = json.find(AimKey) != json.end() ? json[AimKey] : "";
    const std::string anchor = json.find(AnchorKey) != json.end() ? json[AnchorKey] : "";

    const bool resetVelocities = json[ResetVelocitiesKey];
    const bool retargetAnchor = json[RetargetAnchorKey];
    const bool retargetAim = json[RetargetAimKey];

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
        const std::string j = json;
        LWARNING(
            std::format("Could not find '{}' key in JSON. JSON was:\n{}", FocusKey, j)
        );
        return;
    }

    const std::string name = json[RenderableKey][SceneNodeName];
    const bool enabled = json[RenderableKey][SceneNodeEnabled];

    const SceneGraphNode* node = global::renderEngine->scene()->sceneGraphNode(name);
    if (node && node->renderable() != nullptr) {
        node->renderable()->property(RenderableEnabled)->set(enabled);
    }
}

void FlightControllerTopic::disconnect() {
    // Reset global websocketInputStates
    global::websocketInputStates->erase(_topicId);
    *global::websocketInputStates = interaction::WebsocketInputStates();

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

void FlightControllerTopic::setFriction(bool roll, bool rotation, bool zoom) const {
    const interaction::OrbitalNavigator& navigator =
        global::navigationHandler->orbitalNavigator();

    navigator.property(RollFriction)->set(roll);
    navigator.property(RotationalFriction)->set(rotation);
    navigator.property(ZoomFriction)->set(zoom);

    // Update FlightController
    nlohmann::json j;
    j[TypeKey] = Friction;
    j[Friction][FrictionRollKey] = roll;
    j[Friction][FrictionRotationKey] = rotation;
    j[Friction][FrictionZoomKey] = zoom;
    _connection->sendJson(wrappedPayload(j));
}

void FlightControllerTopic::setFriction(const nlohmann::json& json) const {
    setFriction(json[FrictionRollKey], json[FrictionRotationKey], json[FrictionZoomKey]);
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
            if (it.key() != TypeKey || CommandMap.find(it.value()) == CommandMap.end()) {
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
    const bool engaged = json[AutopilotEngagedKey];

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

void FlightControllerTopic::processInputState(const nlohmann::json& json) {
    std::fill(_inputState.axes.begin(), _inputState.axes.end(), 0.f);
    _inputState.isConnected = true;

    // Get "inputState" object from "payload"
    auto input = json[InputState][ValuesKey];

    for (auto it = input.begin(); it != input.end(); it++) {
        const auto mapIt = AxisIndexMap.find(it.key());
        if (mapIt == AxisIndexMap.end()) {
            if (it.key() != TypeKey || CommandMap.find(it.value()) == CommandMap.end()) {
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
    const std::string script = json[LuaScript];
    global::scriptEngine->queueScript(
        script,
        scripting::ScriptEngine::ShouldBeSynchronized::Yes,
        scripting::ScriptEngine::ShouldSendToRemote::Yes
    );
}

} // namespace openspace
