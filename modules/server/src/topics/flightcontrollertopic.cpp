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

#include <modules/server/include/topics/flightcontrollertopic.h>

#include <modules/server/include/connection.h>
#include <modules/server/include/jsonconverters.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/interaction/inputstate.h>
#include <openspace/interaction/websocketcamerastates.h>
#include <openspace/interaction/websocketinputstate.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scene.h>
#include <openspace/util/timemanager.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/fmt.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/lua/ghoul_lua.h>

#include <iterator>
#include <unordered_map>

namespace {
    constexpr const char* BasePathToken = "${BASE}";

    enum class Command {
        Connect = 0,
        Disconnect,
        InputState
    };

    using AxisType = openspace::interaction::WebsocketCameraStates::AxisType;

    constexpr const char* _loggerCat = "FlightControllerTopic";
    constexpr const char* TypeKey = "type";
    constexpr const char* ValuesKey = "values";
    constexpr const char* SuccessKey = "success";

    constexpr const char* FocusNodesKey = "focusNodes";

    constexpr const char* FlightControllerType = "flightcontroller";

    const std::string OrbitX = "orbitX";
    const std::string OrbitY = "orbitY";
    const std::string ZoomIn = "zoomIn";
    const std::string ZoomOut = "zoomOut";
    const std::string LocalRollX = "localRollX";
    const std::string LocalRollY = "localRollY";
    const std::string GlobalRollX = "globalRollX";
    const std::string GlobalRollY = "globalRollY";
    const std::string PanX = "panX";
    const std::string PanY = "panY";

    const std::string Connect = "connect";
    const std::string Disconnect = "disconnect";
    const std::string InputState = "inputState";

    const static std::unordered_map<std::string, AxisType> AxisIndexMap ({
        {OrbitX, AxisType::OrbitX},
        {OrbitY, AxisType::OrbitY},
        {ZoomIn, AxisType::ZoomIn},
        {ZoomOut, AxisType::ZoomOut},
        {LocalRollX, AxisType::LocalRollX},
        {LocalRollY, AxisType::LocalRollY},
        {GlobalRollX, AxisType::GlobalRollX},
        {GlobalRollY, AxisType::GlobalRollY},
        {PanX, AxisType::PanX},
        {PanY, AxisType::PanY}
    });

    const static std::unordered_map<std::string, Command> CommandMap ({
        {Connect, Command::Connect},
        {Disconnect, Command::Disconnect},
        {InputState, Command::InputState}
    });

    const int Axes = 10;

} // namespace

using nlohmann::json;

namespace openspace {

FlightControllerTopic::FlightControllerTopic()
: _isDone(false)
{
    for (auto it = AxisIndexMap.begin(); it != AxisIndexMap.end(); ++it) {
        OsEng.navigationHandler().setWebsocketAxisMapping(
            int(std::distance(AxisIndexMap.begin(), it)),
            it->second);
    }

    OsEng.navigationHandler().addWebsocketInputState(_topicId, _inputState);
}

FlightControllerTopic::~FlightControllerTopic() {
}

bool FlightControllerTopic::isDone() const {
    return _isDone;
}

void FlightControllerTopic::handleJson(const nlohmann::json& json) {
    auto it = CommandMap.find(json[TypeKey]);
    if (it == CommandMap.end()) {
        LWARNING(fmt::format("Poorly formatted JSON command: no '{}' in payload", TypeKey));
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
        default:
            LWARNING(fmt::format("Unrecognized action: {}", it->first));
            break;
    }
}

void FlightControllerTopic::connect() {
    _isDone = false;
    std::fill(_inputState.axes.begin(), _inputState.axes.end(), 0);
    _payload[TypeKey] = Connect;
    setFocusNodes();
    _payload[Connect][FocusNodesKey] = _focusNodes;
    _connection->sendJson(wrappedPayload(_payload));
}

void FlightControllerTopic::setFocusNodes() {
    std::vector<SceneGraphNode*> nodes =
        OsEng.renderEngine().scene()->allSceneGraphNodes();

    std::sort(
          nodes.begin(),
          nodes.end(),
          [](SceneGraphNode* lhs, SceneGraphNode* rhs) {
              return lhs->guiName() < rhs->guiName();
          }
      );;

    for (SceneGraphNode* n : nodes) {
        const std::vector<std::string>& tags = n->tags();
        const auto it = std::find(tags.begin(), tags.end(), "GUI.Interesting");
        if (it != tags.end()) {
            _focusNodes[n->guiName()] = n->identifier();
        }
    }
}

void FlightControllerTopic::disconnect() {
    OsEng.navigationHandler().removeWebsocketInputState(_topicId);
    _isDone = true;
    nlohmann::json j;
    j[TypeKey] = Disconnect;
    j[Disconnect][SuccessKey] = true;
    _connection->sendJson(wrappedPayload(j));
}

void FlightControllerTopic::processInputState(const nlohmann::json& json) {

    std::fill(_inputState.axes.begin(), _inputState.axes.end(), 0);
    _inputState.isConnected = true;

    // Get "inputState" object from "payload"
    auto input = json[InputState][ValuesKey];

    for (auto it = input.begin(); it != input.end(); ++it) {
        const auto mapIt = AxisIndexMap.find(it.key());
        if (mapIt == AxisIndexMap.end()) {
            if (it.key() != TypeKey
                || CommandMap.find(it.value()) == CommandMap.end()) {
                LWARNING(fmt::format(
                                 "No axis, button, or command named {} (value: {})", it.key() , it.value()
                                 ));
            }
            continue;
        }
        _inputState.axes[std::distance(AxisIndexMap.begin(), mapIt)] = float(it.value());
    }
}
} // namespace openspace
