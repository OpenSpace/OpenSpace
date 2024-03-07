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

#include <modules/omni/omnimodule.h>

#include <modules/omni/include/scenario.h>
#include <modules/omni/include/utility.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/memorymanager.h>
#include <ghoul/io/socket/websocket.h>
#include <ghoul/io/socket/tcpsocket.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>

namespace {
    constexpr std::string_view _loggerCat = "Omni";

} // namespace

#include "omnimodule_lua.inl"

namespace openspace::omni {

    std::string_view toString(Type type) {
        switch (type) {
            case Type::ServerConnect: return "server_connect";
            case Type::ServerDisconnect: return "server_disconnect";
            case Type::ServerAuthorized: return "server_authorized";
            case Type::Token: return "token";
            case Type::ServerCode: return "server_code";
            case Type::ServerJoin: return "server_join";
            case Type::ServerLeave: return "server_leave";
            case Type::ServerError: return "server_error";
            case Type::OpenSpaceType: return "openspace";
            default:
                throw ghoul::MissingCaseException();
        }
    }

    Type fromString(std::string_view str) {
        if (str == "server_connect") {
            return Type::ServerConnect;
        }
        else if (str == "server_disconnect") {
            return Type::ServerDisconnect;
        }
        else if (str == "server_authorized") {
            return Type::ServerAuthorized;
        }
        else if (str == "token") {
            return Type::Token;
        }
        else if (str == "server_code") {
            return Type::ServerCode;
        }
        else if (str == "server_join") {
            return Type::ServerJoin;
        }
        else if (str == "server_leave") {
            return Type::ServerLeave;
        }
        else if (str == "server_error") {
            return Type::ServerError;
        }
        else if (str == "openspace") {
            return Type::OpenSpaceType;
        }

        throw ghoul::RuntimeError(fmt::format("Unknown event type '{}'", str));
    }
}

namespace openspace {

OmniModule::OmniModule() : OpenSpaceModule(OmniModule::Name) {
    _server.clear_access_channels(websocketpp::log::alevel::all);
    _server.set_access_channels(websocketpp::log::alevel::connect);
    _server.set_access_channels(websocketpp::log::alevel::disconnect);
    _server.set_access_channels(websocketpp::log::alevel::app);
}

OmniModule::~OmniModule() {
    if (_activeScenario) {
        _activeScenario->disableScenario();
        _activeScenario = nullptr;
    }
    if (_socket->isConnected()) {
        _socket->disconnect(
            static_cast<int>(ghoul::io::WebSocket::ClosingReason::ClosingAll)
        );
    }
    if (_thread.joinable()) {
        _thread.join();
    }
}

void OmniModule::internalInitialize(const ghoul::Dictionary& config) {

    const int Port = 4685;
    const std:: string Address = "localhost";

    global::callback::preSync->emplace_back([this]() {
        ZoneScopedN("OmniModule");

        preSync();
    });

    std::unique_ptr<ghoul::io::TcpSocket> tcpSocket = std::make_unique<ghoul::io::TcpSocket>(Address, Port);
    if (!tcpSocket) {
        LERROR("Error creating tcp socket, aborting...");
        return;
    }
    tcpSocket->connect();

    if (tcpSocket->isConnected()) {
        LDEBUG("Tcp socket connected");
    }
    else if (tcpSocket->isConnecting()) {
        LDEBUG("Tcp socket connecting...");
    }
    else {
        LERROR("Tcp socket could not connect");
        return;
    }

    _socket = std::move(tcpSocket);

    _thread = std::move(std::thread([this]() { handleConnection(); }));

    FactoryManager::ref().addFactory<omni::Scenario>("OmniScenario");
    auto factory = FactoryManager::ref().factory<omni::Scenario>();
    factory->registerClass<omni::Poll>("Poll");
}

void OmniModule::addScenario(std::unique_ptr<omni::Scenario> scenario) {
    scenario->initialize(_socket);
    _scenarios.push_back(std::move(scenario));
}

void OmniModule::enableScenario(std::string_view identifier) {
    auto it = std::find_if(_scenarios.begin(), _scenarios.end(),
        [&identifier](const std::unique_ptr<omni::Scenario>& scenario) {
            return scenario->identifier() == identifier;
        }
    );

    if (it == _scenarios.end()) {
        throw ghoul::RuntimeError(fmt::format(
            "No scenario with identifier '{}' found", identifier
        ));
    }

    omni::Scenario* scenario = it->get();

    // Ignore if this is already the active scenario
    if (scenario->isActive()) {
        return;
    }

    // Disable the current active scenario
    if (_activeScenario) {
        _activeScenario->disableScenario();
    }

    // Enable new scenario
    _activeScenario = scenario;
    _activeScenario->enableScenario();
}

void OmniModule::disableScenario(std::string_view identifier) {
    // Quick exist if we have no active scenario
    if (!_activeScenario) {
        return;
    }

    // If we are trying to disable a non active scenario
    if (identifier != _activeScenario->identifier()) {
        LWARNING(fmt::format(
            "Scenario '{}' does not match the currently active scenario '{}'",
            identifier,
            _activeScenario->identifier()
        ));
        return;
    }

    _activeScenario->disableScenario();
    _activeScenario = nullptr;
}

void OmniModule::sendMessage(const std::string& message) {
    _socket->putMessage(message);
}

void OmniModule::sendJson(const nlohmann::json& json) {
    sendMessage(json.dump());
}

void OmniModule::preSync() {
    std::lock_guard lock(_messageQueueMutex);
    while (!_messageQueue.empty()) {
        const std::string& msg = _messageQueue.front();
        nlohmann::json j = nlohmann::json::parse(msg.c_str());
        handleJson(j);
        _messageQueue.pop_front();
    }
}

void OmniModule::handleConnection() {
    std::string messageString;
    messageString.reserve(256);
    while (_socket->getMessage(messageString)) {
        std::lock_guard lock(_messageQueueMutex);
        _messageQueue.push_back(messageString);
    }
}

void OmniModule::handleJson(const nlohmann::json& json) {
    auto typeJson = json.find(omni::details::MessageKeyType);
    auto payloadJson = json.find(omni::details::MessageKeyPayload);

    if (typeJson == json.end()) {
        LERROR("Message ignored, could not find a message 'type'");
        return;
    }

    omni::Type msgType;
    try {
        msgType = omni::fromString(*typeJson);
    }
    catch (const ghoul::RuntimeError& e) {
        LERROR(fmt::format("Message ignored {}, {}", e.component, e.message));
        return;
    }

    switch (msgType) {
        case omni::Type::ServerCode: {
            auto codeJson = json.find(omni::details::MessageKeyCode);
            _serverCode = *codeJson;
            break;
        }
        case omni::Type::ServerJoin: {
            userJoin(json);
            break;
        }
        case omni::Type::ServerLeave: {
            userLeave(json);
            break;
        }
        case omni::Type::OpenSpaceType: {
            if (!_activeScenario) {
                break;
            }

            //  We only handle messages for the currently active scenario
            auto identifierJson = json.find(omni::details::MessageKeyIdentifier);
            const std::string id = identifierJson->get<std::string>();
            if (_activeScenario->identifier() == id) {
                _activeScenario->handleMessage(json);
            }
            break;
        }
        default:
            break;
    }
}

void OmniModule::userJoin(const nlohmann::json& json) {
    auto userJson = json.find(omni::details::MessageKeyUser);
    auto userRoleJson = json.find(omni::details::MessageKeyRole);

    // TODO: these keys are added by Omni so the checks might be redundant?
    if (userJson == json.end()) {
        LERROR("User joined but could not find user ID");
        return;
    }
    if (!userJson->is_string()) {
        LERROR("'user' must be specified as a string when joining server");
        return;
    }
    if (userRoleJson == json.end() || !userRoleJson->is_string()) {
        LERROR("User role not specified or not in string format when joining");
        return;
    }

    const std::string role = userRoleJson->get<std::string>();

    // Ignore client or host joining
    if (role != "guest") {
        return;
    }

    int userId = omni::details::convertToUserID(*userJson);
    _users.insert(userId);

    // We need to send the asset information if a user joins after activating a scenario
    if (_activeScenario) {
        _activeScenario->sendScenarioData();
    }
}

void OmniModule::userLeave(const nlohmann::json& json) {
    auto userJson = json.find(omni::details::MessageKeyUser);
    auto userRoleJson = json.find(omni::details::MessageKeyRole);

    // TODO: these keys are added by Omni so the checks might be redundant?
    if (userJson == json.end()) {
        LERROR("User left but could not find user ID");
        return;
    }
    if (!userJson->is_string()) {
        LERROR(fmt::format("'{}' must be specified as a string when leaving server",
            omni::details::MessageKeyUser
        ));
        return;
    }
    if (userRoleJson == json.end() || !userRoleJson->is_string()) {
        LERROR("User role not specified or not in string format when leaving");
        return;
    }

    const std::string role = userRoleJson->get<std::string>();

    if (role != "guest") {
        return;
    }

    // Remove user from internal storage
    int userId = omni::details::convertToUserID(*userJson);
    auto it = _users.find(userId);
    if (it != _users.end()) {
        _users.erase(it);
    }
}

scripting::LuaLibrary OmniModule::luaLibrary() const {
    return {
        "omni",
        {
            codegen::lua::CreateScenarioFromDictionary,
            codegen::lua::EnableScenario,
            codegen::lua::DisableScenario
        }
    };
}

std::vector<documentation::Documentation> OmniModule::documentations() const {
    return {
        omni::Scenario::Documentation(),
        omni::Poll::Documentation(),
    };
}

} // namespace openspace
