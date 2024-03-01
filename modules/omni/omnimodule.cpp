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

#include <modules/omni/include/scene.h>
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

    constexpr std::string_view MessageKeyCode = "code";
    constexpr std::string_view MessageKeyMessage = "message";
    constexpr std::string_view MessageKeyRole = "role";
    constexpr std::string_view MessageKeyType = "type";
    constexpr std::string_view MessageKeyUser = "user";
    constexpr std::string_view MessageKeySceneType = "SceneType";

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
            //case Type::OpenSpaceType: return "openspace";
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
        //else if (str == "openspace") {
        //    return Type::OpenSpaceType;
        //}

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
    if (_socket->isConnected()) {
        _socket->disconnect(
            static_cast<int>(ghoul::io::WebSocket::ClosingReason::ClosingAll)
        );
    }
    if (_thread.joinable()) {
        _thread.join();
    }

    //if (_scene) {
    //    delete _scene;
    //}
}

void OmniModule::internalInitialize(const ghoul::Dictionary& config) {
    using namespace ghoul::io; // TODO this has to be included, otherwise wont compile..

    const int Port = 4685;
    const std:: string Address = "localhost";

    global::callback::preSync->emplace_back([this]() {
        ZoneScopedN("OmniModule");

        preSync();
    });

    std::unique_ptr<TcpSocket> tcpSocket = std::make_unique<TcpSocket>(Address, Port);
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

    FactoryManager::ref().addFactory<omni::Scene>("OmniScene");
    auto factory = FactoryManager::ref().factory<omni::Scene>();
    factory->registerClass<omni::Poll>("poll");
    
}

void OmniModule::addScene(omni::Scene* scene) {
    _scene = scene;
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
    auto typeJson = json.find(MessageKeyType);

    if (typeJson == json.end()) {
        LERROR("Message ignored, could not find a message 'type'");
        return;
    }

    omni::Type msgType = omni::fromString(*typeJson);

    switch (msgType) {
        case omni::Type::ServerCode: {
            auto codeJson = json.find(MessageKeyCode);
            _serverCode = *codeJson;
            break;
        }
        case omni::Type::ServerJoin: {
            userJoin(json);
            break;
        }
        case omni::Type::ServerLeave:
        {
            userLeave(json);
            break;
        }
        //case omni::Type::OpenSpaceType: {
        //    break;
        //}
        default:
            return;

    }
}

void OmniModule::userJoin(const nlohmann::json& json) {
    auto userJson = json.find(MessageKeyUser);
    auto userRole = json.find(MessageKeyRole);

    if (userJson == json.end()) {
        LERROR("User joined but could not find user ID");
        return;
    }
    if (!userJson->is_string()) {
        LERROR("'user' must be specified as a string when joining server");
        return;
    }
    if (userRole == json.end() || !userRole->is_string()) {
        LERROR("User role not specified or not in string format when joining");
        return;
    }

    std::string role = *userRole;

    if (role != "guest") {
        return;
    }

    std::string user = *userJson;
    int userId = std::stoi(user, nullptr, 16);
    _users.insert(userId);
}

void OmniModule::userLeave(const nlohmann::json& json) {
    auto userJson = json.find(MessageKeyUser);
    auto userRole = json.find(MessageKeyRole);

    if (userJson == json.end()) {
        LERROR("User left but could not find user ID");
        return;
    }
    if (!userJson->is_string()) {
        LERROR("'user' must be specified as a string when leaving server");
        return;
    }
    if (userRole == json.end() || !userRole->is_string()) {
        LERROR("User role not specified or not in string format when leaving");
        return;
    }

    std::string role = *userRole;

    if (role != "guest") {
        return;
    }

    std::string user = *userJson;
    int userId = std::stoi(user, nullptr, 16);
    auto it = _users.find(userId);
    if (it != _users.end()) {
        _users.erase(it);
    }
}

scripting::LuaLibrary OmniModule::luaLibrary() const {
    return {
        "omni",
        {
            codegen::lua::CreateSceneFromDictionary
        }
    };
}

std::vector<documentation::Documentation> OmniModule::documentations() const {
    return {
        omni::Poll::Documentation(),
    };
}

} // namespace openspace
