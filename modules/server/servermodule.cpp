/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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


#include <modules/server/servermodule.h>
#include <ghoul/io/socket/tcpsocketserver.h>
#include <ghoul/io/socket/websocketserver.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/scripting/scriptengine.h>


#include <ghoul/logging/logmanager.h>
#include <cstdint>

namespace {
    using Json = nlohmann::json;
    const char* _loggerCat = "ServerModule";

    const char* MessageKeyType = "type";
    const char* MessageKeyPayload = "payload";
    const char* MessageKeySubject = "subject";
}

namespace openspace {

ServerModule::ServerModule()
    : OpenSpaceModule("Server")
    , _connectionPool([this](std::shared_ptr<ghoul::io::Socket> socket) {
        handleSocket(socket);
    })
{}

ServerModule::~ServerModule() {
    _connectionPool.clearServers();
}

void ServerModule::internalInitialize() {
    using namespace ghoul::io;

    std::shared_ptr<SocketServer> tcpServer =
        std::static_pointer_cast<SocketServer>(std::make_shared<TcpSocketServer>());
    std::shared_ptr<ghoul::io::SocketServer> wsServer =
        std::static_pointer_cast<SocketServer>(std::make_shared<WebSocketServer>());

    // Temporary hard coded addresses and ports.
    tcpServer->listen("localhost", 8000);
    wsServer->listen("localhost", 8001);

    _connectionPool.addServer(wsServer);
    _connectionPool.addServer(tcpServer);

    OsEng.registerModuleCallback(
        OpenSpaceEngine::CallbackOption::PreSync,
        [this]() {
            _connectionPool.updateConnections();
            consumeMessages();
        }
    );
}


void ServerModule::handleSocket(std::shared_ptr<ghoul::io::Socket> socket) {
    _messageQueue.push_back(Message({
        socket,
        SocketAction::Open,
        ""
    }));

    std::string messageString;
    while (socket->getMessage(messageString)) {
        std::lock_guard<std::mutex> lock(_messageQueueMutex);
        _messageQueue.push_back(Message({
            socket,
            SocketAction::Data,
            std::move(messageString)
        }));
    }

    _messageQueue.push_back(Message({
        socket,
        SocketAction::Close,
        ""
    }));
}

void ServerModule::consumeMessages() {
    std::lock_guard<std::mutex> lock(_messageQueueMutex);
    while (_messageQueue.size() > 0) {
        Message m = _messageQueue.front();
        _messageQueue.pop_front();

        auto isActiveSocket = [&m](const Connection& c) {
            return c.socket() == m.socket.get();
        };
        switch (m.action) {
        case SocketAction::Open: {
            _connections.push_back(Connection(m.socket));
            break;
        }
        case SocketAction::Data: {
            auto connection = std::find_if(_connections.begin(), _connections.end(), isActiveSocket);
            if (connection != _connections.end()) {
                connection->handleMessage(m.messageString);
            }
            break;
        }
        case SocketAction::Close: {
            std::remove_if(_connections.begin(), _connections.end(), isActiveSocket);
            break;
        }
        }
    }
}

void Connection::handleMessage(std::string message) {
    try {
        Json j = Json::parse(message);
        handleJson(j);
    } catch (...) {
        LERROR("Json parse error");
    }
}

void Connection::handleJson(Json j) {
    auto keyJson = j.find(MessageKeyType);
    if (keyJson == j.end() || !keyJson->is_string()) {
        LERROR("Expected string key");
        return;
    }

    auto payloadJson = j.find(MessageKeyPayload);
    if (payloadJson == j.end() || !payloadJson->is_object()) {
        LERROR("Expected object payload");
        return;
    }

    auto subjectJson = j.find(MessageKeySubject);
    if (keyJson == j.end() || !subjectJson->is_number_integer()) {
        LERROR("Expected integer subject");
        return;
    }

    std::string key = *keyJson;
    int subject = *subjectJson;
    
    std::cout << key << std::to_string(subject) << subjectJson->dump();

}

void Connection::sendMessage(const std::string& message) {
    _socket->putMessage(message);
}

void Connection::sendJson(const Json& j) {
    sendMessage(j.dump());
}


/*
void Channel::initialize(const char* data, uint32_t size) {
    const char* instructionEnd = std::find(data, data + size, '\n');
    std::string instruction(data, instructionEnd);

    _dataHandler = createDataHandler(instruction);

    const char* newData = instructionEnd + 1;
    size -= newData - data;

    _dataHandler->initialize(newData, size);
}

void Channel::handleData(const char* data, uint32_t size) {
    _dataHandler->handleData(data, size);
}

void Channel::deinitialize(const char* data, uint32_t size) {
    _dataHandler->deinitialize(data, size);
}

void Channel::sendData(const char* data, uint32_t size) {
    _connection->sendMessage(data, size, _channelId);
}

std::unique_ptr<DataHandler> Channel::createDataHandler(std::string instruction)
{
    if (instruction == "subscribe") {
        //return std::make_unique<SubsciptionHandler>();
    }
    if (instruction == "execute") {
        return std::make_unique<ExecutionHandler>(this);
    }
    return nullptr;
}




DataHandler::DataHandler(Channel* channel)
    : _channel(channel)
{}

ExecutionHandler::ExecutionHandler(Channel* channel)
    : DataHandler(channel) {}

void ExecutionHandler::initialize(const char* data, uint32_t size) {
    handleData(data, size);
}

void ExecutionHandler::handleData(const char* data, uint32_t size) {
    std::string script(data, size);
    OsEng.scriptEngine().queueScript(script, scripting::ScriptEngine::RemoteScripting::Yes);
}

void ExecutionHandler::deinitialize(const char* data, uint32_t size) {
    // Do nothing.
}
*/


} // namespace openspace
