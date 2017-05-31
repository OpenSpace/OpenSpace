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
    const char* _loggerCat = "ServerModule";

    const char* MessageKeyType = "type";
    const char* MessageKeyPayload = "payload";
    const char* MessageKeyTopic = "topic";
}

namespace openspace {

ServerModule::ServerModule()
    : OpenSpaceModule("Server")
{}

ServerModule::~ServerModule() {
    disconnectAll();
    cleanUpFinishedThreads();
}

void ServerModule::internalInitialize() {
    using namespace ghoul::io;

    std::unique_ptr<SocketServer> tcpServer = std::make_unique<TcpSocketServer>();
    std::unique_ptr<SocketServer> wsServer = std::make_unique<WebSocketServer>();

    // Temporary hard coded addresses and ports.
    tcpServer->listen("localhost", 8000);
    wsServer->listen("localhost", 8001);

    _servers.push_back(std::move(tcpServer));
    _servers.push_back(std::move(wsServer));

    OsEng.registerModuleCallback(
        OpenSpaceEngine::CallbackOption::PreSync,
        [this]() { preSync(); }
    );
}

void ServerModule::preSync() {
    // Set up new connections.
    for (auto& server : _servers) {
        std::shared_ptr<ghoul::io::Socket> socket;
        while ((socket = server->nextPendingSocket())) {
            std::unique_ptr<Connection> conneciton = std::make_unique<Connection>(socket, std::thread());
            Connection* c = conneciton.get();
            conneciton->thread = std::thread([this, c] () { handleConnection(c); });
            _connections.push_back(std::move(conneciton));
        }
    }

    // Consume all messages put into the message queue by the socket threads.
    consumeMessages();
    
    // Join threads for sockets that disconnected.
    cleanUpFinishedThreads();
}
                               
void ServerModule::cleanUpFinishedThreads() {
    for (auto& connection : _connections) {
        if (!connection->socket || !connection->socket->isConnected()) {
            if (connection->thread.joinable()) {
                connection->thread.join();
                connection->active = false;
            }
        }
    }
    _connections.erase(std::remove_if(
        _connections.begin(),
        _connections.end(),
        [](const auto& connection) {
            return !connection->active;
        }
    ), _connections.end());
}
    
void ServerModule::disconnectAll() {
    for (auto& connection : _connections) {
        if (connection->socket && connection->socket->isConnected()) {
            connection->socket->disconnect();
        }
    }
}
    
void ServerModule::handleConnection(Connection* connection) {
    std::string messageString;
    while (connection->socket->getMessage(messageString)) {
        std::lock_guard<std::mutex> lock(_messageQueueMutex);
        _messageQueue.push_back(Message({
            connection,
            std::move(messageString)
        }));
    }
}

void ServerModule::consumeMessages() {
    std::lock_guard<std::mutex> lock(_messageQueueMutex);
    while (_messageQueue.size() > 0) {
        Message m = _messageQueue.front();
        _messageQueue.pop_front();
        m.conneciton->handleMessage(m.messageString);
    }
}

Connection::Connection(std::shared_ptr<ghoul::io::Socket> s, std::thread t)
    : socket(s)
    , thread(std::move(t))
    , active(true)
{
    _topicFactory.registerClass<GetPropertyTopic>("authenticate");
    _topicFactory.registerClass<GetPropertyTopic>("get");
    _topicFactory.registerClass<SetPropertyTopic>("set");
    _topicFactory.registerClass<SubscribePropertyTopic>("subscribe");
}

void Connection::handleMessage(std::string message) {
    try {
        nlohmann::json j = nlohmann::json::parse(message);
        handleJson(j);
    } catch (...) {
        LERROR("Json parse error");
    }
}

void Connection::handleJson(nlohmann::json j) {
    auto topicJson = j.find(MessageKeyTopic);
    auto payloadJson = j.find(MessageKeyPayload);

    if (topicJson == j.end() || !topicJson->is_number_integer()) {
        LERROR("Topic must be an integer");
        return;
    }

    if (payloadJson == j.end() || !payloadJson->is_object()) {
        LERROR("Payload must be an object");
        return;
    }
    
    // The topic id may be an already discussed topic, or a new one.
    size_t topicId = *topicJson;
    auto topicIt = _topics.find(topicId);
    
    if (topicIt == _topics.end()) {
        // The topic id is not registered: Initialize a new topic.
        auto typeJson = j.find(MessageKeyType);
        if (typeJson == j.end() || !typeJson->is_string()) {
            LERROR("A type must be specified as a string when a new topic is initialized");
            return;
        }
        std::string type = *typeJson;
        std::unique_ptr<Topic> topic = _topicFactory.create(type);
        topic->initialize(this, topicId);
        topic->handleJson(payloadJson);
        if (!topic.isDone()) {
            _topics.emplace(topicId, topic);
        }
    } else {
        // Dispatch the message to the existing topic.
        std::unique_ptr<Topic>& topic = topicIt->second;
        topic->handleJson(payloadJson);
        if (topic->isDone()) {
            _topics.erase(topicIt);
        }
    }
}

void Connection::sendMessage(const std::string& message) {
    socket->putMessage(message);
}

void Connection::sendJson(const nlohmann::json& j) {
    sendMessage(j.dump());
}

} // namespace openspace
