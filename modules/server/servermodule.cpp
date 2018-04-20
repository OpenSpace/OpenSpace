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

#include <modules/server/servermodule.h>
#include <ghoul/io/socket/websocket.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/scripting/scriptengine.h>

namespace {
    const char* _loggerCat = "ServerModule";
}

namespace openspace {

ServerModule::ServerModule()
    : OpenSpaceModule(ServerModule::Name)
{}

ServerModule::~ServerModule() {
    disconnectAll();
    cleanUpFinishedThreads();
}

void ServerModule::internalInitialize(const ghoul::Dictionary& configuration) {
    using namespace ghoul::io;

    std::unique_ptr<TcpSocketServer> tcpServer = std::make_unique<TcpSocketServer>();
    std::unique_ptr<WebSocketServer> wsServer = std::make_unique<WebSocketServer>();

    // Temporary hard coded addresses and ports.
    tcpServer->listen("localhost", 8000);
    wsServer->listen("localhost", 8001);
    LDEBUG(fmt::format("TCP Server listening on {}:{}",
        tcpServer->address(), tcpServer->port()));

    LDEBUG(fmt::format("WS Server listening on {}:{}",
        wsServer->address(), wsServer->port()));

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
            socket->startStreams();
            std::shared_ptr<Connection> connection =
                std::make_shared<Connection>(socket, server->address());
            connection->setThread(
                std::thread([this, connection] () { handleConnection(connection); })
            );
            _connections.push_back({ std::move(connection), false });
        }
    }

    // Consume all messages put into the message queue by the socket threads.
    consumeMessages();

    // Join threads for sockets that disconnected.
    cleanUpFinishedThreads();
}

void ServerModule::cleanUpFinishedThreads() {
    for (auto& connectionData : _connections) {
        std::shared_ptr<Connection>& connection = connectionData.connection;
        if (!connection->socket() || !connection->socket()->isConnected()) {
            if (connection->thread().joinable()) {
                connection->thread().join();
                connectionData.markedForRemoval = true;
            }
        }
    }
    _connections.erase(std::remove_if(
        _connections.begin(),
        _connections.end(),
        [](const auto& connectionData) {
            return connectionData.markedForRemoval;
        }
    ), _connections.end());
}

void ServerModule::disconnectAll() {
    for (auto& connectionData : _connections) {
        std::shared_ptr<Connection>& connection = connectionData.connection;
        if (connection->socket() && connection->socket()->isConnected()) {
            connection->socket()->disconnect(
                static_cast<int>(ghoul::io::WebSocket::ClosingReason::ClosingAll)
            );
        }
    }
}

void ServerModule::handleConnection(std::shared_ptr<Connection> connection) {
    std::string messageString;
    while (connection->socket()->getMessage(messageString)) {
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
        if (std::shared_ptr<Connection> c = m.connection.lock()) {
            c->handleMessage(m.messageString);
        }
    }
}

} // namespace openspace
