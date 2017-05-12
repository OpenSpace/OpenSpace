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

#include <ghoul/logging/logmanager.h>
#include <cstdint>

namespace {
    const char* _loggerCat = "ServerModule";
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
    while (true) {
        uint32_t channelId;
        if (!socket->get<uint32_t>(&channelId)) {
            LERROR("Failed to read channel id from socket.");
            return;
        }

        ChannelAction channelAction;
        if (!socket->get<ChannelAction>(&channelAction)) {
            LERROR("Failed to read channel action from socket.");
            return;
        }
      
        switch (channelAction) {
        case ChannelAction::Initialize:
        case ChannelAction::Deinitialize: {
            std::lock_guard<std::mutex> lock(_messageQueueMutex);
            _messageQueue.push_back(Message({
                socket,
                channelId,
                channelAction,
                std::vector<char>()
            }));
            break;
        }
        case ChannelAction::Data: {
            uint32_t messageSize;
            if (!socket->get<uint32_t>(&messageSize)) {
                LERROR("Failed to read message size from socket.");
                return;
            }
            std::vector<char> messageBuffer(messageSize);
            if (!socket->get<char>(messageBuffer.data(), messageSize)) {
                LERROR("Failed to read message body from socket.");
                return;
            }
            std::lock_guard<std::mutex> lock(_messageQueueMutex);
            _messageQueue.push_back(Message({
                socket,
                channelId,
                channelAction,
                std::move(messageBuffer)
            }));
            break;
        }
        default:
            LERROR("Unsupported channel action.");
            return;
        }
    }
}

void ServerModule::consumeMessages() {
    std::lock_guard<std::mutex> lock(_messageQueueMutex);
    while (_messageQueue.size() > 0) {
        // todo...
    }
}

} // namespace openspace
