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
#include <openspace/engine/openspaceengine.h>

#include <ghoul/logging/logmanager.h>
#include <cstdint>

namespace {
    const char* _loggerCat = "ServerModule";
}

namespace openspace {

ServerModule::ServerModule()
    : OpenSpaceModule("Server")
{}

void ServerModule::internalInitialize() {
    _connectionPool.listen(
        "localhost",
        80800,
        [this](std::shared_ptr<ghoul::io::TcpSocket> socket) {
            handleSocket(socket);
        }
    );
   
    OsEng.registerModuleCallback(
        OpenSpaceEngine::CallbackOption::Deinitialize,
        [this]() { _connectionPool.close(); }
    );
}

void ServerModule::handleSocket(std::shared_ptr<ghoul::io::TcpSocket> socket) {
    bool disconnect = false;
    std::vector<char> messageBuffer;
    std::map<uint32_t, std::unique_ptr<ChannelHandler>> channelHandlers;
    while (!disconnect) {
        uint32_t channelId;
        ChannelAction channelAction;
        uint32_t messageSize;
        
        if (!socket->get<uint32_t>(&channelId)) {
            LERROR("Failed to read channel id from socket.");
            return;
        }
        if (!socket->get<ChannelAction>(&channelAction)) {
            LERROR("Failed to read channel action from socket.");
            return;
        }

        auto it = channelHandlers.find(channelId);

        switch (channelAction) {
        case ChannelAction::Initialize:
            if (it != channelHandlers.end()) {
                std::unique_ptr<ChannelHandler> channelHandler = factory.create();
                channelHandlers.insert(channelHandler);
            }
            break;
        case ChannelAction::Deinitialize:
            if (it != channelHandlers.end()) {
                it->deinitialize();
                channelHandlers.erase(it);
            }
            break;
        case ChannelAction::Data:
            if (!socket->get<uint32_t>(&messageSize)) return;
            messageBuffer.resize(messageSize);
            if (!socket->get<char>(messageBuffer.data(), messageSize)) return;

            if (it != channelHandlers.end()) {
                it->handleData(messageBuffer.data(), messageSize);
                channelHandlers.erase(it);
            }
            break;
        default:
            LWARNING("Unsupported channel action.");
        }
    }
}

} // namespace openspace
