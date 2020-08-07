/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#include <modules/softwareintegration/softwareintegrationmodule.h>

#include <modules/softwareintegration/rendering/renderablepointscloud.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/screenspacerenderable.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/util/factorymanager.h>
#include <openspace/query/query.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/fmt.h>
#include <ghoul/io/socket/tcpsocket.h>
#include <ghoul/io/socket/tcpsocketserver.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/templatefactory.h>
#include <functional>

namespace {
    constexpr const char* _loggerCat = "SoftwareIntegrationModule";
} // namespace

namespace openspace {

    const unsigned int SoftwareConnection::ProtocolVersion = 1;

    SoftwareIntegrationModule::SoftwareIntegrationModule() : OpenSpaceModule(Name) {}

    SoftwareConnection::Message::Message(MessageType type, std::vector<char> content)
        : type(type)
        , content(std::move(content))
    {}

    SoftwareConnection::SoftwareConnectionLostError::SoftwareConnectionLostError()
        : ghoul::RuntimeError("Connection lost", "Connection")
    {}

    SoftwareConnection::SoftwareConnection(std::unique_ptr<ghoul::io::TcpSocket> socket)
        : _socket(std::move(socket))
    {}


    void SoftwareIntegrationModule::internalInitialize(const ghoul::Dictionary&) {
        auto fRenderable = FactoryManager::ref().factory<Renderable>();
        ghoul_assert(fRenderable, "No renderable factory existed");

        fRenderable->registerClass<RenderablePointsCloud>("RenderablePointsCloud");

        start(4700);
    }

    void SoftwareIntegrationModule::internalDeinitializeGL() {

    }

    // Connection 
    bool SoftwareConnection::isConnectedOrConnecting() const {
        return _socket->isConnected() || _socket->isConnecting();
    }

    // Connection 
    void SoftwareConnection::disconnect() {
        if (_socket) {
            _socket->disconnect();
        }
    }

    // Connection 
    ghoul::io::TcpSocket* SoftwareConnection::socket() {
        return _socket.get();
    }

    // Connection 
    SoftwareConnection::Message SoftwareConnection::receiveMessage() {
        // Header consists of...
        size_t HeaderSize = 
            9 * sizeof(char);

        // Create basic buffer for receiving first part of messages
        std::vector<char> headerBuffer(HeaderSize);
        std::vector<char> messageBuffer;

        // Receive the header data
        if (!_socket->get(headerBuffer.data(), HeaderSize)) {
            LERROR("Failed to read header from socket. Disconnecting.");
            throw SoftwareConnectionLostError();
        }

        std::string version;
        version.push_back(headerBuffer[0]);
        const uint32_t protocolVersionIn = std::stoi(version);

        // Make sure that header matches the protocol version
        if (!(protocolVersionIn == ProtocolVersion)) {
            LERROR(fmt::format(
                "Protocol versions do not match. Remote version: {}, Local version: {}",
                protocolVersionIn,
                ProtocolVersion
            ));
            throw SoftwareConnectionLostError();
        }

        std::string type;
        type.push_back(headerBuffer[1]);
        type.push_back(headerBuffer[2]);
        type.push_back(headerBuffer[3]);
        type.push_back(headerBuffer[4]);

        std::string messageSizeIn;
        messageSizeIn.push_back(headerBuffer[5]);
        messageSizeIn.push_back(headerBuffer[6]);
        messageSizeIn.push_back(headerBuffer[7]);
        messageSizeIn.push_back(headerBuffer[8]);

        const size_t messageSize = stoi(messageSizeIn);

        messageBuffer.resize(messageSize);
        if (!_socket->get(messageBuffer.data(), messageSize)) {
            LERROR("Failed to read message from socket. Disconnecting.");
            throw SoftwareConnectionLostError();
        }

        // And delegate decoding depending on type
        if( type == "ASGN")
            return Message(MessageType::AddSceneGraphNode, messageBuffer);
        else if (type == "RSGN")
            return Message(MessageType::RemoveSceneGraphNode, messageBuffer);
        else if (type == "UPCO")
            return Message(MessageType::Color, messageBuffer);
        else if (type == "UPOP")
            return Message(MessageType::Opacity, messageBuffer);
        else if( type == "UPSI")
            return Message(MessageType::Size, messageBuffer);
    }

    // Server
    void SoftwareIntegrationModule::start(int port)
    {
        _socketServer.listen(port);

        _serverThread = std::thread([this]() { handleNewPeers(); });
        _eventLoopThread = std::thread([this]() { eventLoop(); });
    }

    // Server
    void SoftwareIntegrationModule::stop() {
        _shouldStop = true;
        _socketServer.close();
    }

    // Server
    void SoftwareIntegrationModule::handleNewPeers() {
        while (!_shouldStop) {
            std::unique_ptr<ghoul::io::TcpSocket> socket =
                _socketServer.awaitPendingTcpSocket();

            socket->startStreams();

            const size_t id = _nextConnectionId++;
            std::shared_ptr<Peer> p = std::make_shared<Peer>(Peer{
                id,
                "",
                SoftwareConnection(std::move(socket)),
                SoftwareConnection::Status::Connecting,
                std::thread()
                });
            auto it = _peers.emplace(p->id, p);
            it.first->second->thread = std::thread([this, id]() {
                handlePeer(id);
            });
        }
    }

    // Server
    std::shared_ptr<SoftwareIntegrationModule::Peer> SoftwareIntegrationModule::peer(size_t id) {
        std::lock_guard<std::mutex> lock(_peerListMutex);
        auto it = _peers.find(id);
        if (it == _peers.end()) {
            return nullptr;
        }
        return it->second;
    }

    void SoftwareIntegrationModule::handlePeer(size_t id) {
        while (!_shouldStop) {
            std::shared_ptr<Peer> p = peer(id);
            if (!p) {
                return;
            }

            if (!p->connection.isConnectedOrConnecting()) {
                return;
            }
            try {
                SoftwareConnection::Message m = p->connection.receiveMessage();
                _incomingMessages.push({ id, m });
            }
            catch (const SoftwareConnection::SoftwareConnectionLostError&) {
                LERROR(fmt::format("Connection lost to {}", p->id));
                _incomingMessages.push({
                    id,
                    SoftwareConnection::Message(
                        SoftwareConnection::MessageType::Disconnection, std::vector<char>()
                    )
                    });
                return;
            }
        }
    }

    void SoftwareIntegrationModule::eventLoop() {
        while (!_shouldStop) {
            PeerMessage pm = _incomingMessages.pop();
            handlePeerMessage(std::move(pm));
        }
    }

    void SoftwareIntegrationModule::handlePeerMessage(PeerMessage peerMessage) {
        const size_t peerId = peerMessage.peerId;
        auto it = _peers.find(peerId);
        if (it == _peers.end()) {
            return;
        }

        std::shared_ptr<Peer>& peer = it->second;

        /* LERROR(fmt::format("Name: {}", sName)); */

        const SoftwareConnection::MessageType messageType = peerMessage.message.type;
        std::vector<char>& message = peerMessage.message.content;
        std::string ms(message.begin(), message.end());
        switch (messageType) {
        case SoftwareConnection::MessageType::Connection: {
            //handleData(*peer, std::move(data));
            break;
        }
        case SoftwareConnection::MessageType::AddSceneGraphNode: {
            std::string length_of_identifier;
            length_of_identifier.push_back(message[0]);
            length_of_identifier.push_back(message[1]);

            size_t offset = 2;
            int counter = 0;
            
            int lengthOfIdentifier = stoi(length_of_identifier);
            std::string identifier;
            while (counter != lengthOfIdentifier)
            {
                identifier.push_back(message[offset]);
                offset++;
                counter++;
            }

            std::string length_of_color;
            length_of_color.push_back(message[offset]);
            length_of_color.push_back(message[offset + 1]);
            offset += 2;

            int lengthOfColor = stoi(length_of_color);
            std::string color;
            counter = 0;
            while (counter != lengthOfColor)
            {
                color.push_back(message[offset]);
                offset++;
                counter++;
            }

            std::string length_of_file;
            length_of_file.push_back(message[offset]);
            length_of_file.push_back(message[offset + 1]);
            offset += 2;

            int lengthOfFile = stoi(length_of_file);
            std::string file;
            counter = 0;
            while (counter != lengthOfFile)
            {
                file.push_back(message[offset]);
                offset++;
                counter++;
            }

            std::string length_of_opacity;
            length_of_opacity.push_back(message[offset]);
            offset += 1;

            int lengthOfOpacity = stoi(length_of_opacity);
            std::string opacity;
            counter = 0;
            while (counter != lengthOfOpacity)
            {
                opacity.push_back(message[offset]);
                offset++;
                counter++;
            }

            std::string length_of_size;
            length_of_size.push_back(message[offset]);
            offset += 1;

            int lengthOfSize = stoi(length_of_size);
            std::string size;
            counter = 0;
            while (counter != lengthOfSize)
            {
                size.push_back(message[offset]);
                offset++;
                counter++;
            }

            std::string length_of_gui;
            length_of_gui.push_back(message[offset]);
            length_of_gui.push_back(message[offset + 1]);
            offset += 2;

            int lengthOfGui = stoi(length_of_gui);
            std::string gui;
            counter = 0;
            while (counter != lengthOfGui)
            {
                gui.push_back(message[offset]);
                offset++;
                counter++;
            }

            LERROR(fmt::format("Identifier: {}", identifier));
            LERROR(fmt::format("Color: {}", color));
            LERROR(fmt::format("File: {}", file));
            LERROR(fmt::format("Opacity: {}", opacity));
            LERROR(fmt::format("Size: {}", size));
            LERROR(fmt::format("Gui: {}", gui));

            break;
        }
        case SoftwareConnection::MessageType::RemoveSceneGraphNode: {
            std::string identifier(message.begin(), message.end());
            LERROR(fmt::format("Identifier: {}", identifier));
            break;
        }
        case SoftwareConnection::MessageType::Color: {
            std::string length_of_identifier;
            length_of_identifier.push_back(message[0]);
            length_of_identifier.push_back(message[1]);

            size_t offset = 2;

            int lengthOfIdentifier = stoi(length_of_identifier);
            int counter = 0; 
            std::string identifier;
            while( counter != lengthOfIdentifier)
            {
                identifier.push_back(message[offset]);
                offset++;
                counter++;
            }

            std::string length_of_value;
            length_of_value.push_back(message[offset]);
            length_of_value.push_back(message[offset + 1]);
            offset += 2;

            int lengthOfValue = stoi(length_of_value);
            std::string value;
            counter = 0;
            while (counter != lengthOfValue)
            {
                value.push_back(message[offset]);
                offset++;
                counter++;
            }

            const Renderable* myrenderable = renderable(identifier);

            properties::Property* colorProperty = myrenderable->property("Color");

            LERROR(fmt::format("Identifier: {}", identifier));
            LERROR(fmt::format("Color: {}", value));

            break;
        }
        case SoftwareConnection::MessageType::Opacity: {
            std::string length_of_identifier;
            length_of_identifier.push_back(message[0]);
            length_of_identifier.push_back(message[1]);

            size_t offset = 2;

            int lengthOfIdentifier = stoi(length_of_identifier);
            int counter = 0;
            std::string identifier;
            while (counter != lengthOfIdentifier)
            {
                identifier.push_back(message[offset]);
                offset++;
                counter++;
            }

            std::string length_of_value;
            length_of_value.push_back(message[offset]);
            offset += 1;

            int lengthOfValue = stoi(length_of_value);
            std::string value;
            counter = 0;
            while (counter != lengthOfValue)
            {
                value.push_back(message[offset]);
                offset++;
                counter++;
            }

            LERROR(fmt::format("Identifier: {}", identifier));
            LERROR(fmt::format("Opacity: {}", value));

            break;
        }
        case SoftwareConnection::MessageType::Size: {
            std::string length_of_identifier;
            length_of_identifier.push_back(message[0]);
            length_of_identifier.push_back(message[1]);

            size_t offset = 2;

            int lengthOfIdentifier = stoi(length_of_identifier);
            int counter = 0;
            std::string identifier;
            while (counter != lengthOfIdentifier)
            {
                identifier.push_back(message[offset]);
                offset++;
                counter++;
            }

            std::string length_of_value;
            length_of_value.push_back(message[offset]);
            offset += 1;

            int lengthOfValue = stoi(length_of_value);
            std::string value;
            counter = 0;
            while (counter != lengthOfValue)
            {
                value.push_back(message[offset]);
                offset++;
                counter++;
            }

            LERROR(fmt::format("Identifier: {}", identifier));
            LERROR(fmt::format("Size: {}", value));
            break;
        }
        case SoftwareConnection::MessageType::Disconnection: {
            disconnect(*peer);
            break;
        }
        default:
            LERROR(fmt::format(
                "Unsupported message type: {}", static_cast<int>(messageType)
            ));
            break;
        }
    }

    // Server
    bool SoftwareIntegrationModule::isConnected(const Peer& peer) const {
        return peer.status != SoftwareConnection::Status::Connecting &&
            peer.status != SoftwareConnection::Status::Disconnected;
    }

    // Server
    void SoftwareIntegrationModule::disconnect(Peer& peer) {
        if (isConnected(peer)) {
            _nConnections = nConnections() - 1;
        }

        peer.connection.disconnect();
        peer.thread.join();
        _peers.erase(peer.id);
    }

    size_t SoftwareIntegrationModule::nConnections() const {
        return _nConnections;
    }

    std::vector<documentation::Documentation> SoftwareIntegrationModule::documentations() const {
        return {
            RenderablePointsCloud::Documentation(),
        };
    }

    scripting::LuaLibrary SoftwareIntegrationModule::luaLibrary() const {
        scripting::LuaLibrary res;
        res.name = "softwareintegration";
        res.scripts = {
            absPath("${MODULE_SOFTWAREINTEGRATION}/scripts/network.lua")
        };
        return res;
    }
} // namespace openspace

