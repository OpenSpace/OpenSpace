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

    const unsigned int Connection::ProtocolVersion = 1;

    SoftwareIntegrationModule::SoftwareIntegrationModule() : OpenSpaceModule(Name) {}

    Connection::Message::Message(MessageType type, std::vector<char> content)
        : type(type)
        , content(std::move(content))
    {}

    Connection::ConnectionLostError::ConnectionLostError()
        : ghoul::RuntimeError("Connection lost", "Connection")
    {}

    Connection::Connection(std::unique_ptr<ghoul::io::TcpSocket> socket)
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
    bool Connection::isConnectedOrConnecting() const {
        return _socket->isConnected() || _socket->isConnecting();
    }

    // Connection 
    void Connection::disconnect() {
        if (_socket) {
            _socket->disconnect();
        }
    }

    // Connection 
    ghoul::io::TcpSocket* Connection::socket() {
        return _socket.get();
    }

    // Connection 
    Connection::Message Connection::receiveMessage() {
        // Header consists of...
        size_t HeaderSize = 
            9 * sizeof(char);

        // Create basic buffer for receiving first part of messages
        std::vector<char> headerBuffer(HeaderSize);
        std::vector<char> messageBuffer;

        // Receive the header data
        if (!_socket->get(headerBuffer.data(), HeaderSize)) {
            LERROR("Failed to read header from socket. Disconnecting.");
            throw ConnectionLostError();
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
            throw ConnectionLostError();
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
            throw ConnectionLostError();
        }

        // And delegate decoding depending on type
        if( type == "addS")
            return Message(MessageType::AddSceneGraph, messageBuffer);
        else if (type == "delS")
            return Message(MessageType::RemoveSceneGraph, messageBuffer);
        else if (type == "colo")
            return Message(MessageType::Color, messageBuffer);
        else if (type == "opac")
            return Message(MessageType::Opacity, messageBuffer);
        else if( type == "size")
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
                Connection(std::move(socket)),
                Connection::Status::Connecting,
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
                Connection::Message m = p->connection.receiveMessage();
                _incomingMessages.push({ id, m });
            }
            catch (const Connection::ConnectionLostError&) {
                LERROR(fmt::format("Connection lost to {}", p->id));
                _incomingMessages.push({
                    id,
                    Connection::Message(
                        Connection::MessageType::Disconnection, std::vector<char>()
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

        const Connection::MessageType messageType = peerMessage.message.type;
        std::vector<char>& message = peerMessage.message.content;
        std::string ms(message.begin(), message.end());
        switch (messageType) {
        case Connection::MessageType::Connection: {
            //handleData(*peer, std::move(data));
            break;
        }
        case Connection::MessageType::AddSceneGraph: {

            LERROR("Hej ny scene graph");
            break;
        }
        case Connection::MessageType::RemoveSceneGraph: {
            std::string identifier(message.begin(), message.end());
            LERROR(fmt::format("Identifier: {}", identifier));
            break;
        }
        case Connection::MessageType::Color: {
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

            LERROR(fmt::format("Identifier: {}", identifier));
            LERROR(fmt::format("Color: {}", value));

            break;
        }
        case Connection::MessageType::Opacity: {
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
        case Connection::MessageType::Size: {
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
        case Connection::MessageType::Disconnection: {
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
        return peer.status != Connection::Status::Connecting &&
            peer.status != Connection::Status::Disconnected;
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

