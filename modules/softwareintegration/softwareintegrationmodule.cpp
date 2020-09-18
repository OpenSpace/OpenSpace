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
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/factorymanager.h>
#include <openspace/query/query.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/fmt.h>
#include <ghoul/glm.h>
#include <ghoul/io/socket/tcpsocket.h>
#include <ghoul/io/socket/tcpsocketserver.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/templatefactory.h>

#include <functional>
#include <iomanip>
#include <sstream>

using namespace std::string_literals;

namespace {
    constexpr const char* _loggerCat = "SoftwareIntegrationModule";
} // namespace

namespace openspace {

    SoftwareIntegrationModule::SoftwareIntegrationModule() : OpenSpaceModule(Name) {}

    void SoftwareIntegrationModule::internalInitialize(const ghoul::Dictionary&) {
        auto fRenderable = FactoryManager::ref().factory<Renderable>();
        ghoul_assert(fRenderable, "No renderable factory existed");

        fRenderable->registerClass<RenderablePointsCloud>("RenderablePointsCloud");

        start(4700);
    }

    void SoftwareIntegrationModule::internalDeinitializeGL() {

    }

    void SoftwareIntegrationModule::start(int port)
    {
        _socketServer.listen(port);

        _serverThread = std::thread([this]() { handleNewPeers(); });
        _eventLoopThread = std::thread([this]() { eventLoop(); });
    }

    void SoftwareIntegrationModule::stop() {
        _shouldStop = true;
        _socketServer.close();
    }

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

        const SoftwareConnection::MessageType messageType = peerMessage.message.type;
        std::vector<char>& message = peerMessage.message.content;
        switch (messageType) {
        case SoftwareConnection::MessageType::Connection: {
            std::string software(message.begin(), message.end());
            LINFO(fmt::format("OpenSpace has connected with {} through socket.", software));
            break;
        } 
        case SoftwareConnection::MessageType::AddSceneGraphNode: {
            std::string identifier = readIdentifier(message);
            glm::vec3 color = readColor(message);
            std::string file = readString(message);
            float opacity = readFloatValue(message);
            float size = readFloatValue(message);
            std::string guiName = readString(message);

            ghoul::Dictionary renderable = {
                { "Type", "RenderablePointsCloud"s },
                { "Color", static_cast<glm::dvec3>(color)},
                { "File", file },
                { "Opacity", static_cast<double>(opacity) },
                { "Size", static_cast<double>(size)}
            };

            ghoul::Dictionary gui = {
                { "Name", guiName },
                { "Path", "/Examples"s }
            };

            ghoul::Dictionary node = {
                { "Identifier", identifier },
                { "Renderable", renderable },
                { "GUI", gui }
            };

            try {
                SceneGraphNode* sgn = global::renderEngine.scene()->loadNode(node);
                if (!sgn) {
                    LERROR("Scene", "Could not load scene graph node");
                }
                global::renderEngine.scene()->initializeNode(sgn);
            }
            catch (const documentation::SpecificationError& e) {
                return LERROR(fmt::format("Documentation SpecificationError: Error loading scene graph node {}",
                        e.what())
                );
            }
            catch (const ghoul::RuntimeError& e) {
                return LERROR(fmt::format("RuntimeError: Error loading scene graph node {}",
                    e.what())
                );
            }

            SoftwareConnection callback; 
            callback.handleProperties(identifier);

            break;
        }
        case SoftwareConnection::MessageType::RemoveSceneGraphNode: {
            std::string identifier(message.begin(), message.end());
            LERROR(fmt::format("Identifier: {}", identifier));

            openspace::global::scriptEngine.queueScript(
                "openspace.removeSceneGraphNode('" + identifier + "');",
                scripting::ScriptEngine::RemoteScripting::Yes
            );
            
            break;
        }
        case SoftwareConnection::MessageType::Color: {
            std::string identifier = readIdentifier(message);
            glm::vec3 color = readColor(message);

            // Update color of renderable
            const Renderable* myrenderable = renderable(identifier);
            properties::Property* colorProperty = myrenderable->property("Color");
            colorProperty->set(color); 
            break;
        }
        case SoftwareConnection::MessageType::Opacity: {
            std::string identifier = readIdentifier(message);
            float opacity = readFloatValue(message);

            // Update opacity of renderable
            const Renderable* myrenderable = renderable(identifier);
            properties::Property* opacityProperty = myrenderable->property("Opacity");
            opacityProperty->set(opacity);
            break;
        }
        case SoftwareConnection::MessageType::Size: {
            std::string identifier = readIdentifier(message);
            float size = readFloatValue(message);

            // Update size of renderable
            const Renderable* myrenderable = renderable(identifier);
            properties::Property* sizeProperty = myrenderable->property("Size");
            sizeProperty->set(size);
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

    std::string SoftwareIntegrationModule::readIdentifier(std::vector<char>& message) {

        std::string length;
        length.push_back(message[0]);
        length.push_back(message[1]);

        int lengthOfIdentifier = stoi(length);
        int counter = 0;
        messageOffset = 2;

        std::string identifier;
        while (counter != lengthOfIdentifier)
        {
            identifier.push_back(message[messageOffset]);
            messageOffset++;
            counter++;
        }

        return identifier;
    }

    // Read size value or opacity value
    float SoftwareIntegrationModule::readFloatValue(std::vector<char>& message) {

        std::string length;
        length.push_back(message[messageOffset]);
        messageOffset += 1;

        int lengthOfValue = stoi(length);
        std::string value;
        int counter = 0;
        while (counter != lengthOfValue)
        {
            value.push_back(message[messageOffset]);
            messageOffset++;
            counter++;
        }
        float floatValue = std::stof(value);

        return floatValue;
    }

    glm::vec3 SoftwareIntegrationModule::readColor(std::vector<char>& message) {

        std::string lengthOfColor; // Not used for now, but sent in message
        lengthOfColor.push_back(message[messageOffset]);
        lengthOfColor.push_back(message[messageOffset + 1]);
        messageOffset += 2;

        // Red
        std::string red;
        while (message[messageOffset] != ',')
        {
            if (message[messageOffset] == '(')
                messageOffset++;
            else {
                red.push_back(message[messageOffset]);
                messageOffset++;
            }
        }

        // Green
        std::string green;
        messageOffset++;
        while (message[messageOffset] != ',')
        {
            green.push_back(message[messageOffset]);
            messageOffset++;
        }

        // Blue
        std::string blue;
        messageOffset++;
        while (message[messageOffset] != ')')
        {
            blue.push_back(message[messageOffset]);
            messageOffset++;
        }
        messageOffset++;

        // Convert rgb string to floats
        float r = std::stof(red);
        float g = std::stof(green);
        float b = std::stof(blue);

        glm::vec3 color(r, g, b);

        return color;
    }

    // Read File path or GUI Name
    std::string SoftwareIntegrationModule::readString(std::vector<char>& message) {

        std::string length;
        length.push_back(message[messageOffset]);
        length.push_back(message[messageOffset + 1]);
        messageOffset += 2;

        int lengthOfString = stoi(length);
        std::string name;
        int counter = 0;
        while (counter != lengthOfString)
        {
            name.push_back(message[messageOffset]);
            messageOffset++;
            counter++;
        }

        return name;
    }

    bool SoftwareIntegrationModule::isConnected(const Peer& peer) const {
        return peer.status != SoftwareConnection::Status::Connecting &&
            peer.status != SoftwareConnection::Status::Disconnected;
    }

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

