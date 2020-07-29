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

    const unsigned int SoftwareIntegrationModule::ProtocolVersion = 1;

    SoftwareIntegrationModule::SoftwareIntegrationModule() : OpenSpaceModule(Name) {}

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

    // Server
    void SoftwareIntegrationModule::start(int port)
    {
        _socketServer.listen(port);
        _socketServer.awaitPendingTcpSocket();
        //_serverThread = std::thread([this]() { handleNewPeers(); });
    }

    // Server
    void SoftwareIntegrationModule::setDefaultHostAddress(std::string defaultHostAddress) {
        std::lock_guard<std::mutex> lock(_hostInfoMutex);
        _defaultHostAddress = std::move(defaultHostAddress);
    }

    // Server
    std::string SoftwareIntegrationModule::defaultHostAddress() const {
        std::lock_guard<std::mutex> lock(_hostInfoMutex);
        return _defaultHostAddress;
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
               // handlePeer(id);
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

    // Server
    bool SoftwareIntegrationModule::isConnected(const Peer& peer) const {
        return peer.status != Connection::Status::Connecting &&
            peer.status != Connection::Status::Disconnected;
    }

    // Server
    void SoftwareIntegrationModule::disconnect(Peer& peer) {
        if (isConnected(peer)) {
            //nConnections() - 1;
        }

        size_t hostPeerId = 0;
        {
            std::lock_guard<std::mutex> lock(_hostInfoMutex);
            hostPeerId = _hostPeerId;
        }

        // Make sure any disconnecting host is first degraded to client,
        // in order to notify other clients about host disconnection.
        if (peer.id == hostPeerId) {
            //setToClient(peer);
        }

        peer.connection.disconnect();
        peer.thread.join();
        _peers.erase(peer.id);
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

