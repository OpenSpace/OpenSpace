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

#ifndef __OPENSPACE_MODULE_SOFTWAREINTEGRATION___SOFTWAREINTEGRATIONMODULE___H__
#define __OPENSPACE_MODULE_SOFTWAREINTEGRATION___SOFTWAREINTEGRATIONMODULE___H__

#include <openspace/util/openspacemodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/network/messagestructures.h>
#include <openspace/util/concurrentqueue.h>
#include <ghoul/glm.h>
#include <ghoul/io/socket/tcpsocket.h>
#include <ghoul/io/socket/tcpsocketserver.h>
#include <ghoul/misc/exception.h>
#include <atomic>
#include <string>
#include <unordered_map>
#include <vector>

namespace openspace {

class SoftwareConnection {
public:
    // Connection
    enum class Status : uint32_t {
        Disconnected = 0,
        Connecting
    };

    enum class MessageType : uint32_t {
        Connection = 0,
        AddSceneGraphNode,
        RemoveSceneGraphNode,
        Color,
        Opacity,
        Size,
        Disconnection
    };

    struct Message {
        Message() = default;
        Message(MessageType type, std::vector<char> content);

        MessageType type;
        std::vector<char> content;
    };

    class SoftwareConnectionLostError : public ghoul::RuntimeError {
    public:
        explicit SoftwareConnectionLostError();
    };

    SoftwareConnection(std::unique_ptr<ghoul::io::TcpSocket> socket);

    // Connection
    bool isConnectedOrConnecting() const;
    void disconnect();
    ghoul::io::TcpSocket* socket();

    SoftwareConnection::Message receiveMessage();

    static const unsigned int ProtocolVersion;

private:
    // Connection
    std::unique_ptr<ghoul::io::TcpSocket> _socket;

};

class SoftwareIntegrationModule : public OpenSpaceModule {
public:
    constexpr static const char* Name = "SoftwareIntegration";

    SoftwareIntegrationModule();
    virtual ~SoftwareIntegrationModule() = default;

    // Server
    void start(int port);
    void stop();
    size_t nConnections() const;

    size_t messageOffset = 0;

    std::vector<documentation::Documentation> documentations() const override;
    scripting::LuaLibrary luaLibrary() const override;

private:
    // Server
    struct Peer {
        size_t id;
        std::string name;
        SoftwareConnection connection;
        SoftwareConnection::Status status;
        std::thread thread;
    };

    struct PeerMessage {
        size_t peerId;
        SoftwareConnection::Message message;
    };

    // Server
    bool isConnected(const Peer& peer) const;
    void disconnect(Peer& peer);
    void handleNewPeers();
    void eventLoop();
    std::shared_ptr<Peer> peer(size_t id);
    void handlePeer(size_t id);
    void handlePeerMessage(PeerMessage peerMessage);
    std::unordered_map<size_t, std::shared_ptr<Peer>> _peers;
    mutable std::mutex _peerListMutex;
    std::thread _serverThread;
    std::thread _eventLoopThread;
    ghoul::io::TcpSocketServer _socketServer;
    size_t _nextConnectionId = 1;
    std::atomic_bool _shouldStop = false;
    std::atomic_size_t _nConnections = 0;
    ConcurrentQueue<PeerMessage> _incomingMessages;

    std::string readIdentifier(std::vector<char>& message);
    float readFloatValue(std::vector<char>& message);
    glm::vec3 readColor(std::vector<char>& message);
    std::string readString(std::vector<char>& message);

    void internalInitialize(const ghoul::Dictionary&) override;
    void internalDeinitializeGL() override;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SOFTWAREINTEGRATION___SOFTWAREINTEGRATIONMODULE___H__
