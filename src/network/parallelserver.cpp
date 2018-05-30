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

#include <openspace/network/parallelserver.h>

#include <ghoul/fmt.h>
#include <ghoul/io/socket/tcpsocket.h>
#include <ghoul/logging/logmanager.h>
#include <functional>

// @TODO(abock): In the entire class remove std::shared_ptr<Peer> by const Peer& where
//               possible to simplify the interface

namespace {
    constexpr const char* _loggerCat = "ParallelServer";
} // namespace

namespace openspace {

void ParallelServer::start(int port, const std::string& password,
                           const std::string& changeHostPassword)
{
    _socketServer.listen("localhost", port);
    _passwordHash = std::hash<std::string>{}(password);
    _changeHostPasswordHash = std::hash<std::string>{}(changeHostPassword);

    _serverThread = std::thread([this](){ handleNewPeers(); });
    _eventLoopThread = std::thread([this]() { eventLoop(); });
}

void ParallelServer::setDefaultHostAddress(std::string defaultHostAddress) {
    std::lock_guard<std::mutex> lock(_hostInfoMutex);
    _defaultHostAddress = std::move(defaultHostAddress);
}

std::string ParallelServer::defaultHostAddress() const {
    std::lock_guard<std::mutex> lock(_hostInfoMutex);
    return _defaultHostAddress;
}

void ParallelServer::stop() {
    _shouldStop = true;
    _socketServer.close();
}

void ParallelServer::handleNewPeers() {
    while (!_shouldStop) {
        std::unique_ptr<ghoul::io::TcpSocket> socket =
            _socketServer.awaitPendingTcpSocket();

        socket->startStreams();

        const size_t id = _nextConnectionId++;
        std::shared_ptr<Peer> p = std::make_shared<Peer>(Peer{
            id,
            "",
            ParallelConnection(std::move(socket)),
            ParallelConnection::Status::Connecting,
            std::thread()
        });
        auto it = _peers.emplace(p->id, p);
        it.first->second->thread = std::thread([this, id]() {
            handlePeer(id);
        });
    }
}

std::shared_ptr<ParallelServer::Peer> ParallelServer::peer(size_t id) {
    std::lock_guard<std::mutex> lock(_peerListMutex);
    auto it = _peers.find(id);
    if (it == _peers.end()) {
        return nullptr;
    }
    return it->second;
}

void ParallelServer::handlePeer(size_t id) {
    while (!_shouldStop) {
        std::shared_ptr<Peer> p = peer(id);
        if (!p) {
            return;
        }

        if (!p->parallelConnection.isConnectedOrConnecting()) {
            return;
        }
        try {
            ParallelConnection::Message m = p->parallelConnection.receiveMessage();
            _incomingMessages.push({id, m});
        } catch (const ParallelConnection::ConnectionLostError&) {
            LERROR(fmt::format("Connection lost to {}", p->id));
            _incomingMessages.push({
                id,
                ParallelConnection::Message(
                    ParallelConnection::MessageType::Disconnection, std::vector<char>()
                )
            });
            return;
        }
    }
}

void ParallelServer::eventLoop() {
    while (!_shouldStop) {
        PeerMessage pm = _incomingMessages.pop();
        handlePeerMessage(std::move(pm));
    }
}

void ParallelServer::handlePeerMessage(PeerMessage peerMessage) {
    const size_t peerId = peerMessage.peerId;
    auto it = _peers.find(peerId);
    if (it == _peers.end()) {
        return;
    }

    std::shared_ptr<Peer>& peer = it->second;

    const ParallelConnection::MessageType messageType = peerMessage.message.type;
    std::vector<char>& data = peerMessage.message.content;
    switch (messageType) {
        case ParallelConnection::MessageType::Authentication:
            handleAuthentication(peer, std::move(data));
            break;
        case ParallelConnection::MessageType::Data:
            handleData(peer, std::move(data));
            break;
        case ParallelConnection::MessageType::HostshipRequest:
            handleHostshipRequest(peer, std::move(data));
            break;
        case ParallelConnection::MessageType::HostshipResignation:
            handleHostshipResignation(peer, std::move(data));
            break;
        case ParallelConnection::MessageType::Disconnection:
            disconnect(peer);
            break;
        default:
            LERROR(fmt::format(
                "Unsupported message type: {}", static_cast<int>(messageType)
            ));
            break;
    }
}

void ParallelServer::handleAuthentication(std::shared_ptr<Peer> peer,
                                          std::vector<char> message)
{
    std::stringstream input(std::string(message.begin(), message.end()));

    // 8 bytes passcode
    uint64_t passwordHash = 0;
    input.read(reinterpret_cast<char*>(&passwordHash), sizeof(uint64_t));

    if (passwordHash != _passwordHash) {
        LERROR(fmt::format("Connection {} provided incorrect passcode.", peer->id));
        disconnect(peer);
        return;
    }

    // 4 bytes name size
    uint32_t nameSize = 0;
    input.read(reinterpret_cast<char*>(&nameSize), sizeof(uint32_t));

    // <nameSize> bytes name
    std::string name(nameSize, static_cast<char>(0));
    input.read(&name[0], nameSize);

    if (nameSize == 0) {
        name = "Anonymous";
    }

    setName(peer, name);

    LINFO(fmt::format("Connection established with {} \"{}\"", peer->id, name));

    std::string defaultHostAddress;
    {
        std::lock_guard<std::mutex> _hostMutex(_hostInfoMutex);
        defaultHostAddress = _defaultHostAddress;
    }
    if (_hostPeerId == 0 &&
        peer->parallelConnection.socket()->address() == defaultHostAddress)
    {
        // Directly promote the conenction to host (initialize)
        // if there is no host, and ip matches default host ip.
        LINFO(fmt::format("Connection {} directly promoted to host.", peer->id));
        assignHost(peer);
        for (std::pair<const size_t, std::shared_ptr<Peer>>& it : _peers) {
            // sendConnectionStatus(it->second) ?
            sendConnectionStatus(peer);
        }
    }
    else {
        setToClient(peer);
    }

    setNConnections(nConnections() + 1);
}

void ParallelServer::handleData(std::shared_ptr<Peer> peer, std::vector<char> data) {
    if (peer->id != _hostPeerId) {
        LINFO(fmt::format(
            "Connection {} tried to send data without being the host. Ignoring", peer->id
        ));
    }
    sendMessageToClients(ParallelConnection::MessageType::Data, data);

}

void ParallelServer::handleHostshipRequest(std::shared_ptr<Peer> peer,
                                           std::vector<char> message)
{
    std::stringstream input(std::string(message.begin(), message.end()));

    LINFO(fmt::format("Connection {} requested hostship.", peer->id));

    uint64_t passwordHash = 0;
    input.read(reinterpret_cast<char*>(&passwordHash), sizeof(uint64_t));

    if (passwordHash != _changeHostPasswordHash) {
        LERROR(fmt::format("Connection {} provided incorrect host password.", peer->id));
        return;
    }

    size_t oldHostPeerId = 0;
    {
        std::lock_guard<std::mutex> lock(_hostInfoMutex);
        oldHostPeerId = _hostPeerId;
    }

    if (oldHostPeerId == peer->id) {
        LINFO(fmt::format("Connection {} is already the host.", peer->id));
        return;
    }

    assignHost(peer);
    LINFO(fmt::format("Switched host from {} to {}.", oldHostPeerId, peer->id));
}

void ParallelServer::handleHostshipResignation(std::shared_ptr<Peer> peer,
                                               std::vector<char>)
{
    LINFO(fmt::format("Connection {} wants to resign its hostship.", peer->id));

    size_t oldHostPeerId = 0;
    {
        std::lock_guard<std::mutex> lock(_hostInfoMutex);
        oldHostPeerId = _hostPeerId;
    }

    setToClient(peer);

    LINFO(fmt::format("Connection {} resigned as host.", peer->id));
}

bool ParallelServer::isConnected(std::shared_ptr<Peer> peer) const {
    return peer->status != ParallelConnection::Status::Connecting &&
           peer->status != ParallelConnection::Status::Disconnected;
}

void ParallelServer::sendMessage(std::shared_ptr<Peer> peer,
                                 ParallelConnection::MessageType messageType,
                                 const std::vector<char>& message)
{
    peer->parallelConnection.sendMessage({ messageType, message });
}

void ParallelServer::sendMessageToAll(ParallelConnection::MessageType messageType,
                                      const std::vector<char>& message)
{
    for (std::pair<const size_t, std::shared_ptr<Peer>>& it : _peers) {
        if (isConnected(it.second)) {
            it.second->parallelConnection.sendMessage({ messageType, message });
        }
    }
}

void ParallelServer::sendMessageToClients(ParallelConnection::MessageType messageType,
                                          const std::vector<char>& message)
{
    for (std::pair<const size_t, std::shared_ptr<Peer>>& it : _peers) {
        if (it.second->status == ParallelConnection::Status::ClientWithHost) {
            it.second->parallelConnection.sendMessage({ messageType, message });
        }
    }
}

void ParallelServer::disconnect(std::shared_ptr<Peer> peer) {
    if (isConnected(peer)) {
        setNConnections(nConnections() - 1);
    }

    size_t hostPeerId = 0;
    {
        std::lock_guard<std::mutex> lock(_hostInfoMutex);
        hostPeerId = _hostPeerId;
    }

    // Make sure any disconnecting host is first degraded to client,
    // in order to notify other clients about host disconnection.
    if (peer->id == hostPeerId) {
        setToClient(peer);
    }

    peer->parallelConnection.disconnect();
    peer->thread.join();
    _peers.erase(peer->id);
}

void ParallelServer::setName(std::shared_ptr<Peer> peer, std::string name) {
    peer->name = name;
    size_t hostPeerId = 0;
    {
        std::lock_guard<std::mutex> lock(_hostInfoMutex);
        hostPeerId = _hostPeerId;
    }

    // Make sure everyone gets the new host name.
    if (peer->id == hostPeerId) {
        {
            std::lock_guard<std::mutex> lock(_hostInfoMutex);
            _hostName = name;
        }

        for (std::pair<const size_t, std::shared_ptr<Peer>>& it : _peers) {
            // sendConnectionStatus(it->second) ?
            sendConnectionStatus(peer);
        }
    }
}

void ParallelServer::assignHost(std::shared_ptr<Peer> newHost) {
    {
        std::lock_guard<std::mutex> lock(_hostInfoMutex);
        std::shared_ptr<ParallelServer::Peer> oldHost = peer(_hostPeerId);

        if (oldHost) {
            oldHost->status = ParallelConnection::Status::ClientWithHost;
        }
        _hostPeerId = newHost->id;
        _hostName = newHost->name;
    }
    newHost->status = ParallelConnection::Status::Host;

    for (std::pair<const size_t, std::shared_ptr<Peer>>& it : _peers) {
        if (it.second != newHost) {
            it.second->status = ParallelConnection::Status::ClientWithHost;
        }
        sendConnectionStatus(it.second);
    }
}

void ParallelServer::setToClient(std::shared_ptr<Peer> peer) {
    if (peer->status == ParallelConnection::Status::Host) {
        {
            std::lock_guard<std::mutex> lock(_hostInfoMutex);
            _hostPeerId = 0;
            _hostName = "";
        }

        // If host becomes client, make all clients hostless.
        for (std::pair<const size_t, std::shared_ptr<Peer>>& it : _peers) {
            it.second->status = ParallelConnection::Status::ClientWithoutHost;
            sendConnectionStatus(it.second);
        }
    } else {
        peer->status = (_hostPeerId > 0) ?
            ParallelConnection::Status::ClientWithHost :
            ParallelConnection::Status::ClientWithoutHost;
        sendConnectionStatus(peer);
    }
}

void ParallelServer::setNConnections(size_t nConnections) {
    _nConnections = nConnections;
    std::vector<char> data;
    const uint32_t n = static_cast<uint32_t>(_nConnections);
    data.insert(
        data.end(),
        reinterpret_cast<const char*>(&n),
        reinterpret_cast<const char*>(&n) + sizeof(uint32_t)
    );
    sendMessageToAll(ParallelConnection::MessageType::NConnections, data);
}

void ParallelServer::sendConnectionStatus(std::shared_ptr<Peer> peer) {
    std::vector<char> data;
    const uint32_t outStatus = static_cast<uint32_t>(peer->status);
    data.insert(
        data.end(),
        reinterpret_cast<const char*>(&outStatus),
        reinterpret_cast<const char*>(&outStatus) + sizeof(uint32_t)
    );

    const uint32_t outHostNameSize = static_cast<uint32_t>(_hostName.size());
    data.insert(
        data.end(),
        reinterpret_cast<const char*>(&outHostNameSize),
        reinterpret_cast<const char*>(&outHostNameSize) + sizeof(uint32_t)
    );

    data.insert(
        data.end(),
        reinterpret_cast<const char*>(_hostName.data()),
        reinterpret_cast<const char*>(_hostName.data() + outHostNameSize)
    );

    sendMessage(peer, ParallelConnection::MessageType::ConnectionStatus, data);
}

size_t ParallelServer::nConnections() const {
    return _nConnections;
}

} // namespace openspace
