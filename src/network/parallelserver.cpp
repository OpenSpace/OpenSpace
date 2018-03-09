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

#include <ghoul/logging/logmanager.h>

#include <functional>

namespace {
const char* _loggerCat = "ParallelServer";
} // namespace

namespace openspace {

void ParallelServer::start(
    int port,
    const std::string& password,
    const std::string& changeHostPassword)
{
    _socketServer.listen("localhost", port);
    _passwordHash = std::hash<std::string>{}(password);
    _changeHostPasswordHash = std::hash<std::string>{}(changeHostPassword);

    _serverThread = std::thread([this](){
        handleNewPeers();
    });
    _eventLoopThread = std::thread([this]() {
        eventLoop();
    });
}

void ParallelServer::stop() {
    _shouldStop = true;
    _socketServer.close();
}

void ParallelServer::handleNewPeers() {
    while (!_shouldStop) {
        std::unique_ptr<ghoul::io::TcpSocket> socket =
            _socketServer.awaitPendingTcpSocket();

        size_t id = _nextConnectionId++;
        Peer p{
            id,
            "",
            ParallelConnection(std::move(socket)),
            ParallelConnection::Status::Connecting,
            std::thread()
        };
        auto it = _peers.emplace(p.id, std::move(p));
        it.first->second.thread = std::thread([this, id]() {
            handlePeer(id);
        });
    }
}
    
void ParallelServer::handlePeer(size_t id) {
    while (!_shouldStop) {
        std::lock_guard<std::mutex> lock(_peerListMutex);
        const auto& it = _peers.find(id);
        if (it == _peers.end()) {
            return;
        }
        Peer& p = it->second;
        if (!p.parallelConnection.isConnectedOrConnecting()) {
            return;
        }
        try {
            ParallelConnection::Message m = p.parallelConnection.receiveMessage();
            _incomingMessages.push({id, m});
        } catch (const ParallelConnection::ConnectionLostError&) {
            LERROR(fmt::format("Connection lost to {}", p.id));
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
    ParallelConnection::MessageType messageType = peerMessage.message.type;
    size_t peerId = peerMessage.peerId;
    std::vector<char>& data = peerMessage.message.content;
    
    switch (messageType) {
        case ParallelConnection::MessageType::Authentication:
            handleAuthentication(peerId, std::move(data));
            break;
        case ParallelConnection::MessageType::Data:
            handleData(peerId, std::move(data));
            break;
        case ParallelConnection::MessageType::HostshipRequest:
            handleHostshipRequest(peerId, std::move(data));
            break;
        case ParallelConnection::MessageType::HostshipResignation:
            handleHostshipResignation(peerId, std::move(data));
            break;
        default:
            LERROR(fmt::format("Unsupported message type: {}",
                static_cast<int>(messageType)));
            break;
    }
}
    
void ParallelServer::handleAuthentication(size_t peerId, std::vector<char> message) {
    std::stringstream input(std::string(message.begin(), message.end()));

    // 8 bytes passcode
    uint64_t passwordHash = 0;
    input.read(reinterpret_cast<char*>(&passwordHash), sizeof(uint64_t));

    if (passwordHash != _passwordHash) {
        LERROR(fmt::format("Connection {} provided incorrect passcode.", peerId));
        disconnect(peerId);
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

    setName(peerId, name);
    const auto& it = _peers.find(peerId);
    if (it == _peers.end()) {
        return;
    }

    LINFO(fmt::format("Connection established with {} \"{}\"", peerId, name));

    std::string hostAddress;
    {
        std::lock_guard<std::mutex> _hostAddressMutex(_hostInfoMutex);
        hostAddress = _hostAddress;
    }
    if (_hostPeerId == -1 &&
        it->second.parallelConnection.socket()->address() == hostAddress)
    {
        // Directly promote the conenction to host (initialize)
        // if it is the first to connect from host ip.
        LINFO(fmt::format("Connection {} directly promoted to host.", peerId));
        assignHost(peerId);
    }
    else {
        setToClient(peerId);
    }

    setNConnections(nConnections() + 1);
}
    
void ParallelServer::handleData(size_t peerId, std::vector<char> data) {
    if (peerId != _hostPeerId) {
        LINFO(fmt::format(
            "Connection {} tried to send data without being the host. Ignoring", peerId
        ));
    }
    sendMessageToClients(ParallelConnection::MessageType::Data, data);

}

void ParallelServer::handleHostshipRequest(size_t peerId, std::vector<char> message) {
    std::stringstream input(std::string(message.begin(), message.end()));

    LINFO(fmt::format("Connection {} requested hostship.", peerId));

    uint64_t passwordHash = 0;
    input.read(reinterpret_cast<char*>(&passwordHash), sizeof(uint64_t));

    if (passwordHash != _changeHostPasswordHash) {
        LERROR(fmt::format("Connection {} provided incorrect host password.", peerId));
        return;
    }

    size_t oldHostPeerId = 0;
    {
        std::lock_guard<std::mutex> lock(_hostInfoMutex);
        oldHostPeerId = _hostPeerId;
    }

    const int newHostPeerId = peerId;

    if (oldHostPeerId == newHostPeerId) {
        LINFO(fmt::format("Connection {} is already the host.", peerId));
        return;
    }

    setToClient(oldHostPeerId);
    assignHost(newHostPeerId);
    LINFO(fmt::format("Switched host from {} to {}.", oldHostPeerId, newHostPeerId));
}
    
void ParallelServer::handleHostshipResignation(size_t peerId, std::vector<char> data) {
    LINFO(fmt::format("Connection {} wants to resign its hostship.", peerId));

    size_t oldHostPeerId = 0;
    {
        std::lock_guard<std::mutex> lock(_hostInfoMutex);
        oldHostPeerId = _hostPeerId;
    }

    if (oldHostPeerId != peerId) {
        LINFO(fmt::format("Connection is not the host.", peerId));
        return;
    }

    setToClient(peerId);
    LINFO(fmt::format("Connection {} resigned as host.", peerId));
}


void ParallelServer::sendMessage(size_t peerId,
    ParallelConnection::MessageType messageType,
    const std::vector<char>& message)
{
    const auto& it = _peers.find(peerId);
    if (it == _peers.end()) {
        return;
    }
    it->second.parallelConnection.sendMessage(
        ParallelConnection::Message(messageType, message)
    );
}

void ParallelServer::sendMessageToAll(ParallelConnection::MessageType messageType,
    const std::vector<char>& message)
{
    for (auto& it : _peers) {
        if (it.second.status != ParallelConnection::Status::Connecting &&
            it.second.status != ParallelConnection::Status::Disconnected) {
            it.second.parallelConnection.sendMessage(
                ParallelConnection::Message(messageType, message)
            );
        }
    }
}

void ParallelServer::sendMessageToClients(ParallelConnection::MessageType messageType,
    const std::vector<char>& message)
{
    for (auto& it : _peers) {
        if (it.second.status == ParallelConnection::Status::ClientWithHost) {
            it.second.parallelConnection.sendMessage(
                ParallelConnection::Message(messageType, message)
            );
        }
    }
}

void ParallelServer::disconnect(size_t peerId) {
    const auto& it = _peers.find(peerId);
    if (it == _peers.end()) {
        return;
    }
    it->second.parallelConnection.disconnect();
    it->second.thread.join();
    _peers.erase(it);
}

void ParallelServer::setName(size_t peerId, std::string name) {
    const auto& it = _peers.find(peerId);
    if (it == _peers.end()) {
        return;
    }
    it->second.name = name;
    
    size_t hostPeerId = 0;
    {
        std::lock_guard<std::mutex> lock(_hostInfoMutex);
        hostPeerId = _hostPeerId;
    }

    // Make sure everyone gets the new host name.
    if (peerId == hostPeerId) {
        {
            std::lock_guard<std::mutex> lock(_hostInfoMutex);
            _hostName = name;
        }

        for (auto& it : _peers) {
            sendConnectionStatus(it.first);
        }
    }
}

void ParallelServer::assignHost(size_t peerId) {
    const auto& it = _peers.find(peerId);
    if (it == _peers.end()) {
        return;
    }

    {
        std::lock_guard<std::mutex> lock(_hostInfoMutex);
        _hostPeerId = peerId;
        _hostAddress = it->second.parallelConnection.socket()->address();
        _hostName = it->second.name;
    }

    setConnectionStatus(peerId, ParallelConnection::Status::Host);
}

void ParallelServer::setToClient(size_t peerId) {
    ParallelConnection::Status status = ParallelConnection::Status::ClientWithoutHost;
    {
        std::lock_guard<std::mutex> lock(_hostInfoMutex);
        if (_hostPeerId == peerId) {
            _hostPeerId = 0;
            _hostAddress = "";
            _hostName = "";
            status = ParallelConnection::Status::ClientWithHost;
        }
    }
    setConnectionStatus(peerId, status);
}

void ParallelServer::setNConnections(size_t nConnections) {
    _nConnections = nConnections;
    std::vector<char> data;
    const uint32_t n = _nConnections;
    data.insert(
        data.end(),
        reinterpret_cast<const char*>(&n),
        reinterpret_cast<const char*>(&n) + sizeof(uint32_t)
    );
    sendMessageToAll(ParallelConnection::MessageType::NConnections, data);
}

void ParallelServer::setConnectionStatus(size_t peerId, ParallelConnection::Status status)
{
    const auto& it = _peers.find(peerId);
    if (it == _peers.end()) {
        return;
    }
    it->second.status = status;
    sendConnectionStatus(peerId);
}

void ParallelServer::sendConnectionStatus(size_t peerId) {
    const auto& it = _peers.find(peerId);
    if (it == _peers.end()) {
        return;
    }

    std::vector<char> data;
    const uint32_t outStatus = static_cast<uint32_t>(it->second.status);
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

    sendMessage(peerId, ParallelConnection::MessageType::ConnectionStatus, data);
}

size_t ParallelServer::nConnections() const {
    return _nConnections;
}

} // namespace openspace
