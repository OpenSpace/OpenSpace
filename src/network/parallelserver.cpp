/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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
    _socketServer.listen(port);
    _passwordHash = std::hash<std::string>{}(password);
    _changeHostPasswordHash = std::hash<std::string>{}(changeHostPassword);

    _serverThread = std::thread([this](){ handleNewPeers(); });
    _eventLoopThread = std::thread([this]() { eventLoop(); });
}

void ParallelServer::setDefaultHostAddress(std::string defaultHostAddress) {
    std::lock_guard lock(_hostInfoMutex);
    _defaultHostAddress = std::move(defaultHostAddress);
}

std::string ParallelServer::defaultHostAddress() const {
    std::lock_guard lock(_hostInfoMutex);
    return _defaultHostAddress;
}

void ParallelServer::stop() {
    _shouldStop = true;
    _socketServer.close();
}

void ParallelServer::handleNewPeers() {
    while (!_shouldStop) {
        std::unique_ptr<ghoul::io::TcpSocket> s = _socketServer.awaitPendingTcpSocket();

        s->startStreams();

        const size_t id = _nextConnectionId++;
        std::shared_ptr<Peer> p = std::make_shared<Peer>(Peer{
            id,
            "",
            "",
            ParallelConnection(std::move(s)),
            ParallelConnection::Status::Connecting,
            ParallelConnection::ViewStatus::HostView,
            std::thread()
        });
        auto it = _peers.emplace(p->id, p);
        it.first->second->thread = std::thread([this, id]() { handlePeer(id); });
    }
}

std::shared_ptr<ParallelServer::Peer> ParallelServer::peer(size_t id) {
    std::lock_guard lock(_peerListMutex);
    const auto it = _peers.find(id);
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
            PeerMessage msg;
            msg.peerId = id;
            msg.message = m;
            _incomingMessages.push(msg);
        }
        catch (const ParallelConnection::ConnectionLostError&) {
            LERROR(fmt::format("Connection lost to {}", p->id));
            PeerMessage msg;
            msg.peerId = id;
            msg.message = ParallelConnection::Message(
                ParallelConnection::MessageType::Disconnection, std::vector<char>()
            );
            _incomingMessages.push(msg);
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

    const ParallelConnection::MessageType type = peerMessage.message.type;
    std::vector<char>& data = peerMessage.message.content;
    switch (type) {
        case ParallelConnection::MessageType::Authentication:
            handleAuthentication(peer, std::move(data));
            break;
        case ParallelConnection::MessageType::Data:
            handleData(*peer, std::move(data));
            break;
        case ParallelConnection::MessageType::HostshipRequest:
            handleHostshipRequest(peer, std::move(data));
            break;
        case ParallelConnection::MessageType::HostshipResignation:
            handleHostshipResignation(*peer);
            break;
        case ParallelConnection::MessageType::ViewRequest:
            handleViewRequest(*peer);
            break;
        case ParallelConnection::MessageType::ViewResignation:
            handleViewResignation(*peer);
            break;
        case ParallelConnection::MessageType::IndependentSessionOn:
            handleIndependentSessionOn(*peer);
            break;
        case ParallelConnection::MessageType::IndependentSessionOff:
            handleIndependentSessionOff(*peer);
            break;
        case ParallelConnection::MessageType::Disconnection:
            disconnect(*peer);
            break;
        default:
            LERROR(fmt::format("Unsupported message type: {}", static_cast<int>(type)));
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
        disconnect(*peer);
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

    setName(*peer, name);
    assignViewerModel(*peer);

    LINFO(fmt::format("Connection established with {} ('{}', {})", peer->id, name, peer->model));

    std::string defaultHostAddress;
    {
        std::lock_guard _hostMutex(_hostInfoMutex);
        defaultHostAddress = _defaultHostAddress;
    }
    if (_hostPeerId == 0 &&
        peer->parallelConnection.socket()->address() == defaultHostAddress)
    {
        // Directly promote the connection to host (initialize) if there is no host, and
        // ip matches default host ip.
        LINFO(fmt::format("Connection {} directly promoted to host", peer->id));
        assignHost(peer);
        for (std::pair<const size_t, std::shared_ptr<Peer>>& it : _peers) {
            // sendConnectionStatus(it->second) ?
            sendConnectionStatus(*peer);
        }
    }
    else {
        setToClient(*peer);
    }

    setNConnections(nConnections() + 1);
}

void ParallelServer::handleData(const Peer& peer, std::vector<char> data) {
    if (peer.id == _hostPeerId) {
        sendMessageToClients(ParallelConnection::MessageType::Data, data);
    }
    else if (peer.viewStatus == ParallelConnection::ViewStatus::IndependentView) {
        sendMessageToAll(ParallelConnection::MessageType::Data, data);
    }
    else {
        LINFO(fmt::format(
            "Ignoring connection {} trying to send data without being host"
            " or using independent view", peer.id
        ));
    }
}

void ParallelServer::handleHostshipRequest(std::shared_ptr<Peer> peer,
                                           std::vector<char> message)
{
    std::stringstream input(std::string(message.begin(), message.end()));

    LINFO(fmt::format("Connection {} requested hostship", peer->id));

    uint64_t passwordHash = 0;
    input.read(reinterpret_cast<char*>(&passwordHash), sizeof(uint64_t));

    if (passwordHash != _changeHostPasswordHash) {
        LERROR(fmt::format("Connection {} provided incorrect host password", peer->id));
        return;
    }

    size_t oldHostPeerId = 0;
    {
        std::lock_guard lock(_hostInfoMutex);
        oldHostPeerId = _hostPeerId;
    }

    if (oldHostPeerId == peer->id) {
        LINFO(fmt::format("Connection {} is already the host", peer->id));
        return;
    }

    assignHost(peer);
    LINFO(fmt::format("Switched host from {} to {}", oldHostPeerId, peer->id));
}

void ParallelServer::handleHostshipResignation(Peer& peer) {
    LINFO(fmt::format("Connection {} wants to resign its hostship", peer.id));

    setToClient(peer);

    LINFO(fmt::format("Connection {} resigned as host", peer.id));
}

void ParallelServer::handleViewRequest(Peer& peer) {
    if (_independentViewAllowed) {
        setViewStatus(peer, ParallelConnection::ViewStatus::IndependentView);
        LINFO(fmt::format("{} is now using host-independent viewpoint", peer.name));
        sendViewStatusChange(peer);
    }
}

void ParallelServer::handleViewResignation(Peer& peer) {
    setViewStatus(peer, ParallelConnection::ViewStatus::HostView);
    LINFO(fmt::format("{} is now using the host's viewpoint", peer.name));
    sendViewStatusChange(peer);
}

void ParallelServer::handleIndependentSessionOn(Peer& peer) {
    sendIndependentSessionOn();
    _independentViewAllowed = true;

    LINFO(fmt::format("Host ({}) is now allowing host-independent viewpoints", peer.id));
}

void ParallelServer::handleIndependentSessionOff(Peer& peer) {
    sendIndependentSessionOff();
    _independentViewAllowed = false;

    LINFO(fmt::format("Host ({}) is now NOT allowing host-independent viewpoints", peer.id));
}

bool ParallelServer::isConnected(const Peer& peer) const {
    return peer.status != ParallelConnection::Status::Connecting &&
           peer.status != ParallelConnection::Status::Disconnected;
}

void ParallelServer::sendMessage(Peer& peer, ParallelConnection::MessageType messageType,
                                 const std::vector<char>& message)
{
    peer.parallelConnection.sendMessage({ messageType, message });
}

void ParallelServer::sendMessageToAll(ParallelConnection::MessageType messageType,
                                      const std::vector<char>& message)
{
    for (std::pair<const size_t, std::shared_ptr<Peer>>& it : _peers) {
        if (isConnected(*it.second)) {
            it.second->parallelConnection.sendMessage({ messageType, message });
        }
    }
}

void ParallelServer::sendMessageToClients(ParallelConnection::MessageType messageType,
                                          const std::vector<char>& message)
{
    for (std::pair<const size_t, std::shared_ptr<Peer>>& it : _peers) {
        if (it.second->status == ParallelConnection::Status::ClientWithHost ||
            it.second->viewStatus == ParallelConnection::ViewStatus::IndependentView)
        {
            it.second->parallelConnection.sendMessage({ messageType, message });
        }
    }
}

void ParallelServer::disconnect(Peer& peer) {
    if (isConnected(peer)) {
        setNConnections(nConnections() - 1);
    }

    size_t hostPeerId = 0;
    {
        std::lock_guard lock(_hostInfoMutex);
        hostPeerId = _hostPeerId;
    }

    // Make sure any disconnecting host is first degraded to client, in order to notify
    // other clients about host disconnection.
    if (peer.id == hostPeerId) {
        setToClient(peer);
    }

    removeViewerModel(peer);

    peer.parallelConnection.disconnect();
    peer.thread.join();
    _peers.erase(peer.id);
}

void ParallelServer::setName(Peer& peer, std::string name) {
    peer.name = std::move(name);
    size_t hostPeerId = 0;
    {
        std::lock_guard lock(_hostInfoMutex);
        hostPeerId = _hostPeerId;
    }

    // Make sure everyone gets the new host name.
    if (peer.id == hostPeerId) {
        {
            std::lock_guard lock(_hostInfoMutex);
            _hostName = peer.name;
        }

        for (std::pair<const size_t, std::shared_ptr<Peer>>& it : _peers) {
            // sendConnectionStatus(it->second) ?
            sendConnectionStatus(peer);
        }
    }
}

void ParallelServer::assignHost(std::shared_ptr<Peer> newHost) {
    {
        std::lock_guard lock(_hostInfoMutex);
        std::shared_ptr<ParallelServer::Peer> oldHost = peer(_hostPeerId);

        if (oldHost) {
            oldHost->status = ParallelConnection::Status::ClientWithHost;
            oldHost->viewStatus = ParallelConnection::ViewStatus::IndependentView;
        }
        _hostPeerId = newHost->id;
        _hostName = newHost->name;
    }
    newHost->status = ParallelConnection::Status::Host;
    newHost->viewStatus = ParallelConnection::ViewStatus::Host;

    for (std::pair<const size_t, std::shared_ptr<Peer>>& it : _peers) {
        if (it.second != newHost) {
            it.second->status = ParallelConnection::Status::ClientWithHost;
            it.second->viewStatus = ParallelConnection::ViewStatus::HostView;
        }
        sendConnectionStatus(*it.second);
    }
}

void ParallelServer::setToClient(Peer& peer) {
    if (peer.status == ParallelConnection::Status::Host) {
        {
            std::lock_guard lock(_hostInfoMutex);
            _hostPeerId = 0;
            _hostName.clear();
        }

        // If host becomes client, make all clients hostless.
        for (std::pair<const size_t, std::shared_ptr<Peer>>& it : _peers) {
            it.second->status = ParallelConnection::Status::ClientWithoutHost;
            it.second->viewStatus = ParallelConnection::ViewStatus::IndependentView;
            sendConnectionStatus(*it.second);
        }
    }
    else {
        peer.status = (_hostPeerId > 0) ?
            ParallelConnection::Status::ClientWithHost :
            ParallelConnection::Status::ClientWithoutHost;
        peer.viewStatus = (_hostPeerId > 0) ?
            ParallelConnection::ViewStatus::Host :
            ParallelConnection::ViewStatus::HostView;
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

void ParallelServer::sendConnectionStatus(Peer& peer) {
    std::vector<char> data;
    const uint32_t outStatus = static_cast<uint32_t>(peer.status);
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
        _hostName.data(),
        _hostName.data() + outHostNameSize
    );

    sendMessage(peer, ParallelConnection::MessageType::ConnectionStatus, data);
}

void ParallelServer::setViewStatus(Peer& peer, ParallelConnection::ViewStatus viewStatus) {
    peer.viewStatus = viewStatus;
}

void ParallelServer::sendIndependentSessionOn() {
    std::vector<char> data;
    sendMessageToAll(ParallelConnection::MessageType::IndependentSessionOn, data);
}

void ParallelServer::sendIndependentSessionOff() {
    std::vector<char> data;
    sendMessageToAll(ParallelConnection::MessageType::IndependentSessionOff, data);
}

// Sends ViewStatus and model information about the given peer to all clients.
void ParallelServer::sendViewStatusChange(Peer& peer) {
    std::vector<char> data;
    const uint32_t outViewStatus = static_cast<uint32_t>(peer.viewStatus);
    data.insert(
        data.end(),
        reinterpret_cast<const char*>(&outViewStatus),
        reinterpret_cast<const char*>(&outViewStatus) + sizeof(uint32_t)
    );

    const uint32_t outPeerNameSize = static_cast<uint32_t>(peer.name.size());
    data.insert(
        data.end(),
        reinterpret_cast<const char*>(&outPeerNameSize),
        reinterpret_cast<const char*>(&outPeerNameSize) + sizeof(uint32_t)
    );

    data.insert(
        data.end(),
        peer.name.data(),
        peer.name.data() + outPeerNameSize
    );

    const uint32_t outPeerModelSize = static_cast<uint32_t>(peer.model.size());
    data.insert(
        data.end(),
        reinterpret_cast<const char*>(&outPeerModelSize),
        reinterpret_cast<const char*>(&outPeerModelSize) + sizeof(uint32_t)
    );

    data.insert(
        data.end(),
        peer.model.data(),
        peer.model.data() + outPeerModelSize
    );

    sendMessageToAll(ParallelConnection::MessageType::ViewStatusChange, data);
}

// Assign a model name to peer.
void ParallelServer::assignViewerModel(Peer& peer) {
    // Check if peer already has an assigned model.
    if (peer.model != "") {
        LERROR(fmt::format("User '{}' already has an assigned model.", peer.name));
        return;
    }

    // Assign first available model to peer.
    for (int i = 0; i < sizeof(modelList) / sizeof(modelList[0]); ++i)
    {
        if (modelList[i].second == false) {
            modelList[i].second = true;
            peer.model = modelList[i].first;
            LINFO(fmt::format("Set '{}'s model to '{}'.", peer.name, peer.model));
            return;
        }
    }

    LERROR(fmt::format("Ran out of available models. None assigned to '{}'.", peer.name));
}

// Remove the model assigned to peer.
void ParallelServer::removeViewerModel(Peer& peer) {
    for (int i = 0; i < sizeof(modelList) / sizeof(modelList[0]); ++i)
    {
        if (modelList[i].first == peer.model) {
            peer.model = "";
            modelList[i].second = false;
            LINFO(fmt::format("Removed model from '{}'.", peer.name));
            return;
        }
    }

    LERROR(fmt::format("Could not remove '{}'s model because they had none assigned.", peer.name));
}

size_t ParallelServer::nConnections() const {
    return _nConnections;
}

} // namespace openspace
