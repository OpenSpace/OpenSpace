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

namespace {
const char* _loggerCat = "ParallelServer";
} // namespace

namespace openspace {

void ParallelServer::start(int port, const std::string& password) {
    _socketServer.listen("localhost", port);
    _password = password;
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
        std::unique_ptr<ghoul::io::TcpSocket> socket = _socketServer.awaitPendingTcpSocket();
        size_t id = _nextConnectionId++;
        Peer p = {
            id,
            std::move(socket),
            std::thread([this, id] () {
                handlePeer(id);
            })
        };
        _peers.emplace(p.id, std::move(p));
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
    
void ParallelServer::handleAuthentication(size_t peerId, std::vector<char> data) {
    LINFO("Authentication message received");
}
    
void ParallelServer::handleData(size_t peerId, std::vector<char> data) {
    LINFO("Data message received");
}

void ParallelServer::handleHostshipRequest(size_t peerId, std::vector<char> data) {
    LINFO("Hostship request received");
}
    
void ParallelServer::handleHostshipResignation(size_t peerId, std::vector<char> data) {
    LINFO("Hostship resignation received");
}


} // namespace openspace
