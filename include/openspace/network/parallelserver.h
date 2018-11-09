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

#ifndef __OPENSPACE_CORE___PARALLELSERVER___H__
#define __OPENSPACE_CORE___PARALLELSERVER___H__

#include <openspace/network/parallelconnection.h>

#include <openspace/util/concurrentqueue.h>
#include <ghoul/io/socket/tcpsocketserver.h>
#include <atomic>
#include <string>
#include <unordered_map>

namespace openspace {

class ParallelServer {
public:
    void start(int port, const std::string& password,
        const std::string& changeHostPassword);

    void setDefaultHostAddress(std::string defaultHostAddress);

    std::string defaultHostAddress() const;

    void stop();

    size_t nConnections() const;

private:
    struct Peer {
        //Peer(size_t id_, std::string name_, ParallelConnection parallelConnection_,
            //ParallelConnection::Status status_, std::thread )

        size_t id;
        std::string name;
        ParallelConnection parallelConnection;
        ParallelConnection::Status status;
        std::thread thread;
    };

    struct PeerMessage {
        size_t peerId;
        ParallelConnection::Message message;
    };

    bool isConnected(std::shared_ptr<Peer> peer) const;

    void sendMessage(std::shared_ptr<Peer> peer,
        ParallelConnection::MessageType messageType,
        const std::vector<char>& message);

    void sendMessageToAll(ParallelConnection::MessageType messageType,
        const std::vector<char>& message);

    void sendMessageToClients(ParallelConnection::MessageType messageType,
        const std::vector<char>& message);

    void disconnect(std::shared_ptr<Peer> peer);
    void setName(std::shared_ptr<Peer> peer, std::string name);
    void assignHost(std::shared_ptr<Peer> newHost);
    void setToClient(std::shared_ptr<Peer> peer);
    void setNConnections(size_t nConnections);
    void sendConnectionStatus(std::shared_ptr<Peer> peer);

    void handleAuthentication(std::shared_ptr<Peer> peer, std::vector<char> message);
    void handleData(std::shared_ptr<Peer> peer, std::vector<char> data);
    void handleHostshipRequest(std::shared_ptr<Peer> peer, std::vector<char> message);
    void handleHostshipResignation(std::shared_ptr<Peer> peer, std::vector<char> data);
    void handleDisconnection(std::shared_ptr<Peer> peer);

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
    size_t _passwordHash;
    size_t _changeHostPasswordHash;
    size_t _nextConnectionId = 1;
    std::atomic_bool _shouldStop = false;

    std::atomic_size_t _nConnections = 0;
    std::atomic_size_t _hostPeerId = 0;

    mutable std::mutex _hostInfoMutex;
    std::string _hostName;
    std::string _defaultHostAddress;

    ConcurrentQueue<PeerMessage> _incomingMessages;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___PARALLELSERVER___H__
