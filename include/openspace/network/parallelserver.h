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
#include <ghoul/io/socket/tcpsocket.h>

#include <string>
#include <unordered_map>
#include <atomic>

namespace openspace {

class ParallelServer {
public:
    void start(int port, const std::string& password);
    void stop();
private:
    struct Peer {
        size_t id;
        ParallelConnection parallelConnection;
        std::thread thread;
    };

    struct PeerMessage {
        size_t peerId;
        ParallelConnection::Message message;
    };

    void queueInMessage(const ParallelConnection::Message& message);

    void handleAuthentication(size_t id, std::vector<char> data);
    void handleData(size_t id, std::vector<char> data);
    void handleHostshipRequest(size_t id, std::vector<char> data);
    void handleHostshipResignation(size_t id, std::vector<char> data);

    void handleNewPeers();
    void eventLoop();
    void handlePeer(size_t id);
    void handlePeerMessage(PeerMessage peerMessage);

    std::unordered_map<size_t, Peer> _peers;
    std::mutex _peerListMutex;

    std::thread _serverThread;
    std::thread _eventLoopThread;
    ghoul::io::TcpSocketServer _socketServer;
    std::string _password;
    size_t _nextConnectionId = 0;
    std::atomic_bool _shouldStop = false;

    ConcurrentQueue<PeerMessage> _incomingMessages;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___PARALLELSERVER___H__
