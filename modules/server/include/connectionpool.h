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

#ifndef __OPENSPACE_MODULE_SERVER___CONNECTIONPOOL___H__
#define __OPENSPACE_MODULE_SERVER___CONNECTIONPOOL___H__

#include <functional>
#include <memory>
#include <mutex>
#include <vector>

namespace ghoul::io {
    class Socket;
    class SocketServer;
} // namespace ghoul::io

namespace openspace {

class ConnectionPool {
public:
    using SocketHandleFunc = std::function<void(ghoul::io::Socket& socket)>;

    ConnectionPool(SocketHandleFunc handleSocket);
    ~ConnectionPool();

    void addServer(std::shared_ptr<ghoul::io::SocketServer> server);
    void removeServer(ghoul::io::SocketServer* server);
    void clearServers();
    void updateConnections();

private:
    void disconnectAllConnections();
    std::mutex _connectionMutex;
    void removeDisconnectedSockets();
    void acceptNewSockets();

    SocketHandleFunc _handleSocket;
    std::vector<std::shared_ptr<ghoul::io::SocketServer>> _socketServers;
    std::vector<std::unique_ptr<ghoul::io::Socket>> _sockets;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SERVER___CONNECTIONPOOL___H__
