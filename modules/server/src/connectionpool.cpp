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

#include <modules/server/include/connectionpool.h>

#include <ghoul/io/socket/socket.h>
#include <ghoul/io/socket/socketserver.h>
#include <algorithm>

namespace openspace {

ConnectionPool::ConnectionPool(SocketHandleFunc handleSocket)
    : _handleSocket(std::move(handleSocket))
{}

ConnectionPool::~ConnectionPool() {
    disconnectAllConnections();
}

void ConnectionPool::addServer(std::shared_ptr<ghoul::io::SocketServer> server) {
    _socketServers.push_back(std::move(server));
}

void ConnectionPool::removeServer(ghoul::io::SocketServer* server) {
    _socketServers.erase(
        std::remove_if(
            _socketServers.begin(),
            _socketServers.end(),
            [server](const std::shared_ptr<ghoul::io::SocketServer>& s) {
                return s.get() == server;
            }
        ),
        _socketServers.end()
    );
}

void ConnectionPool::clearServers() {
    _socketServers.clear();
}

void ConnectionPool::updateConnections() {
    removeDisconnectedSockets();
    acceptNewSockets();
}

void ConnectionPool::acceptNewSockets() {
    for (std::shared_ptr<ghoul::io::SocketServer>& server : _socketServers) {
        std::unique_ptr<ghoul::io::Socket> socket;
        while ((socket = server->nextPendingSocket())) {
            _handleSocket(*socket);
            _sockets.push_back(std::move(socket));
        }
    }
}

void ConnectionPool::removeDisconnectedSockets() {
    _sockets.erase(
        std::remove_if(
            _sockets.begin(),
            _sockets.end(),
            [](const std::unique_ptr<ghoul::io::Socket>& socket) {
                return !socket || !socket->isConnected();
            }
        ),
        _sockets.end()
    );
}

void ConnectionPool::disconnectAllConnections() {
    for (const std::unique_ptr<ghoul::io::Socket>& socket : _sockets) {
        if (socket && socket->isConnected()) {
            socket->disconnect();
        }
    }
    _sockets.clear();
}

} // namespace openspace
