/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <modules/server/connectionpool.h>
#include <ghoul/io/socket/socket.h>

#include <vector>
#include <algorithm>


namespace openspace {

ConnectionPool::ConnectionPool(std::function<void(std::shared_ptr<ghoul::io::Socket> socket)> handleSocket)
    : _handleSocket(std::move(handleSocket))
{}

ConnectionPool::~ConnectionPool() {
    disconnectAllConnections();
}

void ConnectionPool::addServer(std::shared_ptr<ghoul::io::SocketServer> server) {
    _socketServers.push_back(server);
}

void ConnectionPool::removeServer(ghoul::io::SocketServer* server) {
    std::remove_if(_socketServers.begin(), _socketServers.end(), [server](const auto& s) {
        return s.get() == server;
    });
}

void ConnectionPool::clearServers() {
    _socketServers.clear();
}

void ConnectionPool::updateConnections() {
    removeDisconnectedSockets();
    acceptNewSockets();
}

void ConnectionPool::acceptNewSockets() {
    for (auto& server : _socketServers) {
        std::shared_ptr<ghoul::io::Socket> socket;
        while (socket = server->nextPendingSocket()) {
            std::thread connectionThread([this, socket]() {
                // Pass control to `_handleSocket`
                // Close the socket once the function returns.
                _handleSocket(socket);
                if (socket->isConnected()) {
                    socket->disconnect();
                }
            });
            _connections.push_back({
                std::move(connectionThread),
                socket
            });
        }
    }
}

void ConnectionPool::removeDisconnectedSockets() {
    for (auto& connection : _connections) {
        if ((!connection.second || !connection.second->isConnected()) && connection.first.joinable()) {
            connection.first.join();
        }
    }
    std::remove_if(_connections.begin(), _connections.end(), [](const Connection& connection) {
        return !connection.second || !connection.second->isConnected();
    });
}

void ConnectionPool::disconnectAllConnections() {
    _connections.clear();
}


} // namespace openspace
