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

namespace openspace {

ConnectionPool::ConnectionPool()
    : _listening(false)
    , _handleSocket(nullptr)
    , _serverThread(nullptr)
{}

void ConnectionPool::listen(
    std::string address,
    int port,
    std::function<void(std::shared_ptr<ghoul::io::TcpSocket> socket)> handleSocket)
{
    _handleSocket = std::move(handleSocket);
    _listening = true;
    _socketServer.listen(address, port);

    _serverThread = std::make_unique<std::thread>([this]() {
        handleConnections();
    });
}

void ConnectionPool::handleConnections() {
    while (_listening) {
        removeDisconnectedSockets();
        std::shared_ptr<ghoul::io::TcpSocket> socket = _socketServer.awaitPendingConnection();
        if (!socket || !socket->isConnected()) {
            continue;
        }
        std::lock_guard<std::mutex> lock(_connectionMutex);
        Connection connection = {
            socket,
            std::make_unique<std::thread>([this, socket]() {
                // Pass control to `_handleSocket`
                // Close the socket once the function returns.
                _handleSocket(socket);
                std::lock_guard<std::mutex> lock(_connectionMutex);
                if (socket->isConnected()) {
                    socket->disconnect();
                }
            })
        };
        _connections.emplace(socket->socketId(), std::move(connection));
    }
}

void ConnectionPool::removeDisconnectedSockets() {
    std::lock_guard<std::mutex> lock(_connectionMutex);
    for (auto it = _connections.begin(); it != _connections.end();) {
        if (!it->second.socket->isConnected()) {
            if (it->second.thread && it->second.thread->joinable()) {
                it->second.thread->join();
            }
            it->second.thread = nullptr;
            _connections.erase(it);
        } else {
            ++it;
        }
    }
}

void ConnectionPool::close() {
    disconnectAllConnections();
    closeServer();
}

void ConnectionPool::disconnectAllConnections() {
    for (auto& it : _connections) {
        if (it.second.socket->isConnected()) {
            it.second.socket->disconnect();
        }
        it.second.thread->join();
    }
    _connections.clear();
}

void ConnectionPool::closeServer() {
    if (!_listening) return;
    _listening = false;
    _socketServer.close();
    _serverThread->join();
}

} // namespace openspace
