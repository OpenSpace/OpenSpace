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
        handleIncomingConnections();
    });
}

void ConnectionPool::handleIncomingConnections() {
    while (_listening) {
        std::shared_ptr<ghoul::io::TcpSocket> socket = _socketServer.awaitPendingConnection();
        if (!socket || !socket->connected()) {
            continue;
        }
        std::lock_guard<std::mutex> lock(_connectionMutex);
        Connection connection = {
            socket,
            std::make_unique<std::thread>([this, socket]() {
                // Pass control to `_handleSocket`
                // Close the socket once the function returns.
                _handleSocket(socket);
                disconnectSocket(socket);
            })
        };
        _connections.emplace(socket->socketId(), std::move(connection));
    }
}

void ConnectionPool::disconnectSocket(std::shared_ptr<ghoul::io::TcpSocket> socket) {
    std::lock_guard<std::mutex> lock(_connectionMutex);

    if (socket->connected()) {
        socket->disconnect();
    }
    int socketId = socket->socketId();
    auto it = _connections.find(socketId);

    if (it == _connections.end()) {
        return;
    }

    Connection& connection = it->second;
    connection.thread->join();

    _connections.erase(it);
}

void ConnectionPool::close() {
    disconnectAllConnections();
    closeServer();
}

void ConnectionPool::disconnectAllConnections() {
    for (auto& it : _connections) {
        if (it.second.socket->connected()) {
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
