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

#ifndef __OPENSPACE_MODULE_SERVER___CONNECTIONPOOL___H__
#define __OPENSPACE_MODULE_SERVER___CONNECTIONPOOL___H__

#include <ghoul/io/socket/tcpsocketserver.h>

#include <memory>
#include <thread>
#include <mutex>
#include <map>
#include <functional>

namespace openspace {


class ConnectionPool {
public:
    ConnectionPool();
    void listen(
        std::string address,
        int port,
        std::function<void(std::shared_ptr<ghoul::io::TcpSocket> socket)> handleSocket);
    void close();

private:
    struct Connection {
        std::shared_ptr<ghoul::io::TcpSocket> socket;
        std::unique_ptr<std::thread> thread;
    };

    void handleConnections();
    void closeServer();
    void disconnectAllConnections();
    std::mutex _connectionMutex;
    void removeDisconnectedSockets();
    ghoul::io::TcpSocketServer _socketServer;

    std::atomic<bool> _listening;
    std::function<void(std::shared_ptr<ghoul::io::TcpSocket>)> _handleSocket;
    std::unique_ptr<std::thread> _serverThread;
    std::map<int, Connection> _connections;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SERVER___CONNECTIONPOOL___H__
