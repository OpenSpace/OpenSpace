/*****************************************************************************************
 *                                                                                       *
 * GHOUL                                                                                 *
 * General Helpful Open Utility Library                                                  *
 *                                                                                       *
 * Copyright (c) 2012-2026                                                               *
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

#ifndef __GHOUL___WEBSOCKETSERVER___H__
#define __GHOUL___WEBSOCKETSERVER___H__

#include <ghoul/io/socket/socketserver.h>

#include <ghoul/io/socket/tcpsocketserver.h>

#ifdef WIN32
#pragma warning(push)
#pragma warning(disable: 4244 4267)
#endif // WIN32

#include <websocketpp/config/core.hpp>
#include <websocketpp/roles/server_endpoint.hpp>
#include <websocketpp/server.hpp>

#ifdef WIN32
#pragma warning(pop)
#endif // WIN32

namespace ghoul::io {

class Socket;
class WebSocket;

class WebSocketServer : public SocketServer {
public:
    WebSocketServer();
    ~WebSocketServer() override = default;

    int port() const override;
    void close() override;
    void listen(int port) override;
    bool isListening() const override;
    bool hasPendingSockets() const override;

    /**
     * Get next pending connection. Non-blocking. Can return `nullptr`.
     */
    std::unique_ptr<WebSocket> nextPendingWebSocket();
    std::unique_ptr<Socket> nextPendingSocket() override;

    /**
     * Get next pending connection. Blocking. Only returns nullptr if the socket server
     * closes.
     */
    std::unique_ptr<WebSocket> awaitPendingWebSocket();
    std::unique_ptr<Socket> awaitPendingSocket() override;

private:
    websocketpp::server<websocketpp::config::core> _server;
    TcpSocketServer _tcpSocketServer;
};

} // namespace ghoul::io

#endif // __GHOUL___WEBSOCKETSERVER___H__
