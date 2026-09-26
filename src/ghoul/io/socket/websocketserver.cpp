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

#include <ghoul/io/socket/websocketserver.h>

#include <ghoul/io/socket/tcpsocket.h>
#include <ghoul/io/socket/websocket.h>
#include <utility>

namespace ghoul::io {

WebSocketServer::WebSocketServer() {
    // Set up WebSocket++ logging
    _server.clear_access_channels(websocketpp::log::alevel::all);
    _server.set_access_channels(websocketpp::log::alevel::connect);
    _server.set_access_channels(websocketpp::log::alevel::disconnect);
    _server.set_access_channels(websocketpp::log::alevel::app);
}

int WebSocketServer::port() const {
    return _tcpSocketServer.port();
}

void WebSocketServer::close() {
    return _tcpSocketServer.close();
}

void WebSocketServer::listen(int port) {
    return _tcpSocketServer.listen(port);
}

bool WebSocketServer::isListening() const {
    return _tcpSocketServer.isListening();
}

bool WebSocketServer::hasPendingSockets() const {
    return _tcpSocketServer.hasPendingSockets();
}

std::unique_ptr<WebSocket> WebSocketServer::nextPendingWebSocket() {
    std::unique_ptr<TcpSocket> tcpSocket = _tcpSocketServer.nextPendingTcpSocket();
    if (!tcpSocket) {
        return nullptr;
    }
    return std::make_unique<WebSocket>(std::move(tcpSocket), _server);
}

std::unique_ptr<Socket> WebSocketServer::nextPendingSocket() {
    return std::unique_ptr<Socket>(nextPendingWebSocket());
}

std::unique_ptr<WebSocket> WebSocketServer::awaitPendingWebSocket() {
    std::unique_ptr<TcpSocket> tcpSocket = _tcpSocketServer.awaitPendingTcpSocket();
    if (!tcpSocket) {
        return nullptr;
    }
    return std::make_unique<WebSocket>(std::move(tcpSocket), _server);
}

std::unique_ptr<Socket> WebSocketServer::awaitPendingSocket() {
    return std::unique_ptr<Socket>(awaitPendingWebSocket());
}

} // namespace ghoul::io
