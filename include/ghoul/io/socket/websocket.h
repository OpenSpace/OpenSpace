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

#ifndef __GHOUL___WEBSOCKET___H__
#define __GHOUL___WEBSOCKET___H__

#include <ghoul/io/socket/socket.h>

#include <condition_variable>
#include <deque>
#include <memory>
#include <mutex>
#include <set>
#include <sstream>

#ifdef WIN32
#pragma warning(push)
#pragma warning(disable: 4244 4267)
#endif // WIN32

#include <websocketpp/common/connection_hdl.hpp>
#include <websocketpp/config/core.hpp>
#include <websocketpp/endpoint.hpp>
#include <websocketpp/roles/server_endpoint.hpp>

#ifdef WIN32
#pragma warning(pop)
#endif // WIN32

namespace ghoul::io {

class TcpSocket;
class WebSocketServerInternal;

/**
 * WebSockets are essentially a wrapper around regular TCP sockets. The difference is how
 * the messages are interpreted. In TCP sockets, we delimit the messages using
 * `_delimiter`.
 *
 * In WebSockets however, the message delimiting is handled by a message header.
 */
class WebSocket : public Socket {
public:
    /**
     * WebSocket close event reason:
     * https://developer.mozilla.org/en-US/docs/Web/API/CloseEvent
     */
    enum class ClosingReason : int {
        Normal      = 1000,
        GoingAway   = 1001,
        // Custom app-specific reasons
        ClosingAll  = 4000
    };

    WebSocket(std::unique_ptr<TcpSocket> socket,
        websocketpp::server<websocketpp::config::core>& server);

    ~WebSocket() override;

    std::string address() const override;
    int port() const override;

    void disconnect(int reason = static_cast<int>(ClosingReason::Normal)) override;

    bool getMessage(std::string& message) override;
    bool putMessage(const std::string& message) override;

    bool isConnected() const override;
    bool isConnecting() const override;

    void startStreams() override;

private:
    void onMessage(const websocketpp::connection_hdl& hdl,
        const websocketpp::server<websocketpp::config::core>::message_ptr& msg);

    void onOpen(const websocketpp::connection_hdl& hdl);
    void onClose(const websocketpp::connection_hdl& hdl);

    std::mutex _connectionHandlesMutex;
    std::set<
        websocketpp::connection_hdl,
        std::owner_less<websocketpp::connection_hdl>
    > _connectionHandles;

    websocketpp::server<websocketpp::config::core>::connection_ptr _socketConnection;

    std::stringstream _outputStream;
    std::mutex _outputStreamMutex;

    std::deque<std::string> _inputMessageQueue;
    std::mutex _inputMessageQueueMutex;
    std::condition_variable _inputNotifier;

    std::unique_ptr<TcpSocket> _tcpSocket;
    bool _isMarkedForClosing = false;
};

} // namespace ghoul::io

#endif // __GHOUL___WEBSOCKET___H__
