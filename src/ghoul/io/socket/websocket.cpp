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

#include <ghoul/io/socket/websocket.h>

#include <ghoul/format.h>
#include <ghoul/io/socket/tcpsocket.h>
#include <ghoul/logging/logmanager.h>
#include <chrono>
#include <functional>
#include <string_view>
#include <utility>

using websocketpp::lib::placeholders::_1;
using websocketpp::lib::placeholders::_2;
using websocketpp::lib::bind;

namespace {
    constexpr std::string_view _loggerCat = "WebSocket";
    constexpr std::chrono::milliseconds MaxWaitDuration = std::chrono::milliseconds(1000);
} // namespace

namespace ghoul::io {

WebSocket::WebSocket(std::unique_ptr<TcpSocket> socket,
                     websocketpp::server<websocketpp::config::core>& server)
    : _tcpSocket(std::move(socket))
{
    server.set_message_handler(bind(&WebSocket::onMessage, this, ::_1, ::_2));
    server.set_open_handler(bind(&WebSocket::onOpen, this, ::_1));
    server.set_close_handler(bind(&WebSocket::onClose, this, ::_1));

    _socketConnection = server.get_connection();
    _socketConnection->register_ostream(&_outputStream);
    _socketConnection->start();

     _tcpSocket->interceptInput(
        [this](const char* data, size_t nBytes) {
            std::string output;
            {
                // Guards _socketConnection/_outputStream against putMessage() on other
                // threads. onOpen/onMessage/onClose run nested inside read_some(), so
                // they must not lock _outputStreamMutex themselves
                const std::unique_lock lock(_outputStreamMutex);
                _socketConnection->read_some(data, nBytes);

                // `read_some` can cause the websocketpp to generate protocol-level output
                // e.g., a Close frame response, or a Pong reply to a Ping. Flush the
                // responses so the client isn't left waiting
                output = _outputStream.str();
                if (!output.empty()) {
                    _outputStream.str("");
                    _outputStream.clear();
                }
            }

            // Write outside the lock, since TcpSocket::put has its own internal locking
            if (!output.empty()) {
                _tcpSocket->put<char>(output.c_str(), output.size());
            }
            if (_isMarkedForClosing) {
                const bool drained =
                    _tcpSocket->waitForOutputQueueDrained(std::chrono::seconds(3));

                if (!drained) {
                    LWARNING("Timed out flushing final output before closing socket");
                }
                _tcpSocket->closeConnection();
            }
            _inputNotifier.notify_one();
        }
    );
}

WebSocket::~WebSocket() {
    LDEBUG("Destroying socket connection");
    _tcpSocket->uninterceptInput();
    _socketConnection->eof();
    _tcpSocket = nullptr;
}

std::string WebSocket::address() const {
    return _tcpSocket->address();
}

int WebSocket::port() const {
    return _tcpSocket->port();
}

void WebSocket::disconnect(int) {
    _tcpSocket->disconnect();
}

bool WebSocket::getMessage(std::string& message) {
    auto messageOrDisconnected = [this]() {
        // `_inputMessageQueueMutex` must be locked when calling this function.
        return (!_tcpSocket->isConnected() && !_tcpSocket->isConnecting()) ||
            !_inputMessageQueue.empty();
    };

    while (!messageOrDisconnected()) {
        std::unique_lock lock(_inputMessageQueueMutex);
        _inputNotifier.wait_for(lock, MaxWaitDuration, messageOrDisconnected);
    }

    const std::unique_lock lock(_inputMessageQueueMutex);
    if (_inputMessageQueue.empty()) {
        return false;
    }

    message = _inputMessageQueue.front();
    _inputMessageQueue.pop_front();
    return true;
}

bool WebSocket::putMessage(const std::string& message) {
    std::string output;

    {
        const std::unique_lock lock(_outputStreamMutex);
        _socketConnection->send(message);
        output = _outputStream.str();
        if (!output.empty()) {
            _outputStream.str("");
            _outputStream.clear();
        }
    }

    // Write outside the lock, since TcpSocket::put has its own internal locking
    if (!output.empty()) {
        _tcpSocket->put<char>(output.c_str(), output.size());
    }
    return true;
}

bool WebSocket::isConnected() const {
    return _tcpSocket && _tcpSocket->isConnected();
}

bool WebSocket::isConnecting() const {
    return _tcpSocket && _tcpSocket->isConnecting();
}

void WebSocket::startStreams() {
    _tcpSocket->startStreams();
}

void WebSocket::onMessage(const websocketpp::connection_hdl&,
                   const websocketpp::server<websocketpp::config::core>::message_ptr& msg)
{
    const std::string msgContent = msg->get_payload();
    const std::unique_lock lock(_inputMessageQueueMutex);
    _inputMessageQueue.push_back(msgContent);
    _inputNotifier.notify_one();
}

void WebSocket::onOpen(const websocketpp::connection_hdl& hdl) {
    LDEBUG(std::format(
        "onOpen: WebSocket opened. Client: {}:{}",
        _tcpSocket->address(), _tcpSocket->port()
    ));
    const std::unique_lock lock(_connectionHandlesMutex);
    _connectionHandles.insert(hdl);
}

void WebSocket::onClose(const websocketpp::connection_hdl& hdl) {
    LDEBUG(std::format(
        "onClose: WebSocket closing. Client: {}:{}",
        _tcpSocket->address(), _tcpSocket->port()
    ));

    const std::unique_lock lock(_connectionHandlesMutex);
    _connectionHandles.erase(hdl);
    _inputNotifier.notify_one();
    _isMarkedForClosing = true;
}

} // namespace ghoul::io
