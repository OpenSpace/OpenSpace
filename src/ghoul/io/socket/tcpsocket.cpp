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

#include <ghoul/io/socket/tcpsocket.h>

#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <algorithm>
#include <cstring>
#include <string_view>
#include <utility>

#ifdef WIN32
#include <Windows.h>
#include <winsock2.h>
#include <ws2def.h>
#include <ws2tcpip.h>
#ifndef _ERRNO
#define _ERRNO WSAGetLastError()
#endif // _ERRNO
#else // ^^^^ WIN32 // !WIN32 vvvv
#ifdef _XCODE
#include <unistd.h>
#endif // _XCODE

#include <sys/types.h>
#include <sys/socket.h>
#include <unistd.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <cerrno>

#ifndef SOCKET_ERROR
#define SOCKET_ERROR (-1)
#endif // SOCKET_ERROR

#ifndef _ERRNO
#define _ERRNO errno
#endif // _ERRNO
#endif // WIN32

namespace {
    constexpr std::string_view _loggerCat = "TcpSocket";
    constexpr char DefaultDelimiter = '\n';
} // namespace

namespace ghoul::io {

std::atomic<bool> TcpSocket::_initializedNetworkApi = false;

TcpSocket::TcpSocketError::TcpSocketError(std::string msg, std::string comp)
    : RuntimeError(std::move(msg), std::move(comp))
{}

TcpSocket::TcpSocket(std::string address, int port)
    : _address(std::move(address))
    , _port(port)
    , _socket(INVALID_SOCKET)
    , _delimiter(DefaultDelimiter)
{}

TcpSocket::TcpSocket(std::string address, int port, _SOCKET socket)
    : _address(std::move(address))
    , _port(port)
    , _isConnected(true)
    , _socket(socket)
    , _delimiter(DefaultDelimiter)
{}

TcpSocket::~TcpSocket() {
    if (_isConnected) {
        disconnect();
    }
    _shouldStopThreads = true;
    if (_inputThread.joinable()) {
        _inputThread.join();
    }
    if (_outputThread.joinable()) {
        _outputThread.join();
    }
}

std::string TcpSocket::address() const {
    return _address;
}

int TcpSocket::port() const {
    return _port;
}

void TcpSocket::startStreams() {
    _inputThread = std::thread([this]() { streamInput(); });
    _outputThread = std::thread([this]() { streamOutput(); });
}

void TcpSocket::connect() {
    if (_isConnected) {
        throw TcpSocket::TcpSocketError("Socket is already connected");
    }
    if (_isConnecting) {
        throw TcpSocket::TcpSocketError("Socket is already trying to connect");
    }
    if (!_initializedNetworkApi) {
        initializeNetworkApi();
    }

    struct addrinfo* addresult = nullptr;
    struct addrinfo hints{};
    std::memset(&hints, 0, sizeof(hints));

    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_protocol = IPPROTO_TCP;
    hints.ai_flags = AI_PASSIVE;

    const std::string p = std::to_string(_port);
    const int result = getaddrinfo(_address.c_str(), p.c_str(), &hints, &addresult);
    if (result != 0) {
        return;
    }

    _isConnecting = true;

    _outputThread = std::thread([this, addresult]() {
        establishConnection(addresult);
        _inputThread = std::thread([this]() { streamInput(); });
        streamOutput();
    });
}

void TcpSocket::closeSocket() {
    if (!_isConnected && !_isConnecting) {
        return;
    }

#ifdef WIN32
    shutdown(_socket, SD_BOTH);
    closesocket(_socket);
#else // ^^^^ WIN32 // !WIN32 vvvv
    shutdown(_socket, SHUT_RDWR);
    close(_socket);
#endif // WIN32
    _socket = INVALID_SOCKET;

    _isConnected = false;
    _isConnecting = false;
}

void TcpSocket::disconnect(int) {
    if (!_isConnected && !_isConnecting) {
        return;
    }

    _shouldStopThreads = true;
    closeSocket();

    _inputNotifier.notify_all();
    _outputNotifier.notify_all();

    if (_inputThread.joinable()) {
        _inputThread.join();
    }
    if (_outputThread.joinable()) {
        _outputThread.join();
    }
    _shouldStopThreads = false;
}

bool TcpSocket::isConnected() const {
    return _isConnected;
}

bool TcpSocket::isConnecting() const {
    return _isConnecting;
}

bool TcpSocket::getMessage(std::string& message) {
    const int delimiterIndex = waitForDelimiter();
    if (delimiterIndex == 0) {
        return false;
    }
    const std::unique_lock lock(_inputQueueMutex);
    message.assign(_inputQueue.begin(), _inputQueue.begin() + delimiterIndex);
    if (static_cast<int>(_inputQueue.size()) >= delimiterIndex + 1) {
        _inputQueue.erase(_inputQueue.begin(), _inputQueue.begin() + delimiterIndex + 1);
    }
    return true;
}

bool TcpSocket::putMessage(const std::string& message) {
    std::string messageWithDelim = message;
    messageWithDelim.push_back(_delimiter);
    const bool success = putBytes(messageWithDelim.data(), messageWithDelim.size());
    return success;
}

void TcpSocket::setDelimiter(char delimiter) {
    _delimiter = delimiter;
}

void TcpSocket::establishConnection(addrinfo* info) {
    _socket = socket(info->ai_family, info->ai_socktype, info->ai_protocol);

    if (_socket == INVALID_SOCKET) {
        freeaddrinfo(info);
        _isConnected = false;
        _isConnecting = false;
        _shouldStopThreads = true;
        _inputNotifier.notify_all();
        _outputNotifier.notify_all();
        return;
    }

    const char trueFlag = 1;
    const char falseFlag = 0;
    int result = 0;

    // Disable Nagle's algorithm
    result = setsockopt(_socket, IPPROTO_TCP, TCP_NODELAY, &trueFlag, sizeof(trueFlag));
    if (result == SOCKET_ERROR) {
        LWARNING(std::format("Socket error: {}", _ERRNO));
    }

    // Disable address reuse
    result = setsockopt(_socket, SOL_SOCKET, SO_REUSEADDR, &falseFlag, sizeof(falseFlag));
    if (result == SOCKET_ERROR) {
        LWARNING(std::format("Socket error: {}", _ERRNO));
        freeaddrinfo(info);
        _isConnecting = false;
        _isConnected = false;
        _shouldStopThreads = true;
        _inputNotifier.notify_all();
        _outputNotifier.notify_all();
        return;
    }
    // Keep alive
    result = setsockopt(_socket, SOL_SOCKET, SO_KEEPALIVE, &trueFlag, sizeof(trueFlag));
    if (result == SOCKET_ERROR) {
        freeaddrinfo(info);
        _isConnecting = false;
        _isConnected = false;
        _shouldStopThreads = true;
        _inputNotifier.notify_all();
        _outputNotifier.notify_all();
        return;
    }

    // Try to connect
    ::connect(_socket, info->ai_addr, static_cast<int>(info->ai_addrlen));
    _isConnected = true;
    _isConnecting = false;
}

void TcpSocket::streamInput() {
    while (_isConnected && !_shouldStopThreads) {
#ifdef WIN32
        int nReadBytes = 0;
        auto failed = [](int nBytes) { return nBytes <= 0; };
#else // ^^^^ WIN32 // !WIN32 vvvv
        ssize_t nReadBytes = 0;
        auto failed = [](ssize_t nBytes) { return nBytes <= 0; };
#endif // WIN32

        nReadBytes = recv(
            _socket,
            _inputBuffer.data(),
            static_cast<int>(_inputBuffer.size()),
            0
        );

        if (failed(nReadBytes)) {
            _shouldStopThreads = true;
            _inputNotifier.notify_all();
            _outputNotifier.notify_all();
            closeSocket();
            return;
        }

        const std::unique_lock lock(_inputInterceptionMutex);

        if (_inputInterceptor) {
            _inputInterceptor(_inputBuffer.data(), nReadBytes);
        }
        else {
            const std::unique_lock inputLock(_inputQueueMutex);
            _inputQueue.insert(
                _inputQueue.end(),
                _inputBuffer.begin(),
                _inputBuffer.begin() + nReadBytes
            );
        }
        _inputNotifier.notify_one();
    }
}

void TcpSocket::streamOutput() {
    while (_isConnected && !_shouldStopThreads) {
        waitForOutput(1);

        size_t nBytesToSend = 0;
        const std::unique_lock lock(_outputQueueMutex);
        while ((nBytesToSend = std::min(_outputQueue.size(), _outputBuffer.size())) > 0) {
            std::copy_n(_outputQueue.begin(), nBytesToSend, _outputBuffer.begin());

#ifdef WIN32
            const int n = static_cast<int>(nBytesToSend);
            int nSentBytes = send(_socket, _outputBuffer.data(), n, 0);

            auto failed = [](int nBytes) { return nBytes <= 0; };
#else // ^^^^ WIN32 // !WIN32 vvvv
            const ssize_t nSentBytes = send(
                _socket,
                _outputBuffer.data(),
                nBytesToSend,
                0
            );
            auto failed = [](ssize_t nBytes) { return nBytes <= 0; };
#endif // WIN32

            if (failed(nSentBytes)) {
                closeSocket();
                _shouldStopThreads = true;
                _inputNotifier.notify_all();
                _outputNotifier.notify_all();
                return;
            }
            _outputQueue.erase(_outputQueue.begin(), _outputQueue.begin() + nSentBytes);
        }
        _outputNotifier.notify_all(); // Let anyone waiting on drainage know
    }
}

void TcpSocket::waitForInput(size_t nBytes) {
    if (nBytes == 0) {
        return;
    }

    auto receivedRequestedInputOrDisconnected = [this, nBytes]() {
        if (_shouldStopThreads || (!_isConnected && !_isConnecting)) {
            return true;
        }
        const std::unique_lock lock(_inputQueueMutex);
        return _inputQueue.size() >= nBytes;
    };

    // Block execution until enough data has come into the input queue
    if (!receivedRequestedInputOrDisconnected()) {
        std::unique_lock lock(_inputBufferMutex);
        _inputNotifier.wait(lock, receivedRequestedInputOrDisconnected);
    }
}

int TcpSocket::waitForDelimiter() {
    size_t currentIndex = 0;
    auto receivedRequestedInputOrDisconnected =
        [this, &currentIndex, d = _delimiter.load()]()
    {
        if (_shouldStopThreads || (!_isConnected && !_isConnecting)) {
            return true;
        }
        const std::unique_lock lock(_inputQueueMutex);
        auto it = std::find(_inputQueue.begin() + currentIndex, _inputQueue.end(), d);
        currentIndex = it - _inputQueue.begin();
        return it != _inputQueue.end();
    };

    // Block execution until the delimiter character was found in the input queue
    if (!receivedRequestedInputOrDisconnected()) {
        std::unique_lock lock(_inputBufferMutex);
        _inputNotifier.wait(lock, receivedRequestedInputOrDisconnected);
    }
    return static_cast<int>(currentIndex);
}

void TcpSocket::waitForOutput(size_t nBytes) {
    if (nBytes == 0) {
        return;
    }

    auto receivedRequestedOutputOrDisconnected = [this, nBytes]() {
        if (_shouldStopThreads || (!_isConnected && !_isConnecting)) {
            return true;
        }
        const std::unique_lock lock(_outputQueueMutex);
        return _outputQueue.size() >= nBytes;
    };

    // Block execution until enough data has come into the output queue
    if (!receivedRequestedOutputOrDisconnected()) {
        std::unique_lock lock(_outputBufferMutex);
        _outputNotifier.wait(lock, receivedRequestedOutputOrDisconnected);
    }
}

void TcpSocket::initializeNetworkApi() {
#ifdef WIN32
    const WORD version = MAKEWORD(2, 2);
    WSADATA wsaData;
    const int error = WSAStartup(version, &wsaData);

    if (error != 0 || LOBYTE(wsaData.wVersion) != 2 || HIBYTE(wsaData.wVersion) != 2) {
        // Incorrect WinSock version
        WSACleanup();

        throw std::runtime_error("Failed to initialize WinSock API");
    }
#endif // WIN32
    _initializedNetworkApi = true;
}

bool TcpSocket::initializedNetworkApi() {
    return _initializedNetworkApi;
}

void TcpSocket::interceptInput(InputInterceptor interceptor) {
    const std::unique_lock lock(_inputInterceptionMutex);
    _inputInterceptor = std::move(interceptor);
}

void TcpSocket::uninterceptInput() {
    const std::unique_lock lock(_inputInterceptionMutex);
    _inputInterceptor = nullptr;
}

void TcpSocket::closeConnection() {
    _shouldStopThreads = true;
    closeSocket();
    _inputNotifier.notify_all();
    _outputNotifier.notify_all();
}

bool TcpSocket::waitForOutputQueueDrained(std::chrono::milliseconds timeout) {
    std::unique_lock lock(_outputQueueMutex);
    return _outputNotifier.wait_for(
        lock,
        timeout,
        [this]() {
            return _outputQueue.empty() || _shouldStopThreads ||
                (!_isConnected && !_isConnecting);
        }
    );
}

bool TcpSocket::getBytes(char* buffer, size_t nItems) {
    waitForInput(nItems);
    if (_shouldStopThreads || (!_isConnected && !_isConnecting)) {
        return false;
    }
    const std::unique_lock lock(_inputQueueMutex);
    std::copy_n(_inputQueue.begin(), nItems, buffer);
    _inputQueue.erase(_inputQueue.begin(), _inputQueue.begin() + nItems);
    return true;
}

bool TcpSocket::peekBytes(char* buffer, size_t nItems) {
    waitForInput(nItems);
    if (_shouldStopThreads || (!_isConnected && !_isConnecting)) {
        return false;
    }
    const std::unique_lock lock(_inputQueueMutex);
    std::copy_n(_inputQueue.begin(), nItems, buffer);
    return true;
}

bool TcpSocket::skipBytes(size_t nItems) {
    waitForInput(nItems);
    if (_shouldStopThreads || (!_isConnected && !_isConnecting)) {
        return false;
    }
    const std::unique_lock lock(_inputQueueMutex);
    _inputQueue.erase(_inputQueue.begin(), _inputQueue.begin() + nItems);
    return true;
}

bool TcpSocket::putBytes(const char* buffer, size_t size) {
    if (_shouldStopThreads) {
        return false;
    }
    const std::unique_lock lock(_outputQueueMutex);
    _outputQueue.insert(_outputQueue.end(), buffer, buffer + size);
    _outputNotifier.notify_one();
    return _isConnected || _isConnecting;
}

} // namespace ghoul::io
