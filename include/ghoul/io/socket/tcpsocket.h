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

#ifndef __GHOUL___TCPSOCKET___H__
#define __GHOUL___TCPSOCKET___H__

#include <ghoul/io/socket/socket.h>

#include <ghoul/io/socket/sockettype.h>
#include <ghoul/misc/exception.h>
#include <array>
#include <condition_variable>
#include <deque>
#include <mutex>
#include <thread>
#include <functional>

struct addrinfo;

namespace ghoul::io {

class TcpSocketServer;

class TcpSocket : public Socket {
public:
    using InputInterceptor = std::function<void(const char* data, size_t nBytes)>;

    struct TcpSocketError final : public RuntimeError {
        explicit TcpSocketError(std::string msg, std::string comp = "");
    };

    TcpSocket(std::string address, int port);
    TcpSocket(std::string address, int port, _SOCKET socket);
    ~TcpSocket() override;
    void connect();
    void startStreams() override;
    void disconnect(int reason = 0) override;
    bool isConnected() const override;
    bool isConnecting() const override;

    std::string address() const override;
    int port() const override;

    bool getMessage(std::string& message) override;
    bool putMessage(const std::string& message) override;
    void setDelimiter(char delimiter);

    static void initializeNetworkApi();
    static bool initializedNetworkApi();

    void interceptInput(InputInterceptor interceptor);
    void uninterceptInput();

    /**
     * Non-blocking counterpart to disconnect(). Stops the I/O loops and closes the
     * socket, but does not join the threads. Safe to call from within those threads
     * themselves (unlike disconnect())
     */
    void closeConnection();

    /**
     * Waits for the socket output to be emptied, or until the timeout is reached
     */
    bool waitForOutputQueueDrained(std::chrono::milliseconds timeout);

    // Methods for binary communication
    template <typename T = char>
    bool get(T* buffer, size_t nItems = 1);

    template <typename T = char>
    bool peek(T* buffer, size_t nItems = 1);

    template <typename T = char>
    bool skip(size_t nItems = 1);

    template <typename T = char>
    bool put(const T* buffer, size_t nItems = 1);

private:
    /**
     * Read size bytes from the socket, store them in buffer and dequeue them from input.
     * Block until size bytes have been read.
     *
     * \return `false` if this fails. Use error() to get the socket error code
     */
    bool getBytes(char* buffer, size_t nItems = 1);

    /**
     * Read size bytes from the socket, store them in buffer. Do NOT dequeue them from
     * input. Block until size bytes have been read.
     *
     * \return `false` if this fails. Use error() to get the socket error code
     */
    bool peekBytes(char* buffer, size_t nItems);

    /**
     * Skip size bytes from the socket. Block until size bytes have been read.
     *
     * \return `false` if this fails. Use error() to get the socket error code
     */
    bool skipBytes(size_t nItems);

    /**
     * Write size bytes from buffer into the socket.
     *
     * \return `false` if this fails. Use error() to get the socket error code
     */
    bool putBytes(const char* buffer, size_t size = 1);

    void closeSocket();
    void establishConnection(addrinfo* info);
    void streamInput();
    void streamOutput();
    int waitForDelimiter();
    void waitForInput(size_t nBytes);
    void waitForOutput(size_t nBytes);

    const std::string _address;
    const int _port;
    std::atomic<bool> _isConnected = false;
    std::atomic<bool> _isConnecting = false;
    std::atomic<bool> _shouldStopThreads = false;
    std::atomic<bool> _shouldCloseSocket = false;

    _SOCKET _socket;
    std::thread _inputThread;
    std::thread _outputThread;

    std::mutex _inputBufferMutex;
    std::mutex _inputQueueMutex;
    std::condition_variable _inputNotifier;
    std::deque<char> _inputQueue;
    std::array<char, 4096> _inputBuffer = { 0 };

    std::mutex _outputBufferMutex;
    std::mutex _outputQueueMutex;
    std::condition_variable _outputNotifier;
    std::deque<char> _outputQueue;
    std::array<char, 4096> _outputBuffer = { 0 };

    std::atomic<char> _delimiter;

    std::mutex _inputInterceptionMutex;
    InputInterceptor _inputInterceptor;

    static std::atomic_bool _initializedNetworkApi;
};

} // namespace ghoul::io

#include <ghoul/io/socket/tcpsocket.inl>

#endif // __GHOUL___TCPSOCKET___H__
