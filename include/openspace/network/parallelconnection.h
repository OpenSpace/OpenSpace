/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#ifndef __OPENSPACE_CORE___PARALLELCONNECTION___H__
#define __OPENSPACE_CORE___PARALLELCONNECTION___H__

#include <openspace/network/messagestructures.h>
#include <ghoul/io/socket/tcpsocket.h>
#include <ghoul/misc/exception.h>
#include <vector>

namespace openspace {

class ParallelConnection  {
public:
    enum class Status : uint32_t {
        Disconnected = 0,
        Connecting,
        ClientWithoutHost,
        ClientWithHost,
        Host
    };

    enum class MessageType : uint8_t {
        Authentication = 0,
        Data,
        ConnectionStatus,
        HostshipRequest,
        HostshipResignation,
        NConnections
    };

    struct Message {
        Message() = default;
        Message(MessageType t, std::vector<char> c);

        MessageType type;
        std::vector<char> content;
    };

    struct DataMessage {
        DataMessage() = default;
        DataMessage(datamessagestructures::Type t, double time, std::vector<char> c);

        datamessagestructures::Type type;
        double timestamp;
        std::vector<char> content;
    };

    class ConnectionLostError : public ghoul::RuntimeError {
    public:
        explicit ConnectionLostError(bool shouldLogError_ = true);

        bool shouldLogError;
    };

    ParallelConnection(std::unique_ptr<ghoul::io::TcpSocket> socket);

    bool isConnectedOrConnecting() const;
    void sendDataMessage(const ParallelConnection::DataMessage& dataMessage);
    bool sendMessage(const ParallelConnection::Message& message);
    void disconnect();
    ghoul::io::TcpSocket* socket();

    ParallelConnection::Message receiveMessage();

    // Gonna do some UTF-like magic once we reach 255 to introduce a second byte or so
    static constexpr uint8_t ProtocolVersion = 7;

private:
    std::unique_ptr<ghoul::io::TcpSocket> _socket;
    bool _shouldDisconnect = false;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___PARALLELCONNECTION___H__
