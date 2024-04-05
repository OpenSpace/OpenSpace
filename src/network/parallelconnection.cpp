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

#include <openspace/network/parallelconnection.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <ghoul/format.h>
#include <ghoul/io/socket/tcpsocket.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr std::string_view _loggerCat = "ParallelConnection";
} // namespace

namespace openspace {

ParallelConnection::Message::Message(MessageType t, std::vector<char> c)
    : type(t)
    , content(std::move(c))
{}

ParallelConnection::DataMessage::DataMessage(datamessagestructures::Type t,
                                             double time,
                                             std::vector<char> c)
    : type(t)
    , timestamp(time)
    , content(std::move(c))
{}

ParallelConnection::ConnectionLostError::ConnectionLostError(bool shouldLogError_)
    : ghoul::RuntimeError("Parallel connection lost", "ParallelConnection")
    , shouldLogError(shouldLogError_)
{}

ParallelConnection::ParallelConnection(std::unique_ptr<ghoul::io::TcpSocket> socket)
    : _socket(std::move(socket))
{}

bool ParallelConnection::isConnectedOrConnecting() const {
    return _socket != nullptr && (_socket->isConnected() || _socket->isConnecting());
}

void ParallelConnection::sendDataMessage(const DataMessage& dataMessage) {
    const uint8_t dataMessageTypeOut = static_cast<uint8_t>(dataMessage.type);
    const double dataMessageTimestamp = dataMessage.timestamp;

    std::vector<char> messageContent;
    messageContent.insert(
        messageContent.end(),
        reinterpret_cast<const char*>(&dataMessageTypeOut),
        reinterpret_cast<const char*>(&dataMessageTypeOut) + sizeof(uint8_t)
    );

    messageContent.insert(
        messageContent.end(),
        reinterpret_cast<const char*>(&dataMessageTimestamp),
        reinterpret_cast<const char*>(&dataMessageTimestamp) + sizeof(double)
    );

    messageContent.insert(messageContent.end(),
        dataMessage.content.begin(),
        dataMessage.content.end()
    );

    sendMessage(Message(MessageType::Data, messageContent));
}

bool ParallelConnection::sendMessage(const Message& message) {
    const uint8_t messageTypeOut = static_cast<uint8_t>(message.type);
    const uint32_t messageSizeOut = static_cast<uint32_t>(message.content.size());
    std::vector<char> header;

    //insert header into buffer
    header.push_back('O');
    header.push_back('S');

    header.insert(header.end(),
        reinterpret_cast<const char*>(&ProtocolVersion),
        reinterpret_cast<const char*>(&ProtocolVersion) + sizeof(uint8_t)
    );

    header.insert(header.end(),
        reinterpret_cast<const char*>(&messageTypeOut),
        reinterpret_cast<const char*>(&messageTypeOut) + sizeof(uint8_t)
    );

    header.insert(header.end(),
        reinterpret_cast<const char*>(&messageSizeOut),
        reinterpret_cast<const char*>(&messageSizeOut) + sizeof(uint32_t)
    );

    const bool res = _socket->put<char>(header.data(), header.size());
    if (!res) {
        return false;
    }
    const bool res2 = _socket->put<char>(message.content.data(), message.content.size());
    return res2;
}

void ParallelConnection::disconnect() {
    _shouldDisconnect = true;
    if (_socket) {
        _socket->disconnect();
    }
}

ghoul::io::TcpSocket* ParallelConnection::socket() {
    return _socket.get();
}

ParallelConnection::Message ParallelConnection::receiveMessage() {
    // Header consists of...
    constexpr size_t HeaderSize =
        2 * sizeof(char) + // OS
        sizeof(uint8_t) +  // Protocol version
        sizeof(uint8_t) +  // Message type
        sizeof(uint32_t);  // message size

    // Create basic buffer for receiving first part of messages
    std::vector<char> headerBuffer(HeaderSize);
    std::vector<char> messageBuffer;

    // Receive the header data
    if (!_socket->get(headerBuffer.data(), HeaderSize)) {
        // The `get` call is blocking until something happens, so we might end up here if
        // the socket properly closed or if the loading legitimately failed
        if (_shouldDisconnect) {
            throw ConnectionLostError(false);
        }
        else {
            LERROR("Failed to read header from socket. Disconnecting");
            throw ConnectionLostError();
        }
    }

    // Make sure that header matches this version of OpenSpace
    if (headerBuffer[0] != 'O' || headerBuffer[1] != 'S') {
        LERROR("Expected to read message header 'OS' from socket");
        throw ConnectionLostError();
    }

    size_t offset = 2;
    const uint8_t protocolVersionIn =
        *reinterpret_cast<uint8_t*>(headerBuffer.data() + offset);
    offset += sizeof(uint8_t);

    if (protocolVersionIn != ProtocolVersion) {
        LERROR(std::format(
            "Protocol versions do not match. Remote version: {}, Local version: {}",
            protocolVersionIn,
            ProtocolVersion
        ));
        throw ConnectionLostError();
    }

    const uint8_t messageTypeIn =
        *reinterpret_cast<uint8_t*>(headerBuffer.data() + offset);
    offset += sizeof(uint8_t);

    const uint32_t messageSizeIn =
        *reinterpret_cast<uint32_t*>(headerBuffer.data() + offset);
    //offset += sizeof(uint32_t);

    const size_t messageSize = messageSizeIn;

    // Receive the payload
    messageBuffer.resize(messageSize);
    if (!_socket->get(messageBuffer.data(), messageSize)) {
        LERROR("Failed to read message from socket. Disconnecting");
        throw ConnectionLostError();
    }

    // And delegate decoding depending on type
    return Message(static_cast<MessageType>(messageTypeIn), messageBuffer);
}

} // namespace openspace
