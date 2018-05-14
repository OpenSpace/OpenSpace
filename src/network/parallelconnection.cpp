/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <cstdint>

namespace {
const uint32_t ProtocolVersion = 4;
const char* _loggerCat = "ParallelConnection";
} // namespace

namespace openspace {

ParallelConnection::ConnectionLostError::ConnectionLostError()
    : ghoul::RuntimeError("Parallel connection lost", "ParallelConnection")
{}

ParallelConnection::ParallelConnection(std::unique_ptr<ghoul::io::TcpSocket> socket)
    : _socket(std::move(socket))
{}

bool ParallelConnection::isConnectedOrConnecting() {
    return _socket->isConnected() || _socket->isConnecting();
}

void ParallelConnection::sendDataMessage(
                                       const ParallelConnection::DataMessage& dataMessage)
{
    uint32_t dataMessageTypeOut = static_cast<uint32_t>(dataMessage.type);

    std::vector<char> messageContent;
    messageContent.insert(messageContent.end(),
        reinterpret_cast<const char*>(&dataMessageTypeOut),
        reinterpret_cast<const char*>(&dataMessageTypeOut) + sizeof(uint32_t));

    messageContent.insert(messageContent.end(),
        dataMessage.content.begin(),
        dataMessage.content.end());

    sendMessage(ParallelConnection::Message(
        ParallelConnection::MessageType::Data,
        messageContent
    ));
}

bool ParallelConnection::sendMessage(const  ParallelConnection::Message& message) {
    uint32_t messageTypeOut = static_cast<uint32_t>(message.type);
    uint32_t messageSizeOut = static_cast<uint32_t>(message.content.size());

    std::vector<char> header;

    //insert header into buffer
    header.push_back('O');
    header.push_back('S');

    header.insert(header.end(),
        reinterpret_cast<const char*>(&ProtocolVersion),
        reinterpret_cast<const char*>(&ProtocolVersion) + sizeof(uint32_t)
    );

    header.insert(header.end(),
        reinterpret_cast<const char*>(&messageTypeOut),
        reinterpret_cast<const char*>(&messageTypeOut) + sizeof(uint32_t)
    );

    header.insert(header.end(),
        reinterpret_cast<const char*>(&messageSizeOut),
        reinterpret_cast<const char*>(&messageSizeOut) + sizeof(uint32_t)
    );

    if (!_socket->put<char>(header.data(), header.size())) {
        return false;
    }
    if (!_socket->put<char>(message.content.data(), message.content.size())) {
        return false;
    }
    return true;
}

void ParallelConnection::disconnect() {
    if (_socket) {
        _socket->disconnect();
    }
}

ghoul::io::TcpSocket* ParallelConnection::socket() {
    return _socket.get();
}

ParallelConnection::Message ParallelConnection::receiveMessage() {
    // Header consists of 'OS' + majorVersion + minorVersion + messageSize
    constexpr size_t headerSize = 2 * sizeof(char) + 3 * sizeof(uint32_t);

    // Create basic buffer for receiving first part of messages
    std::vector<char> headerBuffer(headerSize);
    std::vector<char> messageBuffer;

    // Receive the header data
    if (!_socket->get(headerBuffer.data(), headerSize)) {
        LERROR("Failed to read header from socket. Disconencting.");
        throw ConnectionLostError();
    }

    // Make sure that header matches this version of OpenSpace
    if (!(headerBuffer[0] == 'O' && headerBuffer[1] && 'S')) {
        LERROR("Expected to read message header 'OS' from socket.");
        throw ConnectionLostError();
    }

    uint32_t* ptr = reinterpret_cast<uint32_t*>(&headerBuffer[2]);

    uint32_t protocolVersionIn = *(ptr++);
    uint32_t messageTypeIn = *(ptr++);
    uint32_t messageSizeIn = *(ptr++);

    if (protocolVersionIn != ProtocolVersion) {
        LERROR(fmt::format(
            "Protocol versions do not match. Remote version: {}, Local version: {}",
            protocolVersionIn,
            ProtocolVersion
        ));
        throw ConnectionLostError();
    }

    size_t messageSize = messageSizeIn;

    // Receive the payload
    messageBuffer.resize(messageSize);
    if (!_socket->get(messageBuffer.data(), messageSize)) {
        LERROR("Failed to read message from socket. Disconencting.");
        throw ConnectionLostError();
    }

    // And delegate decoding depending on type
    return ParallelConnection::Message(
        static_cast<ParallelConnection::MessageType>(messageTypeIn),
        messageBuffer
    );
}

} // namespace openspace
