/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#include <modules/softwareintegration/network/softwareconnection.h>

#include <openspace/rendering/renderable.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const char* _loggerCat = "SoftwareConnection";
} // namespace

namespace openspace {

    const unsigned int SoftwareConnection::ProtocolVersion = 1;

    SoftwareConnection::Message::Message(MessageType type, std::vector<char> content)
        : type(type)
        , content(std::move(content))
    {}

    SoftwareConnection::SoftwareConnectionLostError::SoftwareConnectionLostError()
        : ghoul::RuntimeError("Software connection lost", "SoftwareConnection")
    {}

    SoftwareConnection::SoftwareConnection(std::unique_ptr<ghoul::io::TcpSocket> socket)
        : _socket(std::move(socket))
    {}

    bool SoftwareConnection::isConnectedOrConnecting() const {
        return _socket->isConnected() || _socket->isConnecting();
    }

    bool SoftwareConnection::sendMessage(std::string message) {
        if (!_socket->put<char>(message.data(), message.size())) {
            return false;
        }

        return true;
    }

    void SoftwareConnection::disconnect() {
        if (_socket) {
            _socket->disconnect();
        }
    }

    ghoul::io::TcpSocket* SoftwareConnection::socket() {
        return _socket.get();
    }

    SoftwareConnection::Message SoftwareConnection::receiveMessage() {
        // Header consists of version (1 char), message type (4 char) & message size (9 char)
        size_t HeaderSize = 14 * sizeof(char);

        // Create basic buffer for receiving first part of message
        std::vector<char> headerBuffer(HeaderSize);
        std::vector<char> messageBuffer;

        // Receive the header data
        if (!_socket->get(headerBuffer.data(), HeaderSize)) {
            LERROR("Failed to read header from socket. Disconnecting.");
            throw SoftwareConnectionLostError();
        }

        // Read and convert version number: Byte 0
        std::string version;
        version.push_back(headerBuffer[0]);
        const uint32_t protocolVersionIn = std::stoi(version);

        // Make sure that header matches the protocol version
        if (!(protocolVersionIn == ProtocolVersion)) {
            LERROR(fmt::format(
                "Protocol versions do not match. Remote version: {}, Local version: {}",
                protocolVersionIn,
                ProtocolVersion
            ));
            throw SoftwareConnectionLostError();
        }

        // Read message type: Byte 1-4
        std::string type;
        for(int i = 1; i <= 4; i++)
            type.push_back(headerBuffer[i]);

        // Read and convert message size: Byte 5-13
        std::string messageSizeIn;
        for (int i = 5; i <= 13; i++)
            messageSizeIn.push_back(headerBuffer[i]);
        const size_t messageSize = stoi(messageSizeIn);

        // Receive the message data
        messageBuffer.resize(messageSize);
        if (!_socket->get(messageBuffer.data(), messageSize)) {
            LERROR("Failed to read message from socket. Disconnecting.");
            throw SoftwareConnectionLostError();
        }

        // And delegate decoding depending on message type
        if (type == "CONN")
            return Message(MessageType::Connection, messageBuffer);
        if (type == "DATA")
            return Message(MessageType::ReadBinaryData, messageBuffer);
        else if( type == "ASGN")
            return Message(MessageType::AddSceneGraphNode, messageBuffer);
        else if (type == "RSGN")
            return Message(MessageType::RemoveSceneGraphNode, messageBuffer);
        else if (type == "UPCO")
            return Message(MessageType::Color, messageBuffer);
        else if (type == "UPOP")
            return Message(MessageType::Opacity, messageBuffer);
        else if( type == "UPSI")
            return Message(MessageType::Size, messageBuffer);
        else if (type == "TOVI")
            return Message(MessageType::Visibility, messageBuffer);
        else if (type == "DISC")
            return Message(MessageType::Disconnection, messageBuffer);
        else {
            LERROR(fmt::format("Unsupported message type: {}. Disconnecting...", type));
            return Message(MessageType::Disconnection, messageBuffer);
        }
    }

} // namespace openspace

