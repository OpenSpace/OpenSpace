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

    const float SoftwareConnection::ProtocolVersion = 1.0;

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

        if (_isListening)
        {
            if (!_socket->put<char>(message.data(), message.size())) {
                return false;
            }
            LDEBUG(fmt::format("Message sent: {}", message));
        }
        else
        {
            _isListening = true;
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
        // Header consists of version (3 char), message type (4 char) & subject size (9 char)
        size_t HeaderSize = 16 * sizeof(char);

        // Create basic buffer for receiving first part of message
        std::vector<char> headerBuffer(HeaderSize);
        std::vector<char> subjectBuffer;

        // Receive the header data
        if (!_socket->get(headerBuffer.data(), HeaderSize)) {
            LERROR("Failed to read header from socket. Disconnecting.");
            throw SoftwareConnectionLostError();
        }

        // Read and convert version number: Byte 0-2
        std::string version;
        for (int i = 0; i < 3; i++)
            version.push_back(headerBuffer[i]);
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

        // Read message type: Byte 3-6
        std::string type;
        for(int i = 3; i < 7; i++)
            type.push_back(headerBuffer[i]);

        // Read and convert message size: Byte 7-15
        std::string subjectSizeIn;
        for (int i = 7; i < 16; i++)
            subjectSizeIn.push_back(headerBuffer[i]);
        const size_t subjectSize = stoi(subjectSizeIn);

        // Receive the message data
        subjectBuffer.resize(subjectSize);
        if (!_socket->get(subjectBuffer.data(), subjectSize)) {
            LERROR("Failed to read message from socket. Disconnecting.");
            throw SoftwareConnectionLostError();
        }

        // And delegate decoding depending on message type
        if (type == "CONN")
            return Message(MessageType::Connection, subjectBuffer);
        else if (type == "PDAT")
            return Message(MessageType::ReadPointData, subjectBuffer);
        else if (type == "LUMI")
            return Message(MessageType::ReadLuminosityData, subjectBuffer);
        else if (type == "VELO")
            return Message(MessageType::ReadVelocityData, subjectBuffer);
        else if( type == "ASGN")
            return Message(MessageType::AddSceneGraphNode, subjectBuffer);
        else if (type == "RSGN")
            return Message(MessageType::RemoveSceneGraphNode, subjectBuffer);
        else if (type == "UPCO") {
            _isListening = false;
            return Message(MessageType::Color, subjectBuffer);
        }
        else if (type == "UPOP") {
            _isListening = false;
            return Message(MessageType::Opacity, subjectBuffer);
        }
        else if (type == "UPSI") {
            _isListening = false;
            return Message(MessageType::Size, subjectBuffer);
        }
        else if (type == "TOVI") {
            _isListening = false;
            return Message(MessageType::Visibility, subjectBuffer);
        }
        else if (type == "DISC")
            return Message(MessageType::Disconnection, subjectBuffer);
        else {
            LERROR(fmt::format("Unsupported message type: {}. Disconnecting...", type));
            return Message(MessageType::Disconnection, subjectBuffer);
        }
    }

} // namespace openspace

