/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <ghoul/logging/logmanager.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/syncengine.h>
#include <openspace/engine/windowdelegate.h>

namespace {
    constexpr const char* _loggerCat = "SoftwareConnection";
} // namespace

namespace openspace {

const float SoftwareConnection::ProtocolVersion = 1.0;

std::map<std::string, SoftwareConnection::MessageType> SoftwareConnection::mapSIMPTypeToMessageType {
    {"CONN", MessageType::Connection},
    {"PDAT", MessageType::ReadPointData},
    {"RSGN", MessageType::RemoveSceneGraphNode},
    {"UPCO", MessageType::Color},
    {"UPOP", MessageType::Opacity},
    {"UPSI", MessageType::Size},
    {"TOVI", MessageType::Visibility},
    {"DISC", MessageType::Disconnection},
};

SoftwareConnection::Message::Message(SoftwareConnection::MessageType type, std::vector<char> content)
    : type{ type }, content{ std::move(content) }
{}

SoftwareConnection::SoftwareConnectionLostError::SoftwareConnectionLostError()
    : ghoul::RuntimeError("Software connection lost", "SoftwareConnection")
{}

SoftwareConnection::SoftwareConnection(std::unique_ptr<ghoul::io::TcpSocket> socket)
    : _socket(std::move(socket))
{}

bool SoftwareConnection::isConnected() const {
    return _socket->isConnected();
}

bool SoftwareConnection::isConnectedOrConnecting() const {
    return _socket->isConnected() || _socket->isConnecting();
}

bool SoftwareConnection::sendMessage(std::string message) {
    if (_isListening) {
        if (!_socket->put<char>(message.data(), message.size())) {
            return false;
        }
        LDEBUG(fmt::format("Message sent: {}", message));
    }
    else {
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

/**
 * @brief This function is only called on the server node, i.e. the node connected to the external software
 * 
 * @return SoftwareConnection::Message 
 */
SoftwareConnection::Message SoftwareConnection::receiveMessageFromSoftware() {
    // LWARNING("SoftwareConnection::receiveMessageFromSoftware()");
    // if (global::windowDelegate->isMaster()) {
    // }
    
    // Header consists of version (3 char), message type (4 char) & subject size (9 char)
    size_t headerSize = 16 * sizeof(char);

    // Create basic buffer for receiving first part of message
    std::vector<char> headerBuffer(headerSize);
    std::vector<char> subjectBuffer;

    // Receive the header data
    if (!_socket->get(headerBuffer.data(), headerSize)) {
        LERROR("Failed to read header from socket. Disconnecting.");
        throw SoftwareConnectionLostError();
    }

    // Read and convert version number: Byte 0-2
    std::string version;
    for (int i = 0; i < 3; i++) {
        version.push_back(headerBuffer[i]);
    }
    const uint32_t protocolVersionIn = std::stoi(version);

    // Make sure that header matches the protocol version
    if (protocolVersionIn != ProtocolVersion) {
        LERROR(fmt::format(
            "Protocol versions do not match. Remote version: {}, Local version: {}",
            protocolVersionIn,
            ProtocolVersion
        ));
        throw SoftwareConnectionLostError();
    }

    // Read message type: Byte 3-6
    std::string type;
    for (int i = 3; i < 7; i++) {
        type.push_back(headerBuffer[i]);
    }

    // Read and convert message size: Byte 7-15
    std::string subjectSizeIn;
    for (int i = 7; i < 16; i++) {
        subjectSizeIn.push_back(headerBuffer[i]);
    }
    const size_t subjectSize = std::stoi(subjectSizeIn);

    // Receive the message data
    subjectBuffer.resize(subjectSize);
    if (!_socket->get(subjectBuffer.data(), subjectSize)) {
        LERROR("Failed to read message from socket. Disconnecting.");
        throw SoftwareConnectionLostError();
    }

    // And delegate decoding depending on message type
    if (mapSIMPTypeToMessageType.count(type) != 0) {
        if (mapSIMPTypeToMessageType[type] == MessageType::Color
            || mapSIMPTypeToMessageType[type] == MessageType::Opacity
            || mapSIMPTypeToMessageType[type] == MessageType::Size
            || mapSIMPTypeToMessageType[type] == MessageType::Visibility
        ) {
            _isListening = false;
        }

        return Message(mapSIMPTypeToMessageType[type], subjectBuffer);
    }
    else {
        LERROR(fmt::format("Unsupported message type: {}. Disconnecting...", type));
        return Message(MessageType::Disconnection, subjectBuffer);
    }
}

} // namespace openspace
