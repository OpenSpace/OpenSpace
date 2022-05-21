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
#include <openspace/query/query.h>
#include <openspace/rendering/renderable.h>

namespace {
    constexpr const char* _loggerCat = "SoftwareConnection";
} // namespace

namespace openspace {

std::atomic_size_t SoftwareConnection::_nextConnectionId = 1;

SoftwareConnection::SoftwareConnectionLostError::SoftwareConnectionLostError(const std::string& msg)
    : ghoul::RuntimeError(fmt::format("{}{}", "Software connection lost", msg), "SoftwareConnection")
{}

SoftwareConnection::SoftwareConnection(std::unique_ptr<ghoul::io::TcpSocket> socket)
    : _id{ _nextConnectionId++ }, _socket{ std::move(socket) }, _sceneGraphNodes{},
    _thread{}, _shouldStopThread{ false }
{}

SoftwareConnection::SoftwareConnection(SoftwareConnection&& sc)
	: _id{ std::move(sc._id) }, _socket{ std::move(sc._socket) },
    _isConnected{ sc._isConnected }, _sceneGraphNodes{ std::move(sc._sceneGraphNodes) },
    _thread{}, _shouldStopThread{ false }
{}

SoftwareConnection::~SoftwareConnection() {
    LINFO(fmt::format("Remove software connection {}", _id));

    if (!_isConnected) return;
    _isConnected = false;

    for (auto& identifier : _sceneGraphNodes) {
        removePropertySubscriptions(identifier);
    }

    if (_socket) {
        _socket->disconnect();
    }
}

void SoftwareConnection::addPropertySubscription(
    const std::string& propertyName,
    const std::string& identifier,
    std::function<void()> newHandler
) {
    // Get renderable
    auto r = renderable(identifier);
    if (!r) {
        LWARNING(fmt::format(
            "Couldn't add property subscription. Renderable \"{}\" doesn't exist",
            identifier
        ));
        return;
    }

    if (!r->hasProperty(propertyName)) {
        LWARNING(fmt::format(
            "Couldn't add property subscription. Property \"{}\" doesn't exist on \"{}\"",
            propertyName, identifier
        ));
        return;
    }

    auto property = r->property(propertyName);
    OnChangeHandle onChangeHandle = property->onChange(newHandler);

    auto propertySubscriptions = _subscribedProperties.find(identifier);
    if (propertySubscriptions != _subscribedProperties.end()) {
        // At least one property have been subscribed to on this SGN
        auto propertySubscription = propertySubscriptions->second.find(propertyName);
        if (propertySubscription != propertySubscriptions->second.end()) {
            // Property subscription already exists
            removeExistingPropertySubscription(identifier, property, propertySubscription->second);
            propertySubscription->second = onChangeHandle;
        }
        else {
            // Property subscription doesn't exist
            propertySubscriptions->second.emplace(propertyName, onChangeHandle);
        }
    }
    else {
        // No properties have been subscribed to on this SGN
        PropertySubscriptions newPropertySubscriptionMap{ { propertyName, onChangeHandle } };
        _subscribedProperties.emplace(identifier, newPropertySubscriptionMap);
    }
}

void SoftwareConnection::removePropertySubscriptions(const std::string& identifier) {
    // Get renderable
    auto r = renderable(identifier);
    if (!r) {
        LWARNING(fmt::format(
            "Couldn't remove property subscriptions, renderable {} doesn't exist",
            identifier
        ));
        return;
    }

    auto propertySubscriptions = _subscribedProperties.find(identifier);

    if (propertySubscriptions == _subscribedProperties.end()) return;

    for (auto& [propertyName, onChangeHandle] : propertySubscriptions->second) {
        if (!r->hasProperty(propertyName)) {
            LWARNING(fmt::format(
                "Couldn't remove property subscription. Property \"{}\" doesn't exist on \"{}\"",
                propertyName, identifier
            ));
            continue;
        }

        auto propertySubscription = propertySubscriptions->second.find(propertyName);
        if (propertySubscription == propertySubscriptions->second.end()) continue;

        auto property = r->property(propertyName);
        removeExistingPropertySubscription(identifier, property, onChangeHandle);
    }

    _subscribedProperties.erase(propertySubscriptions);
}

void SoftwareConnection::removeExistingPropertySubscription(
    const std::string& identifier,
    properties::Property *property,
    OnChangeHandle onChangeHandle
) {
    property->removeOnChange(onChangeHandle);

    auto propertySubscriptions = _subscribedProperties.find(identifier);
    propertySubscriptions->second.erase(property->identifier());
}

void SoftwareConnection::PointDataMessageHandlerFriends::removePropertySubscription(
    std::shared_ptr<SoftwareConnection> connectionPtr,
    const std::string& propertyName,
    const std::string& identifier
) {
    // Get renderable
    auto r = renderable(identifier);
    if (!r) {
        LWARNING(fmt::format(
            "Couldn't remove property subscription. Renderable \"{}\" doesn't exist",
            identifier
        ));
        return;
    }

    if (!r->hasProperty(propertyName)) {
        LWARNING(fmt::format(
            "Couldn't remove property subscription. Property \"{}\" doesn't exist on \"{}\"",
            propertyName, identifier
        ));
        return;
    }

    auto property = r->property(propertyName);

    auto propertySubscriptions = connectionPtr->_subscribedProperties.find(identifier);
    if (propertySubscriptions != connectionPtr->_subscribedProperties.end()) {
        // At least one property have been subscribed to on this SGN
        auto propertySubscription = propertySubscriptions->second.find(propertyName);
        if (propertySubscription != propertySubscriptions->second.end()) {
            // Property subscription already exists
            connectionPtr->removeExistingPropertySubscription(identifier, property, propertySubscription->second);
        }
    }
}

void SoftwareConnection::disconnect() {
    _socket->disconnect();
}

bool SoftwareConnection::isConnected() const {
    return _isConnected && _socket && _socket->isConnected();
}

bool SoftwareConnection::isConnectedOrConnecting() const {
    return _isConnected && _socket && (_socket->isConnected() || _socket->isConnecting());
}

bool SoftwareConnection::sendMessage(const std::string& message) {
    try {
        if (!_socket || !isConnected()) {
            throw SoftwareConnectionLostError("Connection lost...");
        }
        if (_socket->put<char>(message.data(), message.size())) {
            LDEBUG(fmt::format("Message sent: {}", message));
            return true;
        }
    }
    catch (const SoftwareConnectionLostError& err) {
        LERROR(fmt::format("Couldn't send message: \"{}\", due to: {}", message, err.message));
    }
    catch (const std::exception& err) {
        LERROR(fmt::format("Couldn't send message: \"{}\", due to: {}", message, err.what()));
    }

    return false;
}

void SoftwareConnection::addSceneGraphNode(const std::string& identifier) {
    _sceneGraphNodes.insert(identifier);
}

void SoftwareConnection::removeSceneGraphNode(const std::string& identifier) {
    size_t amountRemoved = _sceneGraphNodes.erase(identifier);
    
    if (amountRemoved > 0) {
        removePropertySubscriptions(identifier);
    }
}

/**
 * @brief This function is only called on the server node, i.e. the node connected to the external software
 * 
 * @return SoftwareConnection::Message 
 */
SoftwareConnection::Message SoftwareConnection::receiveMessageFromSoftware() {
    // Header consists of version (3 char), message type (4 char) & subject size (15 char)
    size_t headerSize = 22 * sizeof(char);

    // Create basic buffer for receiving first part of message
    std::vector<char> headerBuffer(headerSize);
    std::vector<char> subjectBuffer;

    // Receive the header data
    if (!_socket->get(headerBuffer.data(), headerSize)) {
        throw SoftwareConnectionLostError("Failed to read header from socket. Disconnecting.");
    }

    // Read the protocol version number: Byte 0-2
    std::string protocolVersionIn;
    for (int i = 0; i < 3; i++) {
        protocolVersionIn.push_back(headerBuffer[i]);
    }

    // Make sure that header matches the protocol version
    if (protocolVersionIn != simp::ProtocolVersion) {
        throw SoftwareConnectionLostError(fmt::format(
            "Protocol versions do not match. Remote version: {}, Local version: {}",
            protocolVersionIn,
            simp::ProtocolVersion
        ));
    }

    // Read message type: Byte 3-6
    std::string type;
    for (int i = 3; i < 7; i++) {
        type.push_back(headerBuffer[i]);
    }

    // Read and convert message size: Byte 7-22
    std::string subjectSizeIn;
    for (int i = 7; i < 22; i++) {
        subjectSizeIn.push_back(headerBuffer[i]);
    }
    const size_t subjectSize = std::stoi(subjectSizeIn);

    // TODO: Remove this
    // std::string rawHeader;
    // for (int i = 0; i < 22; i++) {
    //     rawHeader.push_back(headerBuffer[i]);
    // }
    // LDEBUG(fmt::format("Message received with header: {}", rawHeader));

    auto typeEnum = simp::getMessageType(type);

    // Receive the message data
    if (typeEnum != simp::MessageType::Disconnection && typeEnum != simp::MessageType::Unknown) {
        subjectBuffer.resize(subjectSize);
        if (!_socket->get(subjectBuffer.data(), subjectSize)) {
            throw SoftwareConnectionLostError("Failed to read message from socket. Disconnecting.");
        }
    }

    return Message{ typeEnum, subjectBuffer, type };
}

bool SoftwareConnection::shouldStopThread() {
	return _shouldStopThread;
}

size_t SoftwareConnection::id() {
	return _id;
}

void SoftwareConnection::setThread(std::thread& t) {
	_thread = std::move(t);
}

void SoftwareConnection::NetworkEngineFriends::stopThread(std::shared_ptr<SoftwareConnection> connectionPtr) {
	connectionPtr->_shouldStopThread = true;
	connectionPtr->disconnect();
	if (connectionPtr->_thread.joinable()) {
		connectionPtr->_thread.join();
	}
}

} // namespace openspace
