/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#include <modules/softwareintegration/network/network.h>
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

SoftwareConnection::SoftwareConnection(std::unique_ptr<ghoul::io::TcpSocket> socket)
    : _id{ _nextConnectionId++ }, _socket{ std::move(socket) }, _sceneGraphNodes{},
    _thread{}, _shouldStopThread{ false }
{
    LDEBUG(fmt::format("Adding software connection {}", _id));
}

SoftwareConnection::SoftwareConnection(SoftwareConnection&& sc)
	: _id{ std::move(sc._id) }, _socket{ std::move(sc._socket) },
    _sceneGraphNodes{ std::move(sc._sceneGraphNodes) },
    _thread{}, _shouldStopThread{ false }
{}

SoftwareConnection::~SoftwareConnection() {
    // When adding new features, always make sure that the
    // destructor is called when disconnecting external
    // since NetworkEngine and MessageHandler has
    // shared_ptrs to SoftwareConnection, which can cause
    // bugs if not handled properly.
    // Tips: use weak_ptr instead of shared_ptr in callbacks.
    LDEBUG(fmt::format("Removing software connection {}", _id));

    _shouldStopThread = true;
    _thread.detach();
	disconnect();
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
    
    // Set new onChange handler
    OnChangeHandle onChangeHandle = property->onChange(newHandler);

    auto propertySubscriptions = _subscribedProperties.find(identifier);
    if (propertySubscriptions != _subscribedProperties.end()) {
        // At least one property have been subscribed to on this SGN
        auto propertySubscription = propertySubscriptions->second.find(propertyName);
        if (propertySubscription != propertySubscriptions->second.end()) {
            // Property subscription already exists
            
            // Remove old handle
            property->removeOnChange(propertySubscription->second);

            // Save new handle 
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

    // if (_subscribedProperties.count(identifier)) {
    //     // At least one property have been subscribed to on this SGN
    //     auto propertySubscription = _subscribedProperties.at(identifier);
    //     if (propertySubscription.count(propertyName)) {
    //         // Property subscription already exists
    //         removeExistingPropertySubscription(identifier, property, propertySubscription.at(propertyName));
    //         propertySubscription.at(propertyName)->onChangeHandle = onChangeHandle;
    //     }
    //     else {
    //         // Property subscription doesn't exist
    //         PropertySubscription newPropertySub{ onChangeHandle };
    //         _subscribedProperties.at(identifier).emplace(
    //             propertyName,
    //             std::make_shared<PropertySubscription>(std::move(newPropertySub))
    //         );
    //     }
    // }
    // else {
    //     // No properties have been subscribed to on this SGN
    //     PropertySubscription newPropertySub{ onChangeHandle };
    //     PropertySubscriptions newPropertySubs{{
    //         propertyName,
    //         std::make_shared<PropertySubscription>(std::move(newPropertySub))
    //     }};
    //     _subscribedProperties.emplace(identifier, std::move(newPropertySubs));
    // }
}

// std::shared_ptr<SoftwareConnection::PropertySubscription> SoftwareConnection::getPropertySubscription(
//     const std::string& propertyName,
//     const std::string& identifier
// ) {
//     if (!_subscribedProperties.count(identifier)) return nullptr;
//     if (!_subscribedProperties.at(identifier).count(propertyName)) return nullptr;

//     return _subscribedProperties.at(identifier).at(propertyName);
// }

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

    auto propertySubscriptionIt = std::begin(propertySubscriptions->second);
    while (propertySubscriptionIt != std::end(propertySubscriptions->second)) {
        auto& [propertyName, onChangeHandle] = *propertySubscriptionIt;

        if (!r->hasProperty(propertyName)) {
            LWARNING(fmt::format(
                "Couldn't remove property subscription. Property \"{}\" doesn't exist on \"{}\"",
                propertyName, identifier
            ));
            continue;
        }

        auto property = r->property(propertyName);
        property->removeOnChange(onChangeHandle);

        propertySubscriptionIt = propertySubscriptions->second.erase(propertySubscriptionIt);
    }

    _subscribedProperties.erase(propertySubscriptions);
}

void SoftwareConnection::removePropertySubscription(
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

    auto propertySubscriptions = _subscribedProperties.find(identifier);
    if (propertySubscriptions != _subscribedProperties.end()) {
        // At least one property have been subscribed to on this SGN
        auto propertySubscription = propertySubscriptions->second.find(propertyName);
        if (propertySubscription != propertySubscriptions->second.end()) {
            // Property subscription already exists

            // Remove onChange handle
            property->removeOnChange(propertySubscription->second);

            // Remove property subscription
            propertySubscriptions->second.erase(propertySubscription);
        }
    }
}

void SoftwareConnection::disconnect() {
    _socket->disconnect();
    LINFO(fmt::format("OpenSpace has disconnected with external software through socket"));
}

bool SoftwareConnection::isConnected() const {
    return _socket && _socket->isConnected();
}

bool SoftwareConnection::isConnectedOrConnecting() const {
    return _socket && (_socket->isConnected() || _socket->isConnecting());
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

size_t SoftwareConnection::id() {
	return _id;
}

void SoftwareConnection::setThread(std::thread& t) {
	_thread = std::move(t);
}

ghoul::io::TcpSocket* SoftwareConnection::socket() {
    return _socket.get();
}

namespace softwareintegration::network::connection {

void eventLoop(
	std::weak_ptr<SoftwareConnection> connectionWeakPtr,
	std::weak_ptr<NetworkState> networkStateWeakPtr
) {
	while (!connectionWeakPtr.expired()) {
		auto connectionPtr = connectionWeakPtr.lock();
		if (connectionPtr->_shouldStopThread) break;

		try {
			IncomingMessage m = receiveMessageFromSoftware(connectionPtr);
			if (networkStateWeakPtr.expired()) break;
			networkStateWeakPtr.lock()->incomingMessages.push(m);
		}
		catch (const SoftwareConnectionLostError& err) {
			if (!networkStateWeakPtr.expired()
				& (!connectionPtr->_shouldStopThread || !connectionPtr->isConnectedOrConnecting())
			) {
				LDEBUG(fmt::format("Connection lost to {}: {}", connectionPtr->id(), err.message));
				auto networkState = networkStateWeakPtr.lock();
				if (networkState->softwareConnections.count(connectionPtr->id())) {
					networkState->softwareConnections.erase(connectionPtr->id());
				}
			}
			break;
		}
	}
}

IncomingMessage receiveMessageFromSoftware(
    std::shared_ptr<SoftwareConnection> connectionPtr
) {
    // Header consists of version (3 char), message type (4 char) & subject size (15 char)
    size_t headerSize = 22 * sizeof(char);

    // Create basic buffer for receiving first part of message
    std::vector<char> headerBuffer(headerSize);
    std::vector<char> subjectBuffer;

    // Receive the header data
    if (!connectionPtr->socket()->get(headerBuffer.data(), headerSize)) {
        throw SoftwareConnectionLostError("Failed to read header from socket. Disconnecting.");
    }

    // Read the protocol version number: Byte 0-2
    std::string protocolVersionIn;
    for (int i = 0; i < 3; i++) {
        protocolVersionIn.push_back(headerBuffer[i]);
    }

    // Make sure that header matches the protocol version
    if (protocolVersionIn != softwareintegration::simp::ProtocolVersion) {
        throw SoftwareConnectionLostError(fmt::format(
            "Protocol versions do not match. Remote version: {}, Local version: {}",
            protocolVersionIn,
            softwareintegration::simp::ProtocolVersion
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

    auto typeEnum = softwareintegration::simp::getMessageType(type);

    // Receive the message data
    if (typeEnum != softwareintegration::simp::MessageType::Unknown) {
        subjectBuffer.resize(subjectSize);
        if (!connectionPtr->socket()->get(subjectBuffer.data(), subjectSize)) {
            throw SoftwareConnectionLostError("Failed to read message from socket. Disconnecting.");
        }
    }

    return { connectionPtr, typeEnum, subjectBuffer, type };
}

} // namespace softwareintegration::network::connection

} // namespace openspace
