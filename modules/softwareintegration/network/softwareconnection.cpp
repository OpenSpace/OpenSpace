/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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
    LDEBUG(std::format("Adding software connection {}", _id));
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
    LDEBUG(std::format("Removing software connection {}", _id));

    _shouldStopOutgoingMessagesThread = true;
    _outgoingMessagesNotifier.notify_all();
    if (_outgoingMessagesThread && _outgoingMessagesThread->joinable()) {
        _outgoingMessagesThread->join();
    }

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
        LWARNING(std::format(
            "Couldn't add property subscription. Renderable \"{}\" doesn't exist",
            identifier
        ));
        return;
    }

    auto property = r->property(propertyName);
    if (!property) {
        LWARNING(std::format(
            "Couldn't add property subscription. Property \"{}\" doesn't exist on \"{}\"",
            propertyName, identifier
        ));
        return;
    }

    // Set new onChange handler
    connection::SceneGraphNodeInfo::OnChangeHandle onChangeHandle = property->onChange(newHandler);

    auto propertySubscriptions = _sceneGraphNodes.find(identifier);
    if (propertySubscriptions == _sceneGraphNodes.end()) {
        LERROR(std::format("Couldn't add property subscription. No SceneGraphNode with identifier {} exists.", identifier));
        return;
    }

    auto propertySubscription = propertySubscriptions->second.propertySubscriptions.find(propertyName);
    if (propertySubscription != propertySubscriptions->second.propertySubscriptions.end()) {
        // Property subscription already exists

        // Remove old onChange handler
        property->removeOnChange(propertySubscription->second.onChangehandle);

        // Save new onChange handler
        propertySubscription->second.onChangehandle = onChangeHandle;
    }
    else {
        // Property subscription doesn't exist
        connection::SceneGraphNodeInfo::PropertySubscription newPropertySubscription{ onChangeHandle };
        propertySubscriptions->second.propertySubscriptions.emplace(propertyName, newPropertySubscription);
    }
}

bool SoftwareConnection::hasPropertySubscription(
    const std::string& identifier,
    const std::string& propertyName
) {
    // Get renderable
    auto r = renderable(identifier);
    if (!r) {
        LDEBUG(std::format(
            "Couldn't check for property subscriptions, renderable {} doesn't exist",
            identifier
        ));
        return false;
    }

    if (!_sceneGraphNodes.count(identifier)) return false;
    return _sceneGraphNodes.at(identifier).propertySubscriptions.count(propertyName);
}

void SoftwareConnection::removePropertySubscriptions(const std::string& identifier) {
    // Get renderable
    auto r = renderable(identifier);
    if (!r) {
        LWARNING(std::format(
            "Couldn't remove property subscriptions, renderable {} doesn't exist",
            identifier
        ));
        return;
    }

    auto propertySubscriptions = _sceneGraphNodes.find(identifier);

    if (propertySubscriptions == _sceneGraphNodes.end()) return;

    auto propertySubscriptionIt = propertySubscriptions->second.propertySubscriptions.begin();
    while (propertySubscriptionIt != propertySubscriptions->second.propertySubscriptions.end()) {
        auto propertySubscriptionCopy = *propertySubscriptionIt;
        propertySubscriptionIt = propertySubscriptions->second.propertySubscriptions.erase(propertySubscriptionIt);

        auto property = r->property(propertySubscriptionCopy.first);
        if (!property) {
            LWARNING(std::format(
                "Couldn't remove property subscription. Property \"{}\" doesn't exist on \"{}\"",
                propertySubscriptionCopy.first, identifier
            ));
            ++propertySubscriptionIt;
            continue;
        }

        property->removeOnChange(propertySubscriptionCopy.second.onChangehandle);
    }
}

void SoftwareConnection::removePropertySubscription(
    const std::string& identifier,
    const std::string& propertyName
) {
    // Get renderable
    auto r = renderable(identifier);
    if (!r) {
        LWARNING(std::format(
            "Couldn't remove property subscription. Renderable \"{}\" doesn't exist",
            identifier
        ));
        return;
    }

    if (!r->hasProperty(propertyName)) {
        LWARNING(std::format(
            "Couldn't remove property subscription. Property \"{}\" doesn't exist on \"{}\"",
            propertyName, identifier
        ));
        return;
    }

    auto property = r->property(propertyName);

    auto propertySubscriptions = _sceneGraphNodes.find(identifier);
    if (propertySubscriptions != _sceneGraphNodes.end()) {
        // At least one property have been subscribed to on this SGN
        auto propertySubscription = propertySubscriptions->second.propertySubscriptions.find(propertyName);
        if (propertySubscription != propertySubscriptions->second.propertySubscriptions.end()) {
            // Property subscription already exists

            // Remove onChange handle
            property->removeOnChange(propertySubscription->second.onChangehandle);

            // Remove property subscription
            propertySubscriptions->second.propertySubscriptions.erase(propertySubscription);
        }
    }
}

bool SoftwareConnection::shouldSendData(const std::string& identifier, const std::string& propertyName) {
    auto sgn = _sceneGraphNodes.find(identifier);
    if (sgn == _sceneGraphNodes.end()) {
        return false;
    }

    auto propertySubscription = sgn->second.propertySubscriptions.find(propertyName);
    if (propertySubscription == sgn->second.propertySubscriptions.end()) {
        return false;
    }

    if (propertySubscription->second.shouldSendMessage) {
        return true;
    }
    else {
        propertySubscription->second.shouldSendMessage = true;
        return false;
    }
}

void SoftwareConnection::setShouldNotSendData(const std::string& identifier, const std::string& propertyName) {
    auto sgn = _sceneGraphNodes.find(identifier);
    if (sgn == _sceneGraphNodes.end()) {
        LERROR(std::format(
            "Couldn't set shouldNotSendData on property {} on SceneGraphNode {}. SceneGraphNode doesn't exist.",
            propertyName, identifier
        ));
        return;
    }

    auto propertySubscription = sgn->second.propertySubscriptions.find(propertyName);
    if (propertySubscription == sgn->second.propertySubscriptions.end()) {
        LERROR(std::format(
            "Couldn't set shouldNotSendData on property {} on SceneGraphNode {}. No subscription on property.",
            propertyName, identifier
        ));
        return;
    }

    propertySubscription->second.shouldSendMessage = false;
}

void SoftwareConnection::disconnect() {
    _socket->disconnect();
    LINFO(std::format("OpenSpace has disconnected with external software"));
}

bool SoftwareConnection::isConnected() const {
    return _socket && _socket->isConnected();
}

bool SoftwareConnection::isConnectedOrConnecting() const {
    return _socket && (_socket->isConnected() || _socket->isConnecting());
}

bool SoftwareConnection::sendMessage(
    const softwareintegration::simp::MessageType& messageType,
    const std::vector<std::byte>& subjectBuffer
) {
    using namespace softwareintegration;
    try {
        if (!_socket || !isConnected()) {
            throw SoftwareConnectionLostError("Connection lost...");
        }
        LDEBUG(std::format(
            "sendMessage: messageType={}, subjectBuffer.size()={}", 
            simp::getStringFromMessageType(messageType), 
            subjectBuffer.size()
        ));
        
        std::vector<std::byte> message{};
        std::string header = std::format(
            "{}{}{}",
            simp::protocolVersion,
            simp::getStringFromMessageType(messageType),
            simp::formatLengthOfSubject(subjectBuffer.size())
        );
        simp::toByteBuffer(message, 0, header);
        simp::toByteBuffer(message, message.size(), subjectBuffer);

        if (!_socket->put(message.data(), message.size())) {
            return false;
        }
    }
    catch (const SoftwareConnectionLostError& err) {
        LERROR(std::format("Couldn't send message with type \"{}\", due to: {}", simp::getStringFromMessageType(messageType), err.message));
    }
    catch (const std::exception& err) {
        LERROR(std::format("Couldn't send message with type \"{}\", due to: {}", simp::getStringFromMessageType(messageType), err.what()));
    }

    LDEBUG(std::format("Sent message with type {}", simp::getStringFromMessageType(messageType)));
    return true;
}

void SoftwareConnection::addSceneGraphNode(const std::string& identifier) {
    if (_sceneGraphNodes.count(identifier)) return;
    _sceneGraphNodes.insert({ identifier, {} });
}

void SoftwareConnection::removeSceneGraphNode(const std::string& identifier) {
    if (!_sceneGraphNodes.count(identifier)) return;
    removeMessageQueue(identifier);
    removePropertySubscriptions(identifier);
    _sceneGraphNodes.erase(identifier);
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

std::mutex& SoftwareConnection::outgoingMessagesMutex() {
    return _outgoingMessagesMutex;
}

/**
 * DANGER! You need to lock outgoing message queue mutex
 * (outgoingMessagesMutex) before calling this function
 *
 * @param entry holds the value in bytes and its length
 */
void SoftwareConnection::addToMessageQueue(
    const std::string& identifier,
    softwareintegration::simp::DataKey dataKey,
    const std::tuple<std::vector<std::byte>, int32_t>& entry
) {
    auto sgn = _sceneGraphNodes.find(identifier);
    if (sgn == _sceneGraphNodes.end()) {
        LERROR(std::format(
            "Couldn't add {} to message queue. No SceneGraphNode with identifier {} exists.",
            softwareintegration::simp::getStringFromDataKey(dataKey), identifier
        ));
        return;
    }

    auto outgoingMessages = sgn->second.outgoingMessages.find(dataKey);
    if (outgoingMessages != sgn->second.outgoingMessages.end()) {
        outgoingMessages->second = entry;
    }
    else {
        sgn->second.outgoingMessages.emplace(dataKey, entry);
    }
}

void SoftwareConnection::removeMessageQueue(const std::string& identifier) {
    auto sgn = _sceneGraphNodes.find(identifier);
    if (sgn == _sceneGraphNodes.end()) {
        LERROR(std::format(
            "Couldn't remove message queue. No SceneGraphNode with identifier {} exists.",
            identifier
        ));
        return;
    }

    sgn->second.outgoingMessages.clear();
}

void SoftwareConnection::notifyMessageQueueHandler() {
    _outgoingMessagesNotifier.notify_one();
}

void SoftwareConnection::handleOutgoingMessages() {
    using namespace softwareintegration;

    _outgoingMessagesThread = std::make_unique<std::thread>([this] {
        std::vector<std::byte> subjectBuffer{};

        while (!_shouldStopOutgoingMessagesThread) {
            auto outgoingMessagesAreReady = [this] {
                bool dataMessagesEmpty = true;
                for (auto& s : _sceneGraphNodes) {
                    if (!s.second.outgoingMessages.empty()) {
                        dataMessagesEmpty = false;
                        break;
                    }
                }
                return _shouldStopOutgoingMessagesThread || !dataMessagesEmpty;
                // return _shouldStopOutgoingMessagesThread || (!dataMessagesEmpty && _outgoingMessagesMutex.try_lock());
            };

            // Block excecution on this thread for as long as queue mutex is locked or queue is empty
            if (!outgoingMessagesAreReady()) {
                std::unique_lock lock(_outgoingMessagesNotifierMutex);
                _outgoingMessagesNotifier.wait(lock, outgoingMessagesAreReady);
            }

            // Check again since we're blocking excecution above
            if (_shouldStopOutgoingMessagesThread) break;

            // Lock queue mutex so that other threads cannot mutate the list while gathering all data to be sent
            std::lock_guard guard{ _outgoingMessagesMutex };

            // TODO: Breakout to function?
            // Add all data to outgoing message subject
            for (auto [identifier, sceneGraphNodeInfo] : _sceneGraphNodes) {
                if (sceneGraphNodeInfo.outgoingMessages.empty()) continue;

                auto r = renderable(identifier);
                if (!r) continue;

                auto guiNameProp = r->property("Name");
                if (!guiNameProp) continue;

                size_t subjectBufferOffset = 0;
                subjectBuffer.clear();
                std::string subjectPrefixString = std::format(
                    "{}{}{}{}", identifier, simp::DELIM, guiNameProp->stringValue(), simp::DELIM
                );
                simp::toByteBuffer(subjectBuffer, subjectBufferOffset, subjectPrefixString);

                // TODO: Breakout to function
                // Add all data for SGN to outgoing message subject
                for (auto& [dataKey, data] : sceneGraphNodeInfo.outgoingMessages) {
                    auto& [dataBuffer, nValues] = data;

                    std::string dataKeyString = std::format(
                        "{}{}", simp::getStringFromDataKey(dataKey), simp::DELIM
                    );

                    std::string nValuesString = nValues > 1 ? std::to_string(nValues) : "";
                    LDEBUG(std::format("Sending {} {} to OpenSpace", nValuesString, simp::getStringFromDataKey(dataKey)));

                    simp::toByteBuffer(subjectBuffer, subjectBufferOffset, dataKeyString);
                    if (nValues > 1) {
                        // Add length to subject if data has multiple values
                        simp::toByteBuffer(subjectBuffer, subjectBufferOffset, nValues);
                    }
                    simp::toByteBuffer(subjectBuffer, subjectBufferOffset, dataBuffer);
                }

                sceneGraphNodeInfo.outgoingMessages.clear();

                if (!subjectBuffer.empty()) {
                    sendMessage(simp::MessageType::Data, subjectBuffer);
                }
            }
        }
    });

}

bool SoftwareConnection::handshakeHasBeenMade() {
    return _handshakeHasBeenMade;
}

void SoftwareConnection::setHandshakeHasBeenMade() {
    _handshakeHasBeenMade = true;
}

namespace softwareintegration::network::connection {

void eventLoop(
	std::weak_ptr<SoftwareConnection> connectionWeakPtr,
	std::weak_ptr<NetworkState> networkStateWeakPtr
) {
    if (connectionWeakPtr.expired()) return;
    connectionWeakPtr.lock()->handleOutgoingMessages();

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
				LDEBUG(std::format("Connection lost to {}: {}", connectionPtr->id(), err.message));
				auto networkState = networkStateWeakPtr.lock();
				if (networkState->softwareConnections.count(connectionPtr->id())) {
					networkState->softwareConnections.erase(connectionPtr->id());
				}
			}
			break;
		}
	}
}

IncomingMessage receiveMessageFromSoftware(std::shared_ptr<SoftwareConnection> connectionPtr) {
    // Header consists of version (5 chars), message type (4 chars) & subject size (15 chars)
    size_t headerSize = 24 * sizeof(char);

    // Create basic buffer for receiving first part of message
    std::vector<char> headerBuffer(headerSize);
    std::vector<std::byte> subjectBuffer;

    // Receive the header data
    if (!connectionPtr->socket()->get(headerBuffer.data(), headerSize)) {
        throw SoftwareConnectionLostError("Failed to read header from socket. Disconnecting.");
    }

    // // Read the protocol version number: Byte 0-4
    std::string protocolVersionIn{ headerBuffer.begin(), headerBuffer.begin() + 5 };

    // Make sure that header matches the protocol version
    if (protocolVersionIn != softwareintegration::simp::protocolVersion) {
        throw SoftwareConnectionLostError(std::format(
            "Protocol versions do not match. Remote version: {}, Local version: {}",
            protocolVersionIn,
            softwareintegration::simp::protocolVersion
        ));
    }

    // Read message type: Byte 5-8
    std::string type{ headerBuffer.begin() + 5, headerBuffer.begin() + 9 };
    auto typeEnum = softwareintegration::simp::getMessageType(type);

    // Read and convert message size: Byte 9-23
    const size_t subjectSize = std::stoll(std::string{ headerBuffer.begin() + 9, headerBuffer.begin() + 24 });

    // Receive the message subject
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
