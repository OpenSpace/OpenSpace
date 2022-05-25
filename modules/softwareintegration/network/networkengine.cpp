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

#include <modules/softwareintegration/network/networkengine.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/scene/scene.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/logging/logmanager.h>


namespace {
	constexpr const char* _loggerCat = "NetworkEngine";
} // namespace

namespace openspace {

using namespace softwareintegration;

NetworkEngine::NetworkEngine(const int port)
	: _port{port}
{}

NetworkEngine::~NetworkEngine() {
	stop();
}

void NetworkEngine::start() {
	_socketServer.listen(_port);

	_serverThread = std::thread([this]() { handleNewSoftwareConnections(); });
	_eventLoopThread = std::thread([this]() { eventLoop(); });
}

void NetworkEngine::stop() {
	_shouldStopServerThread = true;

	{
		std::lock_guard guardSoftwareConnections(_softwareConnectionsMutex);
		for (auto& [id, connectionPtr] : _softwareConnections) {
			SoftwareConnection::NetworkEngineFriends::stopThread(connectionPtr);
		}
	}

	_incomingMessages.interrupt();
	
	_shouldStopEventThread = true;
	_socketServer.close();
	_softwareConnections.clear();

	if (_serverThread.joinable()) {
		_serverThread.join();
    }
	if (_eventLoopThread.joinable()) {
		_eventLoopThread.join();
	}
}

void NetworkEngine::postSync() {
	_pointDataMessageHandler.postSync();
}

void NetworkEngine::handleNewSoftwareConnections() {
	while (!_shouldStopServerThread) {
		std::unique_ptr<ghoul::io::TcpSocket> socket = _socketServer.awaitPendingTcpSocket();

		if (!socket) return;

		socket->startStreams();

		auto p = std::make_shared<SoftwareConnection>(SoftwareConnection{ std::move(socket) });
		std::lock_guard guard(_softwareConnectionsMutex);
		auto [it, peerInserted] = _softwareConnections.emplace(p->id(), p);

		if (peerInserted) {
			auto& connectionPtr = it->second;
			auto thread = std::thread{
				[this, &connectionPtr] {
					peerEventLoop(connectionPtr->id());
				}
			};
			connectionPtr->setThread(thread);
		}
	}
}

void NetworkEngine::peerEventLoop(size_t connection_id) {
	using namespace std::literals::chrono_literals;
	auto connectionPtr = getSoftwareConnection(connection_id);

	while (!connectionPtr->shouldStopThread()) {
		try {
			SoftwareConnection::Message m = connectionPtr->receiveMessageFromSoftware();
			_incomingMessages.push({ connection_id, m });
		}
		catch (const SoftwareConnection::SoftwareConnectionLostError& err) {
			if (connectionPtr->shouldStopThread()) break;

			if (connectionPtr && (!connectionPtr->shouldStopThread() || !connectionPtr->isConnectedOrConnecting())) {
				LDEBUG(fmt::format("Connection lost to {}: {}", connection_id, err.message));
				_incomingMessages.push({
					connection_id,
					SoftwareConnection::Message{ simp::MessageType::Disconnection }
				});
			}
			break;
		}
	}
}

void NetworkEngine::eventLoop() {
	while (!_shouldStopEventThread) {
		// The call to "pop" below will block execution
		// on this thread until interrupt is called
		try {
			auto pm = _incomingMessages.pop();
			handleIncomingMessage(pm);
		}
		catch (const ghoul::RuntimeError&) {
			break;
		}
	}
}

std::shared_ptr<SoftwareConnection> NetworkEngine::getSoftwareConnection(size_t id) {
	std::lock_guard guard(_softwareConnectionsMutex);
	auto it = _softwareConnections.find(id);
	if (it == _softwareConnections.end()) return nullptr;
	return it->second;
}

void NetworkEngine::handleIncomingMessage(IncomingMessage incomingMessage) {
	auto connectionPtr = getSoftwareConnection(incomingMessage.connection_id);

	if(!connectionPtr) {
		LDEBUG(fmt::format("Trying to handle message from disconnected peer. Aborting."));
		return;
	}

	const simp::MessageType messageType = incomingMessage.message.type;
	std::vector<char>& message = incomingMessage.message.content;

	switch (messageType) {
		case simp::MessageType::Connection: {
			size_t offset = 0;
			const std::string software = simp::readString(message, offset);

			// Send back message to software to complete handshake
			connectionPtr->sendMessage(simp::formatConnectionMessage(software));
			LINFO(fmt::format("OpenSpace has connected with {} through socket", software));
			break;
		}
		case simp::MessageType::PointData: {
			LDEBUG("Message recieved.. Point data");
			_pointDataMessageHandler.handlePointDataMessage(message, connectionPtr);
			break;
		}
		case simp::MessageType::RemoveSceneGraphNode: {
			LDEBUG(fmt::format("Message recieved.. Remove SGN"));
			_pointDataMessageHandler.handleRemoveSGNMessage(message, connectionPtr);
			break;
		}
		case simp::MessageType::Color: {
			LDEBUG(fmt::format("Message recieved.. New color"));
			_pointDataMessageHandler.handleFixedColorMessage(message, connectionPtr);
			break;
		}
		case simp::MessageType::Colormap: {
			LDEBUG(fmt::format("Message recieved.. New colormap"));
			_pointDataMessageHandler.handleColormapMessage(message, connectionPtr);
			break;
		}
		case simp::MessageType::AttributeData: {
			LDEBUG(fmt::format("Message recieved.. New attribute data"));
			_pointDataMessageHandler.handleAttributeDataMessage(message, connectionPtr);
			break;
		}
		case simp::MessageType::Opacity: {
			LDEBUG(fmt::format("Message recieved.. New Opacity"));
			_pointDataMessageHandler.handleOpacityMessage(message, connectionPtr);
			break;
		}
		case simp::MessageType::FixedSize: {
			LDEBUG(fmt::format("Message recieved.. New size"));
			_pointDataMessageHandler.handleFixedPointSizeMessage(message, connectionPtr);
			break;
		}
		case simp::MessageType::LinearSize: {
			LDEBUG(fmt::format("Message recieved.. New linear size"));
			_pointDataMessageHandler.handleLinearPointSizeMessage(message, connectionPtr);
			break;
		}
		case simp::MessageType::Visibility: {
			LDEBUG(fmt::format("Message recieved.. New visibility"));
			_pointDataMessageHandler.handleVisibilityMessage(message, connectionPtr);
			break;
		}
		case simp::MessageType::Disconnection: {
			LDEBUG(fmt::format("Message recieved.. Disconnect software connection: {}", incomingMessage.connection_id));
			std::lock_guard guard(_softwareConnectionsMutex);
			if (_softwareConnections.count(incomingMessage.connection_id)) {
				_softwareConnections.erase(incomingMessage.connection_id);
			}
			SoftwareConnection::NetworkEngineFriends::stopThread(connectionPtr);
			break;
		}
		default: {
			LERROR(fmt::format(
				"Unsupported message type: {}", incomingMessage.message.rawMessageType
			));
			break;
		}
	}
}

} // namespace openspace
