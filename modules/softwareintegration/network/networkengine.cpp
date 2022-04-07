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
#include <openspace/navigation/navigationhandler.h>
#include <openspace/scene/scene.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/engine/syncengine.h>
#include <openspace/engine/windowdelegate.h>
#include <ghoul/logging/logmanager.h>
#include <openspace/util/timemanager.h>

//REMOVE

#include <openspace/engine/openspaceengine.h>

namespace {
	constexpr const char* _loggerCat = "SoftwareIntegration_NetworkEngine";
} // namespace

namespace openspace {
NetworkEngine::NetworkEngine(const int port)
	: _port{port}
{}

void NetworkEngine::start() {
	_socketServer.listen(_port);

	_serverThread = std::thread([this]() { handleNewPeers(); });
	_eventLoopThread = std::thread([this]() { eventLoop(); });
}

void NetworkEngine::stop() {
	_shouldStop = true;
	_socketServer.close();

	if (_serverThread.joinable()) {
		_serverThread.join();
	}
	if (_eventLoopThread.joinable()) {
		_eventLoopThread.join();
	}
}

void NetworkEngine::update() {
	_pointDataMessageHandler.preSyncUpdate();
}

bool NetworkEngine::isConnected(const Peer& peer) const {
	return peer.status != SoftwareConnection::Status::Connecting &&
		peer.status != SoftwareConnection::Status::Disconnected;
}

std::shared_ptr<NetworkEngine::Peer> NetworkEngine::peer(size_t id) {
	std::lock_guard<std::mutex> lock(_peerListMutex);
	auto it = _peers.find(id);
	if (it == _peers.end()) {
		return nullptr;
	}
	return it->second;
}

void NetworkEngine::disconnect(Peer& peer) {    
	if (isConnected(peer)) {
		_nConnections -= 1;
	}

	peer.connection.disconnect();
	peer.thread.join();
	_peers.erase(peer.id);
}

void NetworkEngine::eventLoop() {
	while (!_shouldStop) {
		if (!_incomingMessages.empty()) {
			auto pm = _incomingMessages.pop();
			handlePeerMessage(std::move(pm));
		}
	}
}

void NetworkEngine::handleNewPeers() {
	while (!_shouldStop) {
		std::unique_ptr<ghoul::io::TcpSocket> socket =
			_socketServer.awaitPendingTcpSocket();

		if (!socket) {
			return;
		}

		socket->startStreams();

		const size_t id = _nextConnectionId++;
		std::shared_ptr<Peer> p = std::make_shared<Peer>(Peer{
			id,
			"",
			std::thread(),
			SoftwareConnection(std::move(socket)),
			SoftwareConnection::Status::Connecting
		});
		auto it = _peers.emplace(p->id, p);
		it.first->second->thread = std::thread([this, id]() {
			handlePeer(id);
		});
	}
}

void NetworkEngine::handlePeer(size_t id) {
	while (!_shouldStop) {
		std::shared_ptr<Peer> p = peer(id);
		if (!p) {
			return;
		}

		if (!p->connection.isConnectedOrConnecting()) {
			return;
		}

		try {
			SoftwareConnection::Message m = p->connection.receiveMessageFromSoftware();
			_incomingMessages.push({ id, m });
		}
		catch (const SoftwareConnection::SoftwareConnectionLostError&) {
			LERROR(fmt::format("Connection lost to {}", p->id));
			_incomingMessages.push({
				id,
				SoftwareConnection::Message(
					SoftwareConnection::MessageType::Disconnection, std::vector<char>()
				)
			});
			return;
		}
	}
}

void NetworkEngine::handlePeerMessage(PeerMessage peerMessage) {
	const size_t peerId = peerMessage.peerId;
	std::shared_ptr<NetworkEngine::Peer> peerPtr = peer(peerId);

	const SoftwareConnection::MessageType messageType = peerMessage.message.type;
	std::vector<char>& message = peerMessage.message.content;

	switch (messageType) {
	case SoftwareConnection::MessageType::Connection: {
		const std::string software(message.begin(), message.end());
		LINFO(fmt::format("OpenSpace has connected with {} through socket", software));
		break;
	}
	case SoftwareConnection::MessageType::ReadPointData: {
		LDEBUG("Message recieved.. Point Data");
		_pointDataMessageHandler.handlePointDataMessage(message, peerPtr->connection);
		break;
	}
	case SoftwareConnection::MessageType::RemoveSceneGraphNode: {
		const std::string identifier(message.begin(), message.end());
		LDEBUG(fmt::format("Message recieved.. Delete SGN: {}", identifier));

		const std::string currentAnchor =
			global::navigationHandler->orbitalNavigator().anchorNode()->identifier();

		if (currentAnchor == identifier) {
			// If the deleted node is the current anchor, first change focus to the Sun
			openspace::global::scriptEngine->queueScript(
				"openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Anchor', 'Sun')"
				"openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Aim', '')",
				scripting::ScriptEngine::RemoteScripting::Yes
			);
		}
		openspace::global::scriptEngine->queueScript(
			"openspace.removeSceneGraphNode('" + identifier + "');",
			scripting::ScriptEngine::RemoteScripting::Yes
		);
		LDEBUG(fmt::format("Scene graph node '{}' removed.", identifier));
		break;
	}
	case SoftwareConnection::MessageType::Color: {
		const std::string colorMessage(message.begin(), message.end());
		LDEBUG(fmt::format("Message recieved.. New Color: {}", colorMessage));

		_pointDataMessageHandler.handleColorMessage(message);
		break;
	}
	case SoftwareConnection::MessageType::Opacity: {
		const std::string opacityMessage(message.begin(), message.end());
		LDEBUG(fmt::format("Message recieved.. New Opacity: {}", opacityMessage));

		_pointDataMessageHandler.handleOpacityMessage(message);
		break;
	}
	case SoftwareConnection::MessageType::Size: {
		const std::string sizeMessage(message.begin(), message.end());
		LDEBUG(fmt::format("Message recieved.. New Size: {}", sizeMessage));

		_pointDataMessageHandler.handlePointSizeMessage(message);
		break;
	}
	case SoftwareConnection::MessageType::Visibility: {
		const std::string visibilityMessage(message.begin(), message.end());
		LDEBUG(fmt::format("Message recieved.. New Visibility: {}", visibilityMessage));

		_pointDataMessageHandler.handleVisiblityMessage(message);
		break;
	}
	case SoftwareConnection::MessageType::Disconnection: {
		disconnect(*peerPtr);
		break;
	}
	default:
		LERROR(fmt::format(
			"Unsupported message type: {}", static_cast<int>(messageType)
		));
		break;
	}
}

} // namespace openspace
