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

#include <modules/softwareintegration/simp.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/scene/scene.h>
#include <openspace/scripting/scriptengine.h>
// #include <openspace/engine/syncengine.h>
// #include <openspace/engine/windowdelegate.h>
#include <ghoul/logging/logmanager.h>
// #include <openspace/util/timemanager.h>


namespace {
	constexpr const char* _loggerCat = "NetworkEngine";
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
    for (auto [id, peer] : _peers) {
		disconnect(peer);
	}
    
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

bool NetworkEngine::isConnected(const std::shared_ptr<Peer> peer) const {
  return peer->status == Peer::Status::Connected;
}

std::shared_ptr<NetworkEngine::Peer> NetworkEngine::peer(size_t id) {
	// std::lock_guard<std::mutex> lock(_peerListMutex);
	auto it = _peers.find(id);
	if (it == _peers.end()) return nullptr;
	return it->second;
}

void NetworkEngine::disconnect(std::shared_ptr<Peer> peer) {
	if (peer == nullptr) return;
	if (!isConnected(peer)) return; // Already disconnected

	--_nConnections;
	peer->status = Peer::Status::Disconnected;
	peer->connection.disconnect();

	// auto sgnIterator = peer->sceneGraphNodes.begin();
	// while (sgnIterator != peer->sceneGraphNodes.end()) {
	// 	LDEBUG(fmt::format("t{}: disconnect() - removing SGN '{}'", std::this_thread::get_id(), *sgnIterator));
	// 	removeSceneGraphNode(*sgnIterator);
	// 	peer->sceneGraphNodes.erase(sgnIterator);
	// 	++sgnIterator;
	// }

	if (peer->thread.joinable()) peer->thread.join();
	_peers.erase(peer->id);
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

		if (!socket) return;

		socket->startStreams();

		std::shared_ptr<Peer> p = std::make_shared<Peer>(Peer{
			_nextConnectionId++,
			"",
			{},
			SoftwareConnection(std::move(socket)),
			{},
			Peer::Status::Connected
		});
		auto [it, peerInserted] = _peers.emplace(p->id, p);
		if (peerInserted){
			// The thread 'it.first->second->thread' will run 'peerEventLoop()' as fast as it can until stopped
			it->second->thread = std::thread(
				[this, &p] () {
				peerEventLoop(p->id);
			}); 

		}
	}
}

void NetworkEngine::peerEventLoop(size_t id) {
	using namespace std::literals::chrono_literals;

	while (!_shouldStop) {
		std::shared_ptr<Peer> p = peer(id);

		if (!p || !p->connection.isConnectedOrConnecting()
				|| p->status == Peer::Status::Disconnected) {
			break;
		}

		try {
			SoftwareConnection::Message m = p->connection.receiveMessageFromSoftware();

			_incomingMessages.push({ id, m });
		}
		catch (const SoftwareConnection::SoftwareConnectionLostError& err) {
			if (p->status == Peer::Status::Connected) {
				LERROR(fmt::format("Connection lost to {}: {}", p->id, err.message));
				disconnect(p);
			}
			break;
		}
	}
}

void NetworkEngine::handlePeerMessage(PeerMessage peerMessage) {
	const size_t peerId = peerMessage.peerId;
	std::shared_ptr<Peer> peerPtr = peer(peerId);
	if(peerPtr == nullptr) {
		LERROR(fmt::format("No peer with peerId {} could be found", peerId));
		return;
	}

	const simp::MessageType messageType = peerMessage.message.type;
	std::vector<char>& message = peerMessage.message.content;

	switch (messageType) {
	case simp::MessageType::Connection:
	{
		const std::string software{ message.begin(), message.end() };
		LINFO(fmt::format("OpenSpace has connected with {} through socket", software));
		// Send back message to software to complete handshake
		peerPtr->connection.sendMessage(simp::formatConnectionMessage(software));
		break;
	}
	case simp::MessageType::ReadPointData: {
		LDEBUG("Message recieved.. Point Data");
        _pointDataMessageHandler.handlePointDataMessage(message, peerPtr->connection, peerPtr->sceneGraphNodes);
		break;
	}
	case simp::MessageType::RemoveSceneGraphNode: {
		const std::string identifier(message.begin(), message.end());
		LDEBUG(fmt::format("Message recieved.. Delete SGN: {}", identifier));
		removeSceneGraphNode(identifier);

		auto sgnIterator = peerPtr->sceneGraphNodes.begin();
		while (sgnIterator != peerPtr->sceneGraphNodes.end()) {
			if (*sgnIterator == identifier) {
				peerPtr->sceneGraphNodes.erase(sgnIterator);
				break;
			}
			++sgnIterator;
		}
		break;
	}
	case simp::MessageType::Color: {
		const std::string colorMessage(message.begin(), message.end());
		LDEBUG(fmt::format("Message recieved.. New Color: {}", colorMessage));

		_pointDataMessageHandler.handleColorMessage(message);
		break;
	}
	case simp::MessageType::Opacity: {
		const std::string opacityMessage(message.begin(), message.end());
		LDEBUG(fmt::format("Message recieved.. New Opacity: {}", opacityMessage));

		_pointDataMessageHandler.handleOpacityMessage(message);
		break;
	}
	case simp::MessageType::Size: {
		const std::string sizeMessage(message.begin(), message.end());
		LDEBUG(fmt::format("Message recieved.. New Size: {}", sizeMessage));

		_pointDataMessageHandler.handlePointSizeMessage(message);
		break;
	}
	case simp::MessageType::Visibility: {
		const std::string visibilityMessage(message.begin(), message.end());
		LDEBUG(fmt::format("Message recieved.. New Visibility: {}", visibilityMessage));

		_pointDataMessageHandler.handleVisiblityMessage(message);
		break;
	}
	case simp::MessageType::Disconnection: {
		LDEBUG(fmt::format("Message recieved.. Disconnect peer: {}", peerPtr->id));
        disconnect(peerPtr);
		break;
	}
	default:
		LERROR(fmt::format(
			"Unsupported message type: {}", static_cast<int>(messageType)
		));
		break;
	}
}

void NetworkEngine::removeSceneGraphNode(const std::string &identifier) {
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

	// TODO: remove from sceneGraphNodes
	// peer->sceneGraphNodes.erase(sgnIterator);

	LDEBUG(fmt::format("Scene graph node '{}' removed.", identifier));
}

} // namespace openspace
