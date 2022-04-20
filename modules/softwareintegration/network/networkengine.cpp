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
	std::lock_guard<std::mutex> lock(_peerListMutex);
	size_t count = _peers.count(peer->id);
	LDEBUG(fmt::format("t{}: isConnected() - _peers.count(peer.id): {}", std::this_thread::get_id(), count));
	return count != 0;
}

std::shared_ptr<NetworkEngine::Peer> NetworkEngine::peer(size_t id) {
	// std::lock_guard<std::mutex> lock(_peerListMutex);
	auto it = _peers.find(id);
	if (it == _peers.end()) return nullptr;
	return it->second;
}

void NetworkEngine::disconnect(std::shared_ptr<Peer> peer) {    
	// std::lock_guard<std::mutex> lock(_peerListMutex);
	if (peer == nullptr) return;
	if (!isConnected(peer)) return;

	peer->connection.disconnect();
	auto sgnIterator = peer->sceneGraphNodes.begin();
	while (sgnIterator != peer->sceneGraphNodes.end()) {
		LDEBUG(fmt::format("t{}: disconnect() - removing SGN '{}'", std::this_thread::get_id(), *sgnIterator));
		removeSceneGraphNode(*sgnIterator);
		peer->sceneGraphNodes.erase(sgnIterator);
		++sgnIterator;
	}

	LDEBUG(fmt::format("t{}: disconnect() - pre thread.join()", std::this_thread::get_id()));
	peer->thread.join(); // TODO: FIX - this triggers a message to be sent to Glue???
	_peers.erase(peer->id);
	_nConnections -= 1;
	// _shouldStop = true;
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

		const size_t id = _nextConnectionId++;
		std::shared_ptr<Peer> p = std::make_shared<Peer>(Peer{
			id,
			"",
			{},
			SoftwareConnection(std::move(socket)),
			{},
			false,
		});
		auto [it, peerInserted] = _peers.emplace(p->id, p);
		if (peerInserted){
			// it->first == p->id;
			// it->second == p;
			// The thread 'it.first->second->thread' will run 'peerEventLoop()' as fast as it can until stopped
			it->second->thread = std::thread(
				[this, id] () {
				peerEventLoop(id);
			}); 
			// it.first->second->isConnected = true;

		}
	}
}

void NetworkEngine::peerEventLoop(size_t id) {
	using namespace std::literals::chrono_literals;

	while (!_shouldStop) {
		std::shared_ptr<Peer> p = peer(id);

		// bool pConnectedOrConnecting = p->connection.isConnectedOrConnecting();
		// LINFO(fmt::format("t{}: pConnectedOrConnecting: {}", std::this_thread::get_id(), pConnectedOrConnecting));
		if (!p || !p->connection.isConnectedOrConnecting() || p->disconnecting) return;

		try {
			LINFO(fmt::format("t{}: peerEventLoop() - new message from p->id: {}", std::this_thread::get_id(), p->id));
			SoftwareConnection::Message m = p->connection.receiveMessageFromSoftware();

			if (m.type == SoftwareConnection::mapSIMPTypeToMessageType["DISC"])
			{
				p->disconnecting = true;
			}

			_incomingMessages.push({ id, m });

			LINFO(fmt::format("t{}: peerEventLoop() - message received, m.type: {}", 
				std::this_thread::get_id(), 
				static_cast<uint32_t>(m.type)
			));
		}
		catch (const SoftwareConnection::SoftwareConnectionLostError&) {
			// std::lock_guard<std::mutex> lock(_peerListMutex);
			LERROR(fmt::format("t{}: Connection lost to {} with peer id {}", p->name, p->id));
			disconnect(peer(id));
			return;
		}

		std::this_thread::sleep_for(1s); // TODO: Change this?
	}
}

void NetworkEngine::handlePeerMessage(PeerMessage peerMessage) {
	const size_t peerId = peerMessage.peerId;
	std::shared_ptr<Peer> peerPtr = peer(peerId);
	if(peerPtr == nullptr) {
		LERROR(fmt::format("No peer with peerId {} could be found", peerId));
		return;
	}

	const SoftwareConnection::MessageType messageType = peerMessage.message.type;
	std::vector<char>& message = peerMessage.message.content;

	int s = static_cast<int>(messageType);

	switch (messageType) {
	case SoftwareConnection::MessageType::Disconnection:
	{
		LDEBUG(fmt::format("t{}: handlePeerMessage() - Disconnecting peerId {}", std::this_thread::get_id(), peerId));
		disconnect(peerPtr);
		LDEBUG(fmt::format("t{}: handlePeerMessage() - _peers.size()={}, _shouldStop={}", std::this_thread::get_id(), _peers.size(), _shouldStop));
		break;
	}
	case SoftwareConnection::MessageType::Connection:
	{
		const std::string software{ message.begin(), message.end() };
		LINFO(fmt::format("OpenSpace has connected with {} through socket", software));
		break;
	}
	case SoftwareConnection::MessageType::ReadPointData: {
		LDEBUG("Message recieved.. Point Data");
        _pointDataMessageHandler.handlePointDataMessage(message, peerPtr->connection, peerPtr->sceneGraphNodes);
		break;
	}
	case SoftwareConnection::MessageType::RemoveSceneGraphNode: {
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
