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

#include <modules/softwareintegration/network/network.h>

#include <modules/softwareintegration/network/messagehandler.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/scene/scene.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/logging/logmanager.h>


namespace {

	constexpr const char* _loggerCat = "NetworkEngine";

} // namespace

namespace openspace::softwareintegration::network {
	
namespace {

void eventLoop(std::weak_ptr<NetworkState> networkStateWeakPtr) {
	while (!networkStateWeakPtr.expired()) {
		auto networkState = networkStateWeakPtr.lock();
		if (networkState->shouldStopThreads) break;
		// The call to "pop" below will block execution
		// on this thread until interrupt is called
		try {
			auto pm = networkState->incomingMessages.pop();
			messagehandler::handleMessage(pm);
		}
		catch (const ghoul::RuntimeError&) {
			break;
		}
	}
}

void serverLoop(std::weak_ptr<NetworkState> networkStateWeakPtr) {
	while (!networkStateWeakPtr.expired()) {
		auto networkState = networkStateWeakPtr.lock();
		if (networkState->shouldStopThreads|| !networkState->server || !networkState->server->isListening()) break;
		std::unique_ptr<ghoul::io::TcpSocket> socket = networkState->server->awaitPendingTcpSocket();

		if (!socket) return;

		socket->startStreams();

		auto p = std::make_shared<SoftwareConnection>(std::move(socket));
		std::lock_guard guard(networkState->softwareConnectionsMutex);
		auto [it, peerInserted] = networkState->softwareConnections.emplace(p->id(), std::move(p));

		if (peerInserted) {
			auto connectionWeak = std::weak_ptr<SoftwareConnection>{ it->second };
			auto thread = std::thread{
				[connectionWeak, networkStateWeakPtr] {
					connection::eventLoop(connectionWeak, networkStateWeakPtr);
				}
			};
			it->second->setThread(thread);
		}
	}
}

} // namespace

std::shared_ptr<NetworkState> serve(const int port) {
	auto networkState = std::make_shared<NetworkState>();

	// 4700, is the defualt port where the tcp socket will be opened to the ext. software
	networkState->server = std::make_unique<ghoul::io::TcpSocketServer>();
	networkState->server->listen(port);

	std::weak_ptr<NetworkState> networkStateWeakPtr = networkState;
	networkState->serverThread = std::make_unique<std::thread>([networkStateWeakPtr] {
		serverLoop(networkStateWeakPtr);
	});

	networkState->eventLoopThread = std::make_unique<std::thread>([networkStateWeakPtr] {
		eventLoop(networkStateWeakPtr);
	});

	return networkState;
};

void stopServer(std::shared_ptr<NetworkState> networkState) {
	if (networkState->hasStopped) return;
	networkState->hasStopped = true;

	networkState->shouldStopThreads = true;

	networkState->incomingMessages.interrupt();

	networkState->server->close();
	
	{
		std::lock_guard guardSoftwareConnections(networkState->softwareConnectionsMutex);
		networkState->softwareConnections.clear();
	}

	networkState->serverThread->join();
	networkState->eventLoopThread->join();
}

SoftwareConnectionLostError::SoftwareConnectionLostError(const std::string& msg)
    : ghoul::RuntimeError(std::format("{}{}", "Software connection lost", msg), "SoftwareConnection")
{}

} // namespace openspace::softwareintegration::network
