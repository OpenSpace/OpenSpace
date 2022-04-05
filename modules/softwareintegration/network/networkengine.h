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

#ifndef __OPENSPACE_MODULE_SOFTWAREINTEGRATION___NETWORKENGINE___H__
#define __OPENSPACE_MODULE_SOFTWAREINTEGRATION___NETWORKENGINE___H__

#include <modules/softwareintegration/network/common/basenetworkengine.h>
#include <modules/softwareintegration/network/softwareconnection.h>
#include <modules/softwareintegration/pointdatamessagehandler.h>
#include <openspace/util/concurrentqueue.h>
#include <ghoul/io/socket/tcpsocketserver.h>
#include <openspace/util/syncdata.h>

namespace openspace {

class NetworkEngine : public BaseNetworkEngine {
public:
	NetworkEngine(const int port = 4700);

	struct Peer {
		size_t id;
		std::string name;
		std::thread thread;

		SoftwareConnection connection;
		SoftwareConnection::Status status;
	};

	void start() override;
	void stop() override;
	void update() override;

protected:
	void disconnect(Peer& peer);
	void handleNewPeers();
	void handlePeer(size_t id);
	void handlePeerMessage(PeerMessage peerMessage);

private:
	bool isConnected(const Peer& peer) const;

	std::shared_ptr<Peer> peer(size_t id);


	std::unordered_map<size_t, std::shared_ptr<Peer>> _peers;
	mutable std::mutex _peerListMutex;
		
	ghoul::io::TcpSocketServer _socketServer;
	size_t _nextConnectionId = 1;
	std::atomic_size_t _nConnections = 0;
	std::thread _serverThread;
	std::thread _eventLoopThread;

	const int _port;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SOFTWAREINTEGRATION___NETWORKENGINE___H__
