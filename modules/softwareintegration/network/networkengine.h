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

#include <modules/softwareintegration/network/softwareconnection.h>
#include <modules/softwareintegration/pointdatamessagehandler.h>
#include <modules/softwareintegration/interruptibleconcurrentqueue.h>
#include <modules/softwareintegration/simp.h>
#include <ghoul/io/socket/tcpsocketserver.h>

namespace openspace {

class NetworkEngine {
public:
	NetworkEngine(const int port = 4700);
	~NetworkEngine();

	struct IncomingMessage {
		size_t connection_id{1};
		SoftwareConnection::Message message{ simp::MessageType::Unknown };
	};

	void start();
	void stop();
	void postSync();

private:
	void handleNewSoftwareConnections();
	void handleIncomingMessage(IncomingMessage incomingMessage);
	void peerEventLoop(size_t connection_id);
	void eventLoop();

	// The destuction of the object a shared_ptr is pointing to, occurs when the pointer no longer has any owners 
	std::shared_ptr<SoftwareConnection> getSoftwareConnection(size_t id);

	std::unordered_map<size_t, std::shared_ptr<SoftwareConnection>> _softwareConnections;
	std::mutex _softwareConnectionsMutex;
		
	ghoul::io::TcpSocketServer _socketServer;
	std::thread _serverThread;
    std::atomic_bool _shouldStopServerThread = false;
	std::thread _eventLoopThread;
    std::atomic_bool _shouldStopEventThread = false;
	

	const int _port;

    // Message handlers
	PointDataMessageHandler _pointDataMessageHandler;

	InterruptibleConcurrentQueue<IncomingMessage> _incomingMessages;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SOFTWAREINTEGRATION___NETWORKENGINE___H__
