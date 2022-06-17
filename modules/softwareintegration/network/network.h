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
#include <modules/softwareintegration/utils.h>
#include <ghoul/io/socket/tcpsocketserver.h>
#include <modules/softwareintegration/interruptibleconcurrentqueue.h>

#include <functional>
#include <unordered_map>

namespace openspace::softwareintegration::network {

class SoftwareConnectionLostError : public ghoul::RuntimeError {
public:
    explicit SoftwareConnectionLostError(const std::string& msg);
};

struct IncomingMessage {
	std::weak_ptr<SoftwareConnection> connection;
	softwareintegration::simp::MessageType type{ softwareintegration::simp::MessageType::Unknown };
	std::vector<char> content{};
	std::string rawMessageType{""};
};

struct NetworkState {
	ghoul::io::TcpSocketServer server;

	std::thread serverThread;
	std::thread eventLoopThread;

	std::unordered_map<size_t, std::shared_ptr<SoftwareConnection>> softwareConnections{};
	std::mutex softwareConnectionsMutex{};

	std::atomic_bool shouldStopThreads{ false };

	InterruptibleConcurrentQueue<IncomingMessage> incomingMessages{};
};

std::shared_ptr<NetworkState> serve(const int port = 4700);

void stopServer(std::shared_ptr<NetworkState> networkState);

} // namespace openspace::softwareintegration::network

#endif // __OPENSPACE_MODULE_SOFTWAREINTEGRATION___NETWORKENGINE___H__
