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

#include <modules/softwareintegration/network/clientnetworkengine.h>

#include <modules/softwareintegration/network/softwareconnection.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/scene/scene.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/engine/syncengine.h>
#include <openspace/engine/windowdelegate.h>
#include <ghoul/logging/logmanager.h>

#include <openspace/engine/openspaceengine.h>

namespace {
	constexpr const char* _loggerCat = "SoftwareIntegration_ClientNetworkEngine";
} // namespace

namespace openspace {

void ClientNetworkEngine::start() {
	BaseNetworkEngine::start();
}

void ClientNetworkEngine::stop() {
	BaseNetworkEngine::stop();

	if (_eventLoopThread.joinable()) {
		_eventLoopThread.join();
	}
}

void ClientNetworkEngine::update() {
	BaseNetworkEngine::update();
}

void ClientNetworkEngine::eventLoop() {
	bool shouldShowMessage = true;
	bool shouldShowMessage1 = true;
	
	while (!_shouldStop) {
		// if(shouldShowMessage1) {
		// 	LWARNING(fmt::format("On CLIENT"));
		// 	shouldShowMessage1 = false;
		// }

		// if (shouldShowMessage && _message.data().content.size() > 0 ) {
		// 	//  .type == SoftwareConnection::Message::Type::Color
		// 	shouldShowMessage = false;
		// 	std::string content = { std::begin(_message.data().content), std::end(_message.data().content) };
		// 	LWARNING(fmt::format("Connected with {}", content));
		// }
	}
}

} // namespace openspace
