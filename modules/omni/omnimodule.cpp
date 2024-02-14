/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <modules/omni/omnimodule.h>

#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/globals.h>
#include <ghoul/io/socket/websocket.h>
#include <ghoul/io/socket/tcpsocket.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>

namespace {
    constexpr std::string_view _loggerCat = "Omni";
}

namespace openspace {

OmniModule::OmniModule() : OpenSpaceModule(OmniModule::Name) {
    _server.clear_access_channels(websocketpp::log::alevel::all);
    _server.set_access_channels(websocketpp::log::alevel::connect);
    _server.set_access_channels(websocketpp::log::alevel::disconnect);
    _server.set_access_channels(websocketpp::log::alevel::app);
}

OmniModule::~OmniModule() {
    if (_wSocket->isConnected()) {
        _wSocket->disconnect(
            static_cast<int>(ghoul::io::WebSocket::ClosingReason::ClosingAll)
        );
    }
    if (_thread.joinable()) {
        _thread.join();
    }
}

void OmniModule::internalInitialize(const ghoul::Dictionary& config) {
    using namespace ghoul::io;
    //global::callback::preSync->emplace_back([this]() {
    //    ZoneScopedN("OmniModule");

    //    preSync();
    //});
    
    std::unique_ptr<TcpSocket> tcpSocket = std::make_unique<TcpSocket>("localhost", 5051);
    if (!tcpSocket) {
        LERROR("No socket connection to omni");
        return;
    }
    tcpSocket->connect();

    if (tcpSocket->isConnected()) {
        LERROR("TCP Connected");
    }
    else if (tcpSocket->isConnecting()) {
        LERROR("TCP Connecting");
    }
    else {
        LERROR("TCP Not connected");
    }
    _wSocket = std::move(tcpSocket);
    //_wSocket = std::make_unique<WebSocket>(std::move(tcpSocket), _server);
    //_wSocket->startStreams();

    //if (_wSocket->isConnected()) {
    //    LERROR("Connected");
    //}
    //else if (_wSocket->isConnecting()) {
    //    LERROR("Connecting");
    //}
    //else {
    //    LERROR("Not connected");
    //}

    _thread = std::move(std::thread([this]() { handleConnection(); }));
}

void OmniModule::preSync() {
    LDEBUG("Presync");
}

void OmniModule::handleConnection() {
    std::string messageString;
    messageString.reserve(256);
    while (_wSocket->getMessage(messageString)) {
        LERROR(messageString);
    }
    //LERROR(messageString);
}

} // namespace openspace
