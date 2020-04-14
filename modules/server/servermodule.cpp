/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#include <modules/server/servermodule.h>

#include <modules/server/include/serverinterface.h>
#include <modules/server/include/connection.h>
#include <modules/server/include/topics/topic.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <ghoul/fmt.h>
#include <ghoul/io/socket/socket.h>
#include <ghoul/io/socket/tcpsocketserver.h>
#include <ghoul/io/socket/websocket.h>
#include <ghoul/io/socket/websocketserver.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/misc/templatefactory.h>

namespace {
    constexpr const char* KeyInterfaces = "Interfaces";
} // namespace

namespace openspace {

ServerModule::ServerModule()
    : OpenSpaceModule(ServerModule::Name)
    , _interfaceOwner({"Interfaces", "Interfaces", "Server Interfaces"})
{
    addPropertySubOwner(_interfaceOwner);
}

ServerModule::~ServerModule() {
    disconnectAll();
    cleanUpFinishedThreads();
}

ServerInterface* ServerModule::serverInterfaceByIdentifier(const std::string& identifier)
{
    const auto si = std::find_if(
        _interfaces.begin(),
        _interfaces.end(),
        [identifier](std::unique_ptr<ServerInterface>& i) {
            return i->identifier() == identifier;
        }
    );
    if (si == _interfaces.end()) {
        return nullptr;
    }
    return si->get();
}

void ServerModule::internalInitialize(const ghoul::Dictionary& configuration) {
    global::callback::preSync.emplace_back([this]() {
        ZoneScopedN("ServerModule")

        preSync();
    });

    if (!configuration.hasValue<ghoul::Dictionary>(KeyInterfaces)) {
        return;
    }
    ghoul::Dictionary interfaces = configuration.value<ghoul::Dictionary>(KeyInterfaces);

    for (const std::string& key : interfaces.keys()) {
        ghoul::Dictionary interfaceDictionary = interfaces.value<ghoul::Dictionary>(key);

        // @TODO (abock, 2019-09-17);  This is a hack to make the parsing of the
        // openspace.cfg file not corrupt the heap and cause a potential crash at shutdown
        // (see ticket https://github.com/OpenSpace/OpenSpace/issues/982)
        // The AllowAddresses are specified externally and are injected here
        interfaceDictionary.setValue(
            "AllowAddresses",
            configuration.value<ghoul::Dictionary>("AllowAddresses")
        );

        std::unique_ptr<ServerInterface> serverInterface =
            ServerInterface::createFromDictionary(interfaceDictionary);


        if (global::windowDelegate.isMaster()) {
            serverInterface->initialize();
        }

        _interfaceOwner.addPropertySubOwner(serverInterface.get());

        if (serverInterface) {
            _interfaces.push_back(std::move(serverInterface));
        }
    }
}

void ServerModule::preSync() {
    if (!global::windowDelegate.isMaster()) {
        return;
    }

    // Set up new connections.
    for (std::unique_ptr<ServerInterface>& serverInterface : _interfaces) {
        if (!serverInterface->isEnabled()) {
            continue;
        }

        ghoul::io::SocketServer* socketServer = serverInterface->server();

        if (!socketServer) {
            continue;
        }

        std::unique_ptr<ghoul::io::Socket> socket;
        while ((socket = socketServer->nextPendingSocket())) {
            std::string address = socket->address();
            if (serverInterface->clientIsBlocked(address)) {
                // Drop connection if the address is blocked.
                continue;
            }
            socket->startStreams();
            std::shared_ptr<Connection> connection = std::make_shared<Connection>(
                std::move(socket),
                address,
                false,
                serverInterface->password()
            );
            connection->setThread(std::thread(
                [this, connection] () { handleConnection(connection); }
            ));
            if (serverInterface->clientHasAccessWithoutPassword(address)) {
                connection->setAuthorized(true);
            }
            _connections.push_back({ std::move(connection), false });
        }
    }

    // Consume all messages put into the message queue by the socket threads.
    consumeMessages();

    // Join threads for sockets that disconnected.
    cleanUpFinishedThreads();
}

void ServerModule::cleanUpFinishedThreads() {
    ZoneScoped

    for (ConnectionData& connectionData : _connections) {
        Connection& connection = *connectionData.connection;
        if (!connection.socket() || !connection.socket()->isConnected()) {
            if (connection.thread().joinable()) {
                connection.thread().join();
                connectionData.isMarkedForRemoval = true;
            }
        }
    }
    _connections.erase(std::remove_if(
        _connections.begin(),
        _connections.end(),
        [](const ConnectionData& connectionData) {
            return connectionData.isMarkedForRemoval;
        }
    ), _connections.end());
}

void ServerModule::disconnectAll() {
    ZoneScoped

    for (std::unique_ptr<ServerInterface>& serverInterface : _interfaces) {
        if (global::windowDelegate.isMaster()) {
            serverInterface->deinitialize();
        }
    }

    for (ConnectionData& connectionData : _connections) {
        Connection& connection = *connectionData.connection;
        if (connection.socket() && connection.socket()->isConnected()) {
            connection.socket()->disconnect(
                static_cast<int>(ghoul::io::WebSocket::ClosingReason::ClosingAll)
            );
        }
    }
}

void ServerModule::handleConnection(std::shared_ptr<Connection> connection) {
    ZoneScoped

    std::string messageString;
    while (connection->socket()->getMessage(messageString)) {
        std::lock_guard<std::mutex> lock(_messageQueueMutex);
        _messageQueue.push_back({ connection, std::move(messageString) });
    }
}

void ServerModule::consumeMessages() {
    ZoneScoped

    std::lock_guard<std::mutex> lock(_messageQueueMutex);
    while (!_messageQueue.empty()) {
        const Message& m = _messageQueue.front();
        if (std::shared_ptr<Connection> c = m.connection.lock()) {
            c->handleMessage(m.messageString);
        }
        _messageQueue.pop_front();
    }
}

} // namespace openspace
