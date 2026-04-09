/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <openspace/topic/server.h>

#include <modules/globebrowsing/globebrowsingmodule.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/documentation/documentation.h>
#include <openspace/topic/connection.h>
#include <openspace/topic/serverinterface.h>
#include <openspace/topic/topics/authorizationtopic.h>
#include <openspace/topic/topics/actionkeybindtopic.h>
#include <openspace/topic/topics/camerapathtopic.h>
#include <openspace/topic/topics/cameratopic.h>
#include <openspace/topic/topics/documentationtopic.h>
#include <openspace/topic/topics/downloadeventtopic.h>
#include <openspace/topic/topics/enginemodetopic.h>
#include <openspace/topic/topics/errorlogtopic.h>
#include <openspace/topic/topics/eventtopic.h>
#include <openspace/topic/topics/flightcontrollertopic.h>
#include <openspace/topic/topics/getpropertytopic.h>
#include <openspace/topic/topics/luascripttopic.h>
#include <openspace/topic/topics/missiontopic.h>
#include <openspace/topic/topics/profiletopic.h>
#include <openspace/topic/topics/propertytreetopic.h>
#include <openspace/topic/topics/sessionrecordingtopic.h>
#include <openspace/topic/topics/setpropertytopic.h>
#include <openspace/topic/topics/subscriptiontopic.h>
#include <openspace/topic/topics/timetopic.h>
#include <openspace/topic/topics/topic.h>
#include <openspace/topic/topics/triggerpropertytopic.h>
#include <openspace/topic/topics/versiontopic.h>
#include <openspace/util/factorymanager.h>
#include <ghoul/format.h>
#include <ghoul/io/socket/socket.h>
#include <ghoul/io/socket/socketserver.h>
#include <ghoul/io/socket/websocket.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/misc/templatefactory.h>
#include <algorithm>
#include <optional>
#include <thread>

namespace {

    // Settings for controlling socket connection (WebSocket or TcpSocket).
    // This basically acts as a whitelist for the specified connections.
    struct [[codegen::Dictionary(Server)]] Parameters {

        // The interfaces that are allowed to connect.
        std::optional<std::vector<ghoul::Dictionary>> interfaces
            [[codegen::reference("core_serverinterface")]];

        // The IP addresses that are allowed to connect.
        std::optional<std::vector<std::string>> allowAddresses;
    };
#include "server_codegen.cpp"
} // namespace

namespace openspace {

Documentation Server::Documentation() {
    return codegen::doc<Parameters>("core_server");
}

Server::Server()
    : PropertyOwner({ "Server", "Server" })
    , _interfaceOwner({ "Interfaces", "Interfaces", "Server Interfaces" })
{
    addPropertySubOwner(_interfaceOwner);

    global::callback::preSync->emplace_back([this]() {
        using K = CallbackHandle;
        using V = CallbackFunction;
        for (const std::pair<K, V>& it : _preSyncCallbacks) {
            it.second();
        }
    });
}

Server::~Server() {
    disconnectAll();
    cleanUpFinishedThreads();
}

ServerInterface* Server::serverInterfaceByIdentifier(const std::string& identifier)
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

void Server::initialize(const ghoul::Dictionary& configuration) {

    ghoul::TemplateFactory<Topic>* fTopic = FactoryManager::ref().factory<Topic>();

    // Add the topics to the topic factory
    fTopic->registerClass<ActionKeybindTopic>("actionsKeybinds");
    fTopic->registerClass<AuthorizationTopic>("authorize");
    fTopic->registerClass<CameraTopic>("camera");
    fTopic->registerClass<CameraPathTopic>("cameraPath");
    fTopic->registerClass<DocumentationTopic>("documentation");
    fTopic->registerClass<DownloadEventTopic>("downloadEvent");
    fTopic->registerClass<EngineModeTopic>("engineMode");
    fTopic->registerClass<ErrorLogTopic>("errorLog");
    fTopic->registerClass<EventTopic>("event");
    fTopic->registerClass<FlightControllerTopic>("flightcontroller");
    fTopic->registerClass<GetPropertyTopic>("get");
    fTopic->registerClass<LuaScriptTopic>("luascript");
    fTopic->registerClass<MissionTopic>("missions");
    fTopic->registerClass<ProfileTopic>("profile");
    fTopic->registerClass<PropertyTreeTopic>("propertyTree");
    fTopic->registerClass<SessionRecordingTopic>("sessionRecording");
    fTopic->registerClass<SetPropertyTopic>("set");
    fTopic->registerClass<SubscriptionTopic>("subscribe");
    fTopic->registerClass<TimeTopic>("time");
    fTopic->registerClass<TriggerPropertyTopic>("trigger");
    fTopic->registerClass<VersionTopic>("version");

    const Parameters p = codegen::bake<Parameters>(configuration);
    if (!p.interfaces.has_value()) {
        return;
    }

    for (const ghoul::Dictionary interface : p.interfaces.value()) {
        std::unique_ptr<ServerInterface> serverInterface =
            ServerInterface::createFromDictionary(interface);

        serverInterface->initialize();

        _interfaceOwner.addPropertySubOwner(serverInterface.get());

        if (serverInterface) {
            _interfaces.push_back(std::move(serverInterface));
        }
    }
}

void Server::preSync() {
    // Set up new connections
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
            const std::string address = socket->address();
            if (serverInterface->clientIsBlocked(address)) {
                // Drop connection if the address is blocked
                continue;
            }
            socket->startStreams();
            auto connection = std::make_shared<Connection>(
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

    // Consume all messages put into the message queue by the socket threads
    consumeMessages();

    // Join threads for sockets that disconnected
    cleanUpFinishedThreads();
}

void Server::cleanUpFinishedThreads() {
    ZoneScoped;

    for (ConnectionData& connectionData : _connections) {
        Connection& connection = *connectionData.connection;
        if (!connection.socket() || !connection.socket()->isConnected()) {
            if (connection.thread().joinable()) {
                connection.thread().join();
                connectionData.isMarkedForRemoval = true;
            }
        }
    }

    if (!_connections.empty()) {
        _connections.erase(
            std::remove_if(
                _connections.begin(),
                _connections.end(),
                [](const ConnectionData& connectionData) {
                    return connectionData.isMarkedForRemoval;
                }
            ),
            _connections.end()
        );
    }
}

void Server::disconnectAll() {
    ZoneScoped;

    for (std::unique_ptr<ServerInterface>& serverInterface : _interfaces) {
        serverInterface->deinitialize();
    }

    for (const ConnectionData& connectionData : _connections) {
        Connection& connection = *connectionData.connection;
        if (connection.socket() && connection.socket()->isConnected()) {
            connection.socket()->disconnect(
                static_cast<int>(ghoul::io::WebSocket::ClosingReason::ClosingAll)
            );
        }
    }
}

void Server::handleConnection(const std::shared_ptr<Connection>& connection) {
    ZoneScoped;

    std::string messageString;
    messageString.reserve(256);
    while (connection->socket()->getMessage(messageString)) {
        const std::unique_lock lock(_messageQueueMutex);
        _messageQueue.push_back({ connection, messageString });
    }
}

void Server::consumeMessages() {
    ZoneScoped;

    const std::unique_lock lock(_messageQueueMutex);
    while (!_messageQueue.empty()) {
        const Message& m = _messageQueue.front();
        if (const std::shared_ptr<Connection>& c = m.connection.lock()) {
            c->handleMessage(m.messageString);
        }
        _messageQueue.pop_front();
    }
}

Server::CallbackHandle Server::addPreSyncCallback(CallbackFunction cb) {
    const CallbackHandle handle = _nextCallbackHandle++;
    _preSyncCallbacks.emplace_back(handle, std::move(cb));
    return handle;
}

void Server::removePreSyncCallback(CallbackHandle handle) {
    const auto it = std::find_if(
        _preSyncCallbacks.begin(),
        _preSyncCallbacks.end(),
        [handle](const std::pair<CallbackHandle, CallbackFunction>& cb) {
            return cb.first == handle;
        }
    );

    ghoul_assert(
        it != _preSyncCallbacks.end(),
        "handle must be a valid callback handle"
    );

    _preSyncCallbacks.erase(it);
}

void Server::passDataToTopic(const std::string& topicType,
                                   const nlohmann::json& jsonData)
{
    for (const ConnectionData& connectionData : _connections) {
        if (Topic* topic = connectionData.connection->findTopicByType(topicType)) {
            topic->handleJson(jsonData);
        }
    }
}

std::vector<std::shared_ptr<Connection>> Server::connections() {
    std::vector<std::shared_ptr<Connection>> connections;

    for (const ConnectionData& connectionData : _connections) {
        connections.push_back(connectionData.connection);
    }
    return connections;
}

} // namespace openspace
