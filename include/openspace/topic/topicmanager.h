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

#ifndef __OPENSPACE_CORE___TOPICMANAGER___H__
#define __OPENSPACE_CORE___TOPICMANAGER___H__

#include <openspace/properties/propertyowner.h>

#include <openspace/topic/serverinterface.h>

#include <deque>
#include <functional>
#include <memory>
#include <mutex>
#include <utility>

namespace openspace {

constexpr int SOCKET_API_VERSION_MAJOR = 0;
constexpr int SOCKET_API_VERSION_MINOR = 1;
constexpr int SOCKET_API_VERSION_PATCH = 0;

class Connection;

class TopicManager : public PropertyOwner {
public:
    using CallbackHandle = int;
    using CallbackFunction = std::function<void()>;

    TopicManager();
    ~TopicManager() override;

    void initialize(const ghoul::Dictionary& configuration);

    void preSync();

    ServerInterface* serverInterfaceByIdentifier(const std::string& identifier);

    CallbackHandle addPreSyncCallback(CallbackFunction cb);
    void removePreSyncCallback(CallbackHandle handle);

    void passDataToTopic(const std::string& topic, const nlohmann::json& jsonData);
    std::vector<std::shared_ptr<Connection>> connections();

    static openspace::Documentation Documentation();

private:
    struct Message {
        std::weak_ptr<Connection> connection;
        std::string messageString;
    };

    struct ConnectionData {
        std::shared_ptr<Connection> connection;
        bool isMarkedForRemoval = false;
    };

    void handleConnection(const std::shared_ptr<Connection>& connection);
    void cleanUpFinishedThreads();
    void consumeMessages();
    void disconnectAll();

    std::mutex _messageQueueMutex;
    std::deque<Message> _messageQueue;

    std::vector<ConnectionData> _connections;
    std::vector<std::unique_ptr<ServerInterface>> _interfaces;
    PropertyOwner _interfaceOwner;

    /// Callbacks for triggering topic
    int _nextCallbackHandle = 0;
    std::vector<std::pair<CallbackHandle, CallbackFunction>> _preSyncCallbacks;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___TOPICMANAGER___H__
