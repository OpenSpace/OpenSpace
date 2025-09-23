/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef __OPENSPACE_MODULE_SERVER___SERVERMODULE___H__
#define __OPENSPACE_MODULE_SERVER___SERVERMODULE___H__

#include <openspace/util/openspacemodule.h>

#include <modules/server/include/serverinterface.h>

#include <deque>
#include <memory>
#include <mutex>

namespace openspace {

constexpr int SOCKET_API_VERSION_MAJOR = 0;
constexpr int SOCKET_API_VERSION_MINOR = 1;
constexpr int SOCKET_API_VERSION_PATCH = 0;

class Connection;

struct Message {
    std::weak_ptr<Connection> connection;
    std::string messageString;
};

class ServerModule : public OpenSpaceModule {
public:
    static constexpr const char* Name = "Server";
    using CallbackHandle = int;
    using CallbackFunction = std::function<void()>;

    ServerModule();
    virtual ~ServerModule() override;

    ServerInterface* serverInterfaceByIdentifier(const std::string& identifier);

    int skyBrowserUpdateTime() const;

    CallbackHandle addPreSyncCallback(CallbackFunction cb);
    void removePreSyncCallback(CallbackHandle handle);

    static documentation::Documentation Documentation();

protected:
    void internalInitialize(const ghoul::Dictionary& configuration) override;

private:
    struct ConnectionData {
        std::shared_ptr<Connection> connection;
        bool isMarkedForRemoval = false;
    };

    void handleConnection(const std::shared_ptr<Connection>& connection);
    void cleanUpFinishedThreads();
    void consumeMessages();
    void disconnectAll();
    void preSync();

    std::mutex _messageQueueMutex;
    std::deque<Message> _messageQueue;

    std::vector<ConnectionData> _connections;
    std::vector<std::unique_ptr<ServerInterface>> _interfaces;
    properties::PropertyOwner _interfaceOwner;
    int _skyBrowserUpdateTime = 100;

    // Callbacks for tiggering topic
    int _nextCallbackHandle = 0;
    std::vector<std::pair<CallbackHandle, CallbackFunction>> _preSyncCallbacks;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SERVER___SERVERMODULE___H__
