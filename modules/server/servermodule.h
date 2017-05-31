/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <ghoul/io/socket/tcpsocketserver.h>
#include <ghoul/misc/templatefactory.h>

#include <deque>
#include <memory>
#include <thread>
#include <mutex>
#include <cstdint>
#include <map>

#include <ext/json/json.hpp>

namespace openspace {

class Connection;

class Topic {
public:
    virtual ~Topic();
    initialize(Connection* connection, size_t topicId)
    void handleJson(nlohmann::json json);
};

struct Connection {
public:
    Connection(std::shared_ptr<ghoul::io::Socket> s, std::thread t);

    void handleMessage(std::string message);
    void sendMessage(const std::string& message);
    void handleJson(nlohmann::json json);
    void sendJson(const nlohmann::json& json);

    ghoul::TemplateFactory<Topic> _topicFactory;
    std::map<size_t, std::unique_ptr<Topic>> _topics;
    std::shared_ptr<ghoul::io::Socket> socket;
    std::thread thread;
    bool active;
};
   
struct Message {
    Connection* conneciton;
    std::string messageString;
};



class ServerModule : public OpenSpaceModule {
public:
    ServerModule();
    virtual ~ServerModule();
protected:
    void internalInitialize() override;
private:
    void handleConnection(Connection* socket);
    void cleanUpFinishedThreads();
    void consumeMessages();
    void disconnectAll();
    void preSync();

    std::mutex _messageQueueMutex;
    std::deque<Message> _messageQueue;

    std::vector<std::unique_ptr<Connection>> _connections;
    std::vector<std::unique_ptr<ghoul::io::SocketServer>> _servers;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SERVER___SERVERMODULE___H__
