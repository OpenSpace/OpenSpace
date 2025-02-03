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

#ifndef __OPENSPACE_MODULE_SERVER___CONNECTION___H__
#define __OPENSPACE_MODULE_SERVER___CONNECTION___H__

#include <ghoul/misc/templatefactory.h>
#include <openspace/json.h>
#include <memory>
#include <string>
#include <thread>

namespace ghoul::io { class Socket; }

namespace openspace {

using TopicId = size_t;

class Topic;

// @TODO (abock, 2022-05-06) This is not really elegant as there is no need for a
// Connection to be held by a shared_ptr, but there was a problem with the LuaScriptTopic
// otherwise (issue #1940).
// The problem there is that the LuaScriptTopic is keeping a copy of the _connection in
// its lambda to return a script back to the caller. The script is only queued, so is
// executed a bit longer. If the UI gets reloaded in between the creation of the lambda
// and the execution, the _connection will be an invalid pointer and the program will
// crash. Making this a shared_ptr circumvents that problem my having the lamdba retain
// ownership of the _connection and keeping it alive until the message is sent. The
// message doesn't go anywhere since noone is listening, but it's better than a crash.
class Connection : public std::enable_shared_from_this<Connection> {
public:
    Connection(std::unique_ptr<ghoul::io::Socket> s, std::string address,
        bool authorized = false, const std::string& password = "");

    void handleMessage(const std::string& message);
    void sendMessage(const std::string& message);
    void handleJson(const nlohmann::json& json);
    void sendJson(const nlohmann::json& json);
    void setAuthorized(bool status);

    bool isAuthorized() const;

    ghoul::io::Socket* socket();
    std::thread& thread();
    void setThread(std::thread&& thread);

private:
    ghoul::TemplateFactory<Topic> _topicFactory;
    std::map<TopicId, std::unique_ptr<Topic>> _topics;
    std::unique_ptr<ghoul::io::Socket> _socket;
    std::thread _thread;

    std::string _address;
    bool _isAuthorized = false;
    std::map<TopicId, std::string> _messageQueue;
    std::map<TopicId, std::chrono::system_clock::time_point> _sentMessages;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SERVER___CONNECTION___H__
