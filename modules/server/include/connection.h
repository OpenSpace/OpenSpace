/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

class Connection {
public:
    Connection(std::unique_ptr<ghoul::io::Socket> s, std::string address);

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
    bool _requireAuthorization;
    bool _isAuthorized = false;
    std::map<TopicId, std::string> _messageQueue;
    std::map<TopicId, std::chrono::system_clock::time_point> _sentMessages;

    bool isWhitelisted() const;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SERVER___CONNECTION___H__
