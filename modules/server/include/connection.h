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

#ifndef OPENSPACE_MODOULES_SERVER__CONNECTION_H
#define OPENSPACE_MODOULES_SERVER__CONNECTION_H

#include <memory>
#include <string>
#include <ghoul/io/socket/tcpsocketserver.h>
#include <ghoul/io/socket/websocketserver.h>
#include <ghoul/misc/templatefactory.h>
#include <ext/json/json.hpp>
#include <ghoul/logging/logmanager.h>
#include <fmt/format.h>
#include <include/openspace/engine/openspaceengine.h>

#include "topic.h"

namespace openspace {

using TopicId = size_t;

class Connection {
public:
    Connection(std::shared_ptr<ghoul::io::Socket> s, const std::string &address);

    void handleMessage(std::string message);
    void sendMessage(const std::string& message);
    void handleJson(nlohmann::json json);
    void sendJson(const nlohmann::json& json);
    void setAuthorized(const bool status);

    bool isAuthorized();
    
    
    std::shared_ptr<ghoul::io::Socket> socket();
    std::thread& thread();
    void setThread(std::thread&& thread);

private:
    ghoul::TemplateFactory<Topic> _topicFactory;
    std::map<TopicId, std::unique_ptr<Topic>> _topics;
    std::shared_ptr<ghoul::io::Socket> _socket;
    std::thread _thread;

    std::string _address;
    bool _requireAuthorization;
    bool _isAuthorized;
    std::map <TopicId, std::string> _messageQueue;
    std::map <TopicId, std::chrono::system_clock::time_point> _sentMessages;

    bool isWhitelisted();
};

}

#endif //OPENSPACE_MODOULES_SERVER__CONNECTION_H
