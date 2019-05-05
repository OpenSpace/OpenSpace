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

#include <modules/server/include/connection.h>

#include <modules/server/include/topics/authorizationtopic.h>
#include <modules/server/include/topics/bouncetopic.h>
#include <modules/server/include/topics/getpropertytopic.h>
#include <modules/server/include/topics/luascripttopic.h>
#include <modules/server/include/topics/setpropertytopic.h>
#include <modules/server/include/topics/subscriptiontopic.h>
#include <modules/server/include/topics/timetopic.h>
#include <modules/server/include/topics/topic.h>
#include <modules/server/include/topics/triggerpropertytopic.h>
#include <openspace/engine/configuration.h>
#include <openspace/engine/globals.h>
#include <ghoul/io/socket/socket.h>
#include <ghoul/io/socket/tcpsocketserver.h>
#include <ghoul/io/socket/websocketserver.h>
#include <ghoul/logging/logmanager.h>
#include <fmt/format.h>

namespace {
    constexpr const char* _loggerCat = "ServerModule: Connection";

    constexpr const char* MessageKeyType = "type";
    constexpr const char* MessageKeyPayload = "payload";
    constexpr const char* MessageKeyTopic = "topic";

    constexpr const char* AuthenticationTopicKey = "authorize";
    constexpr const char* GetPropertyTopicKey = "get";
    constexpr const char* LuaScriptTopicKey = "luascript";
    constexpr const char* SetPropertyTopicKey = "set";
    constexpr const char* SubscriptionTopicKey = "subscribe";
    constexpr const char* TimeTopicKey = "time";
    constexpr const char* TriggerPropertyTopicKey = "trigger";
    constexpr const char* BounceTopicKey = "bounce";
} // namespace

namespace openspace {

Connection::Connection(std::unique_ptr<ghoul::io::Socket> s, std::string address)
    : _socket(std::move(s))
    , _address(std::move(address))
{
    ghoul_assert(_socket, "Socket must not be nullptr");

    _topicFactory.registerClass<AuthorizationTopic>(AuthenticationTopicKey);
    _topicFactory.registerClass<GetPropertyTopic>(GetPropertyTopicKey);
    _topicFactory.registerClass<LuaScriptTopic>(LuaScriptTopicKey);
    _topicFactory.registerClass<SetPropertyTopic>(SetPropertyTopicKey);
    _topicFactory.registerClass<SubscriptionTopic>(SubscriptionTopicKey);
    _topicFactory.registerClass<TimeTopic>(TimeTopicKey);
    _topicFactory.registerClass<TriggerPropertyTopic>(TriggerPropertyTopicKey);
    _topicFactory.registerClass<BounceTopic>(BounceTopicKey);

    // see if the default config for requiring auth (on) is overwritten
    _requireAuthorization = global::configuration.doesRequireSocketAuthentication;
}

void Connection::handleMessage(const std::string& message) {
    try {
        nlohmann::json j = nlohmann::json::parse(message.c_str());
        try {
            handleJson(j);
        }
        catch (...) {
            LERROR(fmt::format("JSON handling error from: {}", message));
        }
    } catch (...) {
        if (!isAuthorized()) {
            _socket->disconnect();
            LERROR(fmt::format(
                "Could not parse JSON: '{}'. Connection is unauthorized. Disconnecting.",
                message
            ));
            return;
        } else {
            LERROR(fmt::format("Could not parse JSON: '{}'", message));
        }
    }
}

void Connection::handleJson(const nlohmann::json& json) {
    auto topicJson = json.find(MessageKeyTopic);
    auto payloadJson = json.find(MessageKeyPayload);

    if (topicJson == json.end() || !topicJson->is_number_integer()) {
        LERROR("Topic must be an integer");
        return;
    }

    if (payloadJson == json.end() || !payloadJson->is_object()) {
        LERROR("Payload must be an object");
        return;
    }

    // The topic id may be an already discussed topic, or a new one.
    TopicId topicId = *topicJson;
    auto topicIt = _topics.find(topicId);

    if (topicIt == _topics.end()) {
        // The topic id is not registered: Initialize a new topic.
        auto typeJson = json.find(MessageKeyType);
        if (typeJson == json.end() || !typeJson->is_string()) {
            LERROR(fmt::format(
                "A type must be specified (`{}`) as a string when "
                "a new topic is initialized", MessageKeyType
            ));
            return;
        }
        std::string type = *typeJson;

        if (!isAuthorized() && type != AuthenticationTopicKey) {
            LERROR("Connection isn't authorized.");
            return;
        }

        std::unique_ptr<Topic> topic = _topicFactory.create(type);
        topic->initialize(this, topicId);
        topic->handleJson(*payloadJson);
        if (!topic->isDone()) {
            _topics.emplace(topicId, std::move(topic));
        }
    } else {
        if (!isAuthorized()) {
            LERROR("Connection isn't authorized.");
            return;
        }

        // Dispatch the message to the existing topic.
        Topic& topic = *topicIt->second;
        topic.handleJson(*payloadJson);
        if (topic.isDone()) {
            _topics.erase(topicIt);
        }
    }
}

void Connection::sendMessage(const std::string& message) {
    _socket->putMessage(message);
}

void Connection::sendJson(const nlohmann::json& json) {
    sendMessage(json.dump());
}

bool Connection::isAuthorized() const {
    // require either auth to be disabled or client to be authenticated
    return !_requireAuthorization || isWhitelisted() || _isAuthorized;
}

void Connection::setThread(std::thread&& thread) {
    _thread = std::move(thread);
}

std::thread& Connection::thread() {
    return _thread;
}

ghoul::io::Socket* Connection::socket() {
    return _socket.get();
}

void Connection::setAuthorized(bool status) {
    _isAuthorized = status;
}

bool Connection::isWhitelisted() const {
    const std::vector<std::string>& wl = global::configuration.clientAddressWhitelist;
    return std::find(wl.begin(), wl.end(), _address) != wl.end();
}

} // namespace openspace
