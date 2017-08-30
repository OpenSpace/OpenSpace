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

#include <modules/server/include/connection.h>
#include <modules/server/include/authorizationtopic.h>
#include <modules/server/include/getpropertytopic.h>
#include <modules/server/include/luascripttopic.h>
#include <modules/server/include/setpropertytopic.h>
#include <modules/server/include/subscriptiontopic.h>
#include <modules/server/include/timetopic.h>
#include <modules/server/include/triggerpropertytopic.h>

namespace {
const char* _loggerCat = "ServerModule: Connection";

const char* MessageKeyType = "type";
const char* MessageKeyPayload = "payload";
const char* MessageKeyTopic = "topic";

const char* AuthenticationTopicKey = "authorize";
const char* GetPropertyTopicKey = "get";
const char* LuaScriptTopicKey = "luascript";
const char* SetPropertyTopicKey = "set";
const char* SubscriptionTopicKey = "subscribe";
const char* TimeTopicKey = "time";
const char* TriggerPropertyTopicKey = "trigger";
const char* BounceTopicKey = "bounce";

const int ThrottleMessageWaitInMs = 100;
}

namespace openspace {

Connection::Connection(std::shared_ptr<ghoul::io::Socket> s, const std::string &address)
        : socket(s), active(true), _isAuthorized(false), _address(address) {
    _topicFactory.registerClass<AuthorizationTopic>(AuthenticationTopicKey);
    _topicFactory.registerClass<GetPropertyTopic>(GetPropertyTopicKey);
    _topicFactory.registerClass<LuaScriptTopic>(LuaScriptTopicKey);
    _topicFactory.registerClass<SetPropertyTopic>(SetPropertyTopicKey);
    _topicFactory.registerClass<SubscriptionTopic>(SubscriptionTopicKey);
    _topicFactory.registerClass<TimeTopic>(TimeTopicKey);
    _topicFactory.registerClass<TriggerPropertyTopic>(TriggerPropertyTopicKey);
    _topicFactory.registerClass<BounceTopic>(BounceTopicKey);

    // see if the default config for requiring auth (on) is overwritten
    const bool hasAuthenticationConfiguration = OsEng.configurationManager().hasKeyAndValue<bool>(
            ConfigurationManager::KeyRequireSocketAuthentication);
    if (hasAuthenticationConfiguration) {
        _requireAuthorization = OsEng.configurationManager().value<bool>(
                ConfigurationManager::KeyRequireSocketAuthentication);
    } else {
        _requireAuthorization = true;
    }
}

void Connection::handleMessage(std::string message) {
    try {
        nlohmann::json j = nlohmann::json::parse(message.c_str());
        try {
            handleJson(j);
        }
        catch (...) {
            LERROR("JSON handling error from: " + message);
        }
    } catch (...) {
        LERROR("Could not parse JSON: " + message);
    }
}

void Connection::handleJson(nlohmann::json j) {
    auto topicJson = j.find(MessageKeyTopic);
    auto payloadJson = j.find(MessageKeyPayload);

    if (topicJson == j.end() || !topicJson->is_number_integer()) {
        LERROR("Topic must be an integer");
        return;
    }

    if (payloadJson == j.end() || !payloadJson->is_object()) {
        LERROR("Payload must be an object");
        return;
    }

    // The topic id may be an already discussed topic, or a new one.
    TopicId topicId = *topicJson;
    auto topicIt = _topics.find(topicId);

    if (topicIt == _topics.end()) {
        // The topic id is not registered: Initialize a new topic.
        auto typeJson = j.find(MessageKeyType);
        if (typeJson == j.end() || !typeJson->is_string()) {
            LERROR(fmt::format("A type must be specified (`{}`) as a string when "
                               "a new topic is initialized", MessageKeyType));
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
        std::unique_ptr<Topic> &topic = topicIt->second;
        topic->handleJson(*payloadJson);
        if (topic->isDone()) {
            _topics.erase(topicIt);
        }
    }
}

void Connection::sendMessage(const std::string &message) {
    socket->putMessage(message);
}

void Connection::sendJson(const nlohmann::json &j) {
    sendMessage(j.dump());
}

bool Connection::isAuthorized() {
    // require either auth to be disabled or client to be authenticated
    return !_requireAuthorization || isWhitelisted() || _isAuthorized;
}

void Connection::refresh() {
    for (auto &entry : _refreshCalls) {
        // check if value has changed since last call, if it has -- send and update!
        nlohmann::json newValue = entry.method();
        if (newValue != entry.value) {
            entry.value = newValue;
            placeInMessageQueue(newValue, entry.topicId);
        }
    }

    flushQueue();
}

void Connection::addRefreshCall(std::function<nlohmann::json()> func, const TopicId topicId) {
    sendJson(func());
    _refreshCalls.push_back({std::move(func), topicId, nlohmann::json::object()});
}

void Connection::placeInMessageQueue(const nlohmann::json &j, const TopicId topic) {
    placeInMessageQueue(j.dump(), topic);
}

void Connection::placeInMessageQueue(const std::string &message, const TopicId topic) {
    // add or replace message - only one message per topic is allowed
    auto queueIt = _messageQueue.find(topic);
    if (queueIt == _messageQueue.end()) {
        _messageQueue.emplace(topic, message);
    }
    else {
        queueIt->second = message;
    }
}

void Connection::flushQueue() {
    const std::chrono::milliseconds requiredWait(ThrottleMessageWaitInMs);
    const auto now = std::chrono::system_clock::now();
    std::vector<TopicId> toRemove;

    for (auto &entry : _messageQueue) {
        auto sentIt = _sentMessages.find(entry.first);

        if (sentIt == _sentMessages.end() || (now - sentIt->second) > requiredWait) {
            sendMessage(std::string(entry.second));
            
            // store this message as sent and remove it from queue
            _sentMessages[entry.first] = now;
            toRemove.push_back(entry.first);
        }
    }

    for (auto &topic : toRemove) {
        _messageQueue.erase(topic);
    }
}

void Connection::setAuthorized(const bool status) {
    _isAuthorized = status;
}

bool Connection::isWhitelisted() {
    const bool hasWhitelist = OsEng.configurationManager().hasKeyAndValue<std::string>(
        ConfigurationManager::KeyServerClientAddressWhitelist);
    if (hasWhitelist) {
        // get whitelist from config
        auto whitelist = OsEng.configurationManager().value<std::string>(
            ConfigurationManager::KeyServerClientAddressWhitelist);
        // search and compare
        auto search = whitelist.find(_address);
        return search != std::string::npos;
    }

    // no whitelist found!
    return false;
}

} // namespace openspace
