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
#include <modules/server/include/authenticationtopic.h>
#include <modules/server/include/getpropertytopic.h>
#include <modules/server/include/luascripttopic.h>
#include <modules/server/include/setpropertytopic.h>
#include <modules/server/include/subscriptiontopic.h>
#include <modules/server/include/triggerpropertytopic.h>

namespace {
const char* _loggerCat = "ServerModule: Connection";

const char* MessageKeyType = "type";
const char* MessageKeyPayload = "payload";
const char* MessageKeyTopic = "topic";
}

namespace openspace {

Connection::Connection(std::shared_ptr<ghoul::io::Socket> s)
        : socket(s), active(true), _isAuthenticated(false) {
    _topicFactory.registerClass<AuthenticationTopic>("authorize");
    _topicFactory.registerClass<GetPropertyTopic>("get");
    _topicFactory.registerClass<LuaScriptTopic>("luascript");
    _topicFactory.registerClass<SetPropertyTopic>("set");
    _topicFactory.registerClass<SubscriptionTopic>("subscribe");
    _topicFactory.registerClass<TriggerPropertyTopic>("trigger");
    _topicFactory.registerClass<BounceTopic>("bounce");

    // see if the default config for requiring auth (on) is overwritten
    const bool hasAuthenticationConfiguration = OsEng.configurationManager().hasKeyAndValue<bool>(
            ConfigurationManager::KeyRequireSocketAuthentication);
    if (hasAuthenticationConfiguration) {
        _requireAuthentication = OsEng.configurationManager().value<bool>(
                ConfigurationManager::KeyRequireSocketAuthentication);
    } else {
        _requireAuthentication = true;
    }
}

void Connection::handleMessage(std::string message) {
    // We want exceptions when debugging, but we don't want incoming messages
    // to cause crashes on Release.
#if (defined(NDEBUG) || !defined(DEBUG))
    nlohmann::json j = nlohmann::json::parse(message.c_str());
    handleJson(j);
#else
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
#endif
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
    size_t topicId = *topicJson;
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
        std::unique_ptr<Topic> topic = _topicFactory.create(type);
        topic->initialize(this, topicId);
        topic->handleJson(*payloadJson);
        if (!topic->isDone()) {
            _topics.emplace(topicId, std::move(topic));
        }
    } else {
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

bool Connection::needsToBeAuthenticated() {
    // require either auth to be disabled or client to be authenticated
    return !_requireAuthentication || _isAuthenticated;
}

void Connection::refresh() {
    for (auto &entry : _refreshCalls) {
        // check if value has changed since last call, if it has -- send and update!
        nlohmann::json newValue = entry.method();
        if (newValue != entry.value) {
            entry.value = newValue;
            sendJson(newValue);
        }
    }
}

void Connection::addRefreshCall(std::function<nlohmann::json()> func) {
//    struct MethodAndValue mav = { .method=std::move(func), .value=nlohmann::json };
    _refreshCalls.push_back({std::move(func), nlohmann::json::object()});
}

} // namespace openspace
