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

#include <openspace/topic/connection.h>

#include <openspace/engine/configuration.h>
#include <openspace/engine/globals.h>
#include <openspace/util/factorymanager.h>
#include <openspace/topic/topics/topic.h>
#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/io/socket/socket.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/misc/templatefactory.h>

namespace {
    constexpr std::string_view _loggerCat = "ServerModule: Connection";

    constexpr std::string_view MessageKeyType = "type";
    constexpr std::string_view MessageKeyPayload = "payload";
    constexpr std::string_view MessageKeyTopic = "topic";
} // namespace

namespace openspace {

Connection::Connection(std::unique_ptr<ghoul::io::Socket> s, std::string address,
                       bool authorized, const std::string& password)
    : _socket(std::move(s))
    , _address(std::move(address))
    , _isAuthorized(authorized)
    , _password(password)
{
    ghoul_assert(_socket, "Socket must not be nullptr");
}

void Connection::handleMessage(const std::string& message) {
    ZoneScoped;

    try {
        const nlohmann::json j = nlohmann::json::parse(message.c_str());
        try {
            handleJson(j);
        }
        catch (const std::domain_error& e) {
            LERROR(std::format("JSON handling error from: {}. {}", message, e.what()));
        }
    }
    catch (const std::out_of_range& e) {
        LERROR(std::format("JSON handling error from: {}. {}", message, e.what()));
    }
    catch (const std::exception& e) {
        LERROR(e.what());
    }
    catch (...) {
        if (!isAuthorized()) {
            _socket->disconnect();
            LERROR(std::format(
                "Could not parse JSON '{}'. Connection is unauthorized. Disconnecting",
                message
            ));
            return;
        }

        std::string sanitizedString = message;
        std::transform(
            message.begin(),
            message.end(),
            sanitizedString.begin(),
            [](wchar_t c) {
                return std::isprint(c, std::locale("")) ? char(c) : ' ';
            }
        );
        LERROR(std::format("Could not parse JSON '{}'", sanitizedString));
    }
}

void Connection::handleJson(const nlohmann::json& json) {
    ZoneScoped;

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

    // The topic id may be an already discussed topic, or a new one
    const TopicId topicId = topicJson->get<TopicId>();
    auto topicIt = _topics.find(topicId);

    if (topicIt == _topics.end()) {
        ZoneScopedN("New Topic");

        // The topic id is not registered: Initialize a new topic
        auto typeJson = json.find(MessageKeyType);
        if (typeJson == json.end() || !typeJson->is_string()) {
            LERROR("Type must be specified as a string when a new topic is initialized");
            return;
        }
        const std::string type = typeJson->get<std::string>();
        ZoneText(type.c_str(), type.size());

        if (!isAuthorized() && (type != "authorize")) {
            LERROR("Connection is not authorized");
            return;
        }

        ghoul::TemplateFactory<Topic>* fTopic = FactoryManager::ref().factory<Topic>();

        std::unique_ptr<Topic> topic = std::unique_ptr<Topic>(fTopic->create(type));
        topic->initialize(shared_from_this(), topicId);
        topic->handleJson(*payloadJson);
        if (!topic->isDone()) {
            _topics.emplace(topicId, std::move(topic));
        }
    }
    else {
        ZoneScopedN("Existing Topic");

        if (!isAuthorized()) {
            LERROR("Connection is not authorized");
            return;
        }

        // Dispatch the message to the existing topic
        Topic& topic = *topicIt->second;
        topic.handleJson(*payloadJson);
        if (topic.isDone()) {
            _topics.erase(topicIt);
        }
    }
}

void Connection::sendMessage(const std::string& message) {
    ZoneScoped;

    const std::unique_lock lock(_mutex);
    _socket->putMessage(message);
}

void Connection::sendJson(const nlohmann::json& json) {
    ZoneScoped;

    sendMessage(json.dump());
}

bool Connection::isAuthorized() const {
    return _isAuthorized;
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

const std::string& Connection::password() const {
    return _password;
}

Topic* Connection::findTopicByType(const std::string& type) {
    const auto it = std::find_if(
        _topics.begin(),
        _topics.end(),
        [&type](const auto& pair) {
            return pair.second->type() == type;
        });

    if (it == _topics.end()) {
        return nullptr;
    }

    return it->second.get();
}

} // namespace openspace
