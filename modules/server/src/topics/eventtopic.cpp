/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <modules/server/include/topics/eventtopic.h>

#include <modules/server/include/connection.h>
#include <modules/server/include/jsonconverters.h>
#include <openspace/engine/globals.h>
#include <openspace/events/event.h>
#include <openspace/events/eventengine.h>
#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr std::string_view _loggerCat = "EventTopic";

    constexpr std::string_view StartSubscription = "start_subscription";
    constexpr std::string_view StopSubscription = "stop_subscription";
} // namespace

using nlohmann::json;

namespace openspace {

bool EventTopic::isDone() const {
    return !isSubscribed();
}

void EventTopic::handleJson(const nlohmann::json& json) {
    std::vector<std::string> events;

    auto eventJson = json.find("event");
    auto statusJson = json.find("status");

    if (eventJson == json.end()) {
        LERROR("Payload does not contain 'event' key");
        return;
    }

    if (statusJson == json.end() || !statusJson->is_string()) {
        LERROR("Status must be a string");
        return;
    }

    if (json.at("event").is_array()) {
        events = json.at("event").get<std::vector<std::string>>();
    }
    else {
        const std::string& event = json.at("event").get<std::string>();
        if (event == "*" || event == "all") {
            // Iterate over all event types and add them to list
            const uint8_t lastEvent = static_cast<uint8_t>(events::Event::Type::Last);

            for (uint8_t i = 0; i < lastEvent; i++) {
                auto type = static_cast<events::Event::Type>(i);
                events.emplace_back(events::toString(type));
            }
        }
        else {
            events.push_back(event);
        }
    }

    const std::string& status = json.at("status").get<std::string>();

    for (const std::string& event : events) {
        if (status == StartSubscription) {
            const events::Event::Type type = events::fromString(event);

            _subscribedEvents[type] = true;

            auto onCallback = [this, event](ghoul::Dictionary params) {
                // Include the fired event to the caller
                params.setValue("Event", event);
                _connection->sendJson(wrappedPayload(params));
            };

            global::eventEngine->registerEventTopic(_topicId, type, onCallback);
        }
        else if (status == StopSubscription) {
            const events::Event::Type type = events::fromString(event);
            _subscribedEvents.erase(type);
            global::eventEngine->unregisterEventTopic(_topicId, type);
        }
    }
}

bool EventTopic::isSubscribed() const {
    if (_subscribedEvents.empty()) {
        return false;
    }

    const bool hasActiveSubscription = std::any_of(
        _subscribedEvents.begin(),
        _subscribedEvents.end(),
        [](const std::pair<const events::Event::Type, bool>& subscription) {
            return subscription.second;
    });

    return hasActiveSubscription;
}

} // namespace openspace
