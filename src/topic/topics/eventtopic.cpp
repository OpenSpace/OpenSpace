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

#include <openspace/topic/topics/eventtopic.h>

#include <openspace/documentation/schema.h>
#include <openspace/engine/globals.h>
#include <openspace/events/eventengine.h>
#include <openspace/topic/connection.h>
#include <openspace/topic/jsonconverters.h>
#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <algorithm>
#include <cstdint>
#include <string_view>
#include <utility>

using nlohmann::json;

namespace {
    constexpr std::string_view _loggerCat = "EventTopic";
} // namespace

namespace openspace {

void EventTopic::handleJson(const nlohmann::json& json) {
    std::vector<std::string> events;

    auto eventJson = json.find("eventType");
    auto statusJson = json.find("event");

    if (eventJson == json.end()) {
        LERROR("Payload does not contain 'event' key");
        return;
    }

    if (statusJson == json.end() || !statusJson->is_string()) {
        LERROR("Status must be a string");
        return;
    }

    if (json.at("eventType").is_array()) {
        events = json.at("eventType").get<std::vector<std::string>>();
    }
    else {
        const std::string& event = json.at("eventType").get<std::string>();
        if (event == "*" || event == "all") {
            // Iterate over all event types and add them to list
            const uint8_t lastEvent = static_cast<uint8_t>(Event::Type::Last);

            for (uint8_t i = 0; i < lastEvent; i++) {
                Event::Type type = static_cast<Event::Type>(i);
                events.emplace_back(toString(type));
            }
        }
        else {
            events.push_back(event);
        }
    }

    const std::string& status = json.at("event").get<std::string>();

    for (const std::string& event : events) {
        if (status == "start_subscription") {
            const Event::Type type = fromString(event);

            _subscribedEvents[type] = true;

            auto onCallback = [this, event](ghoul::Dictionary params) {
                // Include the fired event to the caller
                params.setValue("event", event);
                sendData(params);
            };

            global::eventEngine->registerEventTopic(_topicId, type, onCallback);
        }
        else if (status == "stop_subscription") {
            const Event::Type type = fromString(event);
            _subscribedEvents.erase(type);
            global::eventEngine->unregisterEventTopic(_topicId, type);
        }
    }
}

bool EventTopic::isDone() const {
    return !isSubscribed();
}

Schema EventTopic::Schema() {
    // @TODO (anden88 2026-04-10): We probably want to write the Event types in somewhere
    // else. Putting them here for now
    nlohmann::json schema = nlohmann::json::parse(R"(
        {
          "$defs": {
            "EventTypeOrWildcard": {
              "anyOf": [
                { "$ref": "#/$defs/EventType" },
                {
                  "type": "string",
                  "enum": ["*", "all"]
                }
              ]
            },
            "EventType": {
              "type": "string",
              "enum": [
                "ActionAdded",
                "ActionRemoved",
                "ApplicationShutdown",
                "AssetLoading",
                "CameraFocusTransition",
                "CameraMovedPosition",
                "CameraPathFinished",
                "CameraPathStarted",
                "Custom",
                "FocusNodeChanged",
                "GuiTreeUpdated",
                "InterpolationFinished",
                "MissionAdded",
                "MissionEventReached",
                "MissionRemoved",
                "ParallelConnection",
                "PlanetEclipsed",
                "PointSpacecraft",
                "ProfileLoadingFinished",
                "PropertyTreePruned",
                "PropertyTreeUpdated",
                "RenderableDisabled",
                "RenderableEnabled",
                "ScheduledScriptExecuted",
                "SessionRecordingPlayback",
                "TimeOfInterestReached"
              ]
            },
            "EventData": {
              "anyOf": [
                { "$ref": "#/$defs/ActionAddedEventData" },
                { "$ref": "#/$defs/ActionRemovedEventData" },
                { "$ref": "#/$defs/ApplicationShutdownEventData" },
                { "$ref": "#/$defs/AssetLoadingEventData" },
                { "$ref": "#/$defs/CameraFocusTransitionEventData" },
                { "$ref": "#/$defs/CameraMovedPositionEventData" },
                { "$ref": "#/$defs/CameraPathFinishedEventData" },
                { "$ref": "#/$defs/CameraPathStartedEventData" },
                { "$ref": "#/$defs/CustomEventData" },
                { "$ref": "#/$defs/FocusNodeChangedEventData" },
                { "$ref": "#/$defs/GuiTreeUpdatedEventData" },
                { "$ref": "#/$defs/InterpolationFinishedEventData" },
                { "$ref": "#/$defs/MissionAddedEventData" },
                { "$ref": "#/$defs/MissionEventReachedEventData" },
                { "$ref": "#/$defs/MissionRemovedEventData" },
                { "$ref": "#/$defs/ParallelConnectionEventData" },
                { "$ref": "#/$defs/PlanetEclipsedEventData" },
                { "$ref": "#/$defs/PointSpacecraftEventData" },
                { "$ref": "#/$defs/ProfileLoadingFinishedEventData" },
                { "$ref": "#/$defs/PropertyTreePrunedEventData" },
                { "$ref": "#/$defs/PropertyTreeUpdatedEventData" },
                { "$ref": "#/$defs/RenderableDisabledEventData" },
                { "$ref": "#/$defs/RenderableEnabledEventData" },
                { "$ref": "#/$defs/ScheduledScriptExecutedEventData" },
                { "$ref": "#/$defs/SessionRecordingPlaybackEventData" },
                { "$ref": "#/$defs/TimeOfInterestReachedEventData" }
              ]
            },
            "ActionAddedEventData": {
              "type": "object",
              "properties": {
                "event": { "const": "ActionAdded" },
                "Uri": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["event", "Uri"]
            },
            "ActionRemovedEventData": {
              "type": "object",
              "properties": {
                "event": { "const": "ActionRemoved" },
                "Uri": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["event", "Uri"]
            },
            "ApplicationShutdownEventData": {
              "type": "object",
              "properties": {
                "event": { "const": "ApplicationShutdown" },
                "State": {
                  "type": "string",
                  "enum": ["Started", "Aborted", "Finished"]
                }
              },
              "additionalProperties": false,
              "required": ["event", "State"]
            },
            "AssetLoadingEventData": {
              "type": "object",
              "properties": {
                "event": { "const": "AssetLoading" },
                "AssetPath": { "type": "string" },
                "State": {
                  "type": "string",
                  "enum": ["Loaded", "Loading", "Unloaded", "Error"]
                }
              },
              "additionalProperties": false,
              "required": ["event", "AssetPath", "State"]
            },
            "CameraFocusTransitionEventData": {
              "type": "object",
              "properties": {
                "event": { "const": "CameraFocusTransition" },
                "Node": { "type": "string" },
                "Transition": {
                  "type": "string",
                  "enum": ["Approaching", "Reaching", "Receding", "Exiting"]
                }
              },
              "additionalProperties": false,
              "required": ["event", "Node", "Transition"]
            },
            "CameraMovedPositionEventData": {
              "type": "object",
              "properties": {
                "event": { "const": "CameraMovedPosition" }
              },
              "additionalProperties": false,
              "required": ["event"]
            },
            "CameraPathFinishedEventData": {
              "type": "object",
              "properties": {
                "event": { "const": "CameraPathFinished" },
                "Origin": { "type": "string" },
                "Destination": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["event", "Origin", "Destination"]
            },
            "CameraPathStartedEventData": {
              "type": "object",
              "properties": {
                "event": { "const": "CameraPathStarted" },
                "Origin": { "type": "string" },
                "Destination": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["event", "Origin", "Destination"]
            },
            "CustomEventData": {
              "type": "object",
              "properties": {
                "event": { "const": "Custom" },
                "Subtype": { "type": "string" },
                "Payload": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["event", "Subtype", "Payload"]
            },
            "FocusNodeChangedEventData": {
              "type": "object",
              "properties": {
                "event": { "const": "FocusNodeChanged" },
                "OldNode": { "type": "string" },
                "NewNode": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["event", "OldNode", "NewNode"]
            },
            "GuiTreeUpdatedEventData": {
              "type": "object",
              "properties": {
                "event": { "const": "GuiTreeUpdated" }
              },
              "additionalProperties": false,
              "required": ["event"]
            },
            "InterpolationFinishedEventData": {
              "type": "object",
              "properties": {
                "event": { "const": "InterpolationFinished" },
                "Property": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["event", "Property"]
            },
            "MissionAddedEventData": {
              "type": "object",
              "properties": {
                "event": { "const": "MissionAdded" },
                "Identifier": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["event", "Identifier"]
            },
            "MissionEventReachedEventData": {
              "type": "object",
              "properties": {
                "event": { "const": "MissionEventReached" }
              },
              "additionalProperties": false,
              "required": ["event"]
            },
            "MissionRemovedEventData": {
              "type": "object",
              "properties": {
                "event": { "const": "MissionRemoved" },
                "Identifier": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["event", "Identifier"]
            },
            "ParallelConnectionEventData": {
              "type": "object",
              "properties": {
                "event": { "const": "ParallelConnection" },
                "State": {
                  "type": "string",
                  "enum": ["Established", "Lost", "HostshipGained", "HostshipLost"]
                }
              },
              "additionalProperties": false,
              "required": ["event", "State"]
            },
            "PlanetEclipsedEventData": {
              "type": "object",
              "properties": {
                "event": { "const": "PlanetEclipsed" },
                "Eclipsee": { "type": "string" },
                "Eclipser": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["event", "Eclipsee", "Eclipser"]
            },
            "PointSpacecraftEventData": {
              "type": "object",
              "properties": {
                "event": { "const": "PointSpacecraft" },
                "Ra": { "type": "number" },
                "Dec": { "type": "number" },
                "Duration": { "type": "number" }
              },
              "additionalProperties": false,
              "required": ["event", "Ra", "Dec", "Duration"]
            },
            "ProfileLoadingFinishedEventData": {
              "type": "object",
              "properties": {
                "event": { "const": "ProfileLoadingFinished" }
              },
              "additionalProperties": false,
              "required": ["event"]
            },
            "PropertyTreePrunedEventData": {
              "type": "object",
              "properties": {
                "event": { "const": "PropertyTreePruned" },
                "Uri": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["event", "Uri"]
            },
            "PropertyTreeUpdatedEventData": {
              "type": "object",
              "properties": {
                "event": { "const": "PropertyTreeUpdated" },
                "Uri": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["event", "Uri"]
            },
            "RenderableDisabledEventData": {
              "type": "object",
              "properties": {
                "event": { "const": "RenderableDisabled" },
                "Node": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["event", "Node"]
            },
            "RenderableEnabledEventData": {
              "type": "object",
              "properties": {
                "event": { "const": "RenderableEnabled" },
                "Node": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["event", "Node"]
            },
            "ScheduledScriptExecutedEventData": {
              "type": "object",
              "properties": {
                "event": { "const": "ScheduledScriptExecuted" },
                "script": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["event", "script"]
            },
            "SessionRecordingPlaybackEventData": {
              "type": "object",
              "properties": {
                "event": { "const": "SessionRecordingPlayback" },
                "State": {
                  "type": "string",
                  "enum": ["Started", "Paused", "Resumed", "Finished"]
                }
              },
              "additionalProperties": false,
              "required": ["event", "State"]
            },
            "TimeOfInterestReachedEventData": {
              "type": "object",
              "properties": {
                "event": { "const": "TimeOfInterestReached" }
              },
              "additionalProperties": false,
              "required": ["event"]
            }
          },
          "title": "EventTopic",
          "type": "object",
          "properties": {
            "topicId": { "const": "event" },
            "topicPayload": {
              "type": "object",
              "anyOf": [
                {
                  "type": "object",
                  "properties": {
                    "event": { "const": "start_subscription" },
                    "eventType": {
                      "anyOf": [
                        { "$ref": "#/$defs/EventTypeOrWildcard" },
                        {
                          "type": "array",
                          "items": { "$ref": "#/$defs/EventType" }
                        }
                      ]
                    }
                  },
                  "additionalProperties": false,
                  "required": ["event", "eventType"]
                },
                {
                  "type": "object",
                  "properties": {
                    "event": { "const": "stop_subscription" },
                    "eventType": {
                      "anyOf": [
                        { "$ref": "#/$defs/EventTypeOrWildcard" },
                        {
                          "type": "array",
                          "items": { "$ref": "#/$defs/EventType" }
                        }
                      ]
                    }
                  },
                  "additionalProperties": false,
                  "required": ["event", "eventType"]
                }
              ]
            },
            "data": { "$ref": "#/$defs/EventData" }
          },
          "additionalProperties": false,
          "required": ["topicId", "topicPayload", "data"]
        }
    )");

    return { "eventtopic", schema };
}

bool EventTopic::isSubscribed() const {
    if (_subscribedEvents.empty()) {
        return false;
    }

    const bool hasActiveSubscription = std::any_of(
        _subscribedEvents.begin(),
        _subscribedEvents.end(),
        [](const std::pair<const Event::Type, bool>& subscription) {
            return subscription.second;
    });

    return hasActiveSubscription;
}

} // namespace openspace
