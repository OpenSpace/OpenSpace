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

    const std::string& status = json.at("status").get<std::string>();

    for (const std::string& event : events) {
        if (status == "start_subscription") {
            const Event::Type type = fromString(event);

            _subscribedEvents[type] = true;

            auto onCallback = [this, event](ghoul::Dictionary params) {
                // Include the fired event to the caller
                params.setValue("Event", event);
                _connection->sendJson(wrappedPayload(params));
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
                { "$ref": "#/$defs/ActionAddedData" },
                { "$ref": "#/$defs/ActionRemovedData" },
                { "$ref": "#/$defs/ApplicationShutdownData" },
                { "$ref": "#/$defs/AssetLoadingData" },
                { "$ref": "#/$defs/CameraFocusTransitionData" },
                { "$ref": "#/$defs/CameraMovedPositionData" },
                { "$ref": "#/$defs/CameraPathFinishedData" },
                { "$ref": "#/$defs/CameraPathStartedData" },
                { "$ref": "#/$defs/CustomData" },
                { "$ref": "#/$defs/FocusNodeChangedData" },
                { "$ref": "#/$defs/GuiTreeUpdatedData" },
                { "$ref": "#/$defs/InterpolationFinishedData" },
                { "$ref": "#/$defs/MissionAddedData" },
                { "$ref": "#/$defs/MissionEventReachedData" },
                { "$ref": "#/$defs/MissionRemovedData" },
                { "$ref": "#/$defs/ParallelConnectionData" },
                { "$ref": "#/$defs/PlanetEclipsedData" },
                { "$ref": "#/$defs/PointSpacecraftData" },
                { "$ref": "#/$defs/ProfileLoadingFinishedData" },
                { "$ref": "#/$defs/PropertyTreePrunedData" },
                { "$ref": "#/$defs/PropertyTreeUpdatedData" },
                { "$ref": "#/$defs/RenderableDisabledData" },
                { "$ref": "#/$defs/RenderableEnabledData" },
                { "$ref": "#/$defs/ScheduledScriptExecutedData" },
                { "$ref": "#/$defs/SessionRecordingPlaybackData" },
                { "$ref": "#/$defs/TimeOfInterestReachedData" }
              ]
            },
            "ActionAddedData": {
              "type": "object",
              "properties": {
                "Event": { "const": "ActionAdded" },
                "Uri": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["Event", "Uri"]
            },
            "ActionRemovedData": {
              "type": "object",
              "properties": {
                "Event": { "const": "ActionRemoved" },
                "Uri": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["Event", "Uri"]
            },
            "ApplicationShutdownData": {
              "type": "object",
              "properties": {
                "Event": { "const": "ApplicationShutdown" },
                "State": {
                  "type": "string",
                  "enum": ["Started", "Aborted", "Finished"]
                }
              },
              "additionalProperties": false,
              "required": ["Event", "State"]
            },
            "AssetLoadingData": {
              "type": "object",
              "properties": {
                "Event": { "const": "AssetLoading" },
                "AssetPath": { "type": "string" },
                "State": {
                  "type": "string",
                  "enum": ["Loaded", "Loading", "Unloaded", "Error"]
                }
              },
              "additionalProperties": false,
              "required": ["Event", "AssetPath", "State"]
            },
            "CameraFocusTransitionData": {
              "type": "object",
              "properties": {
                "Event": { "const": "CameraFocusTransition" },
                "Node": { "type": "string" },
                "Transition": {
                  "type": "string",
                  "enum": ["Approaching", "Reaching", "Receding", "Exiting"]
                }
              },
              "additionalProperties": false,
              "required": ["Event", "Node", "Transition"]
            },
            "CameraMovedPositionData": {
              "type": "object",
              "properties": {
                "Event": { "const": "CameraMovedPosition" }
              },
              "additionalProperties": false,
              "required": ["Event"]
            },
            "CameraPathFinishedData": {
              "type": "object",
              "properties": {
                "Event": { "const": "CameraPathFinished" },
                "Origin": { "type": "string" },
                "Destination": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["Event", "Origin", "Destination"]
            },
            "CameraPathStartedData": {
              "type": "object",
              "properties": {
                "Event": { "const": "CameraPathStarted" },
                "Origin": { "type": "string" },
                "Destination": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["Event", "Origin", "Destination"]
            },
            "CustomData": {
              "type": "object",
              "properties": {
                "Event": { "const": "Custom" },
                "Subtype": { "type": "string" },
                "Payload": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["Event", "Subtype", "Payload"]
            },
            "FocusNodeChangedData": {
              "type": "object",
              "properties": {
                "Event": { "const": "FocusNodeChanged" },
                "OldNode": { "type": "string" },
                "NewNode": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["Event", "OldNode", "NewNode"]
            },
            "GuiTreeUpdatedData": {
              "type": "object",
              "properties": {
                "Event": { "const": "GuiTreeUpdated" }
              },
              "additionalProperties": false,
              "required": ["Event"]
            },
            "InterpolationFinishedData": {
              "type": "object",
              "properties": {
                "Event": { "const": "InterpolationFinished" },
                "Property": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["Event", "Property"]
            },
            "MissionAddedData": {
              "type": "object",
              "properties": {
                "Event": { "const": "MissionAdded" },
                "Identifier": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["Event", "Identifier"]
            },
            "MissionEventReachedData": {
              "type": "object",
              "properties": {
                "Event": { "const": "MissionEventReached" }
              },
              "additionalProperties": false,
              "required": ["Event"]
            },
            "MissionRemovedData": {
              "type": "object",
              "properties": {
                "Event": { "const": "MissionRemoved" },
                "Identifier": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["Event", "Identifier"]
            },
            "ParallelConnectionData": {
              "type": "object",
              "properties": {
                "Event": { "const": "ParallelConnection" },
                "State": {
                  "type": "string",
                  "enum": ["Established", "Lost", "HostshipGained", "HostshipLost"]
                }
              },
              "additionalProperties": false,
              "required": ["Event", "State"]
            },
            "PlanetEclipsedData": {
              "type": "object",
              "properties": {
                "Event": { "const": "PlanetEclipsed" },
                "Eclipsee": { "type": "string" },
                "Eclipser": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["Event", "Eclipsee", "Eclipser"]
            },
            "PointSpacecraftData": {
              "type": "object",
              "properties": {
                "Event": { "const": "PointSpacecraft" },
                "Ra": { "type": "number" },
                "Dec": { "type": "number" },
                "Duration": { "type": "number" }
              },
              "additionalProperties": false,
              "required": ["Event", "Ra", "Dec", "Duration"]
            },
            "ProfileLoadingFinishedData": {
              "type": "object",
              "properties": {
                "Event": { "const": "ProfileLoadingFinished" }
              },
              "additionalProperties": false,
              "required": ["Event"]
            },
            "PropertyTreePrunedData": {
              "type": "object",
              "properties": {
                "Event": { "const": "PropertyTreePruned" },
                "Uri": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["Event", "Uri"]
            },
            "PropertyTreeUpdatedData": {
              "type": "object",
              "properties": {
                "Event": { "const": "PropertyTreeUpdated" },
                "Uri": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["Event", "Uri"]
            },
            "RenderableDisabledData": {
              "type": "object",
              "properties": {
                "Event": { "const": "RenderableDisabled" },
                "Node": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["Event", "Node"]
            },
            "RenderableEnabledData": {
              "type": "object",
              "properties": {
                "Event": { "const": "RenderableEnabled" },
                "Node": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["Event", "Node"]
            },
            "ScheduledScriptExecutedData": {
              "type": "object",
              "properties": {
                "Event": { "const": "ScheduledScriptExecuted" },
                "Script": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["Event", "Script"]
            },
            "SessionRecordingPlaybackData": {
              "type": "object",
              "properties": {
                "Event": { "const": "SessionRecordingPlayback" },
                "State": {
                  "type": "string",
                  "enum": ["Started", "Paused", "Resumed", "Finished"]
                }
              },
              "additionalProperties": false,
              "required": ["Event", "State"]
            },
            "TimeOfInterestReachedData": {
              "type": "object",
              "properties": {
                "Event": { "const": "TimeOfInterestReached" }
              },
              "additionalProperties": false,
              "required": ["Event"]
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
            "data": {
              "type": "object",
              "anyOf": [{ "$ref": "#/$defs/EventData" }]
            }
          },
          "additionalProperties": false,
          "required": ["topicId", "topicPayload", "data"]
        }
    )");

    return {
        "eventtopic",
        schema
    };
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
