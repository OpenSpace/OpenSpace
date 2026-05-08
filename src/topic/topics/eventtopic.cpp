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
                "event": { "const": "ActionAdded" },
                "uri": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["event", "uri"]
            },
            "ActionRemovedData": {
              "type": "object",
              "properties": {
                "event": { "const": "ActionRemoved" },
                "uri": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["event", "uri"]
            },
            "ApplicationShutdownData": {
              "type": "object",
              "properties": {
                "event": { "const": "ApplicationShutdown" },
                "state": {
                  "type": "string",
                  "enum": ["Started", "Aborted", "Finished"]
                }
              },
              "additionalProperties": false,
              "required": ["event", "state"]
            },
            "AssetLoadingData": {
              "type": "object",
              "properties": {
                "event": { "const": "AssetLoading" },
                "assetPath": { "type": "string" },
                "state": {
                  "type": "string",
                  "enum": ["Loaded", "Loading", "Unloaded", "Error"]
                }
              },
              "additionalProperties": false,
              "required": ["event", "assetPath", "state"]
            },
            "CameraFocusTransitionData": {
              "type": "object",
              "properties": {
                "event": { "const": "CameraFocusTransition" },
                "node": { "type": "string" },
                "transition": {
                  "type": "string",
                  "enum": ["Approaching", "Reaching", "Receding", "Exiting"]
                }
              },
              "additionalProperties": false,
              "required": ["event", "node", "transition"]
            },
            "CameraMovedPositionData": {
              "type": "object",
              "properties": {
                "event": { "const": "CameraMovedPosition" }
              },
              "additionalProperties": false,
              "required": ["event"]
            },
            "CameraPathFinishedData": {
              "type": "object",
              "properties": {
                "event": { "const": "CameraPathFinished" },
                "origin": { "type": "string" },
                "destination": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["event", "origin", "destination"]
            },
            "CameraPathStartedData": {
              "type": "object",
              "properties": {
                "event": { "const": "CameraPathStarted" },
                "origin": { "type": "string" },
                "destination": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["event", "origin", "destination"]
            },
            "CustomData": {
              "type": "object",
              "properties": {
                "event": { "const": "Custom" },
                "subtype": { "type": "string" },
                "payload": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["event", "subtype", "payload"]
            },
            "FocusNodeChangedData": {
              "type": "object",
              "properties": {
                "event": { "const": "FocusNodeChanged" },
                "oldNode": { "type": "string" },
                "newNode": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["event", "oldNode", "newNode"]
            },
            "GuiTreeUpdatedData": {
              "type": "object",
              "properties": {
                "event": { "const": "GuiTreeUpdated" }
              },
              "additionalProperties": false,
              "required": ["event"]
            },
            "InterpolationFinishedData": {
              "type": "object",
              "properties": {
                "event": { "const": "InterpolationFinished" },
                "property": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["event", "property"]
            },
            "MissionAddedData": {
              "type": "object",
              "properties": {
                "event": { "const": "MissionAdded" },
                "identifier": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["event", "identifier"]
            },
            "MissionEventReachedData": {
              "type": "object",
              "properties": {
                "event": { "const": "MissionEventReached" }
              },
              "additionalProperties": false,
              "required": ["event"]
            },
            "MissionRemovedData": {
              "type": "object",
              "properties": {
                "event": { "const": "MissionRemoved" },
                "identifier": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["event", "identifier"]
            },
            "ParallelConnectionData": {
              "type": "object",
              "properties": {
                "event": { "const": "ParallelConnection" },
                "state": {
                  "type": "string",
                  "enum": ["Established", "Lost", "HostshipGained", "HostshipLost"]
                }
              },
              "additionalProperties": false,
              "required": ["event", "state"]
            },
            "PlanetEclipsedData": {
              "type": "object",
              "properties": {
                "event": { "const": "PlanetEclipsed" },
                "eclipsee": { "type": "string" },
                "eclipser": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["event", "eclipsee", "eclipser"]
            },
            "PointSpacecraftData": {
              "type": "object",
              "properties": {
                "event": { "const": "PointSpacecraft" },
                "ra": { "type": "number" },
                "dec": { "type": "number" },
                "duration": { "type": "number" }
              },
              "additionalProperties": false,
              "required": ["event", "ra", "dec", "duration"]
            },
            "ProfileLoadingFinishedData": {
              "type": "object",
              "properties": {
                "event": { "const": "ProfileLoadingFinished" }
              },
              "additionalProperties": false,
              "required": ["event"]
            },
            "PropertyTreePrunedData": {
              "type": "object",
              "properties": {
                "event": { "const": "PropertyTreePruned" },
                "uri": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["event", "uri"]
            },
            "PropertyTreeUpdatedData": {
              "type": "object",
              "properties": {
                "event": { "const": "PropertyTreeUpdated" },
                "uri": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["event", "uri"]
            },
            "RenderableDisabledData": {
              "type": "object",
              "properties": {
                "event": { "const": "RenderableDisabled" },
                "node": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["event", "node"]
            },
            "RenderableEnabledData": {
              "type": "object",
              "properties": {
                "event": { "const": "RenderableEnabled" },
                "node": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["event", "node"]
            },
            "ScheduledScriptExecutedData": {
              "type": "object",
              "properties": {
                "event": { "const": "ScheduledScriptExecuted" },
                "script": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["event", "script"]
            },
            "SessionRecordingPlaybackData": {
              "type": "object",
              "properties": {
                "event": { "const": "SessionRecordingPlayback" },
                "state": {
                  "type": "string",
                  "enum": ["Started", "Paused", "Resumed", "Finished"]
                }
              },
              "additionalProperties": false,
              "required": ["event", "state"]
            },
            "TimeOfInterestReachedData": {
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
