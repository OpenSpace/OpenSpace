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

#include "openspace/topic/topics/sessionrecordingtopic.h"

#include <openspace/documentation/schema.h>
#include <openspace/engine/globals.h>
#include <openspace/topic/connection.h>
#include <ghoul/logging/logmanager.h>
#include <string_view>

namespace {
    constexpr std::string_view _loggerCat = "SessionRecordingTopic";
} // namespace

namespace openspace {

SessionRecordingTopic::SessionRecordingTopic() {
    LDEBUG("Starting new SessionRecording state subscription");
}

SessionRecordingTopic::~SessionRecordingTopic() {
    if (_stateCallbackHandle != UnsetOnChangeHandle) {
        global::sessionRecordingHandler->removeStateChangeCallback(_stateCallbackHandle);
    }
}

void SessionRecordingTopic::handleJson(const nlohmann::json& json) {
    const std::string event = json.at("event").get<std::string>();

    if (event == "stop_subscription") {
        _isDone = true;
        return;
    }

    // @TODO (anden88 2026-04-16): There was a "refresh" command one could send that would
    // send the jsondata back (or rather, any event string would do this) Do we need or
    // want to bring back a "refresh" command?
    if (event != "start_subscription") {
        return;
    }

    const std::vector<std::string> properties =
        json.at("properties").get<std::vector<std::string>>();

    if (properties.empty()) {
        return;
    }

    for (const std::string& property : properties) {
        if (property == "files") {
            _sendFiles = true;
        }
        else if (property == "state") {
            _sendState = true;
        }
        else {
            LWARNING(std::format("Got unknown property '{}'", property));
        }
    }

    sendJsonData();

    if (_sendState) {
        _stateCallbackHandle = global::sessionRecordingHandler->addStateChangeCallback(
            [this]() {
                const SessionRecordingHandler::SessionState currentState =
                    global::sessionRecordingHandler->state();
                if (currentState != _lastState) {
                    sendJsonData();
                    _lastState = currentState;
                }
            }
        );
    }
}

bool SessionRecordingTopic::isDone() const {
    return _isDone;
}

Schema SessionRecordingTopic::Schema() {
    nlohmann::json schema = nlohmann::json::parse(R"(
        {
          "$defs": {
            "SessionState": {
              "type": "string",
              "enum": ["idle", "recording", "playing", "playing-paused"],
              "tsEnumNames": ["Idle", "Recording", "Playing", "Paused"]
            }
          },
          "title": "SessionRecordingTopic",
          "type": "object",
          "properties": {
            "topicId": { "const": "sessionRecording" },
            "topicPayload": {
              "type": "object",
              "anyOf": [
                {
                  "type": "object",
                  "properties": {
                    "event": { "const": "start_subscription" },
                    "properties": {
                      "type": "array",
                      "items": {
                        "type": "string",
                        "enum": ["files", "state"],
                        "minItems": 1
                      }
                    }
                  },
                  "additionalProperties": false,
                  "required": ["event", "properties"]
                },
                {
                  "type": "object",
                  "properties": {
                    "event": { "const": "stop_subscription" }
                  },
                  "additionalProperties": false,
                  "required": ["event"]
                }
              ]
            },
            "data": {
              "type": "object",
              "properties": {
                "state": { "$ref": "#/$defs/SessionState" },
                "files": {
                  "type": "array",
                  "items": { "type": "string" }
                }
              },
              "additionalProperties": false
            }
          },
          "additionalProperties": false,
          "required": ["topicId", "topicPayload", "data"]
        }
    )");

    return { "sessionrecordingtopic", schema };
}

void SessionRecordingTopic::sendJsonData() {
    nlohmann::json stateJson;
    if (_sendState) {
        std::string stateString;
        switch (global::sessionRecordingHandler->state()) {
            case SessionRecordingHandler::SessionState::Recording:
                stateString = "recording";
                break;
            case SessionRecordingHandler::SessionState::Playback:
                stateString = "playing";
                break;
            case SessionRecordingHandler::SessionState::PlaybackPaused:
                stateString = "playing-paused";
                break;
            default:
                stateString = "idle";
                break;
        }
        stateJson["state"] = stateString;
    }
    if (_sendFiles) {
        stateJson["files"] = global::sessionRecordingHandler->playbackList();
    }
    if (!stateJson.empty()) {
        _connection->sendJson(wrappedPayload(stateJson));
    }
}

} // namespace openspace
