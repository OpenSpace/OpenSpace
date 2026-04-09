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

#include "openspace/topic/topics/enginemodetopic.h"

#include <openspace/documentation/schema.h>
#include <openspace/engine/globals.h>
#include <openspace/query/query.h>
#include <openspace/topic/connection.h>
#include <ghoul/logging/logmanager.h>
#include <string_view>

using nlohmann::json;

namespace {
    constexpr std::string_view _loggerCat = "EngineModeTopic";
} // namespace

namespace openspace {

EngineModeTopic::EngineModeTopic() {
    LDEBUG("Starting new EngineMode subscription");
}

EngineModeTopic::~EngineModeTopic() {
    if (_modeCallbackHandle != UnsetOnChangeHandle) {
        global::openSpaceEngine->removeModeChangeCallback(_modeCallbackHandle);
    }
}

void EngineModeTopic::handleJson(const nlohmann::json& json) {
    const std::string event = json.at("event").get<std::string>();
    if (event != "start_subscription" && event != "stop_subscription" &&
        event != "refresh")
    {
        LERROR("Unsupported event");
        _isDone = true;
        return;
    }

    if (event == "stop_subscription") {
        _isDone = true;
        return;
    }

    sendJsonData();

    if (event == "start_subscription") {
        _modeCallbackHandle = global::openSpaceEngine->addModeChangeCallback(
            [this]() {
                const OpenSpaceEngine::Mode mode = global::openSpaceEngine->currentMode();
                if (mode != _lastMode) {
                    sendJsonData();
                    _lastMode = mode;
                }
            }
        );
    }
}

bool EngineModeTopic::isDone() const {
    return _isDone;
}

Schema EngineModeTopic::Schema() {
    nlohmann::json schema = nlohmann::json::parse(R"(
        {
          "title": "EngineModeTopic",
          "type": "object",
          "properties": {
            "topicId": { "const": "engineMode" },
            "topicPayload": {
              "type": "object",
              "properties": {
                "event": {
                  "type": "string",
                  "enum": ["start_subscription", "stop_subscription", "refresh"]
                }
              },
              "additionalProperties": false,
              "required": ["event"]
            },
            "data": {
              "type": "object",
              "properties": {
                "mode": {
                  "type": "string",
                  "enum": ["user_control", "session_recording_playback", "camera_path"]
                }
              },
              "additionalProperties": false,
              "required": ["mode"]
            }
          },
          "additionalProperties": false,
          "required": ["topicId", "topicPayload", "data"]
        }

    )");

    return {
        "enginemodetopic",
        schema
    };
}
void EngineModeTopic::sendJsonData() {
    const OpenSpaceEngine::Mode mode = global::openSpaceEngine->currentMode();
    std::string modeString;
    switch (mode) {
        case OpenSpaceEngine::Mode::UserControl:
            modeString = "user_control";
            break;
        case OpenSpaceEngine::Mode::SessionRecordingPlayback:
            modeString = "session_recording_playback";
            break;
        case OpenSpaceEngine::Mode::CameraPath:
            modeString = "camera_path";
            break;
    }

    json stateJson;
    stateJson["mode"] = modeString;

    if (!stateJson.empty()) {
        _connection->sendJson(wrappedPayload(stateJson));
    }
}

} // namespace openspace
