/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include "modules/server/include/topics/sessionrecordingtopic.h"

#include <modules/server/include/connection.h>
#include <openspace/engine/globals.h>
#include <openspace/query/query.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr std::string_view _loggerCat = "SessionRecordingTopic";

    constexpr std::string_view SubscribeEvent = "start_subscription";
    constexpr std::string_view UnsubscribeEvent = "stop_subscription";
    constexpr std::string_view RefreshEvent = "refresh";

    constexpr const char* PropertiesKey = "properties";
    constexpr const char* FilesKey = "files";
    constexpr const char* StateKey = "state";
} // namespace

using nlohmann::json;

namespace openspace {

SessionRecordingTopic::SessionRecordingTopic() {
    LDEBUG("Starting new SessionRecording state subscription");
}

SessionRecordingTopic::~SessionRecordingTopic() {
    if (_stateCallbackHandle != UnsetOnChangeHandle) {
        global::sessionRecordingHandler->removeStateChangeCallback(_stateCallbackHandle);
    }
}

bool SessionRecordingTopic::isDone() const {
    return _isDone;
}

void SessionRecordingTopic::handleJson(const nlohmann::json& json) {
    const std::string event = json.at("event").get<std::string>();
    if (event != SubscribeEvent && event != UnsubscribeEvent &&
        event != RefreshEvent)
    {
        LERROR("Unsupported event");
        _isDone = true;
        return;
    }

    if (event == UnsubscribeEvent) {
        _isDone = true;
        return;
    }

    if (json.find(PropertiesKey) != json.end()) {
        if (!json.at(PropertiesKey).is_array()) {
            LERROR("Properties must be an array of strings");
        }
        const nlohmann::json requestedProperties =
            json.at(PropertiesKey).get<nlohmann::json>();
        for (const auto& p : requestedProperties) {
            if (!p.is_string()) {
                _isDone = true;
                LERROR("Properties must be an array of strings");
                return;
            }
            const std::string v = p.get<std::string>();
            if (v == FilesKey) {
                _sendFiles = true;
            }
            if (v == StateKey) {
                _sendState = true;
            }
        }
    }

    sendJsonData();

    if (event == SubscribeEvent && _sendState) {
        _stateCallbackHandle = global::sessionRecordingHandler->addStateChangeCallback(
            [this]() {
                const interaction::SessionRecordingHandler::SessionState currentState =
                    global::sessionRecordingHandler->state();
                if (currentState != _lastState) {
                    sendJsonData();
                    _lastState = currentState;
                }
            }
        );
    }
}

void SessionRecordingTopic::sendJsonData() {
    json stateJson;
    using SessionRecordingHandler = interaction::SessionRecordingHandler;
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
        stateJson[StateKey] = stateString;
    };
    if (_sendFiles) {
        stateJson[FilesKey] = global::sessionRecordingHandler->playbackList();
    }
    if (!stateJson.empty()) {
        _connection->sendJson(wrappedPayload(stateJson));
    }
}

} // namespace openspace
