/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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
#include <openspace/interaction/sessionrecording.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const char* _loggerCat = "SessionRecordingTopic";
    constexpr const char* EventKey = "event";
    constexpr const char* SubscribeEvent = "start_subscription";
    constexpr const char* UnsubscribeEvent = "stop_subscription";
    constexpr const char* RefreshEvent = "refresh";

    constexpr const char* PropertiesKey = "properties";
    constexpr const char* FilesKey = "files";
    constexpr const char* StateKey = "state";
} // namespace

using nlohmann::json;

namespace openspace {

SessionRecordingTopic::SessionRecordingTopic()
    : _lastState(interaction::SessionRecording::SessionState::Idle)
{
    LDEBUG("Starting new SessionRecording state subscription");
}

SessionRecordingTopic::~SessionRecordingTopic() {
    if (_stateCallbackHandle != UnsetOnChangeHandle) {
        global::sessionRecording.removeStateChangeCallback(_stateCallbackHandle);
    }
}

bool SessionRecordingTopic::isDone() const {
    return _isDone;
}

void SessionRecordingTopic::handleJson(const nlohmann::json& json) {
    using SessionRecording = openspace::interaction::SessionRecording;

    const std::string event = json.at(EventKey).get<std::string>();
    if (event != SubscribeEvent &&
        event != UnsubscribeEvent &&
        event != RefreshEvent)
    {
        LERROR("Unsupported event.");
        _isDone = true;
        return;
    }

    if (event == UnsubscribeEvent) {
        _isDone = true;
        return;
    }

    if (json.find(PropertiesKey) != json.end()) {
        if (!json.at(PropertiesKey).is_array()) {
            LERROR("Properties must be an array of strings.");
        }
        nlohmann::json requestedProperties = json.at(PropertiesKey).get<nlohmann::json>();
        for (const auto& p : requestedProperties) {
            if (!p.is_string()) {
                _isDone = true;
                LERROR("Properties must be an array of strings.");
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

    if (event == SubscribeEvent) {
        if (_sendState) {
            _stateCallbackHandle = global::sessionRecording.addStateChangeCallback(
                [this]() {
                    SessionRecording::SessionState currentState =
                    global::sessionRecording.state();
                    if (currentState != _lastState) {
                       sendJsonData();
                       _lastState = currentState;
                    }
                }
            );
        }
    }
}

void SessionRecordingTopic::sendJsonData() {
    json stateJson;
    using SessionRecording = openspace::interaction::SessionRecording;
    if (_sendState) {
        SessionRecording::SessionState state = global::sessionRecording.state();
        std::string stateString;
        switch (state) {
        case SessionRecording::SessionState::Recording:
            stateString = "recording";
            break;
        case SessionRecording::SessionState::Playback:
            stateString = "playing";
            break;
        default:
            stateString = "idle";
            break;
        }
        stateJson[StateKey] = stateString;
    };
    if (_sendFiles) {
        stateJson[FilesKey] = global::sessionRecording.playbackList();
    }
    if (stateJson.size() > 0) {
        _connection->sendJson(wrappedPayload(stateJson));
    }
}

} // namespace openspace
