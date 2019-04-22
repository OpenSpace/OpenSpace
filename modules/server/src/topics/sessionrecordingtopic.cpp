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
#include <openspace/properties/property.h>
#include <openspace/query/query.h>
#include <openspace/interaction/sessionrecording.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const char* _loggerCat = "SessionRecordingTopic";
    constexpr const char* PropertyKey = "property";
    constexpr const char* EventKey = "event";
    constexpr const char* UnsubscribeEvent = "stop_subscription";
    constexpr const char* StateKey = "recState";
    constexpr const char* PlaybackListKey = "playbackList";
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
    std::string event = json.at(EventKey).get<std::string>();
    if (event == UnsubscribeEvent) {
        _isDone = true;
        return;
    }

    std::string requestedKey = json.at(PropertyKey).get<std::string>();
    LDEBUG("Subscribing to " + requestedKey);

    if (requestedKey == StateKey) {
        _stateCallbackHandle = global::sessionRecording.addStateChangeCallback(
            [this]() {
                openspace::interaction::SessionRecording::SessionState nowState =
                    global::sessionRecording.state();
                if (nowState != _lastState) {
                    _connection->sendJson(state());
                    _lastState = nowState;
                }
            }
        );
        _connection->sendJson(state());
    }
    else if (requestedKey == PlaybackListKey) {
        _connection->sendJson(playbackList());
    }
    else {
        LWARNING("Cannot get " + requestedKey);
        _isDone = true;
    }
}

json SessionRecordingTopic::state() {
    json statJson = { { "state", static_cast<int>(global::sessionRecording.state()) } };
    return wrappedPayload(statJson);
}

json SessionRecordingTopic::playbackList() {
    json statJson = { { "playbackList", "Test string line1\nTest string line2" } };
    return wrappedPayload(statJson);
}

} // namespace openspace
