/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#include "modules/server/include/topics/enginemodetopic.h"

#include <modules/server/include/connection.h>
#include <openspace/engine/globals.h>
#include <openspace/query/query.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const char _loggerCat[] = "EngineModeTopic";

    constexpr const char EventKey[] = "event";
    constexpr const char SubscribeEvent[] = "start_subscription";
    constexpr const char UnsubscribeEvent[] = "stop_subscription";
    constexpr const char RefreshEvent[] = "refresh";

    constexpr const char ModeKey[] = "mode";

} // namespace

using nlohmann::json;

namespace openspace {

EngineModeTopic::EngineModeTopic() {
    LDEBUG("Starting new EngineMode subscription");
}

EngineModeTopic::~EngineModeTopic() {
    if (_modeCallbackHandle != UnsetOnChangeHandle) {
        global::openSpaceEngine->removeModeChangeCallback(_modeCallbackHandle);
    }
}

bool EngineModeTopic::isDone() const {
    return _isDone;
}

void EngineModeTopic::handleJson(const nlohmann::json& json) {
    const std::string event = json.at(EventKey).get<std::string>();
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

    sendJsonData();

    if (event == SubscribeEvent) {
        _modeCallbackHandle = global::openSpaceEngine->addModeChangeCallback(
            [this]() {
                OpenSpaceEngine::Mode currentMode =
                    global::openSpaceEngine->currentMode();
                if (currentMode != _lastMode) {
                    sendJsonData();
                    _lastMode = currentMode;
                }
            }
        );
    }
}

void EngineModeTopic::sendJsonData() {
    json stateJson;

    OpenSpaceEngine::Mode mode = global::openSpaceEngine->currentMode();
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
        default:
            throw ghoul::MissingCaseException();
    }
    stateJson[ModeKey] = modeString;

    if (!stateJson.empty()) {
        _connection->sendJson(wrappedPayload(stateJson));
    }
}

} // namespace openspace
