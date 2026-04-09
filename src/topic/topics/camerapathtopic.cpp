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

#include <openspace/topic/topics/camerapathtopic.h>

#include <openspace/documentation/schema.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/path.h>
#include <openspace/navigation/pathnavigator.h>
#include <openspace/topic/connection.h>
#include <openspace/topic/server.h>
#include <ghoul/misc/assert.h>
#include <algorithm>
#include <cmath>
#include <string_view>

namespace openspace {

CameraPathTopic::CameraPathTopic()
    : _lastUpdateTime(std::chrono::system_clock::now())
{}

CameraPathTopic::~CameraPathTopic() {
    if (_dataCallbackHandle != UnsetOnChangeHandle) {
        global::server->removePreSyncCallback(_dataCallbackHandle);
    }
}

void CameraPathTopic::handleJson(const nlohmann::json& json) {
    const std::string event = json.at("event").get<std::string>();

    if (event == "stop_subscription") {
        _isDone = true;
        return;
    }

    if (event == "start_subscription") {
        _dataCallbackHandle = global::server->addPreSyncCallback(
            [this]() {
                const bool isInPath = (global::openSpaceEngine->currentMode()
                    == OpenSpaceEngine::Mode::CameraPath);

                const auto now = std::chrono::system_clock::now();
                if (isInPath && (now - _lastUpdateTime) > _cameraPathUpdateTime) {
                    sendCameraPathData();
                    _lastUpdateTime = std::chrono::system_clock::now();
                }
            }
        );
    }
}

bool CameraPathTopic::isDone() const {
    return _isDone;
}

Schema CameraPathTopic::Schema() {
    nlohmann::json schema = nlohmann::json::parse(R"(
        {
          "title": "CameraPathTopic",
          "type": "object",
          "properties": {
            "topicId": { "const": "cameraPath" },
            "topicPayload": {
              "type": "object",
              "properties": {
                "event": {
                  "type": "string",
                  "enum": ["start_subscription", "stop_subscription"]
                }
              },
              "additionalProperties": false,
              "required": ["event"]
            },
            "data": {
              "type": "object",
              "properties": {
                "target": { "type": "string" },
                "remainingTime": { "type": "number" },
                "isPaused": { "type": "boolean" }
              },
              "additionalProperties": false,
              "required": ["target", "remainingTime", "isPaused"]
            }
          },
          "additionalProperties": false,
          "required": ["topicId", "topicPayload", "data"]
        }
    )");

    return {
        "camerapathtopic",
        schema
    };
}

void CameraPathTopic::sendCameraPathData() {
    const PathNavigator& pathNavigator = global::navigationHandler->pathNavigator();

    const Path* path = pathNavigator.currentPath();

    if (!path) {
        ghoul_assert(path, "Path must exist");
        return;
    }

    // The time is not exact, and we only care about the number of seconds. Also, any
    // negative values should be interpreted as positive
    int seconds = static_cast<int>(
        std::round(pathNavigator.estimatedRemainingTimeInPath())
    );
    seconds = std::max(seconds, 0);

    const nlohmann::json jsonData = {
        { "target", path->endPoint().nodeIdentifier() },
        { "remainingTime", seconds },
        //{ "remainingDistance", path->remainingDistance() },
        { "isPaused", pathNavigator.isPaused() }
    };

    _connection->sendJson(wrappedPayload(jsonData));
}

} // namespace openspace
