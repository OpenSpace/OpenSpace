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

#include <modules/server/include/topics/camerapathtopic.h>

#include <modules/server/include/connection.h>
#include <modules/server/servermodule.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/path.h>
#include <openspace/navigation/pathnavigator.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr std::string_view SubscribeEvent = "start_subscription";
} // namespace

using nlohmann::json;

namespace openspace {

CameraPathTopic::CameraPathTopic()
    : _lastUpdateTime(std::chrono::system_clock::now())
{}

CameraPathTopic::~CameraPathTopic() {
    if (_dataCallbackHandle != UnsetOnChangeHandle) {
        ServerModule* module = global::moduleEngine->module<ServerModule>();
        if (module) {
            module->removePreSyncCallback(_dataCallbackHandle);
        }
    }
}

bool CameraPathTopic::isDone() const {
    return _isDone;
}

void CameraPathTopic::handleJson(const nlohmann::json& json) {
    const std::string event = json.at("event").get<std::string>();

    if (event != SubscribeEvent) {
        _isDone = true;
        return;
    }

    ServerModule* module = global::moduleEngine->module<ServerModule>();
    _dataCallbackHandle = module->addPreSyncCallback(
        [this]() {
            const bool isInPath =(global::openSpaceEngine->currentMode()
                == OpenSpaceEngine::Mode::CameraPath);

            const auto now = std::chrono::system_clock::now();
            if (isInPath && (now - _lastUpdateTime) > _cameraPathUpdateTime) {
                sendCameraPathData();
                _lastUpdateTime = std::chrono::system_clock::now();
            }
        }
    );
}

void CameraPathTopic::sendCameraPathData() {
    const interaction::PathNavigator& pathNavigator =
        global::navigationHandler->pathNavigator();

    const interaction::Path* path = pathNavigator.currentPath();

    if (!path) {
        ghoul_assert(path, "Path must exist");
        return;
    }

    // The time is not exact, and we only care about the number of seconds. Also,
    // any negative values should be interpreted as positive
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
