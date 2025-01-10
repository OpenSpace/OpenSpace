/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <modules/server/include/topics/cameratopic.h>

#ifdef OPENSPACE_MODULE_SPACE_ENABLED
#include <modules/globebrowsing/globebrowsingmodule.h>
#endif // OPENSPACE_MODULE_SPACE_ENABLED
#include <modules/server/include/connection.h>
#include <modules/server/servermodule.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/globals.h>
#include <openspace/properties/property.h>
#include <openspace/util/distanceconversion.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr std::string_view SubscribeEvent = "start_subscription";
} // namespace

using nlohmann::json;

namespace openspace {

CameraTopic::CameraTopic()
    : _lastUpdateTime(std::chrono::system_clock::now())
{}

CameraTopic::~CameraTopic() {
    if (_dataCallbackHandle != UnsetOnChangeHandle) {
        ServerModule* module = global::moduleEngine->module<ServerModule>();
        if (module) {
            module->removePreSyncCallback(_dataCallbackHandle);
        }
    }
}

bool CameraTopic::isDone() const {
    return _isDone;
}

void CameraTopic::handleJson(const nlohmann::json& json) {
    const std::string event = json.at("event").get<std::string>();

    if (event != SubscribeEvent) {
        _isDone = true;
        return;
    }

    ServerModule* module = global::moduleEngine->module<ServerModule>();
    _dataCallbackHandle = module->addPreSyncCallback(
        [this]() {
            const auto now = std::chrono::system_clock::now();
            if (now - _lastUpdateTime > _cameraPositionUpdateTime) {
                sendCameraData();
                _lastUpdateTime = std::chrono::system_clock::now();
            }
        }
    );
}

void CameraTopic::sendCameraData() {
#ifdef OPENSPACE_MODULE_SPACE_ENABLED
    GlobeBrowsingModule* module = global::moduleEngine->module<GlobeBrowsingModule>();
    glm::dvec3 position = module->geoPosition();
    std::pair<double, std::string_view> altSimplified = simplifyDistance(position.z);

    const nlohmann::json jsonData = {
        { "latitude", position.x },
        { "longitude", position.y },
        { "altitude", altSimplified.first },
        { "altitudeUnit", altSimplified.second }
    };

    _connection->sendJson(wrappedPayload(jsonData));
#else // ^^^ OPENSPACE_MODULE_SPACE_ENABLED ||| !OPENSPACE_MODULE_SPACE_ENABLED vvv
    LWARNINGC(
        "CameraTopic",
        "Cannot send camera data, compiled without globebrowsing support"
    );
#endif // OPENSPACE_MODULE_SPACE_ENABLED
}

} // namespace openspace
