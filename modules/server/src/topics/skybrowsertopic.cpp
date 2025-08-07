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

#include "modules/server/include/topics/skybrowsertopic.h"

#include <modules/server/include/connection.h>
#include <modules/server/servermodule.h>
#include <modules/skybrowser/skybrowsermodule.h>
#include <modules/skybrowser/include/targetbrowserpair.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/globals.h>
#include <openspace/properties/property.h>
#include <openspace/query/query.h>
#include <openspace/util/json_helper.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr std::string_view SubscribeEvent = "start_subscription";
    constexpr std::string_view UnsubscribeEvent = "stop_subscription";
} // namespace

using nlohmann::json;

namespace openspace {

SkyBrowserTopic::SkyBrowserTopic()
    : _lastUpdateTime(std::chrono::system_clock::now())
{
    ServerModule* module = global::moduleEngine->module<ServerModule>();
    if (module) {
        _skyBrowserUpdateTime = std::chrono::milliseconds(module->skyBrowserUpdateTime());
    }
}

SkyBrowserTopic::~SkyBrowserTopic() {
    if (_targetDataCallbackHandle != UnsetOnChangeHandle) {
        ServerModule* module = global::moduleEngine->module<ServerModule>();
        if (module) {
            module->removePreSyncCallback(_targetDataCallbackHandle);
        }
    }
}

bool SkyBrowserTopic::isDone() const {
    return _isDone;
}

void SkyBrowserTopic::handleJson(const nlohmann::json& json) {
    const std::string event = json.at("event").get<std::string>();
    if (event == UnsubscribeEvent) {
        _isDone = true;
        return;
    }

    if (event != SubscribeEvent) {
        _isDone = true;
        return;
    }

    ServerModule* module = global::moduleEngine->module<ServerModule>();
    _targetDataCallbackHandle = module->addPreSyncCallback(
        [this]() {
            const auto now = std::chrono::system_clock::now();
            if (now - _lastUpdateTime > _skyBrowserUpdateTime) {
                sendBrowserData();
                _lastUpdateTime = std::chrono::system_clock::now();
            }
        }
    );
}

void SkyBrowserTopic::sendBrowserData() {
    using namespace openspace;

    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    ghoul::Dictionary data;

    // Set general data
    data.setValue("selectedBrowserId", module->selectedBrowserId());
    data.setValue("cameraInSolarSystem", module->isCameraInSolarSystem());

    // Pass data for all the browsers and the corresponding targets
    const std::vector<std::unique_ptr<TargetBrowserPair>>& pairs = module->pairs();
    ghoul::Dictionary targets;
    for (const std::unique_ptr<TargetBrowserPair>& pair : pairs) {
        const std::string id = pair->browserId();
        const ghoul::Dictionary target = pair->dataAsDictionary();
        targets.setValue(id, target);
    }
    data.setValue("browsers", targets);

    std::string jsonString = ghoul::formatJson(data);

    // Only send message if data actually changed
    if (jsonString != _lastUpdateJsonString) {
        const json jsonData = json::parse(jsonString.begin(), jsonString.end());
        _connection->sendJson(wrappedPayload(jsonData));
    }

    // @TODO (2022-04-28, emmbr) The message is still sent very often; every time the
    // camera moves or the time is changes, because this changes the "roll" parameter
    // of the browser. This is the update that occurs most often. Maybe it could be
    // separated into it's own topic?

    _lastUpdateJsonString = jsonString;
}

} // namespace openspace
