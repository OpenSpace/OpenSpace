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

#include "modules/server/include/topics/skybrowsertopic.h"

#include <modules/server/include/connection.h>
#include <modules/server/servermodule.h>
#include <modules/skybrowser/skybrowsermodule.h>
#include <modules/skybrowser/include/targetbrowserpair.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/globals.h>
#include <openspace/properties/property.h>
#include <openspace/query/query.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr std::string_view SubscribeEvent = "start_subscription";
    constexpr std::string_view UnsubscribeEvent = "stop_subscription";
    constexpr std::string_view SendImageCollection = "send_image_collection";
    constexpr std::string_view MoveHoverCircle = "move_hover_circle";
    constexpr std::string_view DisableHoverCircle = "disable_hover_circle";
    constexpr std::string_view FineTuneTarget = "fine_tune_target";
    constexpr std::string_view StartFineTuningTarget = "start_fine_tuning_target";
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
    if (event == SendImageCollection) {
        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
        nlohmann::json data = json::object({
            { "url", module->wwtImageCollectionUrl() },
            { "imageList", module->imageList() }
        });
        nlohmann::json json = json::object({
            { "type", "image_collection" }, { "data", data }
        });
        _connection->sendJson(wrappedPayload(json));
    }
    if (event == MoveHoverCircle) {
        const std::string url = json.at("url").get<std::string>();
        global::moduleEngine->module<SkyBrowserModule>()->moveHoverCircle(url, false);
    }
    if (event == DisableHoverCircle) {
        global::moduleEngine->module<SkyBrowserModule>()->disableHoverCircle(false);
    }
    if (event == StartFineTuningTarget) {
        const std::string identifier = json.at("identifier").get<std::string>();
        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
        TargetBrowserPair* pair = module->pair(identifier);
        if (pair) {
            pair->startFinetuningTarget();
        }
    }
    if (event == FineTuneTarget) {
        const std::string identifier = json.at("identifier").get<std::string>();
        std::vector vec = json.at("translation").get<std::vector<float>>();
        glm::vec2 translation = glm::vec2(vec[0], vec[1]);

        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
        TargetBrowserPair* pair = module->pair(identifier);
        if (pair) {
            pair->fineTuneTarget(translation);
        }
    }
    if (event == SubscribeEvent) {
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
        return;
    }
}

void SkyBrowserTopic::sendBrowserData() {
    using namespace openspace;

    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

    // Set general data
    nlohmann::json json = {
        { "cameraInSolarSystem", module->isCameraInSolarSystem() }
    };
    _connection->sendJson(wrappedPayload({
            { "type", "browser_data" },
            { "data", json }
        }));
}
} // namespace openspace
