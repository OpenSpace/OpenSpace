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
    constexpr const char* EventKey = "event";
    constexpr const char* SubscribeEvent = "start_subscription";
    constexpr const char* UnsubscribeEvent = "stop_subscription";
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
        SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
        module->removePreSyncCallback(_targetDataCallbackHandle);
    }
}

bool SkyBrowserTopic::isDone() const {
    return _isDone;
}

void SkyBrowserTopic::handleJson(const nlohmann::json& json) {
    std::string event = json.at(EventKey).get<std::string>();
    if (event == UnsubscribeEvent) {
        _isDone = true;
        return;
    }

    if (event != SubscribeEvent) {
        _isDone = true;
        return;
    }

    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    _targetDataCallbackHandle = module->addPreSyncCallback([this]() {
        std::chrono::system_clock::time_point now = std::chrono::system_clock::now();
        if (now - _lastUpdateTime > _skyBrowserUpdateTime) {
            sendBrowserData();
            _lastUpdateTime = std::chrono::system_clock::now();
        }
    });
}

void SkyBrowserTopic::sendBrowserData() {
    using namespace openspace;

    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();
    ghoul::Dictionary data;

    // The current viewport data for OpenSpace
    ghoul::Dictionary openSpace;

    // Set general data
    data.setValue("selectedBrowserId", module->selectedBrowserId());
    data.setValue("cameraInSolarSystem", module->isCameraInSolarSystem());

    // Pass data for all the browsers and the corresponding targets
    if (module->isCameraInSolarSystem()) {
        const std::vector<std::unique_ptr<TargetBrowserPair>>& pairs = module->getPairs();
        ghoul::Dictionary targets;
        for (const std::unique_ptr<TargetBrowserPair>& pair : pairs) {
            std::string id = pair->browserId();
            // Convert deque to vector so ghoul can read it
            std::vector<int> selectedImagesVector;
            const std::deque<int> selectedImages = pair->selectedImages();
            std::for_each(
                selectedImages.begin(),
                selectedImages.end(),
                [&](int i) {
                    selectedImagesVector.push_back(i);
                }
            );

            glm::dvec2 spherical = pair->targetDirectionEquatorial();
            glm::dvec3 cartesian = skybrowser::sphericalToCartesian(spherical);

            ghoul::Dictionary target;
            // Set ("Key", value)
            target.setValue("id", id);
            target.setValue("name", pair->browserGuiName());
            target.setValue("fov", static_cast<double>(pair->verticalFov()));
            target.setValue("ra", spherical.x);
            target.setValue("dec", spherical.y);
            target.setValue("roll", pair->targetRoll());
            target.setValue("color", pair->borderColor());
            target.setValue("cartesianDirection", cartesian);
            target.setValue("ratio", static_cast<double>(pair->browserRatio()));
            target.setValue("isImageCollectionLoaded", pair->isImageCollectionLoaded());
            target.setValue("isFacingCamera", pair->isFacingCamera());
            target.setValue("isUsingRae", pair->isUsingRadiusAzimuthElevation());
            target.setValue("selectedImages", selectedImagesVector);

            std::vector<std::pair<std::string, glm::dvec3>> copies = pair->renderCopies();
            ghoul::Dictionary copiesData;
            for (size_t i = 0; i < copies.size(); i++) {
                copiesData.setValue(copies[i].first, copies[i].second);
            }
            // Set table for the current target
            target.setValue("renderCopies", copiesData);
            targets.setValue(id, target);
        }
        data.setValue("browsers", targets);
    }
    std::string jsonString = ghoul::formatJson(data);
    json jsonData = json::parse(jsonString.begin(), jsonString.end());
    _connection->sendJson(wrappedPayload(jsonData));
}

} // namespace openspace
