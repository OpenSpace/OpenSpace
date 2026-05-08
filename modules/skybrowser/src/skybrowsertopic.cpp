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

#include <modules/skybrowser/include/skybrowsertopic.h>

#include <modules/skybrowser/skybrowsermodule.h>
#include <modules/skybrowser/include/targetbrowserpair.h>
#include <openspace/documentation/schema.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/topic/connection.h>
#include <openspace/topic/server.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/dictionaryjsonformatter.h>
#include <string_view>
#include <vector>

using nlohmann::json;

namespace openspace {

SkyBrowserTopic::SkyBrowserTopic()
    : _lastUpdateTime(std::chrono::system_clock::now())
{
    SkyBrowserModule* module = global::moduleEngine->module<SkyBrowserModule>();

    _skyBrowserUpdateTime =
        std::chrono::milliseconds(module->topicUpdateInterval());
}

SkyBrowserTopic::~SkyBrowserTopic() {
    if (_targetDataCallbackHandle != UnsetOnChangeHandle) {
        global::server->removePreSyncCallback(_targetDataCallbackHandle);
    }
}

void SkyBrowserTopic::handleJson(const nlohmann::json& json) {
    const std::string event = json.at("event").get<std::string>();
    if (event == "stop_subscription") {
        _isDone = true;
        return;
    }

    if (event != "start_subscription") {
        _isDone = true;
        return;
    }

    _targetDataCallbackHandle = global::server->addPreSyncCallback(
        [this]() {
            const auto now = std::chrono::system_clock::now();
            if (now - _lastUpdateTime > _skyBrowserUpdateTime) {
                sendBrowserData();
                _lastUpdateTime = std::chrono::system_clock::now();
            }
        }
    );
}

bool SkyBrowserTopic::isDone() const {
    return _isDone;
}

Schema SkyBrowserTopic::Schema() {
    nlohmann::json schema = nlohmann::json::parse(R"(
        {
          "$defs": {
            "DisplayCopy": {
              "type": "object",
              "properties": {
                "position": {
                  "type": "array",
                  "items": { "type": "number" },
                  "minItems": 3,
                  "maxItems": 3
                },
                "show": { "type": "boolean" },
                "idShowProperty": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["position", "show", "idShowProperty"]
            },
            "SkyBrowser": {
              "type": "object",
              "properties": {
                "id": { "type": "string" },
                "targetId": { "type": "string" },
                "name": { "type": "string" },
                "ra": { "type": "number" },
                "dec": { "type": "number" },
                "cartesianDirection": {
                  "type": "array",
                  "items": { "type": "number" },
                  "minItems": 3,
                  "maxItems": 3
                },
                "selectedImages": {
                  "type": "array",
                  "items": { "type": "integer" }
                },
                "fov": { "type": "number" },
                "roll": { "type": "number" },
                "isFacingCamera": { "type": "boolean" },
                "isUsingRae": { "type": "boolean" },
                "scale": { "type": "number" },
                "ratio": { "type": "number" },
                "borderRadius": { "type": "number" },
                "opacities": {
                  "type": "array",
                  "items": { "type": "number" }
                },
                "color": {
                  "type": "array",
                  "items": { "type": "integer" },
                  "minItems": 3,
                  "maxItems": 3
                },
                "displayCopies": {
                  "type": "object",
                  "additionalProperties": { "$ref": "#/$defs/DisplayCopy" }
                }
              },
              "additionalProperties": false,
              "required": [
                "id",
                "targetId",
                "name",
                "ra",
                "dec",
                "cartesianDirection",
                "selectedImages",
                "fov",
                "roll",
                "isFacingCamera",
                "isUsingRae",
                "scale",
                "ratio",
                "borderRadius",
                "opacities",
                "color",
                "displayCopies"
              ]
            }
          },
          "title": "SkyBrowserTopic",
          "type": "object",
          "properties": {
            "topicId": { "const": "skybrowser" },
            "topicPayload": {
              "type": "object",
              "anyOf": [
                {
                  "type": "object",
                  "properties": {
                    "event": { "const": "start_subscription" }
                  },
                  "additionalProperties": false,
                  "required": ["event"]
                },
                {
                  "type": "object",
                  "properties": {
                    "event": { "const": "stop_subscription" }
                  },
                  "additionalProperties": false,
                  "required": ["event"]
                }
              ]
            },
            "data": {
              "type": "object",
              "properties": {
                "selectedBrowserId": { "type": "string" },
                "cameraInSolarSystem": { "type": "boolean" },
                "browsers": {
                  "type": "object",
                  "additionalProperties": { "$ref": "#/$defs/SkyBrowser" }
                }
              },
              "additionalProperties": false,
              "required": ["selectedBrowserId", "cameraInSolarSystem", "browsers"]
            }
          },
          "additionalProperties": false,
          "required": ["topicId", "topicPayload", "data"]
        }
    )");

    return { "skybrowsertopic", schema };
}

void SkyBrowserTopic::sendBrowserData() {
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
        sendData(jsonData);
    }

    // @TODO (2022-04-28, emmbr) The message is still sent very often; every time the
    // camera moves or the time is changes, because this changes the "roll" parameter of
    // the browser. This is the update that occurs most often. Maybe it could be separated
    // into it's own topic?

    _lastUpdateJsonString = jsonString;
}

} // namespace openspace
