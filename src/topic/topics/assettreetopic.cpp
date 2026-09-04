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

#include <openspace/topic/topics/assettreetopic.h>

#include <openspace/documentation/schema.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/scene/assetmanager.h>

namespace {
    std::string stateToString(openspace::EventAssetLoading::State state) {
        // @TODO (anden88 2026-08-19): The creation of an event and passing it to params
        // is just to get the state value in text. Should we use a local toString function
        // instead?
        openspace::EventAssetLoading e("", state);
        ghoul::Dictionary params = toParameter(e);
        return params.value<std::string>("State");
    }
} // namespace

namespace openspace {

AssetTreeTopic::~AssetTreeTopic() {
    if (_subscriptionId.has_value()) {
        global::openSpaceEngine->assetManager().unsubscribeAssetTree(*_subscriptionId);
    }
}

void AssetTreeTopic::handleJson(const nlohmann::json& json) {
    const std::string event = json.at("event").get<std::string>();

    if (event == "start_subscription") {
        _subscriptionId = global::openSpaceEngine->assetManager().subscribeAssetTree(
            [this](const AssetManager::AssetTreeChange& change) {
                handleChange(change);
            }
        );
        sendFullSnapshot();
    }
    else if (event == "stop_subscription" && _subscriptionId.has_value()) {
        global::openSpaceEngine->assetManager().unsubscribeAssetTree(*_subscriptionId);
        _subscriptionId.reset();
    }
    else if (event == "scan_assets" && _subscriptionId.has_value()) {
        global::openSpaceEngine->assetManager().rescanAssetPaths();
    }
}

bool AssetTreeTopic::isDone() const {
    return !_subscriptionId.has_value();
}

Schema AssetTreeTopic::Schema() {
    nlohmann::json schema = nlohmann::json::parse(R"(
        {
          "$defs": {
            "PathList": {
              "type": "object",
              "properties": {
                "type": { "const": "pathList" },
                "category": {
                  "type": "string",
                  "enum": ["shipped", "user", "other", "rootAssets"]
                },
                "paths": {
                  "type": "array",
                  "items": { "type": "string" }
                }
              },
              "additionalProperties": false,
              "required": ["type", "category", "paths"]
            },
            "StateSnapshot": {
              "type": "object",
              "properties": {
                "type": { "const": "stateSnapshot" },
                "states": {
                  "type": "object",
                  "additionalProperties": {
                    "$ref": "eventtopic.json#/$defs/AssetLoadingEventData/properties/State"
                  }
                }
              },
              "additionalProperties": false,
              "required": ["type", "states"]
            },
            "State": {
              "properties": {
                "type": { "const": "state" },
                "path": { "type": "string" },
                "state": {
                  "$ref": "eventtopic.json#/$defs/AssetLoadingEventData/properties/State"
                }
              },
              "additionalProperties": false,
              "required": ["type", "path", "state"]
            }
          },
          "title": "AssetTreeTopic",
          "type": "object",
          "properties": {
            "topicId": { "const": "assetTree" },
            "topicPayload": {
              "type": "object",
              "properties": {
                "event": {
                  "type": "string",
                  "enum": ["start_subscription", "stop_subscription", "scan_assets"]
                }
              },
              "additionalProperties": false,
              "required": ["event"]
            },
            "data": {
              "type": "object",
              "anyOf": [
                { "$ref": "#/$defs/PathList" },
                { "$ref": "#/$defs/StateSnapshot" },
                { "$ref": "#/$defs/State" }
              ]
            }
          },
          "additionalProperties": false,
          "required": ["topicId", "topicPayload", "data"]
        }

    )");

    return { "assettreetopic", schema };
}

void AssetTreeTopic::sendPathList(std::string_view category,
                                          const std::vector<std::filesystem::path>& paths)
{
    nlohmann::json result = nlohmann::json::array();
    for (const std::filesystem::path& p : paths) {
        result.push_back(p.generic_string());
    }

    nlohmann::json payload;
    payload["type"] = "pathList";
    payload["category"] = category;
    payload["paths"] = result;
    sendData(payload);
}

void AssetTreeTopic::sendFullSnapshot() {
    AssetManager& m = global::openSpaceEngine->assetManager();

    sendPathList("shipped", m.shippedAssetPaths());
    sendPathList("user", m.userAssetPaths());
    sendPathList("other", m.otherAssetPaths());
    sendPathList("rootAssets", m.rootAssetPaths());

    nlohmann::json states;
    for (const auto& [path, state] : m.assetStates()) {
        states[path] = stateToString(state);
    }

    nlohmann::json payload;
    payload["type"] = "stateSnapshot";
    payload["states"] = states;
    sendData(payload);
}

void AssetTreeTopic::handleChange(const AssetManager::AssetTreeChange& change) {
    using Type = AssetManager::AssetTreeChange::Type;
    AssetManager& m = global::openSpaceEngine->assetManager();

    switch (change.type) {
        case Type::Shipped:
            sendPathList("shipped", m.shippedAssetPaths());
            break;
        case Type::User:
            sendPathList("user", m.userAssetPaths());
            break;
        case Type::Other:
            sendPathList("other", m.otherAssetPaths());
            break;
        case Type::RootAssets:
            sendPathList("rootAssets", m.rootAssetPaths());
            break;
        case Type::State: {
            nlohmann::json payload;
            payload["type"] = "state";
            payload["path"] = change.statePath;
            payload["state"] = stateToString(change.state);
            sendData(payload);
            break;
        }
    }
}

} // namespace openspace
