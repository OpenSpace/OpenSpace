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

#include <openspace/topic/topics/subscriptiontopic.h>

#include <openspace/documentation/schema.h>
#include <openspace/properties/property.h>
#include <openspace/query/query.h>
#include <openspace/topic/connection.h>
#include <openspace/topic/jsonconverters.h>
#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <string_view>

using nlohmann::json;

namespace {
    constexpr std::string_view _loggerCat = "SubscriptionTopic";
} // namespace

namespace openspace {

SubscriptionTopic::~SubscriptionTopic() {
    resetCallbacks();
}

void SubscriptionTopic::handleJson(const nlohmann::json& json) {
    const std::string& event = json.at("event").get<std::string>();

    if (event == "start_subscription") {
        std::string uri = json.at("uri").get<std::string>();

        _prop = property(uri);
        resetCallbacks();

        if (_prop) {
            _requestedResourceIsSubscribable = true;
            _isSubscribedTo = true;
            auto onChange = [this, k = uri]() {
                nlohmann::json payload;
                payload["value"] = json::parse(_prop->jsonValue());
                sendData(payload);
            };

            auto onMetaDataChange = [this, k = uri]() {
                nlohmann::json payload;
                payload["metaData"] = _prop->generateJsonDescription();
                sendData(payload);
            };

            _onChangeHandle = _prop->onChange(onChange);
            _onMetaDataChangeHandle = _prop->onMetaDataChange(onMetaDataChange);
            _onDeleteHandle = _prop->onDelete([this]() {
                _onChangeHandle = UnsetCallbackHandle;
                _onMetaDataChangeHandle = UnsetCallbackHandle;
                _onDeleteHandle = UnsetCallbackHandle;
                _isSubscribedTo = false;
            });

            // Immediately send the meta data and value
            onMetaDataChange();
            onChange();
        }
        else {
            LWARNING(std::format("Could not subscribe. Property '{}' not found", uri));
        }
    }
    if (event == "stop_subscription") {
        _isSubscribedTo = false;
        resetCallbacks();
    }
}

bool SubscriptionTopic::isDone() const {
    return !_requestedResourceIsSubscribable || !_isSubscribedTo;
}

Schema SubscriptionTopic::Schema() {
    nlohmann::json schema = nlohmann::json::parse(R"(
        {
          "$defs": {
            "JsonValue": {
              "anyOf": [
                { "type": "string" },
                { "type": "number" },
                { "type": "boolean" },
                {
                  "type": "array",
                  "items": { "$ref": "#/$defs/JsonValue" }
                },
                {
                  "type": "object",
                  "additionalProperties": { "$ref": "#/$defs/JsonValue" }
                },
                { "type": "null" }
              ]
            }
          },
          "title": "SubscriptionTopic",
          "type": "object",
          "properties": {
            "topicId": { "const": "subscribe" },
            "topicPayload": {
              "type": "object",
              "anyOf": [
                {
                  "type": "object",
                  "properties": {
                    "event": { "const": "start_subscription" },
                    "uri": {
                      "type": "string",
                      "description": "The URI of the property to subscribe to"
                    }
                  },
                  "additionalProperties": false,
                  "required": ["event", "uri"]
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
              "anyOf": [
                {
                  "type": "object",
                  "properties": {
                    "value": { "$ref": "#/$defs/JsonValue" }
                  },
                  "additionalProperties": false,
                  "required": ["value"]
                },
                {
                  "type": "object",
                  "properties": {
                    "metaData": { "$ref": "properties.json#/$defs/AnyPropertyMetaData" }
                  },
                  "additionalProperties": false,
                  "required": ["metaData"]
                }
              ]
            }
          },
          "additionalProperties": false,
          "required": ["topicId", "topicPayload", "data"]
        }
    )");

    return { "subscriptiontopic", schema };
}

void SubscriptionTopic::resetCallbacks() {
    if (!_prop) {
        return;
    }
    if (_onChangeHandle != UnsetCallbackHandle) {
        _prop->removeOnChange(_onChangeHandle);
        _onChangeHandle = UnsetCallbackHandle;
    }
    if (_onMetaDataChangeHandle != UnsetCallbackHandle) {
        _prop->removeOnMetaDataChange(_onMetaDataChangeHandle);
        _onMetaDataChangeHandle = UnsetCallbackHandle;
    }
    if (_onDeleteHandle != UnsetCallbackHandle) {
        _prop->removeOnDelete(_onDeleteHandle);
        _onDeleteHandle = UnsetCallbackHandle;
    }
}

} // namespace openspace
