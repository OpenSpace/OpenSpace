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

#include <openspace/topic/topics/propertytreetopic.h>

#include <openspace/documentation/schema.h>
#include <openspace/engine/globals.h>
#include <openspace/properties/property.h>
#include <openspace/query/query.h>
#include <openspace/topic/connection.h>
#include <openspace/topic/jsonconverters.h>
#include <openspace/util/timemanager.h>
#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr std::string_view _loggerCat = "PropertyTreeTopic";
} // namespace

using nlohmann::json;

namespace openspace {

PropertyTreeTopic::~PropertyTreeTopic() {}

bool PropertyTreeTopic::isDone() const {
    return !_isSubscribedTo;
}

std::string PropertyTreeTopic::type() const {
    return "propertyTree";
}

void PropertyTreeTopic::handleJson(const nlohmann::json& json) {
    const std::string& event = json.at("event").get<std::string>();

    if (event == "start_subscription") {
        _isSubscribedTo = true;
    }
    if (event == "stop_subscription") {
        _isSubscribedTo = false;
    }
}

Schema PropertyTreeTopic::Schema() {
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
          "title": "PropertyTreeTopic",
          "type": "object",
          "properties": {
            "topicId": { "const": "propertyTree" },
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
              "anyOf": [
                {
                  "type": "object",
                  "properties": {
                    "value": { "$ref": "#/$defs/JsonValue" },
                    "uri": {
                      "type": "string",
                      "description": "The URI of the property that this value corresponds to"
                    }
                  },
                  "additionalProperties": false,
                  "required": ["value", "uri"]
                },
                {
                  "type": "object",
                  "properties": {
                    "metaData": { "$ref": "properties.json#/$defs/AnyPropertyMetaData" },
                    "uri": {
                      "type": "string",
                      "description": "The URI of the property that this value corresponds to"
                    }
                  },
                  "additionalProperties": false,
                  "required": ["metaData", "uri"]
                }
              ]
            }
          },
          "additionalProperties": false,
          "required": ["topicId", "topicPayload", "data"]
        }
    )");

    return { "propertytreetopic", schema };
}

} // namespace openspace
