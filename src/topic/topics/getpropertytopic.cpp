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

#include <openspace/topic/topics/getpropertytopic.h>

#include <openspace/documentation/schema.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/network/parallelpeer.h>
#include <openspace/query/query.h>
#include <openspace/rendering/luaconsole.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/screenspacerenderable.h>
#include <openspace/scene/scene.h>
#include <openspace/topic/connection.h>
#include <openspace/topic/jsonconverters.h>
#include <ghoul/logging/logmanager.h>

using nlohmann::json;

namespace {
    constexpr std::string_view _loggerCat = "GetPropertyTopic";
} // namespace

namespace openspace {

void GetPropertyTopic::handleJson(const nlohmann::json& json) {
    ZoneScoped;

    const std::string requestedKey = json.at("property").get<std::string>();
    ZoneText(requestedKey.c_str(), requestedKey.size());
    LDEBUG(std::format("Getting property '{}'...", requestedKey));
    nlohmann::json response;

    if (requestedKey == "__rootOwner") {
        response = global::rootPropertyOwner;
    }
    else {
        response = propertyFromKey(requestedKey);
    }
    sendData(response);
}

bool GetPropertyTopic::isDone() const {
    return true;
}

Schema GetPropertyTopic::Schema() {
    nlohmann::json schema = nlohmann::json::parse(R"(
        {
          "title": "GetPropertyTopic",
          "type": "object",
          "properties": {
            "topicId": { "const": "get" },
            "topicPayload": {
              "type": "object",
              "properties": {
                "property": {
                  "anyOf": [{ "const": "__rootOwner" }, { "type": "string" }]
                }
              },
              "additionalProperties": false,
              "required": ["property"]
            },
            "data": {
              "anyOf": [
                {
                  "type": "object",
                  "properties": {
                    "propertyOwner": {
                      "$ref": "properties.json#/$defs/PropertyOwner"
                    }
                  },
                  "additionalProperties": false,
                  "required": ["propertyOwner"]
                },
                {
                  "type": "object",
                  "properties": {
                    "property": { "$ref": "properties.json#/$defs/AnyProperty" }
                  },
                  "additionalProperties": false,
                  "required": ["property"]
                }
              ]
            }
          },
          "additionalProperties": false,
          "required": ["topicId", "topicPayload", "data"]
        }
    )");

    return { "getpropertytopic", schema };
}

json GetPropertyTopic::propertyFromKey(const std::string& key) {
    Property* prop = property(key);
    if (prop) {
        return prop;
    }
    PropertyOwner* node = propertyOwner(key);
    if (node) {
        return node;
    }

    throw ghoul::RuntimeError(std::format("Property '{}' not found", key));
}

} // namespace openspace
