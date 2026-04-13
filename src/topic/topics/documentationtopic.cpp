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

#include <openspace/topic/topics/documentationtopic.h>

#include <openspace/documentation/schema.h>
#include <openspace/engine/globals.h>
#include <openspace/documentation/documentationengine.h>
#include <openspace/topic/connection.h>
#include <openspace/topic/jsonconverters.h>

namespace openspace {

void DocumentationTopic::handleJson(const nlohmann::json& json) {
    const std::string requestedType = json.at("type").get<std::string>();

    nlohmann::json response;

    if (requestedType == "lua") {
        response = DocEng.generateScriptEngineJson();
    }
    else if (requestedType == "factories") {
        response = DocEng.generateFactoryManagerJson();
    }
    else if (requestedType == "keyboard") {
        response = DocEng.generateKeybindingsJson();
    }
    else if (requestedType == "asset") {
        response = DocEng.generatePropertyOwnerJson(global::rootPropertyOwner);
    }
    else if (requestedType == "meta") {
        response = DocEng.generateLicenseListJson();
    }

    _connection->sendJson(wrappedPayload(response));
}

bool DocumentationTopic::isDone() const {
    return true;
}

Schema DocumentationTopic::Schema() {
    nlohmann::json schema = nlohmann::json::parse(R"(
        {
          "$defs": {
            "LuaArgument": {
              "type": "object",
              "properties": {
                "name": { "type": "string" },
                "type": { "type": "string" },
                "defaultValue": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["name", "type", "defaultValue"]
            },
            "SourceLocation": {
              "type": "object",
              "properties": {
                "file": { "type": "string" },
                "line": { "type": "integer" }
              },
              "additionalProperties": false,
              "required": ["file", "line"]
            },
            "LuaFunction": {
              "type": "object",
              "properties": {
                "name": { "type": "string" },
                "arguments": {
                  "type": "array",
                  "items": { "$ref": "#/$defs/LuaArgument" }
                },
                "returnType": { "type": "string" },
                "help": { "type": "string" },
                "sourceLocation": { "$ref": "#/$defs/SourceLocation" }
              },
              "additionalProperties": false,
              "required": ["name", "arguments", "returnType", "help"]
            },
            "LuaLibrary": {
              "type": "object",
              "properties": {
                "library": { "type": "string" },
                "name": { "type": "string" },
                "fullName": { "type": "string" },
                "functions": {
                  "type": "array",
                  "items": { "$ref": "#/$defs/LuaFunction" }
                }
              },
              "additionalProperties": false,
              "required": ["library", "name", "fullName", "functions"]
            },
            "DocumentationMember": {
              "type": "object",
              "properties": {
                "name": { "type": "string" },
                "optional": { "type": "boolean" },
                "type": { "type": "string" },
                "documentation": { "type": "string" },
                "description": { "type": "string" },
                "members": {
                  "type": "array",
                  "items": { "$ref": "#/$defs/DocumentationMember" }
                },
                "reference": {
                  "type": "object",
                  "properties": {
                    "found": { "type": "boolean" },
                    "name": { "type": "string" },
                    "identifier": { "type": "string" }
                  },
                  "additionalProperties": false,
                  "required": ["found"]
                }
              },
              "additionalProperties": false,
              "required": ["name", "optional", "type", "documentation", "members"]
            },
            "FactoryClass": {
              "type": "object",
              "properties": {
                "name": { "type": "string" },
                "identifier": { "type": "string" },
                "description": { "type": "string" },
                "members": {
                  "type": "array",
                  "items": { "$ref": "#/$defs/DocumentationMember" }
                }
              },
              "additionalProperties": false,
              "required": ["name", "identifier", "members"]
            },
            "Factory": {
              "type": "object",
              "properties": {
                "name": { "type": "string" },
                "identifier": { "type": "string" },
                "classes": {
                  "type": "array",
                  "items": { "$ref": "#/$defs/FactoryClass" }
                }
              },
              "additionalProperties": false,
              "required": ["name", "identifier", "classes"]
            },
            "Keybind": {
              "type": "object",
              "properties": {
                "name": { "type": "string" },
                "action": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["name", "action"]
            },
            "KeyboardData": {
              "type": "object",
              "properties": {
                "name": { "const": "Keybindings" },
                "keybindings": {
                  "type": "array",
                  "items": { "$ref": "#/$defs/Keybind" }
                }
              },
              "additionalProperties": false,
              "required": ["name", "keybindings"]
            },
            "LicenseEntry": {
              "type": "object",
              "properties": {
                "name": { "type": "string" },
                "version": { "type": "string" },
                "description": { "type": "string" },
                "author": { "type": "string" },
                "url": { "type": "string" },
                "license": { "type": "string" },
                "identifiers": {
                  "type": "array",
                  "items": { "type": "string" }
                },
                "path": { "type": "string" }
              },
              "additionalProperties": false,
              "required": [
                "name",
                "version",
                "description",
                "author",
                "url",
                "license",
                "identifiers",
                "path"
              ]
            },
            "DocPropertyEntry": {
              "type": "object",
              "properties": {
                "name": { "type": "string" },
                "type": { "type": "string" },
                "uri": { "type": "string" },
                "identifier": { "type": "string" },
                "description": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["name", "type", "uri", "identifier", "description"]
            },
            "DocPropertyOwner": {
              "type": "object",
              "properties": {
                "name": { "type": "string" },
                "description": { "type": "string" },
                "type": { "type": "string" },
                "tags": {
                  "type": "array",
                  "items": { "type": "string" }
                },
                "properties": {
                  "type": "array",
                  "items": { "$ref": "#/$defs/DocPropertyEntry" }
                },
                "propertyOwners": {
                  "type": "array",
                  "items": { "$ref": "#/$defs/DocPropertyOwner" }
                }
              },
              "additionalProperties": false,
              "required": [
                "name",
                "description",
                "type",
                "tags",
                "properties",
                "propertyOwners"
              ]
            },
            "AssetData": {
              "type": "object",
              "properties": {
                "name": { "const": "propertyOwner" },
                "data": {
                  "type": "array",
                  "items": { "$ref": "#/$defs/DocPropertyOwner" }
                }
              },
              "additionalProperties": false,
              "required": ["name", "data"]
            }
          },
          "title": "DocumentationTopic",
          "type": "object",
          "properties": {
            "topicId": { "const": "documentation" },
            "topicPayload": {
              "type": "object",
              "properties": {
                "type": {
                  "type": "string",
                  "enum": ["lua", "factories", "keyboard", "asset", "meta"]
                }
              },
              "additionalProperties": false,
              "required": ["type"]
            },
            "data": {
              "oneOf": [
                {
                  "description": "Response to lua",
                  "type": "array",
                  "items": { "$ref": "#/$defs/LuaLibrary" }
                },
                {
                  "description": "Response to factories",
                  "type": "array",
                  "items": { "$ref": "#/$defs/Factory" }
                },
                {
                  "description": "Response to keyboard",
                  "$ref": "#/$defs/KeyboardData"
                },
                {
                  "description": "Response to asset",
                  "$ref": "#/$defs/AssetData"
                },
                {
                  "description": "Response to meta",
                  "type": "array",
                  "items": { "$ref": "#/$defs/LicenseEntry" }
                }
              ]
            }
          },
          "additionalProperties": false,
          "required": ["topicId", "topicPayload", "data"]
        }
    )");

    return { "documentationtopic", schema };
}

} // namespace openspace
