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

#include <openspace/topic/topics/cameratopic.h>

#ifdef OPENSPACE_MODULE_SPACE_ENABLED
#include <modules/globebrowsing/globebrowsingmodule.h>
#endif // OPENSPACE_MODULE_SPACE_ENABLED
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/globals.h>
#include <openspace/documentation/schema.h>
#include <openspace/topic/connection.h>
#include <openspace/topic/server.h>
#include <openspace/util/distanceconversion.h>
#include <openspace/util/geodetic.h>
#include <string_view>
#include <utility>

namespace openspace {
CameraTopic::CameraTopic()
    : _lastUpdateTime(std::chrono::system_clock::now())
{}

CameraTopic::~CameraTopic() {
    if (_dataCallbackHandle != UnsetOnChangeHandle) {
        global::server->removePreSyncCallback(_dataCallbackHandle);
    }
}

bool CameraTopic::isDone() const {
    return _isDone;
}

void CameraTopic::handleJson(const nlohmann::json& json) {
    const std::string event = json.at("event").get<std::string>();

    if (event != "start_subscription") {
        _isDone = true;
        return;
    }

    _dataCallbackHandle = global::server->addPreSyncCallback(
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
    glm::dvec3 position = geoPositionFromCamera();
    glm::dvec3 direction = geoViewFromCamera();
    const double viewLength = direction.z;
    std::pair<double, std::string_view> altSimplified = simplifyDistance(position.z);
    glm::dvec2 subSolar = subSolarCoordinates();

    glm::dvec2 dir = glm::dvec2(direction) - glm::dvec2(position);
    if (glm::length(dir) > 1e-6) {
        // Avoid sending NaNs/null from bad normalization
        dir = glm::normalize(dir);
    }

    nlohmann::json jsonData = {
        { "latitude", position.x },
        { "longitude", position.y },
        { "altitude", altSimplified.first },
        { "altitudeUnit", altSimplified.second },
        { "altitudeMeters", position.z },
        { "viewLatitude", dir.x },
        { "viewLongitude", dir.y },
        { "viewLength", viewLength },
        { "subSolarLatitude", subSolar.x },
        { "subSolarLongitude", subSolar.y },
    };

    _connection->sendJson(wrappedPayload(jsonData));
}

Schema CameraTopic::Schema() {
    nlohmann::json schema = nlohmann::json::parse(R"(
        {
          "title": "CameraTopic",
          "type": "object",
          "properties": {
            "topicId": { "const": "camera" },
            "topicPayload": {
              "type": "object",
              "properties": {
                "event": { "const": "start_subscription" }
              },
              "additionalProperties": false,
              "required": ["event"]
            },
            "data": {
              "type": "object",
              "properties": {
                "altitude": { "type": "number" },
                "altitudeMeters": { "type": "number" },
                "altitudeUnit": {
                  "type": "string",
                  "enum": [
                    "nanometer",
                    "nanometers",
                    "micrometer",
                    "micrometers",
                    "millimeter",
                    "millimeters",
                    "meter",
                    "meters",
                    "Gigaparsec",
                    "Gigaparsecs",
                    "Megaparsec",
                    "Megaparsecs",
                    "Kiloparsec",
                    "Kiloparsecs",
                    "Parsec",
                    "Parsecs",
                    "Lightyear",
                    "Lightyears",
                    "Lightmonth",
                    "Lightmonths",
                    "Lightday",
                    "Lightdays",
                    "Lighthour",
                    "Lighthours",
                    "AU",
                    "km"
                  ]
                },
                "latitude": { "type": "number" },
                "longitude": { "type": "number" },
                "subSolarLatitude": { "type": "number" },
                "subSolarLongitude": { "type": "number" },
                "viewLatitude": { "type": "number" },
                "viewLength": { "type": "number" },
                "viewLongitude": { "type": "number" }
              },
              "additionalProperties": false,
              "required": [
                "latitude",
                "longitude",
                "altitude",
                "altitudeUnit",
                "altitudeMeters",
                "viewLatitude",
                "viewLongitude",
                "viewLength",
                "subSolarLatitude",
                "subSolarLongitude"
              ]
            }
          },
          "additionalProperties": false,
          "required": ["topicId", "topicPayload", "data"]
        }
    )");

    return { "cameratopic", schema };
}

} // namespace openspace
