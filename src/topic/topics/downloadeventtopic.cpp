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

#include <openspace/topic/topics/downloadeventtopic.h>

#include <openspace/documentation/schema.h>
#include <openspace/engine/globals.h>
#include <openspace/topic/connection.h>
#include <openspace/util/downloadeventengine.h>
#include <ghoul/format.h>
#include <string_view>

namespace {
    constexpr std::chrono::milliseconds CallbackUpdateInterval(250);
} // namespace

namespace openspace {

DownloadEventTopic::~DownloadEventTopic() {
    if (_isSubscribedTo) {
        global::downloadEventEngine->unsubscribe(_subscriptionID);
        _isSubscribedTo = false;
    }
}

void DownloadEventTopic::handleJson(const nlohmann::json& json) {
    const std::string& event = json.at("event").get<std::string>();

    if (event == "start_subscription") {
        _isSubscribedTo = true;

        auto callback = [this](const DownloadEventEngine::DownloadEvent& e) {
            // Limit how often we send data to frontend to reduce traffic
            if (e.type == DownloadEventEngine::DownloadEvent::Type::Progress) {
                const auto now = std::chrono::steady_clock::now();
                auto& last = _lastCallback[e.id];

                if (now - last >= CallbackUpdateInterval) {
                    last = now;
                }
                else {
                    return;
                }
            }

            nlohmann::json payload;
            payload["type"] = e.type;
            payload["id"] = e.id;
            payload["downloadedBytes"] = e.downloadedBytes;
            if (e.totalBytes.has_value()) {
                payload["totalBytes"] = e.totalBytes.value();
            }

            sendData(payload);
        };
        _subscriptionID = global::downloadEventEngine->subscribe(callback);
    }

    else if (event == "stop_subscription") {
        global::downloadEventEngine->unsubscribe(_subscriptionID);
        _isSubscribedTo = false;
    }
}

bool DownloadEventTopic::isDone() const {
    return !_isSubscribedTo;
}

Schema DownloadEventTopic::Schema() {
    nlohmann::json schema = nlohmann::json::parse(R"(
        {
          "$defs": {
            "DownloadType": {
              "type": "integer",
              "enum": [0, 1, 2, 3],
              "tsEnumNames": ["Started", "Progress", "Finished", "Failed"]
            }
          },
          "title": "DownloadEventTopic",
          "type": "object",
          "properties": {
            "topicId": { "const": "downloadEvent" },
            "topicPayload": {
              "type": "object",
              "properties": {
                "event": {
                  "type": "string",
                  "enum": ["start_subscription", "stop_subscription"]
                }
              },
              "additionalProperties": false,
              "required": ["event"]
            },
            "data": {
              "type": "object",
              "properties": {
                "type": { "$ref": "#/$defs/DownloadType" },
                "id": { "type": "string" },
                "downloadedBytes": { "type": "integer" },
                "totalBytes": { "type": "integer" }
              },
              "additionalProperties": false,
              "required": ["type", "id", "downloadedBytes"]
            }
          },
          "additionalProperties": false,
          "required": ["topicId", "topicPayload", "data"]
        }
    )");

    return { "downloadeventtopic", schema };
}

} // namespace openspace
