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

#include <openspace/topic/topics/errorlogtopic.h>

#include <openspace/documentation/schema.h>
#include <openspace/topic/connection.h>
#include <openspace/topic/notificationlog.h>
#include <ghoul/logging/log.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/stringconversion.h>
#include <string_view>
#include <utility>

namespace openspace {

ErrorLogTopic::~ErrorLogTopic() {
    if (_log) {
        ghoul::logging::LogManager::ref().removeLog(_log);
        _log = nullptr;
    }
}

void ErrorLogTopic::handleJson(const nlohmann::json& json) {
    const std::string& event = json.at("event").get<std::string>();

    if (event == "start_subscription") {
        // Check if we got log settings on subscription
        auto settingsJson = json.find("settings");

        Settings settings;
        if (settingsJson != json.end()) {
            if (auto ll = settingsJson->find("logLevel"); ll != settingsJson->end()) {
                std::string level = ll->get<std::string>();
                settings.logLevel = ghoul::from_string<ghoul::logging::LogLevel>(level);
            }

            if (auto ts = settingsJson->find("timeStamping"); ts != settingsJson->end()) {
                bool value = ts->get<bool>();
                settings.timeStamping = value;
            }

            if (auto ds = settingsJson->find("dateStamping"); ds != settingsJson->end()) {
                bool value = ds->get<bool>();
                settings.dateStamping = value;
            }

            auto cs = settingsJson->find("categoryStamping");
            if  (cs != settingsJson->end()) {
                bool value = cs->get<bool>();
                settings.categoryStamping = value;
            }

            auto lls = settingsJson->find("logLevelStamping");
            if (lls != settingsJson->end()) {
                bool value = lls->get<bool>();
                settings.logLevelStamping = value;
            }
        }

        _logSettings = settings;
        _isSubscribedTo = true;
        createLog();
    }

    if (event == "stop_subscription") {
        _isSubscribedTo = false;

        ghoul::logging::LogManager::ref().removeLog(_log);
        _log = nullptr;
    }

    if (event == "update_log_level") {
        ghoul::logging::LogManager::ref().removeLog(_log);
        _log = nullptr;

        std::string level = json.at("logLevel").get<std::string>();
        _logSettings.logLevel = ghoul::from_string<ghoul::logging::LogLevel>(level);
        createLog();
    }
}

void ErrorLogTopic::createLog() {
    if (_log) {
        return;
    }

    auto onLogging = [this](std::string_view timeStamp, std::string_view dateStamp,
                            std::string_view category, ghoul::logging::LogLevel level,
                            std::string_view message)
    {
        nlohmann::json payload;
        payload["message"] = message;

        if (_logSettings.timeStamping) {
            payload["timeStamp"] = timeStamp;
        }

        if (_logSettings.categoryStamping) {
            payload["category"] = category;
        }

        if (_logSettings.dateStamping) {
            payload["dateStamp"] = dateStamp;
        }

        if (_logSettings.logLevelStamping) {
            payload["level"] = ghoul::to_string(level);
        }

        _connection->sendJson(wrappedPayload(std::move(payload)));
    };

    auto log = std::make_unique<NotificationLog>(
        onLogging,
        _logSettings.logLevel
    );
    _log = log.get();
    ghoul::logging::LogManager::ref().addLog(std::move(log));
}

bool ErrorLogTopic::isDone() const {
    return !_isSubscribedTo;
}

Schema ErrorLogTopic::Schema() {
    nlohmann::json schema = nlohmann::json::parse(R"(
        {
          "$defs": {
            "LogLevel": {
              "type": "string",
              "enum": [
                "All",
                "Trace",
                "Debug",
                "Info",
                "Warning",
                "Error",
                "Fatal",
                "None"
              ],
              "tsEnumNames": [
                "All",
                "Trace",
                "Debug",
                "Info",
                "Warning",
                "Error",
                "Fatal",
                "NoLogging"
              ]
            },
            "Settings": {
              "type": "object",
              "properties": {
                "logLevel": { "$ref": "#/$defs/LogLevel" },
                "timeStamping": { "type": "boolean" },
                "dateStamping": { "type": "boolean" },
                "categoryStamping": { "type": "boolean" },
                "logLevelStamping": { "type": "boolean" }
              },
              "additionalProperties": false
            }
          },
          "title": "ErrorLogTopic",
          "type": "object",
          "properties": {
            "topicId": { "const": "errorLog" },
            "topicPayload": {
              "type": "object",
              "anyOf": [
                {
                  "type": "object",
                  "properties": {
                    "event": { "const": "start_subscription" },
                    "settings": { "$ref": "#/$defs/Settings" }
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
                },
                {
                  "type": "object",
                  "properties": {
                    "event": { "const": "update_log_level" },
                    "logLevel": { "$ref": "#/$defs/LogLevel" }
                  },
                  "additionalProperties": false,
                  "required": ["event", "logLevel"]
                }
              ]
            },
            "data": {
              "type": "object",
              "properties": {
                "message": { "type": "string" },
                "timeStamp": { "type": "string" },
                "dateStamp": { "type": "string" },
                "category": { "type": "string" },
                "level": { "$ref": "#/$defs/LogLevel" }
              },
              "additionalProperties": false,
              "required": ["message"]
            }
          },
          "additionalProperties": false,
          "required": ["topicId", "topicPayload", "data"]
        }
    )");

    return {
        "errorlogtopic",
        schema
    };
}


} // namespace openspace
