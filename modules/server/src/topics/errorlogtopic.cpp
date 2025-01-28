/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <modules/server/include/topics/errorlogtopic.h>

#include <ghoul/logging/callbacklog.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr std::string_view StartSubscription = "start_subscription";
    constexpr std::string_view StopSubscription = "stop_subscription";

    constexpr std::string_view SettingsKey = "settings";
    constexpr std::string_view TimeStampingKey = "timeStamping";
    constexpr std::string_view DateStampingKey = "dateStamping";
    constexpr std::string_view CategoryStampingKey = "categoryStamping";
    constexpr std::string_view LogLevelStampingKey = "logLevelStamping";
    constexpr std::string_view LogLevelKey = "logLevel";
} // namespace

namespace openspace {

ErrorLogTopic::~ErrorLogTopic() {
    if (_log) {
        ghoul::logging::LogManager::ref().removeLog(_log);
        _log = nullptr;
    }
}

void ErrorLogTopic::handleJson(const nlohmann::json& json) {
    const std::string& event = json.at("event").get<std::string>();

    if (event == StartSubscription) {
        // Default settings for logging
        auto timeStamping = ghoul::logging::Log::TimeStamping::Yes;
        auto dateStamping = ghoul::logging::Log::DateStamping::Yes;
        auto categoryStamping = ghoul::logging::Log::CategoryStamping::Yes;
        auto logLevelStamping = ghoul::logging::Log::LogLevelStamping::Yes;
        auto logLevel = ghoul::logging::LogLevel::AllLogging;

        // Check if we got log settings on subscription
        auto settings = json.find(SettingsKey);

        if (settings != json.end()) {
            if (auto ts = settings->find(TimeStampingKey); ts != settings->end()) {
                timeStamping = ghoul::logging::Log::TimeStamping(ts->get<bool>()).value;
            }

            if (auto ds = settings->find(DateStampingKey); ds != settings->end()) {
                dateStamping = ghoul::logging::Log::DateStamping(ds->get<bool>()).value;
            }

            if (auto cs = settings->find(CategoryStampingKey); cs != settings->end()) {
                categoryStamping = ghoul::logging::Log::CategoryStamping(
                    cs->get<bool>()).value;
            }

            if(auto lls = settings->find(LogLevelStampingKey); lls != settings->end()){
                logLevelStamping = ghoul::logging::Log::LogLevelStamping(
                    lls->get<bool>()).value;
            }

            if (auto ls = settings->find(LogLevelKey); ls != settings->end()) {
                std::string level = ls->get<std::string>();
                logLevel = ghoul::from_string<ghoul::logging::LogLevel>(level);
            }

        }

        auto onLogging = [this](std::string message) {
            _connection->sendJson(wrappedPayload(std::move(message)));
        };

        auto log = std::make_unique<ghoul::logging::CallbackLog>(
            onLogging,
            timeStamping,
            dateStamping,
            categoryStamping,
            logLevelStamping,
            logLevel
        );
        _log = log.get();

        ghoul::logging::LogManager::ref().addLog(std::move(log));
        _isSubscribedTo = true;
    }

    if (event == StopSubscription) {
        _isSubscribedTo = false;

        ghoul::logging::LogManager::ref().removeLog(_log);
        _log = nullptr;
    }
}

bool ErrorLogTopic::isDone() const {
    return !_isSubscribedTo;
}

} // namespace openspace
