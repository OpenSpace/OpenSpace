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

#include <modules/server/include/topics/downloadeventtopic.h>

#include <modules/sync/downloadeventengine.h>
#include <openspace/engine/globals.h>
#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr std::string_view _loggerCat = "DownloadEventTopic";
    constexpr std::string_view StartSubscription = "start_subscription";
    constexpr std::string_view StopSubscription = "stop_subscription";
} // namespace

namespace openspace {

DownloadEventTopic::~DownloadEventTopic() {
    if (_isSubscribedTo) {
        global::downloadEventEngine->unsubscribe(_subscriptionID);
        _isSubscribedTo = false;
    }
}

bool DownloadEventTopic::isDone() const {
    return !_isSubscribedTo;
}

void DownloadEventTopic::handleJson(const nlohmann::json& json) {
    const std::string& event = json.at("event").get<std::string>();


    if (event == StartSubscription) {
        _isSubscribedTo = true;

        auto callback = [this](const DownloadEventEngine::DownloadEvent& event) {
            nlohmann::json payload = {};

            payload["type"] = event.type;
            payload["id"] = event.id;
            payload["downloadedBytes"] = event.downloadedBytes;
            if (event.totalBytes.has_value()) {
                payload["totalBytes"] = event.totalBytes.value();
            }

            LINFO(std::format("Sending package: id: {}, downloadedBytes: {}, totalBytes: {}",
                event.id, event.downloadedBytes, event.totalBytes.value_or(0))
            );

            _connection->sendJson(wrappedPayload(payload));
        };
        _subscriptionID = global::downloadEventEngine->subscribe(callback);
    }

    else if (event == StopSubscription) {
        global::downloadEventEngine->unsubscribe(_subscriptionID);
        _isSubscribedTo = false;
    }
}

} // namespace openspace
