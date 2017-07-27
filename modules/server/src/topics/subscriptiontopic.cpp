/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include "include/subscriptiontopic.h"

namespace {
std::string _loggerCat = "SubscriptionTopic";
}

namespace openspace {

SubscriptionTopic::SubscriptionTopic()
        : Topic()
        , _requestedResourceIsSubscribable(false) {
    LDEBUG("Starting new subscription");
}

SubscriptionTopic::~SubscriptionTopic() {
    // TODO: remove post draw call
}

bool SubscriptionTopic::isDone() {
    return !_requestedResourceIsSubscribable || !_connection->active;
}

void SubscriptionTopic::handleJson(nlohmann::json json) {
    std::string requestedKey = json.at("subscriptionProperty").get<std::string>();
    std::string initial = requestedKey.substr(0, 8);

    if (initial == "special:") {
        std::string key = requestedKey.substr(8);
        handleSpecialCase(key);
    } else {
        // trigger regular subscription
    }
}

void SubscriptionTopic::handleSpecialCase(std::string instruction) {
    if (instruction == "currentTime") {
        _requestedResourceIsSubscribable = true;
        _connection->addRefreshCall([this]() {
            nlohmann::json timeJson = {{ "time", OsEng.timeManager().time().ISO8601() }};
            return wrappedPayload(timeJson);
        });
    }
    else {
        LERROR("Unknown special case: " << instruction);
        _requestedResourceIsSubscribable = false;
    }
}

} // namespace openspace
