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

#include <openspace/query/query.h>
#include <openspace/properties/property.h>
#include "include/subscriptiontopic.h"

namespace {
const std::string _loggerCat = "SubscriptionTopic";
const int UNSET_ONCHANGE_HANDLE = -1;
}

using nlohmann::json;

namespace openspace {

SubscriptionTopic::SubscriptionTopic()
        : Topic()
        , _requestedResourceIsSubscribable(false)
        , _onChangeHandle(-1) {
    LDEBUG("Starting new subscription");
}

SubscriptionTopic::~SubscriptionTopic() {
    // TODO: remove post draw call

    if (_onChangeHandle != UNSET_ONCHANGE_HANDLE) {
        _prop->removeOnChange(_onChangeHandle);
    }
}

bool SubscriptionTopic::isDone() {
    return !_requestedResourceIsSubscribable || !_connection->active;
}

void SubscriptionTopic::handleJson(json j) {
    std::string requestedKey = j.at("subscriptionProperty").get<std::string>();
    std::string initial = requestedKey.substr(0, 8);

    LDEBUG("Subscribing to property '" + requestedKey + "'...");

    if (initial == "special:") {
        std::string key = requestedKey.substr(8);
        handleSpecialCase(key);
    } else {
        startSubscription(requestedKey);
    }
}

void SubscriptionTopic::handleSpecialCase(std::string instruction) {
    if (instruction == "currentTime") {
        _requestedResourceIsSubscribable = true;
        _connection->addRefreshCall([this]() {
            json timeJson = {{ "time", OsEng.timeManager().time().ISO8601() }};
            return wrappedPayload(timeJson);
        });
    }
    else {
        LERROR("Unknown special case: " << instruction);
    }
}

void SubscriptionTopic::startSubscription(const std::string &key) {
    _prop = property(key);
    if (_prop != nullptr) {
        _requestedResourceIsSubscribable = true;
        auto onChange = [this, key]() {
            LDEBUG("Updating subscription '" + key + "'.");
            json payload = json::parse(_prop->toJson());
            _connection->sendJson(wrappedPayload(payload));
        };
        _onChangeHandle = _prop->onChange(onChange);
        onChange();
    } else {
        LWARNING("Could not subscribe. Property '" + key + "' not found.");
    }
}

} // namespace openspace
