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
#include <modules/server/include/jsonconverters.h>
#include "include/subscriptiontopic.h"

namespace {
const char* _loggerCat = "SubscriptionTopic";
const char* PropertyKey = "property";
const int UNSET_ONCHANGE_HANDLE = -1;
}

using nlohmann::json;

namespace openspace {

    SubscriptionTopic::SubscriptionTopic()
        : Topic()
        , _requestedResourceIsSubscribable(false)
        , _onChangeHandle(-1) {}

SubscriptionTopic::~SubscriptionTopic() {
    if (_onChangeHandle != UNSET_ONCHANGE_HANDLE) {
        _prop->removeOnChange(_onChangeHandle);
    }
}

bool SubscriptionTopic::isDone() {
    return !_requestedResourceIsSubscribable || !_connection->active;
}

void SubscriptionTopic::handleJson(json j) {
    std::string key = j.at(PropertyKey).get<std::string>();
    LDEBUG("Subscribing to property '" + key + "'...");

    _prop = property(key);
    if (_prop != nullptr) {
        _requestedResourceIsSubscribable = true;
        auto onChange = [this, key]() {
            LDEBUG("Updating subscription '" + key + "'.");
            _connection->sendJson(wrappedPayload(_prop));
        };
        _onChangeHandle = _prop->onChange(onChange);

        // immediately send the value
        onChange();
    }
    else {
        LWARNING("Could not subscribe. Property '" + key + "' not found.");
    }
}

} // namespace openspace
