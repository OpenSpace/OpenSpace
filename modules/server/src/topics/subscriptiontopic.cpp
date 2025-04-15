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

#include <modules/server/include/topics/subscriptiontopic.h>

#include <modules/server/include/connection.h>
#include <modules/server/include/jsonconverters.h>
#include <openspace/properties/property.h>
#include <openspace/query/query.h>
#include <openspace/util/timemanager.h>
#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr std::string_view _loggerCat = "SubscriptionTopic";

    constexpr std::string_view StartSubscription = "start_subscription";
    constexpr std::string_view StopSubscription = "stop_subscription";
} // namespace

using nlohmann::json;

namespace openspace {

SubscriptionTopic::~SubscriptionTopic() {
    resetCallbacks();
}

bool SubscriptionTopic::isDone() const {
    return !_requestedResourceIsSubscribable || !_isSubscribedTo;
}

void SubscriptionTopic::resetCallbacks() {
    if (!_prop) {
        return;
    }
    if (_onChangeHandle != UnsetCallbackHandle) {
        _prop->removeOnChange(_onChangeHandle);
        _onChangeHandle = UnsetCallbackHandle;
    }
    if (_onDeleteHandle != UnsetCallbackHandle) {
        _prop->removeOnDelete(_onDeleteHandle);
        _onDeleteHandle = UnsetCallbackHandle;
    }
}

void SubscriptionTopic::handleJson(const nlohmann::json& json) {
    const std::string& event = json.at("event").get<std::string>();

    if (event == StartSubscription) {
        std::string uri = json.at("property").get<std::string>();

        _prop = property(uri);
        resetCallbacks();

        if (_prop) {
            _requestedResourceIsSubscribable = true;
            _isSubscribedTo = true;
            auto onChange = [this, k = uri]() {
                nlohmann::json payload = {
                    { "value", json::parse(_prop->jsonValue()) }
                };
                _connection->sendJson(wrappedPayload(payload));
            };

            auto onMetaDataChange = [this, k = uri]() {
                nlohmann::json payload = {};
                payload["metaData"] = _prop->generateJsonDescription();
                _connection->sendJson(wrappedPayload(payload));
            };

            _onChangeHandle = _prop->onChange(onChange);
            _onMetaDataChangeHandle = _prop->onMetaDataChange(onMetaDataChange);
            _onDeleteHandle = _prop->onDelete([this]() {
                _onChangeHandle = UnsetCallbackHandle;
                _onDeleteHandle = UnsetCallbackHandle;
                _isSubscribedTo = false;
            });

            // Immediately send the value and meta data
            onChange();
			onMetaDataChange();
        }
        else {
            LWARNING(std::format("Could not subscribe. Property '{}' not found", uri));
        }
    }
    if (event == StopSubscription) {
        _isSubscribedTo = false;
        if (_prop && _onChangeHandle != UnsetCallbackHandle) {
            _prop->removeOnChange(_onChangeHandle);
            _onChangeHandle = UnsetCallbackHandle;
        }
        if (_prop && !_onDeleteHandle) {
            _prop->removeOnDelete(_onDeleteHandle);
            _onDeleteHandle = UnsetCallbackHandle;
        }
    }
}

} // namespace openspace
