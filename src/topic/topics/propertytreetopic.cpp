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

#include <openspace/topic/topics/propertytreetopic.h>

#include <openspace/engine/globals.h>
#include <openspace/topic/connection.h>
#include <openspace/topic/jsonconverters.h>
#include <openspace/properties/property.h>
#include <openspace/query/query.h>
#include <openspace/util/timemanager.h>
#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr std::string_view _loggerCat = "PropertyTreeTopic";

    constexpr std::string_view StartSubscription = "start_subscription";
    constexpr std::string_view StopSubscription = "stop_subscription";
    constexpr std::string_view PropertyChanged = "property_changed";
} // namespace

using nlohmann::json;

namespace openspace {

PropertyTreeTopic::~PropertyTreeTopic() {
    resetCallbacks();
}

bool PropertyTreeTopic::isDone() const {
    return !_requestedResourceIsSubscribable || !_isSubscribedTo;
}

std::string PropertyTreeTopic::type() const {
    return "propertyTree";
}

void PropertyTreeTopic::resetCallbacks() {
    if (!_prop) {
        return;
    }
    if (_onChangeHandle != UnsetCallbackHandle) {
        _prop->removeOnChange(_onChangeHandle);
        _onChangeHandle = UnsetCallbackHandle;
    }
    if (_onMetaDataChangeHandle != UnsetCallbackHandle) {
        _prop->removeOnMetaDataChange(_onMetaDataChangeHandle);
        _onMetaDataChangeHandle = UnsetCallbackHandle;
    }
    if (_onDeleteHandle != UnsetCallbackHandle) {
        _prop->removeOnDelete(_onDeleteHandle);
        _onDeleteHandle = UnsetCallbackHandle;
    }
}

void PropertyTreeTopic::handleJson(const nlohmann::json& json) {
    const std::string& event = json.at("event").get<std::string>();

    if (event == StartSubscription) {
        nlohmann::json data = {
            { "event", "root" },
            { "payload", global::rootPropertyOwner }
        };

        _connection->sendJson(wrappedPayload(data));
        _isSubscribedTo = true;
        _requestedResourceIsSubscribable = true;
    }
    if (event == StopSubscription) {
        _isSubscribedTo = false;
        resetCallbacks();
    }
    if (event == PropertyChanged) {
        const nlohmann::json& payload = json.at("payload").get<nlohmann::json>();

        _connection->sendJson(wrappedPayload(payload));
    }
}

} // namespace openspace
