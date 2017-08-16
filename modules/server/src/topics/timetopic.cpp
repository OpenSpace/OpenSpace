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
#include "modules/server/include/connection.h"
#include "modules/server/include/timetopic.h"

namespace {
const char* _loggerCat = "TimeTopic";
const char* PropertyKey = "property";
const char* CurrentTimeKey = "currentTime";
const char* DeltaTimeKey = "deltaTime";
const int UNSET_ONCHANGE_HANDLE = -1;
}

using nlohmann::json;

namespace openspace {

TimeTopic::TimeTopic()
        : Topic()
        , _requestedResourceIsSubscribable(false)
        , _onChangeHandle(-1) {
    LDEBUG("Starting new subscription");
}

TimeTopic::~TimeTopic() {
    // TODO: remove post draw call
}

bool TimeTopic::isDone() {
    return !_requestedResourceIsSubscribable || !_connection->active;
}

void TimeTopic::handleJson(json j) {
    std::string requestedKey = j.at(PropertyKey).get<std::string>();
    LDEBUG("Subscribing to " + requestedKey);

    if (requestedKey == CurrentTimeKey) {
        _requestedResourceIsSubscribable = true;
        _connection->addRefreshCall([this]() { return currentTime(); });
    }
    else if (requestedKey == DeltaTimeKey) {
        _requestedResourceIsSubscribable = true;
        _connection->addRefreshCall([this]() { return deltaTime(); });
    }
    else {
        LWARNING("Cannot get " + requestedKey);
    }
}

json TimeTopic::currentTime() {
    json timeJson = { { "time", OsEng.timeManager().time().ISO8601() } };
    return wrappedPayload(timeJson);
}

json TimeTopic::deltaTime() {
    json timeJson = { { "deltaTime", OsEng.timeManager().time().deltaTime() } };
    return wrappedPayload(timeJson);
}

} // namespace openspace
