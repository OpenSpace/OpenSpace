/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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
#include <chrono>

namespace {
const char* _loggerCat = "TimeTopic";
const char* PropertyKey = "property";
const char* CurrentTimeKey = "currentTime";
const char* DeltaTimeKey = "deltaTime";
const int UNSET_ONCHANGE_HANDLE = -1;
const std::chrono::milliseconds TimeUpdateInterval(100);
}

using nlohmann::json;

namespace openspace {

TimeTopic::TimeTopic()
    : Topic()
    , _timeCallbackHandle(UNSET_ONCHANGE_HANDLE)
    , _deltaTimeCallbackHandle(UNSET_ONCHANGE_HANDLE)
    , _lastUpdateTime(std::chrono::system_clock::now())
{
    LDEBUG("Starting new time subscription");
}

TimeTopic::~TimeTopic() {
    if (_timeCallbackHandle != UNSET_ONCHANGE_HANDLE) {
        OsEng.timeManager().removeTimeChangeCallback(_timeCallbackHandle);
    }
    if (_deltaTimeCallbackHandle != UNSET_ONCHANGE_HANDLE) {
        OsEng.timeManager().removeDeltaTimeChangeCallback(_deltaTimeCallbackHandle);
    }
}

bool TimeTopic::isDone() {
    return false;
}

void TimeTopic::handleJson(json j) {

    std::string requestedKey = j.at(PropertyKey).get<std::string>();
    LDEBUG("Subscribing to " + requestedKey);

    if (requestedKey == CurrentTimeKey) {
        _timeCallbackHandle = OsEng.timeManager().addTimeChangeCallback([this]() {
            std::chrono::system_clock::time_point now = std::chrono::system_clock::now();
            if (now - _lastUpdateTime > TimeUpdateInterval) {
                _connection->sendJson(currentTime());
                _lastUpdateTime = now;
            }
        });
        _connection->sendJson(currentTime());
    }
    else if (requestedKey == DeltaTimeKey) {
        _deltaTimeCallbackHandle = OsEng.timeManager().addDeltaTimeChangeCallback(
            [this]() {
                _connection->sendJson(deltaTime());
                if (_timeCallbackHandle != UNSET_ONCHANGE_HANDLE) {
                    _connection->sendJson(currentTime());
                    _lastUpdateTime = std::chrono::system_clock::now();;
                }
            }
        );
        _connection->sendJson(deltaTime());
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
