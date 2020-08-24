/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#include "modules/server/include/topics/deltatimestepstopic.h"

#include <modules/server/include/connection.h>
#include <openspace/engine/globals.h>
#include <openspace/properties/property.h>
#include <openspace/query/query.h>
#include <openspace/util/timemanager.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const char* EventKey = "event";
    constexpr const char* SubscribeEvent = "start_subscription";
    constexpr const char* UnsubscribeEvent = "stop_subscription";
} // namespace

using nlohmann::json;

namespace openspace {

DeltaTimeStepsTopic::DeltaTimeStepsTopic() {}

DeltaTimeStepsTopic::~DeltaTimeStepsTopic() {
    if (_deltaTimeCallbackHandle != UnsetOnChangeHandle) {
        global::timeManager.removeDeltaTimeChangeCallback(
            _deltaTimeCallbackHandle
        );
    }
    if (_deltaTimesListCallbackHandle != UnsetOnChangeHandle) {
        global::timeManager.removeDeltaTimeStepsChangeCallback(
            _deltaTimesListCallbackHandle
        );
    }
}

bool DeltaTimeStepsTopic::isDone() const {
    return _isDone;
}

bool DeltaTimeStepsTopic::dataHasChanged() {
    std::optional<double> nextStep = global::timeManager.nextDeltaTimeStep();
    std::optional<double> prevStep = global::timeManager.previousDeltaTimeStep();

    return (nextStep != _lastNextDeltaTime || prevStep != _lastPrevDeltaTime);
}

void DeltaTimeStepsTopic::handleJson(const nlohmann::json& json) {
    std::string event = json.at(EventKey).get<std::string>();
    if (event == UnsubscribeEvent) {
        _isDone = true;
        return;
    }

    sendDeltaTimesData();

    if (event != SubscribeEvent) {
        _isDone = true;
        return;
    }

    _deltaTimeCallbackHandle = global::timeManager.addDeltaTimeChangeCallback(
        [this]() {
            if (dataHasChanged()) {
                sendDeltaTimesData();
            }
        }
    );

    _deltaTimesListCallbackHandle = global::timeManager.addDeltaTimeStepsChangeCallback(
        [this]() {
            if (dataHasChanged()) {
                sendDeltaTimesData();
            }
        }
    );
}

void DeltaTimeStepsTopic::sendDeltaTimesData() {
    std::optional<double> nextStep = global::timeManager.nextDeltaTimeStep();
    std::optional<double> prevStep = global::timeManager.previousDeltaTimeStep();

    bool hasNext = nextStep.has_value();
    bool hasPrev = prevStep.has_value();

    json deltaTimesListJson = {
        { "hasNextStep", hasNext },
        { "hasPrevStep", hasPrev }
    };

    if (hasNext) {
        deltaTimesListJson["nextStep"] = nextStep.value();
    }

    if (hasPrev) {
        deltaTimesListJson["prevStep"] = prevStep.value();
    }

    _connection->sendJson(wrappedPayload(deltaTimesListJson));

    _lastNextDeltaTime = nextStep;
    _lastPrevDeltaTime = prevStep;
}

} // namespace openspace
