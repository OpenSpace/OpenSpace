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

#include <modules/telemetry/include/general/timetelemetry.h>

#include <openspace/engine/globals.h>
#include <openspace/util/timeconversion.h>
#include <openspace/util/timemanager.h>

namespace {
    // Indices for data items
    constexpr int NumDataItems = 3;
    constexpr int TimeSpeedIndex = 0;
    constexpr int TimeSpeedUnitIndex = 1;
    constexpr int CurrentTimeIndex = 2;

    static const openspace::properties::PropertyOwner::PropertyOwnerInfo
        TimeTelemetryInfo =
    {
        "TimeTelemetry",
        "Time Telemetry",
        "Telemetry that sends out time information to the Open Sound Control receiver."
    };

    constexpr openspace::properties::Property::PropertyInfo TimeUnitOptionInfo = {
        "TimeUnit",
        "Time Unit",
        "The time unit that the telemetry should use for the time speed. For example, if "
        "the unit is set to 'Hour' then the unit for the time speed is simulation hours "
        "per real life second.",
        openspace::properties::Property::Visibility::User
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo PrecisionInfo = {
        "Precision",
        "Precision",
        "Settings for the precision of the telemetry information."
    };

    constexpr openspace::properties::Property::PropertyInfo TimePrecisionInfo = {
        "TimePrecision",
        "TimePrecision",
        "The precision in seconds used to determine when to send updated time data to "
        "the Open Sound Control receiver.",
        openspace::properties::Property::Visibility::User
    };

} // namespace

namespace openspace {

TimeTelemetry::TimeTelemetry(const std::string& ip, int port)
    : TelemetryBase(TimeTelemetryInfo, ip, port)
    , _timeUnitOption(TimeUnitOptionInfo)
    , _precisionProperties(TimeTelemetry::PrecisionProperties(PrecisionInfo))
{
    for (size_t i = 0; i < TimeUnitNames.size(); ++i) {
        _timeUnitOption.addOption(static_cast<int>(i), TimeUnitNames[i].singular.data());
    }

    _timeUnitOption.setValue(static_cast<int>(TimeUnit::Day));
    addProperty(_timeUnitOption);

    addPropertySubOwner(_precisionProperties);
}

TimeTelemetry::PrecisionProperties::PrecisionProperties(
                               properties::PropertyOwner::PropertyOwnerInfo precisionInfo)
    : properties::PropertyOwner(precisionInfo)
    , timePrecision(TimePrecisionInfo, 0.0001, 0.0, 1.0e8)
{
    timePrecision.setExponent(10.f);
    addProperty(timePrecision);
}

bool TimeTelemetry::updateData(const Camera*) {
    double timeSpeed = convertTime(
        global::timeManager->deltaTime(),
        TimeUnit::Second,
        TimeUnits[_timeUnitOption]
    );

    double currentTime = global::timeManager->time().j2000Seconds();

    // Check if this data is new, otherwise don't send it
    double prevTimeSpeed = _timeSpeed;
    double prevTime = _currentTime;
    bool dataWasUpdated = false;

    if (abs(prevTimeSpeed - timeSpeed) > _precisionProperties.timePrecision) {
        _timeSpeed = timeSpeed;
        dataWasUpdated = true;
    }

    if (abs(prevTime - currentTime) > _precisionProperties.timePrecision) {
        _currentTime = currentTime;
        dataWasUpdated = true;
    }

    return dataWasUpdated;
}

void TimeTelemetry::sendData() {
    std::string label = "/Time";
    std::vector<OpenSoundControlDataType> data(NumDataItems);

    data[TimeSpeedIndex] = _timeSpeed;
    data[TimeSpeedUnitIndex] =
        _timeUnitOption.getDescriptionByValue(_timeUnitOption.value());
    data[CurrentTimeIndex] = _currentTime;

    _connection->send(label, data);
}

} // namespace openspace
