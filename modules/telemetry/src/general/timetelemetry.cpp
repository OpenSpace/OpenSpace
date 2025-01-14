/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <openspace/util/timeconversion.h>
#include <openspace/util/timemanager.h>

namespace {
    static const openspace::properties::PropertyOwner::PropertyOwnerInfo
        TimeTelemetryInfo =
    {
        "TimeTelemetry",
        "Time Telemetry",
        "Telemetry that sends out time information over the OSC connection."
    };

    constexpr openspace::properties::Property::PropertyInfo TimeUnitOptionInfo = {
        "TimeUnit",
        "Time Unit",
        "Choose a time unit that the telemetry should use for the time speed. For "
        "example, if the unit is set to 'Hour' then the unit for the time speed "
        "is simulation hours per real life second."
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo PrecisionInfo = {
        "Precision",
        "Precision",
        "Settings for the precision of the telemetry information."
    };

    constexpr openspace::properties::Property::PropertyInfo TimePrecisionInfo = {
        "TimePrecision",
        "TimePrecision",
        "The precision in seconds used to determin when to send updated time data "
        "over the OSC connection.",
        openspace::properties::Property::Visibility::User
    };

} // namespace

namespace openspace {

TimeTelemetry::TimeTelemetry(const std::string& ip, int port)
    : TelemetryBase(TimeTelemetryInfo, ip, port)
    , _timeUnitOption(
        TimeUnitOptionInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
    , _precisionProperty(TimeTelemetry::PrecisionProperty(PrecisionInfo))
{
    // Add all time units as options in the drop down menu
    for (size_t i = 0; i < TimeUnitNamesSingular.size(); ++i) {
        _timeUnitOption.addOption(i, TimeUnitNamesSingular[i].data());
    }

    // Set days as the default time unit, i.e. the unit for the time speed is
    // simulation days per real life second
    _timeUnitOption.setValue(static_cast<int>(TimeUnit::Day));
    addProperty(_timeUnitOption);

    addPropertySubOwner(_precisionProperty);
}

void TimeTelemetry::update(const Camera*) {
    if (!_enabled) {
        return;
    }

    bool hasNewData = getData();

    // Only send data if something new has happened
    if (hasNewData) {
        sendData();
    }
}

void TimeTelemetry::stop() {}

TimeTelemetry::PrecisionProperty::PrecisionProperty(
                               properties::PropertyOwner::PropertyOwnerInfo precisionInfo)
    : properties::PropertyOwner(precisionInfo)
    , timePrecision(TimePrecisionInfo, 0.0001, 0, 1e8)
{
    timePrecision.setExponent(10.f);
    addProperty(timePrecision);
}

bool TimeTelemetry::getData() {
    double timeSpeed = convertTime(
        global::timeManager->deltaTime(),
        TimeUnit::Second,
        TimeUnits[_timeUnitOption]
    );

    double currentTime = global::timeManager->time().j2000Seconds();

    // Check if this data is new, otherwise don't send it
    double prevTimeSpeed = _timeSpeed;
    double prevTime = _currentTime;
    bool shouldSendData = false;

    if (abs(prevTimeSpeed - timeSpeed) > _precisionProperty.timePrecision) {
        _timeSpeed = timeSpeed;
        shouldSendData = true;
    }

    if (abs(prevTime - currentTime) > _precisionProperty.timePrecision) {
        _currentTime = currentTime;
        shouldSendData = true;
    }

    return shouldSendData;
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
