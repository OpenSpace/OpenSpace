/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#include <modules/sonification/include/timesonification.h>

#include <openspace/util/timemanager.h>

namespace {
    constexpr double TimeSpeedPrecision = 0.0001;
    constexpr double TimePrecision = 1;

    static const openspace::properties::PropertyOwner::PropertyOwnerInfo
        TimeSonificationInfo =
    {
       "TimeSonification",
       "Time Sonification",
       "Sonification that alters all other sonificatoins based on the current time and "
       "delta time"
    };

    constexpr openspace::properties::Property::PropertyInfo UnitOptionInfo = {
        "UnitOption",
        "Time Speed Unit",
        "Choose a time unit that the sonification should use for the delta time"
    };

} // namespace

namespace openspace {

TimeSonification::TimeSonification(const std::string& ip, int port)
    : SonificationBase(TimeSonificationInfo, ip, port)
    , _unitOption(UnitOptionInfo, properties::OptionProperty::DisplayType::Dropdown)
{
    _timeSpeed = 0.0;
    _currentTime = 0.0;

    // Add time units option
    _unitOption.addOptions({
        { 0, "Seconds" },
        { 1, "Minutes" },
        { 2, "Hours" },
        { 3, "Days" },
        { 4, "Months" },
        { 5, "Years" }
    });

    // Set days as default unit
    _unitOption.setValue(3);
    _unit = TimeUnit::Day;

    _unitOption.onChange([this]() { reCalculateTimeUnit(); });

    addProperty(_unitOption);
}

void TimeSonification::update(const Camera*) {
    if (!_enabled) {
        return;
    }

    double currentTime = global::timeManager->time().j2000Seconds();
    double timeSpeed = convertTime(global::timeManager->deltaTime(), TimeUnit::Second, _unit);

    if (_unitDirty || abs(_timeSpeed - timeSpeed) > TimeSpeedPrecision ||
        abs(_currentTime - currentTime) > TimePrecision)
    {
        _timeSpeed = timeSpeed;
        _currentTime = currentTime;

        std::string label = "/Time";
        std::vector<OscDataType> data(3);
        data[0] = _timeSpeed;
        data[1] = nameForTimeUnit(_unit).data();
        data[2] = _currentTime;

        _connection->send(label, data);

        if (_unitDirty) {
            _unitDirty = false;
        }
    }
}

void TimeSonification::stop() {}

void TimeSonification::reCalculateTimeUnit() {
    std::string selectedUnit = _unitOption.getDescriptionByValue(_unitOption.value());
    std::transform(
        selectedUnit.begin(),
        selectedUnit.end(),
        selectedUnit.begin(),
        [](unsigned char c) { return std::tolower(c); }
    );

    _unit = timeUnitFromString(selectedUnit);
    _unitDirty = true;
}

} // namespace openspace
