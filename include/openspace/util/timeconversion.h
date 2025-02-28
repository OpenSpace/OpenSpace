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

#ifndef __OPENSPACE_CORE___TIMECONVERSION___H__
#define __OPENSPACE_CORE___TIMECONVERSION___H__

#include <openspace/util/timeconstants.h>

#include <ghoul/misc/assert.h>
#include <ghoul/misc/constexpr.h>
#include <algorithm>
#include <array>
#include <string>

namespace openspace {

// Assumption:  static_cast<int>(TimeUnit) == position in the list
enum class TimeUnit {
    Nanosecond = 0,
    Microsecond,
    Millisecond,
    Second,
    Minute,
    Hour,
    Day,
    Month,
    Year
};

struct TimeUnitName {
    std::string_view singular;
    std::string_view plural;
    std::string_view abbreviation;
};

constexpr std::array<TimeUnit, static_cast<int>(TimeUnit::Year) + 1> TimeUnits = {
    TimeUnit::Nanosecond, TimeUnit::Microsecond, TimeUnit::Millisecond,
    TimeUnit::Second, TimeUnit::Minute, TimeUnit::Hour, TimeUnit::Day,
    TimeUnit::Month, TimeUnit::Year
};

// Note that the syntax here is required when initializing constexpr std::arrays
// with structs
constexpr std::array<TimeUnitName, static_cast<int>(TimeUnit::Year) + 1>
TimeUnitNames { {
    { "Nanosecond", "Nanoseconds", "ns" },
    { "Microsecond", "Microseconds", "us" },
    { "Millisecond", "Milliseconds", "ms" },
    { "Second", "Seconds", "s" },
    { "Minute", "Minutes", "m" },
    { "Hour", "Hours", "h" },
    { "Day", "Days", "d" },
    { "Month", "Months", "M" },
    { "Year", "Years", "Y" }
} };

constexpr bool isValidTimeUnitName(std::string_view name) {
    for (TimeUnit unit : TimeUnits) {
        const TimeUnitName unitName = TimeUnitNames[static_cast<int>(unit)];
        if (name == unitName.singular || name == unitName.plural ||
            name == unitName.abbreviation)
        {
            return true;
        }
    }
    return false;
}

constexpr std::string_view nameForTimeUnit(TimeUnit unit, bool usePluralForm = false) {
    const TimeUnitName unitName = TimeUnitNames[static_cast<int>(unit)];
    return usePluralForm ? unitName.plural : unitName.singular;
}

constexpr std::string_view abbreviationForDistanceUnit(TimeUnit unit) {
    return TimeUnitNames[static_cast<int>(unit)].abbreviation;
}

constexpr TimeUnit timeUnitFromString(std::string_view unitName) {
    int i = 0;
    for (TimeUnit unit : TimeUnits) {
        const TimeUnitName name = TimeUnitNames[static_cast<int>(unit)];
        if (name.singular == unitName || name.plural == unitName ||
            name.abbreviation == unitName)
        {
            return static_cast<TimeUnit>(i);
        }
        i++;
    }

    throw ghoul::MissingCaseException();
}

constexpr std::vector<std::string> timeUnitList() {
    std::vector<std::string> res(TimeUnits.size());
    std::transform(
        TimeUnits.begin(),
        TimeUnits.end(),
        res.begin(),
        [](TimeUnit unit) {
            return std::string(nameForTimeUnit(unit));
        }
    );
    return res;
}

std::pair<double, std::string_view> simplifyTime(double seconds,
    bool forceSingularForm = false);

std::vector<std::pair<double, std::string_view>> splitTime(double seconds,
    bool forceSingularForm = false);

constexpr double convertSeconds(double seconds, TimeUnit requestedUnit) {
    switch (requestedUnit) {
        case TimeUnit::Nanosecond:
            return seconds / 1e-9;
        case TimeUnit::Microsecond:
            return seconds / 1e-6;
        case TimeUnit::Millisecond:
            return seconds / 1e-3;
        case TimeUnit::Second:
            return seconds;
        case TimeUnit::Minute:
            return seconds / timeconstants::SecondsPerMinute;
        case TimeUnit::Hour:
            return seconds / timeconstants::SecondsPerHour;
        case TimeUnit::Day:
            return seconds / timeconstants::SecondsPerDay;
        case TimeUnit::Month:
            return seconds / timeconstants::SecondsPerMonth;
        case TimeUnit::Year:
            return seconds / timeconstants::SecondsPerYear;
        default:
            throw ghoul::MissingCaseException();
    }
}

constexpr double toSecond(TimeUnit unit) {
    switch (unit) {
        case TimeUnit::Nanosecond:
            return 1e-9;
        case TimeUnit::Microsecond:
            return 1e-6;
        case TimeUnit::Millisecond:
            return 1e-3;
        case TimeUnit::Second:
            return 1.0;
        case TimeUnit::Minute:
            return timeconstants::SecondsPerMinute;
        case TimeUnit::Hour:
            return timeconstants::SecondsPerHour;
        case TimeUnit::Day:
            return timeconstants::SecondsPerDay;
        case TimeUnit::Month:
            return timeconstants::SecondsPerMonth;
        case TimeUnit::Year:
            return timeconstants::SecondsPerYear;
        default:
            throw ghoul::MissingCaseException();
    }
}

constexpr double convertUnit(TimeUnit fromUnit, TimeUnit toUnit) {
    return convertSeconds(toSecond(fromUnit), toUnit);
}

constexpr double convertTime(double time, TimeUnit fromUnit, TimeUnit toUnit) {
    return time * convertUnit(fromUnit, toUnit);
}

} // namespace openspace

#endif // __OPENSPACE_CORE___TIMECONVERSION___H__
