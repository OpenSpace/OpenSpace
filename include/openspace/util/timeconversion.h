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

#ifndef __OPENSPACE_CORE___TIMECONVERSION___H__
#define __OPENSPACE_CORE___TIMECONVERSION___H__

#include <ghoul/misc/assert.h>
#include <ghoul/misc/constexpr.h>

#include <array>
#include <chrono>
#include <string>

namespace openspace {

constexpr double SecondsPerYear = 31556952; // seconds in average Gregorian year
constexpr double SecondsPerMonth = SecondsPerYear / 12;
constexpr double SecondsPerDay = static_cast<double>(
    std::chrono::seconds(std::chrono::hours(24)).count()
);
constexpr double SecondsPerHour = static_cast<double>(
    std::chrono::seconds(std::chrono::hours(1)).count()
);
constexpr double SecondsPerMinute = static_cast<double>(
    std::chrono::seconds(std::chrono::minutes(1)).count()
);

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

// Assumption:  Unit names are sequential in memory
constexpr const char* TimeUnitNanosecond   = "nanosecond";
constexpr const char* TimeUnitMicrosecond  = "microsecond";
constexpr const char* TimeUnitMillisecond  = "millisecond";
constexpr const char* TimeUnitSecond       = "second";
constexpr const char* TimeUnitMinute       = "minute";
constexpr const char* TimeUnitHour         = "hour";
constexpr const char* TimeUnitDay          = "day";
constexpr const char* TimeUnitMonth        = "month";
constexpr const char* TimeUnitYear         = "year";

// Assumption:  Unit names are sequential in memory
constexpr const char* TimeUnitNanoseconds = "nanoseconds";
constexpr const char* TimeUnitMicroseconds = "microseconds";
constexpr const char* TimeUnitMilliseconds = "milliseconds";
constexpr const char* TimeUnitSeconds = "seconds";
constexpr const char* TimeUnitMinutes = "minutes";
constexpr const char* TimeUnitHours = "hours";
constexpr const char* TimeUnitDays = "days";
constexpr const char* TimeUnitMonths = "months";
constexpr const char* TimeUnitYears = "years";

constexpr const std::array<TimeUnit, static_cast<int>(TimeUnit::Year) + 1> TimeUnits = {
    TimeUnit::Nanosecond, TimeUnit::Microsecond, TimeUnit::Millisecond,
    TimeUnit::Second, TimeUnit::Minute, TimeUnit::Hour, TimeUnit::Day,
    TimeUnit::Month, TimeUnit::Year
};

constexpr const std::array<const char*, static_cast<int>(TimeUnit::Year) + 1>
TimeUnitNamesSingular = {
    TimeUnitNanosecond, TimeUnitMicrosecond, TimeUnitMillisecond, TimeUnitSecond,
    TimeUnitMinute, TimeUnitHour, TimeUnitDay, TimeUnitMonth, TimeUnitYear
};

constexpr const std::array<const char*, static_cast<int>(TimeUnit::Year) + 1>
TimeUnitNamesPlural = {
    TimeUnitNanoseconds, TimeUnitMicroseconds, TimeUnitMilliseconds, TimeUnitSeconds,
    TimeUnitMinutes, TimeUnitHours, TimeUnitDays, TimeUnitMonths, TimeUnitYears
};

constexpr bool isValidTimeUnitName(const char* name) {
    int i = 0;
    for (const char* val : TimeUnitNamesSingular) {
        if (ghoul::equal(name, val)) {
            return true;
        }
        ++i;
    }

    i = 0;
    for (const char* val : TimeUnitNamesPlural) {
        if (ghoul::equal(name, val)) {
            return true;
        }
        ++i;
    }
    return false;
}

constexpr const char* nameForTimeUnit(TimeUnit unit, bool pluralForm = false) {
    switch (unit) {
        case TimeUnit::Nanosecond:
        case TimeUnit::Microsecond:
        case TimeUnit::Millisecond:
        case TimeUnit::Second:
        case TimeUnit::Minute:
        case TimeUnit::Hour:
        case TimeUnit::Day:
        case TimeUnit::Month:
        case TimeUnit::Year:
            if (pluralForm) {
                return TimeUnitNamesPlural[static_cast<int>(unit)];
            }
            else {
                return TimeUnitNamesSingular[static_cast<int>(unit)];
            }
        default:
            throw ghoul::MissingCaseException();
    }
}

constexpr TimeUnit timeUnitFromString(const char* unitName) {
    int found = -1;
    int i = 0;
    for (const char* val : TimeUnitNamesSingular) {
        if (ghoul::equal(unitName, val)) {
            found = i;
            break;
        }
        ++i;
    }

    i = 0;
    for (const char* val : TimeUnitNamesPlural) {
        if (ghoul::equal(unitName, val)) {
            found = i;
            break;
        }
        ++i;
    }

    if (found != -1) {
        return static_cast<TimeUnit>(found);
    }
    else {
        throw ghoul::MissingCaseException();
    }
}

std::pair<double, std::string> simplifyTime(double seconds,
    bool forceSingularForm = false);

constexpr double convertTime(double t, TimeUnit sourceUnit, TimeUnit destinationUnit) {
    double seconds = t;
    switch (sourceUnit) {
        case TimeUnit::Nanosecond:
            seconds = t * 1e-9;
            break;
        case TimeUnit::Microsecond:
            seconds = t * 1e-6;
            break;
        case TimeUnit::Millisecond:
            seconds = t * 1e-3;
            break;
        case TimeUnit::Minute:
            seconds = t * SecondsPerMinute;
            break;
        case TimeUnit::Hour:
            seconds  = t * SecondsPerHour;
            break;
        case TimeUnit::Day:
            seconds  = t * SecondsPerDay;
            break;
        case TimeUnit::Month:
            seconds  = t * SecondsPerMonth;
            break;
        case TimeUnit::Year:
            seconds  = t * SecondsPerYear;
            break;
        default: ;
    }

    switch (destinationUnit) {
        case TimeUnit::Nanosecond:
            return seconds / 1e-9;
        case TimeUnit::Microsecond:
            return seconds / 1e-6;
        case TimeUnit::Millisecond:
            return seconds / 1e-3;
        case TimeUnit::Second:
            return seconds;
        case TimeUnit::Minute:
            return seconds / SecondsPerMinute;
        case TimeUnit::Hour:
            return seconds / SecondsPerHour;
        case TimeUnit::Day:
            return seconds / SecondsPerDay;
        case TimeUnit::Month:
            return seconds / SecondsPerMonth;
        case TimeUnit::Year:
            return seconds / SecondsPerYear;
        default:
            throw ghoul::MissingCaseException();
    }
}

} // namespace openspace

#endif // __OPENSPACE_CORE___TIMECONVERSION___H__
