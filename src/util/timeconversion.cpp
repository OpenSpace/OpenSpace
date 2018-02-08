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

#include <openspace/util/timeconversion.h>

#include <ghoul/glm.h>
#include <ghoul/misc/assert.h>

#include <chrono>

namespace {
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
} // namespace

namespace openspace {

std::pair<double, std::string> simplifyTime(double seconds, bool forceSingularForm) {
    double secondsVal = glm::abs(seconds);

    if (secondsVal == 0.0) {
        return { 0.0, forceSingularForm ? "second" : "seconds" };
    }
    else if (secondsVal > 1e-3 && secondsVal < SecondsPerMinute) {
        return { seconds, (seconds == 1.0 || forceSingularForm) ? "second" : "seconds" };
    }

    if (secondsVal <= 1e-9) {
        double val = seconds / 1e-9;
        return { val, (val == 1.0 || forceSingularForm) ? "nanosecond" : "nanoseconds" };
    }
    else if (secondsVal <= 1e-6) {
        double val = seconds / 1e-6;
        return {
            val,
            (val == 1.0 || forceSingularForm) ? "microsecond" : "microseconds"
        };
    }
    else if (secondsVal <= 1e-3) {
        double val = seconds / 1e-3;
        return {
            val,
            (val == 1.0 || forceSingularForm) ? "millisecond" : "milliseconds" 
        };
    }

    if (secondsVal >= SecondsPerYear) {
        double val = seconds / SecondsPerYear;
        return { val, (val == 1.0 || forceSingularForm) ? "year" : "years" };
    }
    else if (secondsVal >= SecondsPerMonth) {
        double val = seconds / SecondsPerMonth;
        return { val, (val == 1.0 || forceSingularForm) ? "month" : "months" };
    }
    else if (secondsVal >= SecondsPerDay) {
        double val = seconds / SecondsPerDay;
        return { val, (val == 1.0 || forceSingularForm) ? "day" : "days" };
    }
    else if (secondsVal >= SecondsPerHour) {
        double val = seconds / SecondsPerHour;
        return { val, (val == 1.0 || forceSingularForm) ? "hour" : "hours" };
    }
    else {
        double val = seconds / SecondsPerMinute;
        return { val, (val == 1.0 || forceSingularForm) ? "minute" : "minutes" };
    }
}

double convertTime(double seconds, TimeUnit requestedUnit) {
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

std::string nameForTimeUnit(TimeUnit unit, bool pluralForm) {
    switch (unit) {
        case TimeUnit::Nanosecond:
            return pluralForm ? "nanoseconds" : "nanosecond";
        case TimeUnit::Microsecond:
            return pluralForm ? "microseconds" : "microsecond";
        case TimeUnit::Millisecond:
            return pluralForm ? "milliseconds" : "millisecond";
        case TimeUnit::Second:
            return pluralForm ? "seconds" : "second";
        case TimeUnit::Minute:
            return pluralForm ? "minutes" : "minute";
        case TimeUnit::Hour:
            return pluralForm ? "hours" : "hour";
        case TimeUnit::Day:
            return pluralForm ? "days" : "day";
        case TimeUnit::Month:
            return pluralForm ? "months" : "month";
        case TimeUnit::Year:
            return pluralForm ? "years" : "year";
        default:
            throw ghoul::MissingCaseException();
    }
}

TimeUnit timeUnitFromString(const std::string& unit) {
    if (unit == "nanosecond" || unit == "nanoseconds") {
        return TimeUnit::Nanosecond;
    }
    else if (unit == "microsecond" || unit == "microseconds") {
        return TimeUnit::Microsecond;
    }
    else if (unit == "millisecond" || unit == "milliseconds") {
        return TimeUnit::Millisecond;
    }
    else if (unit == "second" || unit == "seconds") {
        return TimeUnit::Second;
    }
    else if (unit == "minute" || unit == "minutes") {
        return TimeUnit::Minute;
    }
    else if (unit == "hour" || unit == "hours") {
        return TimeUnit::Hour;
    }
    else if (unit == "day" || unit == "days") {
        return TimeUnit::Day;
    }
    else if (unit == "month" || unit == "months") {
        return TimeUnit::Month;
    }
    else if (unit == "year" || unit == "years") {
        return TimeUnit::Year;
    }
    else {
        throw ghoul::MissingCaseException();
    }
}


} // namespace openspace
