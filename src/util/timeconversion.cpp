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

#include <openspace/util/timeconversion.h>

#include <ghoul/glm.h>

#include <chrono>
#include <cmath>

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

std::pair<double, std::string> simplifyTime(double seconds) {
    double secondsVal = std::abs(seconds);

    if (secondsVal > 1e-3 && secondsVal < SecondsPerMinute) {
        return { seconds, seconds == 1.0 ? "second" : "seconds" };
    }

    if (secondsVal <= 1e-9) {
        double val = seconds / 1e-9;
        return { val, val == 1.0 ? "nanosecond" : "nanoseconds" };
    }
    else if (secondsVal <= 1e-6) {
        double val = seconds / 1e-6;
        return { val, val == 1.0 ? "microsecond" : "microseconds" };
    }
    else if (secondsVal <= 1e-3) {
        double val = seconds / 1e-3;
        return { val, val == 1.0 ? "millisecond" : "milliseconds" };
    }

    if (secondsVal >= SecondsPerYear) {
        double val = seconds / SecondsPerYear;
        return { val, val == 1.0 ? "year" : "years" };
    }
    else if (secondsVal >= SecondsPerMonth) {
        double val = seconds / SecondsPerMonth;
        return { val, val == 1.0 ? "month" : "months" };
    }
    else if (secondsVal >= SecondsPerDay) {
        double val = seconds / SecondsPerDay;
        return { val, val == 1.0 ? "day" : "days" };
    }
    else if (secondsVal >= SecondsPerHour) {
        double val = seconds / SecondsPerHour;
        return { val, val == 1.0 ? "hour" : "hours" };
    }
    else {
        double val = seconds / SecondsPerMinute;
        return { val, val == 1.0 ? "minute" : "minutes" };
    }
}

} // namespace openspace
