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

#include <openspace/util/timeconversion.h>

#include <ghoul/glm.h>

#include <cstring>

namespace {
    std::pair<double, openspace::TimeUnit> extractUnit(double seconds) {
        using namespace openspace;

        const double secondsVal = glm::abs(seconds);

        if (secondsVal == 0.0) {
            return { 0.0, TimeUnit::Second };
        }
        else if (secondsVal <= 1e-6) {
            const double val = seconds / 1e-9;
            return { val, TimeUnit::Nanosecond };
        }
        else if (secondsVal <= 1e-3) {
            const double val = seconds / 1e-6;
            return { val, TimeUnit::Microsecond };
        }
        else if (secondsVal <= 1.0) {
            const double val = seconds / 1e-3;
            return { val, TimeUnit::Millisecond };
        }
        else if (secondsVal <= timeconstants::SecondsPerMinute) {
            return { seconds, TimeUnit::Second };
        }
        else if (secondsVal <= timeconstants::SecondsPerHour) {
            const double val = seconds / timeconstants::SecondsPerMinute;
            return { val, TimeUnit::Minute };
        }
        else if (secondsVal <= timeconstants::SecondsPerDay) {
            const double val = seconds / timeconstants::SecondsPerHour;
            return { val, TimeUnit::Hour };
        }
        else if (secondsVal <= timeconstants::SecondsPerMonth) {
            const double val = seconds / timeconstants::SecondsPerDay;
            return { val, TimeUnit::Day };
        }
        else if (secondsVal <= timeconstants::SecondsPerYear) {
            const double val = seconds / timeconstants::SecondsPerMonth;
            return { val, TimeUnit::Month };
        }
        else {
            const double val = seconds / timeconstants::SecondsPerYear;
            return { val, TimeUnit::Year };
        }
    }
} // namespace

namespace openspace {

std::pair<double, std::string_view> simplifyTime(double seconds, bool forceSingularForm) {
    const std::pair<double, TimeUnit> p = extractUnit(seconds);
    const bool pluralForm = (p.first != 1.0 && !forceSingularForm);
    return { p.first, nameForTimeUnit(p.second, pluralForm) };
}

std::vector<std::pair<double, std::string_view>> splitTime(double seconds,
                                                           bool forceSingularForm)
{
    std::vector<std::pair<double, std::string_view>> res;

    double secondsVal = std::abs(seconds);

    do {
        const std::pair<double, TimeUnit> p = extractUnit(secondsVal);

        if (p.second == TimeUnit::Nanosecond) {
            // We have reached the lowest supported time unit

            const bool pluralForm = (p.first != 1.0 && !forceSingularForm);
            res.emplace_back(p.first, nameForTimeUnit(p.second, pluralForm));
            break;
        }

        const double pt = std::floor(p.first);

        // Add the unit the list
        const bool pluralForm = (p.first != 1.0 && !forceSingularForm);
        res.emplace_back(pt, nameForTimeUnit(p.second, pluralForm));

        // Adjust the remaining time
        secondsVal -= convertTime(pt, p.second, TimeUnit::Second);
    } while (secondsVal != 0.0);

    return res;
}

} // namespace openspace
