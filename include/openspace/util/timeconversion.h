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

#include <array>
#include <string>
#include <utility>

namespace openspace {

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
constexpr std::array<TimeUnit, static_cast<int>(TimeUnit::Year) + 1>
TimeUnits = {
    TimeUnit::Nanosecond, TimeUnit::Microsecond, TimeUnit::Millisecond,
    TimeUnit::Second, TimeUnit::Minute, TimeUnit::Hour, TimeUnit::Day,
    TimeUnit::Month, TimeUnit::Year
};

std::pair<double, std::string> simplifyTime(double seconds,
    bool forceSingularForm = false);

double convertTime(double seconds, TimeUnit requestedUnit);

std::string nameForTimeUnit(TimeUnit unit, bool pluralForm = false);
TimeUnit timeUnitFromString(const std::string& unit);

} // namespace openspace

#endif // __OPENSPACE_CORE___TIMECONVERSION___H__
