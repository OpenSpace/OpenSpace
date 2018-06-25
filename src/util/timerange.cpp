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

#include <openspace/util/timerange.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/spicemanager.h>

#include <ghoul/misc/dictionary.h>

namespace {
    constexpr const char* KeyStart = "Start";
    constexpr const char* KeyEnd = "End";
} // namespace

namespace openspace {

documentation::Documentation TimeRange::Documentation() {
    using namespace documentation;
    return {
        "Time Range",
        "core_util_timerange",
        {
            {
                KeyStart,
                new StringAnnotationVerifier("A string representing a valid date"),
                Optional::No,
                "The start date of the time range"
            },
            {
                KeyEnd,
                new StringAnnotationVerifier("A string representing a valid date"),
                Optional::No,
                "The end date of the time range"
            }
        }
    };
}

TimeRange::TimeRange(double startTime, double endTime)
    : start(startTime)
    , end(endTime)
{}

TimeRange::TimeRange(const ghoul::Dictionary& dict) {
    if (!initializeFromDictionary(dict, *this)) {
        throw std::runtime_error("Unable to read TimeRange from dictionary");
    }
}

bool TimeRange::initializeFromDictionary(const ghoul::Dictionary& dict,
                                         TimeRange& timeRange)
{
    std::string startTimeStr;
    std::string endTimeStr;

    bool success = true;
    success &= dict.getValue(KeyStart, startTimeStr);
    success &= dict.getValue(KeyEnd, endTimeStr);
    if (success) {
        // Parse to date.
        // @TODO converting string to time stamp should not rely on Spice
        timeRange.start = SpiceManager::ref().ephemerisTimeFromDate(startTimeStr);
        timeRange.end = SpiceManager::ref().ephemerisTimeFromDate(endTimeStr);
        return true;
    }
    else {
        // Could not read TimeRange from Dict
        return false;
    }
}

void TimeRange::include(double val) {
    if (start > val) {
        start = val;
    }
    if (end < val) {
        end = val;
    }
}

void TimeRange::include(const TimeRange& other) {
    if (other.start < start) {
        start = other.start;
    }

    if (other.end > end) {
        end = other.end;
    }
}

double TimeRange::duration() const {
    return end - start;
}

bool TimeRange::isDefined() const {
    return start <= end;
}

bool TimeRange::isEmpty() const {
    return !isDefined();
}

bool TimeRange::inRange(double min, double max) const {
    return (min >= start && max <= end);
}

bool TimeRange::includes(double val) const {
    return (start <= val && val <= end);
}

bool TimeRange::includes(const TimeRange& o) const {
    return start <= o.start && o.end <= end;
}

} // namespace openspace
