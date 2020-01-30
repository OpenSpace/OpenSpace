/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#include <modules/globebrowsing/src/timequantizer.h>

#include <openspace/util/time.h>
#include <ghoul/glm.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/exception.h>
#include <algorithm>

namespace openspace::globebrowsing {

TimeQuantizer::TimeQuantizer(const Time& start, const Time& end, double resolution)
    : _timerange(start.j2000Seconds(), end.j2000Seconds())
    , _resolution(resolution)
{}

TimeQuantizer::TimeQuantizer(const Time& start, const Time& end,
                             const std::string& resolution)
    : TimeQuantizer(start, end, parseTimeResolutionStr(resolution))
{}

double TimeQuantizer::parseTimeResolutionStr(const std::string& resolutionStr) {
    const char unit = resolutionStr.back();
    std::string numberString = resolutionStr.substr(0, resolutionStr.length() - 1);

    char* p;
    double value = strtol(numberString.c_str(), &p, 10);
    _resolutionValue = value;
    _resolutionUnit = unit;
    if (*p) { // not a number
        throw ghoul::RuntimeError("Cannot convert " + numberString + " to number");
    }
    else {
        // convert value to seconds, based on unit.
        // The switch statment has intentional fall throughs
        switch (unit) {
            case 'y':
                value *= 365;
                [[fallthrough]];
            case 'd':
                value *= 24.0;
                [[fallthrough]];
            case 'h': value *= 60.0;
                [[fallthrough]];
            case 'm':
                value *= 60.0;
                [[fallthrough]];
            case 's':
                value *= 1.0;
                break;
            default:
                throw ghoul::RuntimeError(
                    "Invalid unit format '" + std::string(1, unit) +
                    "'. Expected 'y', 'd', 'h', 'm' or 's'."
                );
        }
        return value;
    }
}

bool TimeQuantizer::quantize(Time& t, bool clamp) const {
    const double unquantized = t.j2000Seconds();
    if (_timerange.includes(unquantized)) {
        const double quantized = std::floor((unquantized - _timerange.start) /
                                 _resolution) *
                                 _resolution + _timerange.start;
        t.setTime(quantized);
        return true;
    }
    else if (clamp) {
        const double clampedTime = glm::clamp(
            unquantized,
            _timerange.start,
            _timerange.end
        );
        t.setTime(clampedTime);
        return true;
    }
    else {
        return false;
    }
}

std::vector<Time> TimeQuantizer::quantized(const Time& start, const Time& end) const {
    Time s = start;
    quantize(s, true);

    Time e = end;
    quantize(e, true);

    const double startSeconds = s.j2000Seconds();
    const double endSeconds = e.j2000Seconds();
    const double delta = endSeconds - startSeconds;

    ghoul_assert(int(delta) % int(_resolution) == 0, "Quantization error");
    const int nSteps = static_cast<int>(delta / _resolution);

    std::vector<Time> result(nSteps + 1);
    for (int i = 0; i <= nSteps; ++i) {
        result[i].setTime(startSeconds + i * _resolution);
    }

    return result;
}

bool TimeQuantizer::quantize2(Time& t, bool clamp) const {
    const double unquantized = t.j2000Seconds();
    if (_timerange.includes(unquantized)) {
        switch (_resolutionUnit) {
        case 'y':
            incrementYear(_dt, const Time& start, const Time& simTime);
            break;

        default:
            break;
        }
        t.setTime(_dt.ISO8601());
        return true;
    }
    else if (clamp) {
        const double clampedTime = glm::clamp(
            unquantized,
            _timerange.start,
            _timerange.end
        );
        t.setTime(clampedTime);
        return true;
    }
    else {
        return false;
    }
}

DateTime::DateTime(std::string initDateTime) {
    year = std::stoi(initDateTime.substr(index_year, len_year));
    month = std::stoi(initDateTime.substr(index_month, len_nonYear));
    day = std::stoi(initDateTime.substr(index_day, len_nonYear));
    hour = std::stoi(initDateTime.substr(index_hour, len_nonYear));
    minute = std::stoi(initDateTime.substr(index_minute, len_nonYear));
    second = std::stoi(initDateTime.substr(index_second, len_nonYear));
};

std::string DateTime::ISO8601() {
    char str[19];
    snprintf(str + index_year, len_year + 1, "%04d-", year);
    snprintf(str + index_month, len_nonYear + 1, "%02d-", month);
    snprintf(str + index_day, len_nonYear + 1, "%02dT", day);
    snprintf(str + index_hour, len_nonYear + 1, "%02d:", hour);
    snprintf(str + index_minute, len_nonYear + 1, "%02d:", minute);
    snprintf(str + index_second, len_nonYear, "%02d", second);
    return std::string(str);
};

void DateTime::incrementYear(double start, double unquantizedTime, double resolution) {
    double quantized = std::floor((unquantizedTime - start) / resolution);
    int incYears = static_cast<int>(quantized);
    double remainder = quantized - static_cast<double>(incYears);
    year += incYears;
};

void TimeQuantizer::incrementYear(DateTime& dt, const Time& start, const Time& simTime) {
    dt.incrementYear(start.j2000Seconds(), simTime.j2000Seconds(),
        parseTimeResolutionStr("1y"));
}

} // namespace openspace::globebrowsing
