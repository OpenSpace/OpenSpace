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

#include <modules/globebrowsing/other/timequantizer.h>

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

double TimeQuantizer::parseTimeResolutionStr(const std::string& resoltutionStr) {
    const char unit = resoltutionStr.back();
    std::string numberString = resoltutionStr.substr(0, resoltutionStr.length() - 1);

    char* p;
    double value = strtol(numberString.c_str(), &p, 10);
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

} // namespace openspace::globebrowsing
