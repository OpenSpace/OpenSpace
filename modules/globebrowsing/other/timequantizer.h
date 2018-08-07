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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___TIMEQUANTIZER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___TIMEQUANTIZER___H__

#include <openspace/util/timerange.h>
#include <string>
#include <vector>

namespace openspace { class Time; }

namespace openspace::globebrowsing {

/**
* Used to quantize time to descrete values.
*/
struct TimeQuantizer {
    TimeQuantizer() = default;
    TimeQuantizer(const Time& start, const Time& end, double resolution);
    TimeQuantizer(const Time& start, const Time& end, const std::string& resolution);

    /**
    * Takes a time resulition string and parses it into a double
    * value representing the time resolution as seconds.
    *
    * Example: parseTimeResolutionStr("1d");
    *
    * \param resoltutionStr with the format {number}{unit}
    *        where supported units are:
    *        (s)econds, (m)inutes, (h)ours, (d)ays, (y)ears
    * \return the time resolution in seconds
    */
    static double parseTimeResolutionStr(const std::string& resoltutionStr);

    /**
    * Quantizes a OpenSpace Time into descrete values. If the provided Time \p t is
    * outside the time range, it will be clamped to the the time range.
    *
    * \param t Time instance, which will be quantized
    * \param clamp Whether or not time should be clamped if not t is in the time range
    * \return wether or not time was quantized
    */
    bool quantize(Time& t, bool clamp) const;

    /**
    * Returns a list of quantized Time objects that represent all the valid quantized
    * Time%s between \p start and \p end.
    *
    * \param start The start time for the time range quantization
    * \param end The end time for the time range quantization
    * \return A list of quantized times between \p start and \end
    */
    std::vector<Time> quantized(const Time& start, const Time& end) const;

private:
    TimeRange _timerange;
    double _resolution;
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___TIMEQUANTIZER___H__
