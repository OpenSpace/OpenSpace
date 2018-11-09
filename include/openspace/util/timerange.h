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

#ifndef __OPENSPACE_CORE___TIMERANGE___H__
#define __OPENSPACE_CORE___TIMERANGE___H__

#include <limits>

namespace ghoul { class Dictionary; }

namespace openspace {

namespace documentation { struct Documentation; }

struct TimeRange {
    /**
     * Default constructor initializes an empty time range.
     */
    TimeRange() = default;

    /**
     * Initializes a TimeRange with both start and end time. Initializing empty timeranges
     * is OK.
     */
    TimeRange(double startTime, double endTime);

    /**
     * Throws exception if unable to parse the provided \class ghoul::Dictionary
     */
    TimeRange(const ghoul::Dictionary& dict);

    /**
     * \return \c true if timeRange could be initialized from the dictionary, \c false
     *         otherwise.
     */
    static bool initializeFromDictionary(const ghoul::Dictionary& dict,
        TimeRange& timeRange);

    void include(double val);

    void include(const TimeRange& other);

    double duration() const;

    bool isDefined() const;

    bool isEmpty() const;

    bool inRange(double min, double max) const;

    bool includes(double val) const;

    bool includes(const TimeRange& o) const;

    static documentation::Documentation Documentation();

    double start = std::numeric_limits<double>::max();
    double end = -std::numeric_limits<double>::max();
};

} // namespace openspace

#endif // __OPENSPACE_CORE___TIMERANGE___H__
