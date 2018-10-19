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

#include <modules/base/timeframe/timeframeinterval.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>

namespace {
    constexpr const openspace::properties::Property::PropertyInfo HasStartInfo = {
        "HasStart",
        "Has Start",
        "If enabled, this TimeFrame will be inactive before the Start"
    };

    constexpr const openspace::properties::Property::PropertyInfo StartInfo = {
        "Start",
        "Start",
        "Specifies the time when this TimeFrame becomes active"
    };

    constexpr const openspace::properties::Property::PropertyInfo HasEndInfo = {
        "HasEnd",
        "Has End",
        "If enabled, this TimeFrame will be inactive after the End"
    };

    constexpr const openspace::properties::Property::PropertyInfo EndInfo = {
        "End",
        "End",
        "Specifies the time when this TimeFrame becomes inactive"
    };
} // namespace

namespace openspace {

documentation::Documentation TimeFrameInterval::Documentation() {
    using namespace openspace::documentation;
    return {
        "Time Frame Interval",
        "base_time_frame_interval",
        {
            {
                HasStartInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                HasStartInfo.description
            },
            {
                StartInfo.identifier,
                new OrVerifier({ new DoubleVerifier, new StringVerifier }),
                Optional::Yes,
                StartInfo.description
            },
            {
                HasEndInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                HasEndInfo.description
            },
            {
                EndInfo.identifier,
                new OrVerifier({ new DoubleVerifier, new StringVerifier }),
                Optional::Yes,
                EndInfo.description
            },
        }
    };
}

bool TimeFrameInterval::isActive(const Time& time) const {
    if (_hasStart && time.j2000Seconds() < _start ) {
        return false;
    }
    if (_hasEnd && time.j2000Seconds() >= _end ) {
        return false;
    }
    return true;
}

TimeFrameInterval::TimeFrameInterval()
    : _hasStart(HasStartInfo, false)
    , _start(StartInfo, 0, 0, 1E9)
    , _hasEnd(HasEndInfo, false)
    , _end(EndInfo, 0, 0, 1E9)
{
    addProperty(_hasStart);
    addProperty(_start);
    addProperty(_hasEnd);
    addProperty(_end);
}

TimeFrameInterval::TimeFrameInterval(const ghoul::Dictionary& dictionary)
    : TimeFrame()
    , _hasStart(HasStartInfo, false)
    , _start(StartInfo, 0, 0, 1E9)
    , _hasEnd(HasEndInfo, false)
    , _end(EndInfo, 0, 0, 1E9)
{
    addProperty(_hasStart);
    addProperty(_start);
    addProperty(_hasEnd);
    addProperty(_end);

    documentation::testSpecificationAndThrow(Documentation(),
                                             dictionary,
                                             "TimeFrameInterval");

    if (dictionary.hasValue<std::string>(StartInfo.identifier)) {
        _start = SpiceManager::ref().ephemerisTimeFromDate(
            dictionary.value<std::string>(StartInfo.identifier)
        );
        _hasStart = true;
    } else if (dictionary.hasValue<double>(StartInfo.identifier)) {
        _start = dictionary.value<double>(StartInfo.identifier);
        _hasStart = true;
    }

    if (dictionary.hasValue<std::string>(EndInfo.identifier)) {
        _end = SpiceManager::ref().ephemerisTimeFromDate(
            dictionary.value<std::string>(EndInfo.identifier)
        );
        _hasEnd = true;
    }
    else if (dictionary.hasValue<double>(EndInfo.identifier)) {
        _end = dictionary.value<double>(EndInfo.identifier);
        _hasEnd = true;
    }
}

} // namespace openspace
