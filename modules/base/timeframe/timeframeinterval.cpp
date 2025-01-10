/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
#include <optional>

namespace {
    constexpr openspace::properties::Property::PropertyInfo HasStartInfo = {
        "HasStart",
        "Has Start",
        "If enabled, this TimeFrame will be inactive before the Start.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo StartInfo = {
        "Start",
        "Start",
        "Specifies the time when this TimeFrame becomes active.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo HasEndInfo = {
        "HasEnd",
        "Has End",
        "If enabled, this TimeFrame will be inactive after the End.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo EndInfo = {
        "End",
        "End",
        "Specifies the time when this TimeFrame becomes inactive.",
        openspace::properties::Property::Visibility::User
    };

    struct [[codegen::Dictionary(TimeFrameInterval)]] Parameters {
        // [[codegen::verbatim(StartInfo.description)]]
        std::optional<std::variant<double, std::string>> start;

        // [[codegen::verbatim(EndInfo.description)]]
        std::optional<std::variant<double, std::string>> end;
    };
#include "timeframeinterval_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation TimeFrameInterval::Documentation() {
    return codegen::doc<Parameters>("base_time_frame_interval");
}

bool TimeFrameInterval::isActive(const Time& time) const {
    if (_hasStart && time.j2000Seconds() < _start) {
        return false;
    }
    if (_hasEnd && time.j2000Seconds() >= _end) {
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
    : _hasStart(HasStartInfo, false)
    , _start(StartInfo, 0, 0, 1E9)
    , _hasEnd(HasEndInfo, false)
    , _end(EndInfo, 0, 0, 1E9)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    if (p.start.has_value()) {
        if (std::holds_alternative<double>(*p.start)) {
            _start = std::get<double>(*p.start);
        }
        else {
            _start = SpiceManager::ref().ephemerisTimeFromDate(
                std::get<std::string>(*p.start)
            );
        }
    }
    _hasStart = p.start.has_value();
    addProperty(_hasStart);
    addProperty(_start);

    if (p.end.has_value()) {
        if (std::holds_alternative<double>(*p.end)) {
            _end = std::get<double>(*p.end);
        }
        else {
            _end = SpiceManager::ref().ephemerisTimeFromDate(
                std::get<std::string>(*p.end)
            );
        }
    }
    _hasEnd = p.end.has_value();
    addProperty(_hasEnd);
    addProperty(_end);
}

} // namespace openspace
