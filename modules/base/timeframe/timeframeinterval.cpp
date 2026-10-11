/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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
#include <openspace/logging/logmanager.h>
#include <openspace/misc/dictionary.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>
#include <openspace/util/timemanager.h>
#include <optional>
#include <string>
#include <variant>

namespace {
    using namespace openspace;

    constexpr std::string_view _loggerCat = "TimeFrameInterval";

    constexpr BoolPropertyInfo HasStartInfo = {
        .identifier = "HasStart",
        .name = "Has start",
        .description = "If enabled, this TimeFrame will be inactive before the Start "
            "time.",
        .visibility = Visibility::User,
        .defaults = {
            .value = false
        }
    };

    constexpr DoublePropertyInfo StartInfo = {
        .identifier = "Start",
        .name = "Start",
        .description = "Specifies the time when this TimeFrame becomes active.",
        .visibility = Visibility::User,
        .defaults = {
            .value = 0,
            .minMax = { 0, 1E9 }
        }
    };

    constexpr PropertyInfo JumpToStartInfo = {
        .identifier = "JumpToStart",
        .name = "Jump to start",
        .description = "Performs a time jump to the start of the interval. Disabled if "
            "no start time is specified.",
        .visibility = Visibility::NoviceUser
    };

    constexpr BoolPropertyInfo HasEndInfo = {
        .identifier = "HasEnd",
        .name = "Has end",
        .description = "If enabled, this TimeFrame will be inactive after the End time.",
        .visibility = Visibility::User,
        .defaults = {
            .value = false
        }
    };

    constexpr DoublePropertyInfo EndInfo = {
        .identifier = "End",
        .name = "End",
        .description = "Specifies the time when this TimeFrame becomes inactive.",
        .visibility = Visibility::User,
        .defaults = {
            .value = 0,
            .minMax = { 0, 1E9 }
        }
    };

    constexpr PropertyInfo JumpToEndInfo = {
        .identifier = "JumpToEnd",
        .name = "Jump to end",
        .description = "Performs a time jump to the end of the interval. Disabled if no "
            "end time is specified.",
        .visibility = Visibility::NoviceUser
    };

    // This `TimeFrame`'s validity is determined by a single start and end time in between
    // which the scene graph node is valid. The start time is inclusive, while the end
    // time is exclusive. If the start time or end time is not specified, the value
    // corresponds to $-\infty$ and $\infty$ respectively. If both the start and end time
    // are not specified, the `TimeFrameInterval` will always be valid.
    struct [[codegen::Dictionary(TimeFrameInterval)]] Parameters {
        // [[codegen::verbatim(StartInfo.description)]]
        std::optional<std::variant<double, std::string>> start;

        // [[codegen::verbatim(EndInfo.description)]]
        std::optional<std::variant<double, std::string>> end;
    };
} // namespace
#include "timeframeinterval_codegen.cpp"

namespace openspace {

Documentation TimeFrameInterval::Documentation() {
    return codegen::doc<Parameters>(
        "base_timeframe_interval",
        TimeFrame::Documentation()
    );
}

TimeFrameInterval::TimeFrameInterval(const Dictionary& dictionary)
    : _hasStart(HasStartInfo)
    , _start(StartInfo)
    , _jumpToStart(JumpToStartInfo)
    , _hasEnd(HasEndInfo)
    , _end(EndInfo)
    , _jumpToEnd(JumpToEndInfo)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _jumpToStart.onChange([this]() {
        if (_hasStart) {
            global::timeManager->setTimeNextFrame(Time(_start));
        }
        else {
            LWARNING("Cannot jump to start because no start time is specified");
        }
    });
    addProperty(_jumpToStart);

    _jumpToEnd.onChange([this]() {
        if (_hasEnd) {
            global::timeManager->setTimeNextFrame(Time(_end));
        }
        else {
            LWARNING("Cannot jump to end because no end time is specified");
        }
    });
    addProperty(_jumpToEnd);

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

    if (!_hasStart) {
        _jumpToStart.setReadOnly(true);
    }

    if (!_hasEnd) {
        _jumpToEnd.setReadOnly(true);
    }
}

void TimeFrameInterval::update(const Time& time) {
    if (_hasStart && time.j2000Seconds() < _start) {
        _isInTimeFrame = false;
    }
    else if (_hasEnd && time.j2000Seconds() >= _end) {
        _isInTimeFrame = false;
    }
    else {
        _isInTimeFrame = true;
    }
}

} // namespace openspace
