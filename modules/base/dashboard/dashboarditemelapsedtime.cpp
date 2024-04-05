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

#include <modules/base/dashboard/dashboarditemelapsedtime.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/timeconversion.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/time.h>
#include <ghoul/font/font.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo FormatStringInfo = {
        "FormatString",
        "Format String",
        "The format text describing how this dashboard item renders its text. This text "
        "must contain exactly one {} which is a placeholder that will contain the value "
        "of the elapsed time.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ReferenceTimeInfo = {
        "ReferenceTime",
        "Reference Time",
        "The reference time relative to which the elapsed time is specified. The format "
        "must be an ISO 8601-compliant date string",
        // @VISIBILITY(2.75)
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo SimplifyTimeInfo = {
        "SimplifyTime",
        "Simplify Time",
        "If this value is enabled, the elapsed time will be simplified into seconds, "
        "minutes, hours, etc. If the value is disabled, the elapsed time is always "
        "presented in seconds. The default value for this is 'true'.",
        // @VISIBILITY(2.25)
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo LowestTimeUnitInfo = {
        "LowestTimeUnit",
        "Lowest Time Unit when Simplifying",
        "If 'SimplifyTime' is enabled, this is the lowest time unit that will be shown. "
        "All finer grained timesteps will be ignored.",
        // @VISIBILITY(2.75)
        openspace::properties::Property::Visibility::User
    };

    struct [[codegen::Dictionary(DashboardItemElapsedTime)]] Parameters {
        // [[codegen::verbatim(FormatStringInfo.description)]]
        std::optional<std::string> formatString;

        // [[codegen::verbatim(ReferenceTimeInfo.description)]]
        std::string referenceTime;

        // [[codegen::verbatim(SimplifyTimeInfo.description)]]
        std::optional<bool> simplifyTime;

        enum class [[codegen::map(openspace::TimeUnit)]] TimeUnit {
            Nanosecond,
            Microsecond,
            Millisecond,
            Second,
            Minute,
            Hour,
            Day,
            Month,
            Year
        };

        // [[codegen::verbatim(LowestTimeUnitInfo.description)]]
        std::optional<TimeUnit> lowestTimeUnit;
    };
#include "dashboarditemelapsedtime_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation DashboardItemElapsedTime::Documentation() {
    return codegen::doc<Parameters>("base_dashboarditem_elapsedtime");
}

DashboardItemElapsedTime::DashboardItemElapsedTime(const ghoul::Dictionary& dictionary)
    : DashboardTextItem(dictionary, 15.f)
    , _formatString(FormatStringInfo, "Elapsed time: {}")
    , _referenceTime(ReferenceTimeInfo)
    , _simplifyTime(SimplifyTimeInfo, true)
    , _lowestTimeUnit(LowestTimeUnitInfo)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _formatString = p.formatString.value_or(_formatString);

    _referenceTime.onChange([this]() {
        _referenceJ2000 = Time::convertTime(_referenceTime);
    });
    _referenceTime = p.referenceTime;
    addProperty(_referenceTime);

    _simplifyTime = p.simplifyTime.value_or(_simplifyTime);
    addProperty(_simplifyTime);

    for (const TimeUnit u : TimeUnits) {
        _lowestTimeUnit.addOption(static_cast<int>(u), std::string(nameForTimeUnit(u)));
    }
    _lowestTimeUnit = static_cast<int>(TimeUnit::Second);
    const TimeUnit u = codegen::map<TimeUnit>(
        p.lowestTimeUnit.value_or(Parameters::TimeUnit::Second)
    );
    _lowestTimeUnit = static_cast<int>(u);
    addProperty(_lowestTimeUnit);
}

void DashboardItemElapsedTime::render(glm::vec2& penPosition) {
    ZoneScoped;

    const double delta = global::timeManager->time().j2000Seconds() - _referenceJ2000;

    if (_simplifyTime) {
        using namespace std::chrono;

        const TimeUnit lowestTime = TimeUnit(_lowestTimeUnit.value());
        const std::string_view lowestUnitS = nameForTimeUnit(lowestTime, false);
        const std::string_view lowestUnitP = nameForTimeUnit(lowestTime, true);

        const std::vector<std::pair<double, std::string_view>> ts = splitTime(delta);
        std::string time;
        for (const std::pair<double, std::string_view>& t : ts) {
            time += std::format("{} {} ", t.first, t.second);
            if (t.second == lowestUnitS || t.second == lowestUnitP) {
                // We have reached the lowest unit the user was interested in
                break;
            }
        }

        // Remove the " " at the end
        time = time.substr(0, time.size() - 2);

        RenderFont(
            *_font,
            penPosition,
            // @CPP26(abock): This can be replaced with std::runtime_format
            std::vformat(_formatString.value(), std::make_format_args(time))
        );
    }
    else {
        std::string time = std::format("{} s", delta);
        RenderFont(
            *_font,
            penPosition,
            // @CPP26(abock): This can be replaced with std::runtime_format
            std::vformat(_formatString.value(), std::make_format_args(time))
        );
    }

    penPosition.y -= _font->height();
}

glm::vec2 DashboardItemElapsedTime::size() const {
    ZoneScoped;

    const double delta = global::timeManager->time().j2000Seconds() - _referenceJ2000;
    // @CPP26(abock): This can be replaced with std::runtime_format
    return _font->boundingBox(
        std::vformat(_formatString.value(), std::make_format_args(delta))
    );
}

} // namespace openspace
