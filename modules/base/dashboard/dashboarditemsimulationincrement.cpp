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

#include <modules/base/dashboard/dashboarditemsimulationincrement.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/util/timeconversion.h>
#include <openspace/util/timemanager.h>
#include <ghoul/font/font.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo SimplificationInfo = {
        "Simplification",
        "Time Simplification",
        "If this value is enabled, the time is displayed in nuanced units, such as "
        "minutes, hours, days, years, etc. If this value is disabled, it is always "
        "displayed in seconds",
        // @VISIBILITY(2.33)
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo RequestedUnitInfo = {
        "RequestedUnit",
        "Requested Unit",
        "If the simplification is disabled, this time unit is used as a destination to "
        "convert the seconds into",
        // @VISIBILITY(2.33)
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo TransitionFormatInfo = {
        "TransitionFormat",
        "Transition Format",
        "Format string used to format the text used while in a delta time transition, "
        "that is if the current delta time is being interpolated to reach a target "
        "delta time. This format gets five parameters in this order:  The target delta "
        "time value, the target delta time unit, the string 'Paused' if the delta time "
        "is paused or the empty string otherwise, the current delta time value, and the "
        "current delta time unit",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo RegularFormatInfo = {
        "RegularFormat",
        "Regular Format",
        "The format string used to format the text if the target delta time is the same "
        "as the current delta time. This format gets three parameters in this order:  "
        "The target delta value, the target delta unit, and the string 'Paused' if the "
        "delta time is paused or the empty string otherwise",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    std::vector<std::string> unitList() {
        std::vector<std::string> res(openspace::TimeUnits.size());
        std::transform(
            openspace::TimeUnits.begin(),
            openspace::TimeUnits.end(),
            res.begin(),
            [](openspace::TimeUnit unit) -> std::string {
                return std::string(nameForTimeUnit(unit));
            }
        );
        return res;
    }

    struct [[codegen::Dictionary(DashboardItemSimulationIncrement)]] Parameters {
        // [[codegen::verbatim(SimplificationInfo.description)]]
        std::optional<bool> simplification;

        // [[codegen::verbatim(RequestedUnitInfo.description)]]
        std::optional<std::string> requestedUnit [[codegen::inlist(unitList())]];

        // [[codegen::verbatim(TransitionFormatInfo.description)]]
        std::optional<std::string> transitionFormat;

        // [[codegen::verbatim(RegularFormatInfo.description)]]
        std::optional<std::string> regularFormat;
    };
#include "dashboarditemsimulationincrement_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation DashboardItemSimulationIncrement::Documentation() {
    return codegen::doc<Parameters>(
        "base_dashboarditem_simulationincrement",
        DashboardTextItem::Documentation()
    );
}

DashboardItemSimulationIncrement::DashboardItemSimulationIncrement(
                                                      const ghoul::Dictionary& dictionary)
    : DashboardTextItem(dictionary)
    , _doSimplification(SimplificationInfo, true)
    , _requestedUnit(RequestedUnitInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _transitionFormat(
        TransitionFormatInfo,
        "Simulation increment: {:.1f} {:s} / second{:s} (current: {:.1f} {:s})"
    )
    , _regularFormat(RegularFormatInfo, "Simulation increment: {:.1f} {:s} / second{:s}")
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _doSimplification = p.simplification.value_or(_doSimplification);
    _doSimplification.onChange([this]() {
        _requestedUnit.setVisibility(
            _doSimplification ?
            properties::Property::Visibility::Hidden :
            properties::Property::Visibility::User
        );
    });
    addProperty(_doSimplification);

    for (const TimeUnit u : TimeUnits) {
        _requestedUnit.addOption(static_cast<int>(u), std::string(nameForTimeUnit(u)));
    }
    _requestedUnit = static_cast<int>(TimeUnit::Second);
    if (p.requestedUnit.has_value()) {
        const TimeUnit unit = timeUnitFromString(*p.requestedUnit);
        _requestedUnit = static_cast<int>(unit);
    }
    _requestedUnit.setVisibility(properties::Property::Visibility::Hidden);
    addProperty(_requestedUnit);

    _transitionFormat = p.transitionFormat.value_or(_transitionFormat);
    addProperty(_transitionFormat);

    _regularFormat = p.regularFormat.value_or(_regularFormat);
    addProperty(_regularFormat);
}

void DashboardItemSimulationIncrement::render(glm::vec2& penPosition) {
    ZoneScoped;

    const double targetDt = global::timeManager->targetDeltaTime();
    const double currentDt = global::timeManager->deltaTime();
    std::pair<double, std::string> targetDeltaTime;
    std::pair<double, std::string> currentDeltaTime;
    if (_doSimplification) {
        targetDeltaTime = simplifyTime(targetDt);
        if (targetDt != currentDt) {
            currentDeltaTime = simplifyTime(currentDt);
        }
    }
    else {
        const TimeUnit unit = static_cast<TimeUnit>(_requestedUnit.value());

        const double convTarget = convertTime(targetDt, TimeUnit::Second, unit);
        targetDeltaTime = std::pair(
            convTarget,
            std::string(nameForTimeUnit(unit, convTarget != 1.0))
        );

        if (targetDt != currentDt) {
            const double convCurrent = convertTime(currentDt, TimeUnit::Second, unit);
            currentDeltaTime = std::pair(
                convCurrent,
                std::string(nameForTimeUnit(unit, convCurrent != 1.0))
            );
        }
    }

    std::string pauseText = global::timeManager->isPaused() ? " (Paused)" : "";

    try {
        if (targetDt != currentDt && !global::timeManager->isPaused()) {
            // We are in the middle of a transition
            RenderFont(
                *_font,
                penPosition,
                // @CPP26(abock): This can be replaced with std::runtime_format
                std::vformat(
                    _transitionFormat.value(),
                    std::make_format_args(
                        targetDeltaTime.first, targetDeltaTime.second,
                        pauseText,
                        currentDeltaTime.first, currentDeltaTime.second
                    )
                )
            );
        }
        else {
            RenderFont(
                *_font,
                penPosition,
                // @CPP26(abock): This can be replaced with std::runtime_format
                std::vformat(
                    _regularFormat.value(),
                    std::make_format_args(
                        targetDeltaTime.first,
                        targetDeltaTime.second,
                        pauseText
                    )
                )
            );
        }
    }
    catch (const std::format_error&) {
        LERRORC("DashboardItemDate", "Illegal format string");
    }
    penPosition.y -= _font->height();
}

glm::vec2 DashboardItemSimulationIncrement::size() const {
    ZoneScoped;

    const double t = global::timeManager->targetDeltaTime();
    std::pair<double, std::string> deltaTime;
    if (_doSimplification) {
        deltaTime = simplifyTime(t);
    }
    else {
        const TimeUnit unit = static_cast<TimeUnit>(_requestedUnit.value());
        const double convertedT = convertTime(t, TimeUnit::Second, unit);
        deltaTime = std::pair(
            convertedT,
            std::string(nameForTimeUnit(unit, convertedT != 1.0))
        );
    }

    return _font->boundingBox(
        std::format(
            "Simulation increment: {:.1f} {:s} / second",
            deltaTime.first, deltaTime.second
        )
    );
}

} // namespace openspace
