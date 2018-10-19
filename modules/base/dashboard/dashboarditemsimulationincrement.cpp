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

#include <modules/base/dashboard/dashboarditemsimulationincrement.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/util/timeconversion.h>
#include <openspace/util/timemanager.h>
#include <ghoul/font/font.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>

namespace {
    constexpr const char* KeyFontMono = "Mono";
    constexpr const float DefaultFontSize = 10.f;

    constexpr openspace::properties::Property::PropertyInfo FontNameInfo = {
        "FontName",
        "Font Name",
        "This value is the name of the font that is used. It can either refer to an "
        "internal name registered previously, or it can refer to a path that is used."
    };

    constexpr openspace::properties::Property::PropertyInfo FontSizeInfo = {
        "FontSize",
        "Font Size",
        "This value determines the size of the font that is used to render the date."
    };

    constexpr openspace::properties::Property::PropertyInfo SimplificationInfo = {
        "Simplification",
        "Time Simplification",
        "If this value is enabled, the time is displayed in nuanced units, such as "
        "minutes, hours, days, years, etc. If this value is disabled, it is always "
        "displayed in seconds."
    };

    constexpr openspace::properties::Property::PropertyInfo RequestedUnitInfo = {
        "RequestedUnit",
        "Requested Unit",
        "If the simplification is disabled, this time unit is used as a destination to "
        "convert the seconds into."
    };

    std::vector<std::string> unitList() {
        std::vector<std::string> res(openspace::TimeUnits.size());
        std::transform(
            openspace::TimeUnits.begin(),
            openspace::TimeUnits.end(),
            res.begin(),
            [](openspace::TimeUnit unit) -> std::string { return nameForTimeUnit(unit); }
        );
        return res;
    }
} // namespace

namespace openspace {

documentation::Documentation DashboardItemSimulationIncrement::Documentation() {
    using namespace documentation;
    return {
        "DashboardItem Simulation Increment",
        "base_dashboarditem_simulationincrement",
        {
            {
                "Type",
                new StringEqualVerifier("DashboardItemSimulationIncrement"),
                Optional::No
            },
            {
                FontNameInfo.identifier,
                new StringVerifier,
                Optional::Yes,
                FontNameInfo.description
            },
            {
                FontSizeInfo.identifier,
                new IntVerifier,
                Optional::Yes,
                FontSizeInfo.description
            },
            {
                SimplificationInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                SimplificationInfo.description
            },
            {
                RequestedUnitInfo.identifier,
                new StringInListVerifier(unitList()),
                Optional::Yes,
                RequestedUnitInfo.description
            }
        }
    };
}

DashboardItemSimulationIncrement::DashboardItemSimulationIncrement(
                                                      const ghoul::Dictionary& dictionary)
    : DashboardItem(dictionary)
    , _fontName(FontNameInfo, KeyFontMono)
    , _fontSize(FontSizeInfo, DefaultFontSize, 6.f, 144.f, 1.f)
    , _doSimplification(SimplificationInfo, true)
    , _requestedUnit(RequestedUnitInfo, properties::OptionProperty::DisplayType::Dropdown)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "DashboardItemSimulationIncrement"
    );

    if (dictionary.hasKey(FontNameInfo.identifier)) {
        _fontName = dictionary.value<std::string>(FontNameInfo.identifier);
    }
    _fontName.onChange([this](){
        _font = global::fontManager.font(_fontName, _fontSize);
    });
    addProperty(_fontName);

    if (dictionary.hasKey(FontSizeInfo.identifier)) {
        _fontSize = static_cast<float>(dictionary.value<double>(FontSizeInfo.identifier));
    }
    _fontSize.onChange([this](){
        _font = global::fontManager.font(_fontName, _fontSize);
    });
    addProperty(_fontSize);

    if (dictionary.hasKey(SimplificationInfo.identifier)) {
        _doSimplification = dictionary.value<bool>(SimplificationInfo.identifier);
    }
    _doSimplification.onChange([this]() {
        if (_doSimplification) {
            _requestedUnit.setVisibility(properties::Property::Visibility::Hidden);
        }
        else {
            _requestedUnit.setVisibility(properties::Property::Visibility::User);
        }
    });
    addProperty(_doSimplification);

    for (TimeUnit u : TimeUnits) {
        _requestedUnit.addOption(static_cast<int>(u), nameForTimeUnit(u));
    }
    _requestedUnit = static_cast<int>(TimeUnit::Second);
    if (dictionary.hasKey(RequestedUnitInfo.identifier)) {
        std::string value = dictionary.value<std::string>(RequestedUnitInfo.identifier);
        TimeUnit unit = timeUnitFromString(value.c_str());
        _requestedUnit = static_cast<int>(unit);
    }
    _requestedUnit.setVisibility(properties::Property::Visibility::Hidden);
    addProperty(_requestedUnit);

    _font = global::fontManager.font(_fontName, _fontSize);
}

void DashboardItemSimulationIncrement::render(glm::vec2& penPosition) {
    const double targetDt = global::timeManager.targetDeltaTime();
    const double currentDt = global::timeManager.deltaTime();
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
        targetDeltaTime = { convTarget, nameForTimeUnit(unit, convTarget != 1.0) };

        if (targetDt != currentDt) {
            const double convCurrent = convertTime(currentDt, TimeUnit::Second, unit);
            currentDeltaTime = { convCurrent, nameForTimeUnit(unit, convCurrent != 1.0) };
        }
    }

    std::string pauseText = global::timeManager.isPaused() ? " (Paused)" : "";

    penPosition.y -= _font->height();
    if (targetDt != currentDt && !global::timeManager.isPaused()) {
        // We are in the middle of a transition
        RenderFont(
            *_font,
            penPosition,
            fmt::format(
                "Simulation increment: {:.1f} {:s} / second{:s} (current: {:.1f} {:s})",
                targetDeltaTime.first, targetDeltaTime.second,
                pauseText,
                currentDeltaTime.first, currentDeltaTime.second
            )
        );
    }
    else {
        RenderFont(
            *_font,
            penPosition,
            fmt::format(
                "Simulation increment: {:.1f} {:s} / second{:s}",
                targetDeltaTime.first, targetDeltaTime.second,
                pauseText
            )
        );
    }
}

glm::vec2 DashboardItemSimulationIncrement::size() const {
    double t = global::timeManager.targetDeltaTime();
    std::pair<double, std::string> deltaTime;
    if (_doSimplification) {
        deltaTime = simplifyTime(t);
    }
    else {
        TimeUnit unit = static_cast<TimeUnit>(_requestedUnit.value());
        double convertedT = convertTime(t, TimeUnit::Second, unit);
        deltaTime = { convertedT, nameForTimeUnit(unit, convertedT != 1.0) };
    }

    return ghoul::fontrendering::FontRenderer::defaultRenderer().boundingBox(
        *_font,
        fmt::format(
            "Simulation increment: {:.1f} {:s} / second",
            deltaTime.first, deltaTime.second
        )
    ).boundingBox;
}

} // namespace openspace
