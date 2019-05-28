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

#include <modules/base/dashboard/dashboarditemvelocity.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/camera.h>
#include <openspace/util/distanceconversion.h>
#include <ghoul/font/font.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/logging/logmanager.h>

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
        "This value determines the size of the font that is used to render the velocity."
    };

    constexpr openspace::properties::Property::PropertyInfo SimplificationInfo = {
        "Simplification",
        "Simplification",
        "If this value is enabled, the velocity is displayed in nuanced units, such as "
        "m/s, AU/s, light years / s etc. If this value is disabled, the unit can be "
        "explicitly requested."
    };

    constexpr openspace::properties::Property::PropertyInfo RequestedUnitInfo = {
        "RequestedUnit",
        "Requested Unit",
        "If the simplification is disabled, this distance unit is used for the velocity "
        "display."
    };

    std::vector<std::string> unitList() {
        std::vector<std::string> res(openspace::DistanceUnits.size());
        std::transform(
            openspace::DistanceUnits.begin(),
            openspace::DistanceUnits.end(),
            res.begin(),
            [](openspace::DistanceUnit unit) -> std::string {
                return nameForDistanceUnit(unit);
            }
        );
        return res;
    }
} // namespace

namespace openspace {

documentation::Documentation DashboardItemVelocity::Documentation() {
    using namespace documentation;
    return {
        "DashboardItem Velocity",
        "base_dashboarditem_velocity",
        {
            {
                "Type",
                new StringEqualVerifier("DashboardItemVelocity"),
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

DashboardItemVelocity::DashboardItemVelocity(const ghoul::Dictionary& dictionary)
    : DashboardItem(dictionary)
    , _fontName(FontNameInfo, KeyFontMono)
    , _fontSize(FontSizeInfo, DefaultFontSize, 6.f, 144.f, 1.f)
    , _doSimplification(SimplificationInfo, true)
    , _requestedUnit(RequestedUnitInfo, properties::OptionProperty::DisplayType::Dropdown)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "DashboardItemVelocity"
    );

    if (dictionary.hasKey(FontNameInfo.identifier)) {
        _fontName = dictionary.value<std::string>(FontNameInfo.identifier);
    }
    if (dictionary.hasKey(FontSizeInfo.identifier)) {
        _fontSize = static_cast<float>(dictionary.value<double>(FontSizeInfo.identifier));
    }

    _fontName.onChange([this]() {
        _font = global::fontManager.font(_fontName, _fontSize);
    });
    addProperty(_fontName);

    _fontSize.onChange([this]() {
        _font = global::fontManager.font(_fontName, _fontSize);
    });
    addProperty(_fontSize);

    if (dictionary.hasKey(SimplificationInfo.identifier)) {
        _doSimplification = dictionary.value<bool>(SimplificationInfo.identifier);
    }
    _doSimplification.onChange([this]() {
        _requestedUnit.setVisibility(
            _doSimplification ?
            properties::Property::Visibility::Hidden :
            properties::Property::Visibility::User
        );
    });
    addProperty(_doSimplification);

    for (DistanceUnit u : DistanceUnits) {
        _requestedUnit.addOption(static_cast<int>(u), nameForDistanceUnit(u));
    }
    _requestedUnit = static_cast<int>(DistanceUnit::Meter);
    if (dictionary.hasKey(RequestedUnitInfo.identifier)) {
        const std::string& value = dictionary.value<std::string>(
            RequestedUnitInfo.identifier
        );
        DistanceUnit unit = distanceUnitFromString(value.c_str());
        _requestedUnit = static_cast<int>(unit);
    }
    _requestedUnit.setVisibility(properties::Property::Visibility::Hidden);
    addProperty(_requestedUnit);

    _font = global::fontManager.font(_fontName, _fontSize);
}

void DashboardItemVelocity::render(glm::vec2& penPosition) {
    const glm::dvec3 currentPos = global::renderEngine.scene()->camera()->positionVec3();
    const glm::dvec3 dt = currentPos - _prevPosition;
    const double speedPerFrame = glm::length(dt);

    const double secondsPerFrame = global::windowDelegate.averageDeltaTime();

    const double speedPerSecond = speedPerFrame / secondsPerFrame;

    std::pair<double, std::string> dist;
    if (_doSimplification) {
        dist = simplifyDistance(speedPerSecond);
    }
    else {
        const DistanceUnit unit = static_cast<DistanceUnit>(_requestedUnit.value());
        const double convertedD = convertDistance(speedPerSecond, unit);
        dist = { convertedD, nameForDistanceUnit(unit, convertedD != 1.0) };
    }

    penPosition.y -= _font->height();
    RenderFont(
        *_font,
        penPosition,
        fmt::format(
            "Camera velocity: {} {}/s", dist.first, dist.second
        )
    );

    _prevPosition = currentPos;
}

glm::vec2 DashboardItemVelocity::size() const {
    const double d = glm::length(1e20);
    std::pair<double, std::string> dist;
    if (_doSimplification) {
        dist = simplifyDistance(d);
    }
    else {
        DistanceUnit unit = static_cast<DistanceUnit>(_requestedUnit.value());
        double convertedD = convertDistance(d, unit);
        dist = { convertedD, nameForDistanceUnit(unit, convertedD != 1.0) };
    }

    return ghoul::fontrendering::FontRenderer::defaultRenderer().boundingBox(
        *_font,
        fmt::format("Camera velocity: {} {}/s", dist.first, dist.second)
    ).boundingBox;
}

} // namespace openspace
