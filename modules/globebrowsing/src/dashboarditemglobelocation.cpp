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

#include <modules/globebrowsing/src/dashboarditemglobelocation.h>

#include <modules/globebrowsing/globebrowsingmodule.h>
#include <modules/globebrowsing/src/basictypes.h>
#include <modules/globebrowsing/src/renderableglobe.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/orbitalnavigator.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/distanceconversion.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/font/font.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/misc/profiling.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo DisplayFormatInfo = {
        "DisplayFormat",
        "Display Format",
        "Choosing the format in which the camera location is displayed.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo SignificantDigitsInfo = {
        "SignificantDigits",
        "Significant Digits",
        "Determines the number of significant digits that are shown in the location "
        "text.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(DashboardItemGlobeLocation)]] Parameters {
        enum class DisplayFormat {
            DecimalDegrees,
            DegreeMinuteSeconds
        };

        // [[codegen::verbatim(DisplayFormatInfo.description)]]
        std::optional<DisplayFormat> displayFormat;

        // [[codegen::verbatim(SignificantDigitsInfo.description)]]
        std::optional<int> significantDigits;
    };
#include "dashboarditemglobelocation_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation DashboardItemGlobeLocation::Documentation() {
    return codegen::doc<Parameters>(
        "globebrowsing_dashboarditem_globelocation",
        DashboardTextItem::Documentation()
    );
}

DashboardItemGlobeLocation::DashboardItemGlobeLocation(
                                                      const ghoul::Dictionary& dictionary)
    : DashboardTextItem(dictionary)
    , _displayFormat(DisplayFormatInfo)
    , _significantDigits(SignificantDigitsInfo, 4, 1, 12)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    auto updateFormatString = [this]() {
        switch (_displayFormat.value()) {
            case static_cast<int>(DisplayFormat::DecimalDegrees):
                _formatString = std::format(
                    "Position: {{:03.{0}f}}, {{:03.{0}f}}  "
                    "Altitude: {{:03.{0}f}} {{}}",
                    _significantDigits.value()
                );
                break;
            case static_cast<int>(DisplayFormat::DegreeMinuteSeconds):
                _formatString = std::format(
                    "Position: {{}}d {{}}' {{:03.{0}f}}\" {{}}, "
                    "{{}}d {{}}' {{:03.{0}f}}\" {{}}  "
                    "Altitude: {{:03.{0}f}} {{}}",
                    _significantDigits.value()
                );
                break;
        }
    };

    _displayFormat.addOptions({
        { static_cast<int>(DisplayFormat::DecimalDegrees), "Decimal Degrees" },
        { static_cast<int>(DisplayFormat::DegreeMinuteSeconds), "Degree Minute Seconds" }
    });
    _displayFormat.onChange(updateFormatString);
    addProperty(_displayFormat);

    if (p.displayFormat.has_value()) {
        switch (*p.displayFormat) {
            case Parameters::DisplayFormat::DecimalDegrees:
                _displayFormat = static_cast<int>(DisplayFormat::DecimalDegrees);
                break;
            case Parameters::DisplayFormat::DegreeMinuteSeconds:
                _displayFormat = static_cast<int>(DisplayFormat::DegreeMinuteSeconds);
                break;
        }
    }
    else {
        _displayFormat = static_cast<int>(DisplayFormat::DecimalDegrees);
    }


    _significantDigits = p.significantDigits.value_or(_significantDigits);
    _significantDigits.onChange(updateFormatString);
    addProperty(_significantDigits);

    _font = global::fontManager->font(_fontName, _fontSize);
    _buffer.resize(128);
    updateFormatString();
}

void DashboardItemGlobeLocation::render(glm::vec2& penPosition) {
    ZoneScoped;

    GlobeBrowsingModule* module = global::moduleEngine->module<GlobeBrowsingModule>();

    const glm::dvec3 position = module->geoPosition();
    double lat = position.x;
    double lon = position.y;
    const double altitude = position.z;

    std::pair<double, std::string_view> dist = simplifyDistance(altitude);

    std::fill(_buffer.begin(), _buffer.end(), char(0));
    char* end = nullptr;
    switch (_displayFormat.value()) {
        case static_cast<int>(DisplayFormat::DecimalDegrees):
        {
            // @CPP26(abock): This can be replaced with std::runtime_format
            end = std::vformat_to(
                _buffer.data(),
                _formatString,
                std::make_format_args(lat, lon, dist.first, dist.second)
            );
            break;
        }
        case static_cast<int>(DisplayFormat::DegreeMinuteSeconds):
        {
            const bool isNorth = lat > 0.0;
            lat = std::abs(lat);

            const bool isEast = lon > 0.0;
            lon = std::abs(lon);

            const double latDeg = std::trunc(lat);
            const double latDegRemainder = lat - latDeg;
            const double latMin = std::trunc(latDegRemainder * 60.f);
            const double latMinRemainder = latDegRemainder * 60.f - latMin;
            const double latSec = latMinRemainder * 60.f;

            const double lonDeg = std::trunc(lon);
            const double lonDegRemainder = lon - lonDeg;
            const double lonMin = std::trunc(lonDegRemainder * 60.f);
            const double lonMinRemainder = lonDegRemainder * 60.f - lonMin;
            const double lonSec = lonMinRemainder * 60.f;


            // @CPP26(abock): This can be replaced with std::runtime_format
            end = std::vformat_to(
                _buffer.data(),
                _formatString,
                std::make_format_args(
                    latDeg, latMin, latSec, isNorth ? "N" : "S",
                    lonDeg, lonMin, lonSec, isEast ? "E" : "W",
                    dist.first, dist.second
                )
            );

            break;
        }
    }

    penPosition.y -= _font->height();
    const std::string_view text = std::string_view(_buffer.data(), end - _buffer.data());
    RenderFont(*_font, penPosition, text);
}

glm::vec2 DashboardItemGlobeLocation::size() const {
    ZoneScoped;

    return _font->boundingBox(
        std::format("Position: {}, {}  Altitude: {}", 1.f, 1.f, 1.f)
    );
}

} // namespace openspace
