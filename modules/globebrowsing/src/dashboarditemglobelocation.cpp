/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <modules/globebrowsing/src/basictypes.h>
#include <modules/globebrowsing/src/renderableglobe.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/interaction/orbitalnavigator.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/distanceconversion.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/font/font.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/misc/profiling.h>

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

    constexpr openspace::properties::Property::PropertyInfo SignificantDigitsInfo = {
        "SignificantDigits",
        "Significant Digits",
        "Determines the number of significant digits that are shown in the location text."
    };

    struct [[codegen::Dictionary(DashboardItemGlobeLocation)]] Parameters {
        // [[codegen::verbatim(FontNameInfo.description)]]
        std::optional<std::string> fontName;

        // [[codegen::verbatim(FontSizeInfo.description)]]
        std::optional<float> fontSize;

        // [[codegen::verbatim(SignificantDigitsInfo.description)]]
        std::optional<int> significantDigits;
    };
#include "dashboarditemglobelocation_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation DashboardItemGlobeLocation::Documentation() {
    return codegen::doc<Parameters>("globebrowsing_dashboarditem_globelocation");
}

DashboardItemGlobeLocation::DashboardItemGlobeLocation(
                                                      const ghoul::Dictionary& dictionary)
    : DashboardItem(dictionary)
    , _fontName(FontNameInfo, KeyFontMono)
    , _fontSize(FontSizeInfo, DefaultFontSize, 10.f, 144.f, 1.f)
    , _significantDigits(SignificantDigitsInfo, 4, 1, 12)
    , _font(global::fontManager->font(KeyFontMono, 10))
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _fontName = p.fontName.value_or(_fontName);
    _fontName.onChange([this]() {
        _font = global::fontManager->font(_fontName, _fontSize);
    });
    addProperty(_fontName);

    _fontSize = p.fontSize.value_or(_fontSize);
    _fontSize.onChange([this]() {
        _font = global::fontManager->font(_fontName, _fontSize);
    });
    addProperty(_fontSize);

    auto updateFormatString = [this]() {
        using namespace fmt::literals;

        _formatString = fmt::format(
            "Position: {{:03.{0}f}}{{}}, {{:03.{0}f}}{{}}  Altitude: {{:03.{0}f}} {{}}",
            _significantDigits.value()
        );
    };
    _significantDigits = p.significantDigits.value_or(_significantDigits);
    _significantDigits.onChange(updateFormatString);
    addProperty(_significantDigits);
    updateFormatString();

    _font = global::fontManager->font(_fontName, _fontSize);
    _buffer.resize(128);
}

void DashboardItemGlobeLocation::render(glm::vec2& penPosition) {
    ZoneScoped

    using namespace globebrowsing;

    const SceneGraphNode* n = global::navigationHandler->orbitalNavigator().anchorNode();
    if (!n) {
        return;
    }
    const RenderableGlobe* globe = dynamic_cast<const RenderableGlobe*>(n->renderable());
    if (!globe) {
        return;
    }

    const glm::dvec3 cameraPosition = global::navigationHandler->camera()->positionVec3();
    const glm::dmat4 inverseModelTransform = glm::inverse(n->modelTransform());
    const glm::dvec3 cameraPositionModelSpace =
        glm::dvec3(inverseModelTransform * glm::dvec4(cameraPosition, 1.0));
    const SurfacePositionHandle posHandle = globe->calculateSurfacePositionHandle(
        cameraPositionModelSpace
    );

    const Geodetic2 geo2 = globe->ellipsoid().cartesianToGeodetic2(
        posHandle.centerToReferenceSurface
    );

    double lat = glm::degrees(geo2.lat);
    double lon = glm::degrees(geo2.lon);

    bool isNorth = lat > 0.0;
    lat = std::abs(lat);

    bool isEast = lon > 0.0;
    lon = std::abs(lon);

    double altitude = glm::length(
        cameraPositionModelSpace - posHandle.centerToReferenceSurface
    );

    if (glm::length(cameraPositionModelSpace) <
        glm::length(posHandle.centerToReferenceSurface))
    {
        altitude = -altitude;
    }

    std::pair<double, std::string> dist = simplifyDistance(altitude);

    std::fill(_buffer.begin(), _buffer.end(), char(0));
    char* end = fmt::format_to(
        _buffer.data(),
        _formatString.c_str(),
        lat, isNorth ? "N" : "S",
        lon, isEast ? "E" : "W",
        dist.first, dist.second
    );
    std::string_view text = std::string_view(_buffer.data(), end - _buffer.data());

    RenderFont(*_font, penPosition, text);
    penPosition.y -= _font->height();
}

glm::vec2 DashboardItemGlobeLocation::size() const {
    ZoneScoped

    return _font->boundingBox(
        fmt::format("Position: {}, {}  Altitude: {}", 1.f, 1.f, 1.f)
    );
}

} // namespace openspace
