/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#include <modules/globebrowsing/src/geojson/geojsonproperties.h>

#include <openspace/documentation/documentation.h>
#include <ghoul/logging/logmanager.h>
#include <geos/io/GeoJSON.h>
#include <cstdio>

// Keys used to read properties from GeoJson files
// @TODO: Go through and decide what keys we actually want. (Started out with just using css keys)
namespace geojson::propertykeys {
    constexpr std::string_view Name = "name";
    constexpr std::string_view Description = "description";

    constexpr std::string_view Opacity = "opacity";

    constexpr std::array<std::string_view, 2> Color = { "color", "stroke" };
    constexpr std::array<std::string_view, 2> FillColor = { "fill", "fill-color" };
    constexpr std::string_view FillOpacity = "fill-opacity";
    constexpr std::string_view LineWidth = "stroke-width";

    constexpr std::string_view PointSize = "point-size"; // TODO: remove?
    constexpr std::array<std::string_view, 3> Texture = { "texture", "sprite", "point-texture" };

    // @TODO: point render mode

    constexpr std::string_view Extrude = "extrude";

    constexpr std::string_view PerformShading = "performShading";

    constexpr std::string_view Tesselate = "tesselate";
    constexpr std::string_view TesselationLevel = "tesselationLevel";
    constexpr std::string_view TesselationMaxDistance = "tesselationMaxDistance";

    constexpr std::string_view AltitudeMode = "altitudeMode";
    constexpr std::string_view AltitudeModeClamp = "clampToGround";
    constexpr std::string_view AltitudeModeAbsolute = "absolute";
    constexpr std::string_view AltitudeModeRelative = "relativeToGround";
}

namespace {

    template<std::size_t SIZE>
    bool keyMatches(const std::string_view key,
                    const std::array<std::string_view, SIZE>& keyAlternativesArray,
                    std::optional<const openspace::properties::Property::PropertyInfo> propInfo = std::nullopt)
    {
        for (auto k : keyAlternativesArray) {
            if (key == k) {
                return true;
            }
        }

        if (propInfo.has_value() && key == (*propInfo).identifier) {
            return true;
        }

        return false;
    }

    bool keyMatches(const std::string_view key, const std::string_view keyAlternative,
        std::optional<const openspace::properties::Property::PropertyInfo> propInfo = std::nullopt)
    {
        const std::array<std::string_view, 1> array = { keyAlternative };
        return keyMatches(key, array, propInfo);
    }

    glm::vec3 hexToRbg(std::string_view hexColor) {
        int r, g, b;
        int ret = std::sscanf(hexColor.data(), "#%02x%02x%02x", &r, &g, &b);
        // TODO: Handle return value to validate color
        return (1.f / 255.f) * glm::vec3(r, g, b);
    }

    glm::vec3 getColorValue(const geos::io::GeoJSONValue& value) {
        glm::vec3 color;
        if (value.isArray()) {
            const std::vector<geos::io::GeoJSONValue>& val = value.getArray();
            if (val.size() != 3) {
                // @TODO: Should add some more information on which file the reading failed for
                LERRORC("GeoJSON", fmt::format(
                    "Failed reading color property. Expected 3 values, got {}", val.size()
                ));
            }
            // @TODO Use verifiers to verify color values
            // @TODO Parse values given in RGB in ranges 0-255?
            color = glm::vec3(
                static_cast<float>(val.at(0).getNumber()),
                static_cast<float>(val.at(1).getNumber()),
                static_cast<float>(val.at(2).getNumber())
            );
        }
        else if (value.isString()) {
            const std::string hex = value.getString();
            // @TODO Verify color
            color = hexToRbg(hex);
        }
        return color;
    };

    constexpr openspace::properties::Property::PropertyInfo OpacityInfo = {
        "Opacity",
        "Opacity",
        "This value determines the opacity of this object. A value of 0 means "
        "completely transparent"
    };

    constexpr openspace::properties::Property::PropertyInfo ColorInfo = {
        "Color",
        "Color",
        "The color of the rendered geometry. For points it will be used as a multiply"
        "color for any provided texture."
    };

    constexpr openspace::properties::Property::PropertyInfo FillOpacityInfo = {
        "FillOpacity",
        "Fill Opacity",
        "This value determines the opacity of the filled portion of a polygon."
    };

    constexpr openspace::properties::Property::PropertyInfo FillColorInfo = {
        "FillColor",
        "Fill Color",
        "The color of the filled portion of a rendered polygon."
    };

    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line Width",
        "The width of any rendered lines."
    };

    constexpr openspace::properties::Property::PropertyInfo PointSizeInfo = {
        "PointSize",
        "Point Size",
        "The size of any rendered points. The size will be scaled based on the "
        "bounding sphere of the globe"
    };

    constexpr openspace::properties::Property::PropertyInfo PointTextureInfo = {
        "PointTexture",
        "Point Texture",
        "A texture to be used for rendering points. No value means to use the default "
        "texture provided by the globebrowsing module. If no texture is provided there "
        "either, the point will be rendered as a plane and colored by the color value."
    };

    constexpr openspace::properties::Property::PropertyInfo PointRenderModeInfo = {
        "PointRenderMode",
        "Point Render Mode",
        "" // @TODO
    };

    constexpr openspace::properties::Property::PropertyInfo ExtrudeInfo = {
        "Extrude",
        "Extrude",
        "If true, extrude the mesh or line to intersect the globe"
    };

    constexpr openspace::properties::Property::PropertyInfo PerformShadingInfo = {
        "PerformShading",
        "Perform Shading",
        "If true, perform shading on any create meshes, either from polygons or "
        "extruded lines. The shading will be computed based on any light sources of the "
        "GeoJson component"
    };

    constexpr openspace::properties::Property::PropertyInfo AltitudeModeInfo = {
        "AltitudeMode",
        "Altitude Mode",
        "" // @TODO
    };

    // TODO: Update tesselation documentation
    constexpr openspace::properties::Property::PropertyInfo TesselateInfo = {
        "Tesselate",
        "Should Tesselate",
        "If false, no tesselation to bend the geometry based on the curvature of the "
        "planet is performed. This leads to increased performance, but tesselation is "
        "neccessary for large geometry that spans a big portion of the globe. Otherwise "
        "it may intersect the surface."
    };

    constexpr openspace::properties::Property::PropertyInfo TesselationLevelInfo = {
        "TesselationLevel",
        "Tesselation Level",
        "The higher the value, the higher will the resolution of the rendered geometry "
        "be." // @TODO: clarify
    };

    constexpr openspace::properties::Property::PropertyInfo TesselationMaxDistanceInfo = {
        "TesselationMaxDistance",
        "Tesselation Max Distance",
        "Anything larger than this will be automatically subdivided." // @TODO (and consider using a distance based on angle instead)
    };

    struct [[codegen::Dictionary(GeoJsonProperties)]] Parameters {
        // [[codegen::verbatim(OpacityInfo.description)]]
        std::optional<float> opacity [[codegen::inrange(0.0, 1.0)]];

        // [[codegen::verbatim(ColorInfo.description)]]
        std::optional<glm::vec3> color [[codegen::color()]];

        // [[codegen::verbatim(FillOpacityInfo.description)]]
        std::optional<float> fillOpacity [[codegen::inrange(0.0, 1.0)]];

        // [[codegen::verbatim(FillColorInfo.description)]]
        std::optional<glm::vec3> fillColor [[codegen::color()]];

        // [[codegen::verbatim(LineWidthInfo.description)]]
        std::optional<float> lineWidth;

        // [[codegen::verbatim(PointSizeInfo.description)]]
        std::optional<float> pointSize;

        // [[codegen::verbatim(PointTextureInfo.description)]]
        std::optional<std::string> pointTexture;

        enum class [[codegen::map(openspace::globebrowsing::GeoJsonProperties::PointRenderMode)]] PointRenderMode {
            AlignToCameraDir,
            AlignToCameraPos,
            AlignToGlobeNormal,
            AlignToGlobeSurface
        };
        // [[codegen::verbatim(PointRenderModeInfo.description)]]
        std::optional<PointRenderMode> pointRenderMode;

        // [[codegen::verbatim(ExtrudeInfo.description)]]
        std::optional<bool> extrude;

        // [[codegen::verbatim(PerformShadingInfo.description)]]
        std::optional<bool> performShading;

        enum class [[codegen::map(openspace::globebrowsing::GeoJsonProperties::AltitudeMode)]] AltitudeMode {
            Absolute,
            RelativeToGround
        };
        // [[codegen::verbatim(AltitudeModeInfo.description)]]
        std::optional<AltitudeMode> altitudeMode;

        // [[codegen::verbatim(TesselateInfo.description)]]
        std::optional<bool> tesselate;

        // [[codegen::verbatim(TesselationLevelInfo.description)]]
        std::optional<int> tesselationLevel;

        // [[codegen::verbatim(TesselationMaxDistanceInfo.description)]]
        std::optional<float> tesselationMaxDistance;
    };
#include "geojsonproperties_codegen.cpp"
} // namespace

namespace openspace::globebrowsing {

documentation::Documentation GeoJsonProperties::Documentation() {
    return codegen::doc<Parameters>("globebrowsing_geojsonproperties");
}

GeoJsonProperties::GeoJsonProperties()
    : properties::PropertyOwner({ "DefaultProperties" }) // TODO: add a description?
    , opacity(OpacityInfo, 1.f, 0.f, 1.f)
    , color(ColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
    , fillOpacity(FillOpacityInfo, 0.7f, 0.f, 1.f)
    , fillColor(FillColorInfo, glm::vec3(0.5f), glm::vec3(0.f), glm::vec3(1.f))
    , lineWidth(LineWidthInfo, 2.f, 0.01f, 100.f)
    , pointSize(PointSizeInfo, 10.f, 0.01f, 100.f)
    , pointTexture(PointTextureInfo)
    , pointRenderModeOption(
        PointRenderModeInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
    , extrude(ExtrudeInfo, false)
    , performShading(PerformShadingInfo, false)
    , altitudeModeOption(
        AltitudeModeInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
    , shouldTesselate(TesselateInfo, false)
    , tesselationLevel(TesselationLevelInfo, 10, 0, 100)
    , tesselationMaxDistance(TesselationMaxDistanceInfo, 100000.f) // TODO: use distance based on angle?
{
    addProperty(opacity);
    color.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(color);
    addProperty(fillOpacity);
    fillColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(fillColor);

    addProperty(lineWidth);

    addProperty(pointSize);
    addProperty(pointTexture);
    pointRenderModeOption.addOptions({
        { static_cast<int>(PointRenderMode::AlignToCameraDir), "AlignToCameraDir"},
        { static_cast<int>(PointRenderMode::AlignToCameraPos), "AlignToCameraPos"},
        { static_cast<int>(PointRenderMode::AlignToGlobeNormal), "AlignToGlobeNormal"},
        { static_cast<int>(PointRenderMode::AlignToGlobeSurface), "AlignToGlobeSurface"}
    });
    addProperty(pointRenderModeOption);

    addProperty(extrude);
    addProperty(performShading);

    altitudeModeOption.addOptions({
        { static_cast<int>(AltitudeMode::Absolute), "Absolute"},
        { static_cast<int>(AltitudeMode::RelativeToGround), "RelativeToGround" }
        //{ static_cast<int>(AltitudeMode::ClampToGround), "ClampToGround" } / TODO: add ClampToGround
    });
    addProperty(altitudeModeOption);

    addProperty(shouldTesselate);
    addProperty(tesselationLevel);
    addProperty(tesselationMaxDistance);
}

void GeoJsonProperties::createFromDictionary(const ghoul::Dictionary& dictionary) {
    const Parameters p = codegen::bake<Parameters>(dictionary);
    opacity = p.opacity.value_or(opacity);
    color = p.color.value_or(color);
    pointSize = p.pointSize.value_or(pointSize);
    pointTexture = p.pointTexture.value_or(pointTexture);
    lineWidth = p.lineWidth.value_or(lineWidth);
    extrude = p.extrude.value_or(extrude);
    performShading = p.performShading.value_or(performShading);

    if (p.altitudeMode.has_value()) {
        altitudeModeOption = static_cast<int>(codegen::map<AltitudeMode>(*p.altitudeMode));
    }

    shouldTesselate = p.tesselate.value_or(shouldTesselate);
    tesselationLevel = p.tesselationLevel.value_or(tesselationLevel);
    tesselationMaxDistance = p.tesselationMaxDistance.value_or(tesselationMaxDistance);
}

GeoJsonProperties::AltitudeMode GeoJsonProperties::altitudeMode() const {
    return static_cast<GeoJsonProperties::AltitudeMode>(altitudeModeOption.value());
}

GeoJsonProperties::PointRenderMode GeoJsonProperties::pointRenderMode() const {
    return static_cast<GeoJsonProperties::PointRenderMode>(
        pointRenderModeOption.value()
    );
}

GeoJsonOverrideProperties propsFromGeoJson(const geos::io::GeoJSONFeature& feature) {
    const std::map<std::string, geos::io::GeoJSONValue>& props = feature.getProperties();
    GeoJsonOverrideProperties result;

    auto parseProperty = [&result](const std::string_view key,
                                   const geos::io::GeoJSONValue& value)
    {
        if (keyMatches(key, geojson::propertykeys::Name)) {
            result.name = value.getString();
        }
        else if (keyMatches(key, geojson::propertykeys::Opacity, OpacityInfo)) {
            result.opacity = static_cast<float>(value.getNumber());
        }
        else if (keyMatches(key, geojson::propertykeys::Color, ColorInfo)) {
            result.color = getColorValue(value);
        }
        else if (keyMatches(key, geojson::propertykeys::FillOpacity, FillOpacityInfo)) {
            result.fillOpacity = static_cast<float>(value.getNumber());
        }
        else if (keyMatches(key, geojson::propertykeys::FillColor, FillColorInfo)) {
            result.fillColor = getColorValue(value);
        }
        else if (keyMatches(key, geojson::propertykeys::LineWidth, LineWidthInfo)) {
            result.lineWidth = static_cast<float>(value.getNumber());
        }
        else if (keyMatches(key, geojson::propertykeys::PointSize, PointSizeInfo)) {
            result.pointSize = static_cast<float>(value.getNumber());
        }
        else if (keyMatches(key, geojson::propertykeys::Texture, PointTextureInfo)) {
            result.pointTexture = value.getString();
        }
        // @TODO: point render mode
        else if (keyMatches(key, geojson::propertykeys::Extrude, ExtrudeInfo)) {
            result.extrude = value.getBoolean();
        }
        else if (keyMatches(key, geojson::propertykeys::AltitudeMode, AltitudeModeInfo)) {
            std::string mode = value.getString();

            if (mode == geojson::propertykeys::AltitudeModeAbsolute) {
                result.altitudeMode = GeoJsonProperties::AltitudeMode::Absolute;
            }
            else if (mode == geojson::propertykeys::AltitudeModeRelative) {
                result.altitudeMode = GeoJsonProperties::AltitudeMode::RelativeToGround;
            }
            // TODO Include when support is implemented
            //else if (mode == geojson::propertykeys::AltitudeModeClamp) {
            //    result.altitudeMode = GeoJsonProperties::AltitudeMode::ClampToGround;
            //}
            else {
                LERRORC("GeoJSON", fmt::format(
                    "Altitude mode '{}' not supported", mode
                ));
            }
        }
        else if (keyMatches(key, geojson::propertykeys::Tesselate, TesselateInfo)) {
            result.shouldTesselate = value.getBoolean();
        }
        else if (keyMatches(key, geojson::propertykeys::TesselationLevel, TesselationLevelInfo)) {
            result.tesselationLevel = static_cast<float>(value.getNumber());
        }
        else if (keyMatches(key, geojson::propertykeys::TesselationMaxDistance, TesselationMaxDistanceInfo)) {
            result.tesselationMaxDistance = static_cast<float>(value.getNumber());
        }
        // TODO: warn for non-supported keys? General thought is no
    };

    for (auto const& [key, value] : props) {
        try {
            parseProperty(key, value);
        }
        catch (const geos::io::GeoJSONValue::GeoJSONTypeError&) {
            // @TODO: Shouls add some more information on which file the reading failed for
            LERRORC("GeoJSON", fmt::format(
                "Error reading GeoJSON property '{}'. Value has wrong type", key
            ));
        }
    }

    return result;
}


float PropertySet::opacity() const {
    return overrideValues.opacity.value_or(defaultValues.opacity);
}

glm::vec3 PropertySet::color() const {
    return overrideValues.color.value_or(defaultValues.color);
}

float PropertySet::fillOpacity() const {
    return overrideValues.fillOpacity.value_or(defaultValues.fillOpacity);
}

glm::vec3 PropertySet::fillColor() const {
    return overrideValues.fillColor.value_or(defaultValues.fillColor);
}

float PropertySet::lineWidth() const {
    return overrideValues.lineWidth.value_or(defaultValues.lineWidth);
}

float PropertySet::pointSize() const {
    return overrideValues.pointSize.value_or(defaultValues.pointSize);
}

std::string PropertySet::pointTexture() const {
    return overrideValues.pointTexture.value_or(defaultValues.pointTexture);
}

GeoJsonProperties::PointRenderMode PropertySet::pointRenderMode() const {
    return overrideValues.pointRenderMode.value_or(defaultValues.pointRenderMode());
}

bool PropertySet::extrude() const {
    return overrideValues.extrude.value_or(defaultValues.extrude);
}

bool PropertySet::performShading() const {
    return overrideValues.performShading.value_or(defaultValues.performShading);
}

GeoJsonProperties::AltitudeMode PropertySet::altitudeMode() const {
    return overrideValues.altitudeMode.value_or(defaultValues.altitudeMode());
}

bool PropertySet::shouldtesselate() const {
    return overrideValues.shouldTesselate.value_or(defaultValues.shouldTesselate);
}

int PropertySet::tesselationLevel() const {
    return overrideValues.tesselationLevel.value_or(defaultValues.tesselationLevel);
}

float PropertySet::tesselationMaxDistance() const {
    return overrideValues.tesselationMaxDistance.value_or(
        defaultValues.tesselationMaxDistance
    );
}

bool PropertySet::hasOverrideTexture() const {
    return overrideValues.pointTexture.has_value();
}

} // namespace openspace::globebrowsing
