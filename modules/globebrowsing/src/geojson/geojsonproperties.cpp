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

#include <modules/globebrowsing/src/geojson/geojsonproperties.h>

#include <modules/globebrowsing/src/renderableglobe.h>
#include <openspace/documentation/documentation.h>
#include <ghoul/logging/logmanager.h>
#include <geos/io/GeoJSON.h>
#include <scn/scan.h>
#include <algorithm>
#include <cstdio>

// Keys used to read properties from GeoJson files
namespace geojson::propertykeys {
    constexpr std::string_view Name = "name";

    constexpr std::string_view Opacity = "opacity";

    constexpr std::array<std::string_view, 2> Color = { "color", "stroke" };
    constexpr std::array<std::string_view, 2> FillColor = { "fill", "fill-color" };
    constexpr std::string_view FillOpacity = "fill-opacity";
    constexpr std::string_view LineWidth = "stroke-width";

    constexpr std::string_view PointSize = "point-size";
    constexpr std::array<std::string_view, 3> Texture = {
        "texture", "sprite", "point-texture"
    };

    constexpr std::array<std::string_view, 2> PointTextureAnchor = {
        "point-anchor", "anchor"
    };
    constexpr std::string_view PointTextureAnchorBottom = "bottom";
    constexpr std::string_view PointTextureAnchorCenter = "center";

    constexpr std::string_view Extrude = "extrude";

    constexpr std::string_view Tessellate = "tessellate";
    constexpr std::string_view TessellationLevel = "tessellationLevel";
    constexpr std::string_view TessellationMaxDistance = "tessellationDistance";

    constexpr std::string_view AltitudeMode = "altitudeMode";
    constexpr std::string_view AltitudeModeClamp = "clampToGround";
    constexpr std::string_view AltitudeModeAbsolute = "absolute";
    constexpr std::string_view AltitudeModeRelative = "relativeToGround";

    constexpr std::string_view PerformShading = "performShading";
} // namespace geojson::propertykeys

namespace {
    using PropertyInfo = openspace::properties::Property::PropertyInfo;

    template <size_t SIZE>
    bool keyMatches(const std::string_view key,
                    const std::array<std::string_view, SIZE>& keyAlternativesArray,
                    std::optional<const PropertyInfo> propInfo = std::nullopt)
    {
        auto it = std::find(
            keyAlternativesArray.begin(),
            keyAlternativesArray.end(),
            key
        );
        if (it != keyAlternativesArray.end()) {
            return true;
        }

        if (propInfo.has_value() && key == (*propInfo).identifier) {
            return true;
        }

        return false;
    }

    bool keyMatches(const std::string_view key, const std::string_view keyAlternative,
                    std::optional<const PropertyInfo> propInfo = std::nullopt)
    {
        const std::array<std::string_view, 1> array = { keyAlternative };
        return keyMatches(key, array, propInfo);
    }

    std::optional<glm::vec3> hexToRgb(std::string_view hexColor) {
        auto ret = scn::scan<int, int, int>(hexColor, "#{:2x}{:2x}{:2x}");
        if (ret) {
            auto [x, y, z] = ret->values();
            return (1.f / 255.f) * glm::vec3(x, y, z);
        }
        else {
            return std::nullopt;
        }
    }

    glm::vec3 getColorValue(const geos::io::GeoJSONValue& value) {
        // Default garish color used for when the color loading fails
        glm::vec3 color = glm::vec3(1.f, 0.f, 1.f);
        if (value.isArray()) {
            const std::vector<geos::io::GeoJSONValue>& val = value.getArray();
            if (val.size() != 3) {
                // @TODO:
                // Should add some more information on which file the reading failed for
                LERRORC(
                    "GeoJson",
                    std::format(
                        "Failed reading color property. Expected 3 values, got {}",
                        val.size()
                    )
                );
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
            const std::string& hex = value.getString();
            std::optional<glm::vec3> c = hexToRgb(hex);
            if (!c) {
                LERRORC(
                    "GeoJson",
                    std::format(
                        "Failed reading color property. Did not find a hex color, got {}",
                        hex
                    )
                );
            }
            else {
                color = *c;
            }
        }
        return color;
    }

    constexpr openspace::properties::Property::PropertyInfo OpacityInfo = {
        "Opacity",
        "Opacity",
        "This value determines the opacity of this object. A value of 0 means "
        "completely transparent.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo ColorInfo = {
        "Color",
        "Color",
        "The color of the rendered geometry. For points it will be used as a multiply"
        "color for any provided texture.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo FillOpacityInfo = {
        "FillOpacity",
        "Fill Opacity",
        "This value determines the opacity of the filled portion of a polygon. Will "
        "also be used for extruded features.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo FillColorInfo = {
        "FillColor",
        "Fill Color",
        "The color of the filled portion of a rendered polygon. Will also be used for "
        "extruded features.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line Width",
        "The width of any rendered lines.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo PointSizeInfo = {
        "PointSize",
        "Point Size",
        "The size of any rendered points. The size will be scaled based on the "
        "bounding sphere of the globe.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo PointTextureInfo = {
        "PointTexture",
        "Point Texture",
        "A texture to be used for rendering points. No value means to use the default "
        "texture provided by the GlobeBrowsing module. If no texture is provided there "
        "either, the point will be rendered as a plane and colored by the color value.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo ExtrudeInfo = {
        "Extrude",
        "Extrude",
        "If true, extrude the geometry to intersect the globe.Lines/polygons will be"
        "extruded with polygons,and points with lines.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo PerformShadingInfo = {
        "PerformShading",
        "Perform Shading",
        "If true, perform shading on any create meshes, either from polygons or "
        "extruded lines. The shading will be computed based on any light sources of the "
        "GeoJson component.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo AltitudeModeInfo = {
        "AltitudeMode",
        "Altitude Mode",
        "The altitude mode decides how any height values of the geo coordinates should "
        "be interpreted. Absolute means that the height is interpreted as the height "
        "above the reference ellipsoid, while RelativeToGround takes the height map "
        "into account. For coordinates with only two values (latitude and longitude), "
        "the height is considered to be equal to zero.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo PointAnchorOptionInfo = {
        "PointTextureAnchor",
        "Point Texture Anchor",
        "Decides the placement of the point texture in relation to the position. "
        "Default is a the bottom of the texture, but it can also be put at the center.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo TessellationEnabledInfo = {
        "Enabled",
        "Enabled",
        "If false, no tessellation to bend the geometry based on the curvature of the "
        "planet is performed. This leads to increased performance, but tessellation is "
        "neccessary for large geometry that spans a big portion of the globe. Otherwise "
        "it may intersect the surface.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo UseTessellationLevelInfo = {
        "UseTessellationLevel",
        "Use Tessellation Level",
        "If true, use the 'Tessellation Level' to control the level of detail for the "
        "tessellation. The distance used will be the 'Tessellation Distance' divided by "
        "the 'Tessellation Level', so the higher the level value, the smaller each "
        "segment in the geomoetry will be.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo TessellationLevelInfo = {
        "TessellationLevel",
        "Tessellation Level",
        "When manual tessellation is enabled, this value will be used to determine how "
        "much tessellation to apply. The resulting distance used for subdividing the "
        "geometry will be the 'Tessellation Distance' divided by this value. Zero means "
        "to use the 'Tessellation Distance' as is.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo TessellationDistanceInfo = {
        "TessellationDistance",
        "Tessellation Distance",
        "Defult distance to use for tessellation of line and polygon geometry. Anything "
        "larger than this distance will be automatically subdivided into smaller pieces "
        "matching this distance, while anything smaller will not be subdivided. Per "
        "default this will be set to a distance corresponding to about 1 degree "
        "longitude on the globe.",
        openspace::properties::Property::Visibility::AdvancedUser
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
        std::optional<float> lineWidth [[codegen::greater(0.0)]];

        // [[codegen::verbatim(PointSizeInfo.description)]]
        std::optional<float> pointSize [[codegen::greater(0.0)]];

        // [[codegen::verbatim(PointTextureInfo.description)]]
        std::optional<std::string> pointTexture;

        // [[codegen::verbatim(ExtrudeInfo.description)]]
        std::optional<bool> extrude;

        // [[codegen::verbatim(PerformShadingInfo.description)]]
        std::optional<bool> performShading;

        enum class
        [[codegen::map(openspace::globebrowsing::GeoJsonProperties::AltitudeMode)]]
        AltitudeMode
        {
            Absolute,
            RelativeToGround
        };
        // [[codegen::verbatim(AltitudeModeInfo.description)]]
        std::optional<AltitudeMode> altitudeMode;

        enum class
        [[codegen::map(openspace::globebrowsing::GeoJsonProperties::PointTextureAnchor)]]
        PointTextureAnchor
        {
            Bottom,
            Center
        };
        // [[codegen::verbatim(PointAnchorOptionInfo.description)]]
        std::optional<PointTextureAnchor> pointTextureAnchor;

        struct Tessellation {
            // [[codegen::verbatim(TessellationEnabledInfo.description)]]
            std::optional<bool> enabled;

            // [[codegen::verbatim(UseTessellationLevelInfo.description)]]
            std::optional<bool> useTessellationLevel;

            // [[codegen::verbatim(TessellationLevelInfo.description)]]
            std::optional<int> tessellationLevel;

            // [[codegen::verbatim(TessellationDistanceInfo.description)]]
            std::optional<float> tessellationDistance;
        };
        std::optional<Tessellation> tessellation;
    };
#include "geojsonproperties_codegen.cpp"
} // namespace

namespace openspace::globebrowsing {

documentation::Documentation GeoJsonProperties::Documentation() {
    return codegen::doc<Parameters>("globebrowsing_geojsonproperties");
}

GeoJsonProperties::Tessellation::Tessellation()
    : properties::PropertyOwner({ "Tessellation" })
    , enabled(TessellationEnabledInfo, true)
    , useLevel(UseTessellationLevelInfo, false)
    , level(TessellationLevelInfo, 10, 0, 100)
    , distance(TessellationDistanceInfo, 100000.f, 0.f, 1e12f)
{
    addProperty(enabled);
    addProperty(distance);
    addProperty(level);
    addProperty(useLevel);
}

GeoJsonProperties::GeoJsonProperties()
    : properties::PropertyOwner({ "DefaultProperties" })
    , opacity(OpacityInfo, 1.f, 0.f, 1.f)
    , color(ColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
    , fillOpacity(FillOpacityInfo, 0.7f, 0.f, 1.f)
    , fillColor(FillColorInfo, glm::vec3(0.5f), glm::vec3(0.f), glm::vec3(1.f))
    , lineWidth(LineWidthInfo, 2.f, 0.01f, 10.f)
    , pointSize(PointSizeInfo, 10.f, 0.01f, 100.f)
    , pointTexture(PointTextureInfo)
    , pointAnchorOption(
        PointAnchorOptionInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
    , extrude(ExtrudeInfo, false)
    , performShading(PerformShadingInfo, false)
    , altitudeModeOption(
        AltitudeModeInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
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

    addProperty(extrude);
    addProperty(performShading);

    altitudeModeOption.addOptions({
        { static_cast<int>(AltitudeMode::Absolute), "Absolute" },
        { static_cast<int>(AltitudeMode::RelativeToGround), "Relative to Ground" }
        // TODO: add ClampToGround
        //{ static_cast<int>(AltitudeMode::ClampToGround), "Clamp to Ground" }
    });
    addProperty(altitudeModeOption);

    pointAnchorOption.addOptions({
        { static_cast<int>(PointTextureAnchor::Bottom), "Bottom" },
        { static_cast<int>(PointTextureAnchor::Center), "Center" }
    });
    addProperty(pointAnchorOption);

    addPropertySubOwner(tessellation);
}

void GeoJsonProperties::createFromDictionary(const ghoul::Dictionary& dictionary,
                                             const RenderableGlobe& globe)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);
    opacity = p.opacity.value_or(opacity);
    color = p.color.value_or(color);

    fillOpacity = p.fillOpacity.value_or(fillOpacity);
    fillColor = p.fillColor.value_or(fillColor);

    lineWidth = p.lineWidth.value_or(lineWidth);

    pointSize = p.pointSize.value_or(pointSize);

    if (p.pointTexture.has_value()) {
        pointTexture = *p.pointTexture;
    }

    if (p.pointTextureAnchor.has_value()) {
        pointAnchorOption = static_cast<int>(codegen::map<PointTextureAnchor>(
            *p.pointTextureAnchor
        ));
    }

    extrude = p.extrude.value_or(extrude);
    performShading = p.performShading.value_or(performShading);

    if (p.altitudeMode.has_value()) {
        altitudeModeOption = static_cast<int>(
            codegen::map<AltitudeMode>(*p.altitudeMode)
        );
    }

    // Set up default value and max value for tessellation distance based on globe size.
    // Distances are computed based on a certain lat/long angle size
    constexpr float DefaultAngle = glm::radians(1.f);
    constexpr float MaxAngle = glm::radians(45.f);
    const float defaultDistance = static_cast<float>(
        globe.ellipsoid().longitudalDistance(0.f, 0.f, DefaultAngle)
    );
    const float maxDistance = static_cast<float>(
        globe.ellipsoid().longitudalDistance(0.f, 0.f, MaxAngle)
    );
    tessellation.distance = defaultDistance;
    tessellation.distance.setMaxValue(maxDistance);

    if (p.tessellation.has_value()) {
        const Parameters::Tessellation pTess = (*p.tessellation);
        tessellation.enabled = pTess.useTessellationLevel.value_or(tessellation.enabled);
        tessellation.useLevel =
            pTess.useTessellationLevel.value_or(tessellation.useLevel);
        tessellation.level = pTess.tessellationLevel.value_or(tessellation.level);
        tessellation.distance =
            pTess.tessellationDistance.value_or(tessellation.distance);
    }
}

GeoJsonProperties::AltitudeMode GeoJsonProperties::altitudeMode() const {
    return static_cast<GeoJsonProperties::AltitudeMode>(altitudeModeOption.value());
}

GeoJsonProperties::PointTextureAnchor GeoJsonProperties::pointTextureAnchor() const {
    return static_cast<GeoJsonProperties::PointTextureAnchor>(pointAnchorOption.value());
}

GeoJsonOverrideProperties propsFromGeoJson(const geos::io::GeoJSONFeature& feature) {
    const std::map<std::string, geos::io::GeoJSONValue>& props = feature.getProperties();
    GeoJsonOverrideProperties result;

    auto parseProperty = [&result](const std::string_view key,
                                   const geos::io::GeoJSONValue& value)
    {
        using namespace geojson;

        if (keyMatches(key, propertykeys::Name)) {
            result.name = value.getString();
        }
        else if (keyMatches(key, propertykeys::Opacity, OpacityInfo)) {
            result.opacity = static_cast<float>(value.getNumber());
        }
        else if (keyMatches(key, propertykeys::Color, ColorInfo)) {
            result.color = getColorValue(value);
        }
        else if (keyMatches(key, propertykeys::FillOpacity, FillOpacityInfo)) {
            result.fillOpacity = static_cast<float>(value.getNumber());
        }
        else if (keyMatches(key, propertykeys::FillColor, FillColorInfo)) {
            result.fillColor = getColorValue(value);
        }
        else if (keyMatches(key, propertykeys::LineWidth, LineWidthInfo)) {
            result.lineWidth = static_cast<float>(value.getNumber());
        }
        else if (keyMatches(key, propertykeys::PointSize, PointSizeInfo)) {
            result.pointSize = static_cast<float>(value.getNumber());
        }
        else if (keyMatches(key, propertykeys::Texture, PointTextureInfo)) {
            result.pointTexture = value.getString();
        }
        else if (keyMatches(key, propertykeys::PointTextureAnchor, PointAnchorOptionInfo))
        {
            std::string mode = value.getString();

            if (mode == propertykeys::PointTextureAnchorBottom) {
                result.pointTextureAnchor = GeoJsonProperties::PointTextureAnchor::Bottom;
            }
            else if (mode == propertykeys::PointTextureAnchorCenter) {
                result.pointTextureAnchor = GeoJsonProperties::PointTextureAnchor::Center;
            }
            else {
                LERRORC("GeoJson", std::format(
                    "Point texture anchor mode '{}' not supported", mode
                ));
            }
        }
        else if (keyMatches(key, propertykeys::Extrude, ExtrudeInfo)) {
            result.extrude = value.getBoolean();
        }
        else if (keyMatches(key, propertykeys::AltitudeMode, AltitudeModeInfo)) {
            std::string mode = value.getString();

            if (mode == propertykeys::AltitudeModeAbsolute) {
                result.altitudeMode = GeoJsonProperties::AltitudeMode::Absolute;
            }
            else if (mode == propertykeys::AltitudeModeRelative) {
                result.altitudeMode = GeoJsonProperties::AltitudeMode::RelativeToGround;
            }
            // @TODO Include when support is implemented
            //else if (mode == propertykeys::AltitudeModeClamp) {
            //    result.altitudeMode = GeoJsonProperties::AltitudeMode::ClampToGround;
            //}
            else {
                LERRORC("GeoJson", std::format(
                    "Altitude mode '{}' not supported", mode
                ));
            }
        }
        else if (keyMatches(key, propertykeys::PerformShading, PerformShadingInfo)) {
            result.performShading = value.getBoolean();
        }
        else if (keyMatches(key, propertykeys::Tessellate, TessellationEnabledInfo)) {
            result.tessellationEnabled = value.getBoolean();
        }
        else if (keyMatches(key, propertykeys::TessellationLevel, TessellationLevelInfo))
        {
            result.useTessellationLevel = true;
            result.tessellationLevel = static_cast<int>(value.getNumber());
        }
        else if (
            keyMatches(
                key,
                propertykeys::TessellationMaxDistance,
                TessellationDistanceInfo
            ))
        {
            result.tessellationDistance = static_cast<float>(value.getNumber());
        }
    };

    for (auto const& [key, value] : props) {
        try {
            parseProperty(key, value);
        }
        catch (const geos::io::GeoJSONValue::GeoJSONTypeError&) {
            // @TODO: Should add some more information on which file the reading failed
            LERRORC("GeoJson", std::format(
                "Error reading GeoJson property '{}'. Value has wrong type", key
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

GeoJsonProperties::PointTextureAnchor PropertySet::pointTextureAnchor() const {
    return overrideValues.pointTextureAnchor.value_or(
        defaultValues.pointTextureAnchor()
    );
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

bool PropertySet::tessellationEnabled() const {
    return overrideValues.tessellationEnabled.value_or(
        defaultValues.tessellation.enabled
    );
}

bool PropertySet::useTessellationLevel() const {
    return overrideValues.useTessellationLevel.value_or(
        defaultValues.tessellation.useLevel
    );
}

int PropertySet::tessellationLevel() const {
    return overrideValues.tessellationLevel.value_or(defaultValues.tessellation.level);
}

float PropertySet::tessellationDistance() const {
    return overrideValues.tessellationDistance.value_or(
        defaultValues.tessellation.distance
    );
}

bool PropertySet::hasOverrideTexture() const {
    return overrideValues.pointTexture.has_value();
}

} // namespace openspace::globebrowsing
