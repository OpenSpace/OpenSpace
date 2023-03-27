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

    constexpr std::string_view Opacity = "opacity";

    constexpr std::string_view Color = "color";
    constexpr std::string_view StrokeColor = "stroke";

    constexpr std::string_view FillColor = "fill"; // TODO: polygon instead of fill?
    constexpr std::string_view FillColorAlt2 = "fill-color";

    constexpr std::string_view FillOpacity = "fill-opacity";

    constexpr std::string_view PointSize = "point-size"; // TODO: remove?
    constexpr std::string_view LineWidth = "stroke-width";
    constexpr std::string_view Extrude = "extrude";

    // TODO: Add tesselate?

    constexpr std::string_view AltitudeMode = "altitudeMode";
    constexpr std::string_view AltitudeModeClamp = "clampToGround";
    constexpr std::string_view AltitudeModeAbsolute = "absolute";
    constexpr std::string_view AltitudeModeRelative = "relativeToGround";
}

namespace {

    glm::vec3 hexToRbg(std::string_view hexColor) {
        int r, g, b;
        int ret = std::sscanf(hexColor.data(), "#%02x%02x%02x", &r, &g, &b);
        // TODO: Hnadle return value to validate color
        LINFOC("GeoJSON", fmt::format("Return value from hex color read: {}", ret)); // TODO: remove
        return (1.f / 255.f) * glm::vec3(r, g, b);
    }

    glm::vec3 getColorValue(const geos::io::GeoJSONValue& value) {
        glm::vec3 color;
        if (value.isArray()) {
            const std::vector<geos::io::GeoJSONValue>& val = value.getArray();
            if (val.size() != 3) {
                // @TODO: Shouls add some more information on which file the reading failed for
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
            // @TODO Verufy color
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
        "The color of the rendered geometry."
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

    constexpr openspace::properties::Property::PropertyInfo PointSizeInfo = {
        "PointSize",
        "Point Size",
        "The size of any rendered points."
    };

    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line Width",
        "The width of any rendered lines."
    };

    constexpr openspace::properties::Property::PropertyInfo ExtrudeInfo = {
        "Extrude",
        "Extrude",
        "If true, extrude the mesh or line to intersect the globe"
    };

    constexpr openspace::properties::Property::PropertyInfo AltitudeModeInfo = {
        "AltitudeMode",
        "Altitude Mode",
        "" // TODO
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

        // [[codegen::verbatim(PointSizeInfo.description)]]
        std::optional<float> pointSize;

        // [[codegen::verbatim(LineWidthInfo.description)]]
        std::optional<float> lineWidth;

        // [[codegen::verbatim(ExtrudeInfo.description)]]
        std::optional<bool> extrude;

        enum class [[codegen::map(openspace::globebrowsing::GeoJsonProperties::AltitudeMode)]] AltitudeMode {
            Absolute,
            RelativeToGround
        };
        // [[codegen::verbatim(AltitudeModeInfo.description)]]
        std::optional<AltitudeMode> altitudeMode;
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
    , color(ColorInfo, glm::vec3(0.5f), glm::vec3(0.f), glm::vec3(1.f))
    , fillOpacity(FillOpacityInfo, 0.7f, 0.f, 1.f)
    , fillColor(FillColorInfo, glm::vec3(0.5f), glm::vec3(0.f), glm::vec3(1.f))
    , pointSize(PointSizeInfo, 1.f, 0.01f, 100.f)
    , lineWidth(LineWidthInfo, 1.f, 0.01f, 100.f)
    , extrude(ExtrudeInfo, false)
    , altitudeModeOption(AltitudeModeInfo, properties::OptionProperty::DisplayType::Dropdown)
{
    addProperty(opacity);
    color.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(color);
    addProperty(fillOpacity);
    fillColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(fillColor);

    addProperty(pointSize);
    addProperty(lineWidth);
    addProperty(extrude);

    altitudeModeOption.addOptions({
        { static_cast<int>(AltitudeMode::Absolute), "Absolute"},
        { static_cast<int>(AltitudeMode::RelativeToGround), "RelativeToGround" }
        //{ static_cast<int>(AltitudeMode::ClampToGround), "ClampToGround" } / TODO: add ClampToGround
    });
    addProperty(altitudeModeOption);
}

void GeoJsonProperties::createFromDictionary(const ghoul::Dictionary& dictionary) {
    const Parameters p = codegen::bake<Parameters>(dictionary);
    opacity = p.opacity.value_or(opacity);
    color = p.color.value_or(color);
    pointSize = p.pointSize.value_or(pointSize);
    lineWidth = p.lineWidth.value_or(lineWidth);
    extrude = p.extrude.value_or(extrude);

    if (p.altitudeMode.has_value()) {
        altitudeModeOption = static_cast<int>(codegen::map<AltitudeMode>(*p.altitudeMode));
    }
}

GeoJsonProperties::AltitudeMode GeoJsonProperties::altitudeMode() const {
    return static_cast<GeoJsonProperties::AltitudeMode>(altitudeModeOption.value());
}

GeoJsonOverrideProperties propsFromGeoJson(const geos::io::GeoJSONFeature& feature) {
    const std::map<std::string, geos::io::GeoJSONValue>& props = feature.getProperties();
    GeoJsonOverrideProperties result;

    auto parseProperty = [&result](std::string_view key,
                                   const geos::io::GeoJSONValue& value)
    {
        if (key == geojson::propertykeys::Name) {
            result.name = value.getString();
        }
        else if (key == geojson::propertykeys::Opacity) {
            result.opacity = static_cast<float>(value.getNumber());
        }
        else if (key == geojson::propertykeys::Color ||
                 key == geojson::propertykeys::StrokeColor)
        {
            result.color = getColorValue(value);
        }
        else if (key == geojson::propertykeys::FillOpacity) {
            result.fillOpacity = static_cast<float>(value.getNumber());
        }
        else if (key == geojson::propertykeys::FillColor ||
                 key == geojson::propertykeys::FillColorAlt2)
        {
            result.fillColor = getColorValue(value);
        }
        else if (key == geojson::propertykeys::PointSize) {
            result.pointSize = static_cast<float>(value.getNumber());
        }
        else if (key == geojson::propertykeys::LineWidth) {
            result.lineWidth = static_cast<float>(value.getNumber());
        }
        else if (key == geojson::propertykeys::Extrude) {
            result.extrude = value.getBoolean();
        }
        else if (key == geojson::propertykeys::AltitudeMode) {
            std::string mode = value.getString();

            if (mode == geojson::propertykeys::AltitudeModeAbsolute) {
                result.altitudeMode = GeoJsonProperties::AltitudeMode::Absolute;
            }
            else if (mode == geojson::propertykeys::AltitudeModeRelative) {
                result.altitudeMode = GeoJsonProperties::AltitudeMode::RelativeToGround;
            }
            // TODO Inlcude when support is implementesd
            //else if (mode == geojson::propertykeys::AltitudeModeClamp) {
            //    result.altitudeMode = GeoJsonProperties::AltitudeMode::ClampToGround;
            //}
            else {
                LERRORC("GeoJSON", fmt::format(
                    "Altitude mode '{}' not supported", mode
                ));
            }
        }
    };

    for (auto const& [key, value] : props) {
        try {
            parseProperty(key, value);
        }
        catch (const geos::io::GeoJSONValue::GeoJSONTypeError& e) {
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

float PropertySet::pointSize() const {
    return overrideValues.pointSize.value_or(defaultValues.pointSize);
}

float PropertySet::lineWidth() const {
    return overrideValues.lineWidth.value_or(defaultValues.lineWidth);
}

bool PropertySet::extrude() const {
    return overrideValues.extrude.value_or(defaultValues.extrude);
}

GeoJsonProperties::AltitudeMode PropertySet::altitudeMode() const {
    return overrideValues.altitudeMode.value_or(defaultValues.altitudeMode());
}

} // namespace openspace::globebrowsing
