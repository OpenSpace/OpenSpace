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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___GEOJSONPROPERTIES___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___GEOJSONPROPERTIES___H__

#include <openspace/properties/propertyowner.h>

#include <openspace/properties/optionproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/vector/vec3property.h>
#include <ghoul/glm.h>
#include <optional>
#include <string_view>

namespace geos::io { class GeoJSONFeature; }

namespace openspace::globebrowsing {

struct GeoJsonProperties : public properties::PropertyOwner {
    GeoJsonProperties();
    void createFromDictionary(const ghoul::Dictionary& dictionary);

    static documentation::Documentation Documentation();

    // These are based on the KML specification
    enum class AltitudeMode {
        // Compute position as an altitude above the reference ellipsoid
        Absolute = 0,
        // Compute position using altitude above the height map
        RelativeToGround
        // Stick to planet surface (TODO: use GDAL to render layer instead and use as default)
        //ClampToGround
    };

    AltitudeMode altitudeMode() const;

    properties::FloatProperty opacity;
    properties::Vec3Property color;
    properties::FloatProperty fillOpacity;
    properties::Vec3Property fillColor;
    properties::FloatProperty lineWidth;

    properties::FloatProperty pointSize;
    properties::StringProperty pointTexture;

    properties::BoolProperty extrude;
    properties::BoolProperty performShading;
    properties::OptionProperty altitudeModeOption;

    // TODO: make separate propertowner, for better distinguishemnt
    properties::BoolProperty shouldTesselate;
    properties::IntProperty tesselationLevel;
    properties::FloatProperty tesselationMaxDistance;
};

// Optional versions of all the properties above, that can be read from a geoJson file
// and used to override any default values
struct GeoJsonOverrideProperties {
    std::optional<std::string> name;

    std::optional<float> opacity;
    std::optional<glm::vec3> color;
    std::optional<float> fillOpacity;
    std::optional<glm::vec3> fillColor;
    std::optional<float> lineWidth;

    std::optional<float> pointSize;
    std::optional<std::string> pointTexture;

    std::optional<bool> extrude;
    std::optional<bool> performShading;
    std::optional<GeoJsonProperties::AltitudeMode> altitudeMode;

    std::optional<bool> shouldTesselate;
    std::optional<int> tesselationLevel;
    std::optional<float> tesselationMaxDistance;
};

GeoJsonOverrideProperties propsFromGeoJson(const geos::io::GeoJSONFeature& feature);

struct PropertySet {
    // This value set should be a reference to the main component's propertyowner
    GeoJsonProperties& defaultValues;
    // This is a unique set of properties to use for overriding the default values
    GeoJsonOverrideProperties overrideValues;

    float opacity() const;
    glm::vec3 color() const;
    float fillOpacity() const;
    glm::vec3 fillColor() const;
    float lineWidth() const;

    float pointSize() const;
    std::string pointTexture() const;

    bool extrude() const;
    bool performShading() const;
    GeoJsonProperties::AltitudeMode altitudeMode() const;

    bool shouldtesselate() const;
    int tesselationLevel() const;
    float tesselationMaxDistance() const;

    bool hasOverrideTexture() const;
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___GEOJSONPROPERTIES___H__
