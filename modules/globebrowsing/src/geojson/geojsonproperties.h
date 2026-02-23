/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <openspace/properties/misc/optionproperty.h>
#include <openspace/properties/misc/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/vector/vec3property.h>
#include <ghoul/glm.h>
#include <optional>

namespace geos::io { class GeoJSONFeature; }

namespace openspace {

class RenderableGlobe;

struct GeoJsonProperties : public PropertyOwner {
    GeoJsonProperties();

    void createFromDictionary(const ghoul::Dictionary& dictionary,
        const RenderableGlobe& globe);

    static openspace::Documentation Documentation();

    /**
     * These are based on the KML specification.
     */
    enum class AltitudeMode {
        /// Compute position as an altitude above the reference ellipsoid
        Absolute = 0,
        /// Compute position using altitude above the height map
        RelativeToGround
        /// Stick to planet surface (TODO: use GDAL to render layer instead and use as
        /// default)
        // ClampToGround
    };

    enum class PointTextureAnchor {
        Bottom = 0,
        Center
    };

    struct Tessellation : public PropertyOwner {
        Tessellation();
        BoolProperty enabled;
        BoolProperty useLevel;
        IntProperty level;
        FloatProperty distance;
    } tessellation;

    AltitudeMode altitudeMode() const;
    PointTextureAnchor pointTextureAnchor() const;

    FloatProperty opacity;
    Vec3Property color;
    FloatProperty fillOpacity;
    Vec3Property fillColor;
    FloatProperty lineWidth;

    FloatProperty pointSize;
    StringProperty pointTexture;
    OptionProperty pointAnchorOption;

    BoolProperty extrude;
    BoolProperty performShading;
    OptionProperty altitudeModeOption;
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
    std::optional<std::filesystem::path> pointTexture;
    std::optional<GeoJsonProperties::PointTextureAnchor> pointTextureAnchor;

    std::optional<bool> extrude;
    std::optional<bool> performShading;
    std::optional<GeoJsonProperties::AltitudeMode> altitudeMode;

    std::optional<bool> tessellationEnabled;
    std::optional<bool> useTessellationLevel;
    std::optional<int> tessellationLevel;
    std::optional<float> tessellationDistance;
};

GeoJsonOverrideProperties propsFromGeoJson(const geos::io::GeoJSONFeature& feature);

struct PropertySet {
    /// This value set should be a reference to the main component's propertyowner
    GeoJsonProperties& defaultValues;
    /// This is a unique set of properties to use for overriding the default values
    GeoJsonOverrideProperties overrideValues;

    float opacity() const;
    glm::vec3 color() const;
    float fillOpacity() const;
    glm::vec3 fillColor() const;
    float lineWidth() const;

    float pointSize() const;
    std::filesystem::path pointTexture() const;
    GeoJsonProperties::PointTextureAnchor pointTextureAnchor() const;

    bool extrude() const;
    bool performShading() const;
    GeoJsonProperties::AltitudeMode altitudeMode() const;

    bool tessellationEnabled() const;
    bool useTessellationLevel() const;
    int tessellationLevel() const;
    float tessellationDistance() const;

    bool hasOverrideTexture() const;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___GEOJSONPROPERTIES___H__
