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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___GEOJSONCOMPONENT___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___GEOJSONCOMPONENT___H__

#include <openspace/properties/propertyowner.h>

#include <modules/globebrowsing/src/basictypes.h>
#include <modules/globebrowsing/src/geojson/geojsonproperties.h>
#include <modules/globebrowsing/src/geojson/globegeometryfeature.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/selectionproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/triggerproperty.h>
#include <openspace/properties/vector/vec2property.h>
#include <openspace/rendering/helper.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/glm.h>
#include <vector>

namespace openspace {
    struct RenderData;
    class LightSource;
} // namespace::openspace

namespace openspace::documentation { struct Documentation; }
namespace openspace::rendering::helper { struct VertexXYZNormal; }
namespace ghoul::opengl { class ProgramObject; }
namespace geos::io { class GeoJSONFeature; }

namespace openspace::globebrowsing {

class RenderableGlobe;

/**
 * TODO: short documentation
 */
class GeoJsonComponent : public properties::PropertyOwner {
public:
    GeoJsonComponent(const ghoul::Dictionary& dictionary, RenderableGlobe& globe);

    void initialize();
    void initializeGL();
    void deinitializeGL();

    bool isReady() const;
    bool enabled() const;

    void render(const RenderData& data);
    void update();

    static documentation::Documentation Documentation();

private:
    void fillSelectionProperty();
    void selectionPropertyHasChanged();

    void readFile();
    void parseSingleFeature(const geos::io::GeoJSONFeature& feature);

    std::unique_ptr<ghoul::opengl::ProgramObject> _linesAndPolygonsProgram = nullptr;
    std::unique_ptr<ghoul::opengl::ProgramObject> _pointsProgram = nullptr;

    std::vector<GlobeGeometryFeature> _geometryFeatures;

    properties::BoolProperty _enabled;
    properties::FloatProperty _opacity;
    properties::StringProperty _geoJsonFile;
    properties::FloatProperty _heightOffset;
    properties::Vec2Property _latLongOffset;

    GeoJsonProperties _defaultProperties;

    properties::SelectionProperty _featureSelection;

    properties::BoolProperty _drawWireframe;
    properties::BoolProperty _preventUpdatesFromHeightMap;
    // Temporary property to get around height map update issues. TODO: remove
    properties::TriggerProperty _forceUpdateData;

    RenderableGlobe& _globeNode;

    bool _dataIsDirty = true;
    bool _dataIsInitialized = false;

    size_t _nVerticesToDraw = 0;

    std::vector<std::unique_ptr<LightSource>> _lightSources;

    rendering::helper::LightSourceRenderData _lightsourceRenderData;

    properties::PropertyOwner _lightSourcePropertyOwner;
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___GEOJSONCOMPONENT___H__
