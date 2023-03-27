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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___GLOBEGEOMETRYFEATURE___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___GLOBEGEOMETRYFEATURE___H__

#include <openspace/properties/propertyowner.h>

#include <modules/globebrowsing/src/basictypes.h>
#include <modules/globebrowsing/src/geojson/geojsonproperties.h>
#include <openspace/rendering/helper.h>
#include <openspace/rendering/texturecomponent.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/glm.h>
#include <vector>

namespace openspace::documentation { struct Documentation; }
namespace rendering::helper {
    struct VertexXYZNormal;
    struct LightSourceRenderData;
}
namespace geos::geom { class Geometry; }

namespace openspace::globebrowsing {

class RenderableGlobe;

/**
 * TODO: short documentation
 */
class GlobeGeometryFeature : public properties::PropertyOwner {
public:
    GlobeGeometryFeature(RenderableGlobe& globe,
        GeoJsonProperties& defaultProperties,
        GeoJsonOverrideProperties& overrideProperties);

    using Vertex = rendering::helper::VertexXYZNormal;

    // TODO: Use instead of numbers
    //enum class RenderPass {
    //    GeometryOnly,
    //    GeometryAndShading
    //};

    enum class GeometryType {
        LineString = 0,
        Point,
        Polygon,
        Error
    };

    enum class RenderType {
        Lines = 0,
        Points,
        Polygon
    };

    // Each geometry feature might translate into several render features
    struct RenderFeature {
        RenderType type;
        GLuint vaoId = 0;
        GLuint vboId = 0;
        size_t nVertices = 0;
        bool isExtrusionFeature = false;

        // Store the geodetic coordinates of each vertex, so we can quickly recompute then using the height
        std::vector<glm::vec3> vertices;

        // Keep the heights around
        std::vector<double> heights;
    };

    std::string key() const;
    bool enabled() const;

    void setEnabled(bool value);
    void setOffsets(const glm::vec3& offsets);

    void initializeGL(ghoul::opengl::ProgramObject* pointsProgram,
        ghoul::opengl::ProgramObject* linesAndPolygonsProgram);
    void deinitializeGL();
    bool isReady() const;
    bool isPoints() const;
    bool useHeightMap() const;

    void createFromSingleGeosGeometry(const geos::geom::Geometry* geo, int index);

    // 2 pass rendering to get correct culling for polygons
    void render(const RenderData& renderData, int pass, float mainOpacity,
        const rendering::helper::LightSourceRenderData& lightSourceData);

    void bufferVertexData(const RenderFeature& feature,
        const std::vector<Vertex>& vertexData);

    bool shouldUpdateDueToHeightMapChange() const;

    void update(bool dataIsDirty, bool preventHeightUpdates);
    void updateGeometry();

private:
    void renderPoints(const RenderFeature& feature) const;

    void renderLines(const RenderFeature& feature) const;

    void renderPolygons(const RenderFeature& feature, bool shouldRenderTwice,
        int renderPass) const;


    /**
     * Create the vertex information for the line and point parts of the feature.
     * Returns the resulting vertex positions, so we can use them for extrusion
     */
    std::vector<std::vector<glm::vec3>> createPointAndLineGeometry();

    /**
     * Create the triangle geometry for the extruded edges
     */
    void createExtrudedGeometry(const std::vector<std::vector<glm::vec3>>& edgeVertices);

    /**
     * Create the triangle geometry for the polygon part of the feature (the area
     * contained by the shape)
     */
    void createPolygonGeometry();

    /// Compute the heights to the surface at the reference points
    std::vector<double> getCurrentReferencePointsHeights() const;

    GeometryType _type;
    RenderableGlobe& _globe;

    // Coordinates for geometry. For polygons, the first is always the outer ring
    // and any following are the inner rings (holes)
    std::vector<std::vector<Geodetic3>> _geoCoordinates;

    // Coordinates for any triangles representing the geometry (only relevant for polygons)
    std::vector<Geodetic3> _triangleCoordinates;

    std::vector<RenderFeature> _renderFeatures;

    glm::vec3 _offsets = glm::vec3(0.f); // lat, long, distance (meters). Passed from parent on property change

    bool _isEnabled = true;
    std::string _key;
    PropertySet _properties;

    std::vector<Geodetic3> _heightUpdateReferencePoints;
    std::vector<double> _lastControlHeights;

    std::unique_ptr<TextureComponent> _pointTexture;

    ghoul::opengl::ProgramObject* _linesAndPolygonsProgram = nullptr;
    ghoul::opengl::ProgramObject* _pointsProgram = nullptr;
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___GLOBEGEOMETRYFEATURE___H__
