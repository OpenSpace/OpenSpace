/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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
#include <ghoul/glm.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <chrono>
#include <vector>

namespace openspace::documentation { struct Documentation; }
namespace rendering::helper {
    struct LightSourceRenderData;
    struct VertexXYZNormal;
} // namespace rendering::helper

namespace geos::geom { class Geometry; }

namespace openspace::globebrowsing {

class RenderableGlobe;

/**
 * This class is responsible for rendering the geomoetry features of globes, created e.g.
 * from GeoJson files.
 */
class GlobeGeometryFeature {
public:
    GlobeGeometryFeature(const RenderableGlobe& globe,
        GeoJsonProperties& defaultProperties,
        GeoJsonOverrideProperties& overrideProperties);

    using Vertex = rendering::helper::VertexXYZNormal;

    // TODO: Use instead of numbers
    //enum class RenderPass {
    //    GeometryOnly,
    //    GeometryAndShading
    //};

    enum class PointRenderMode {
        AlignToCameraDir = 0,
        AlignToCameraPos,
        AlignToGlobeNormal,
        AlignToGlobeSurface
    };

    enum class GeometryType {
        LineString = 0,
        Point,
        Polygon,
        Error
    };

    enum class RenderType {
        Lines = 0,
        Points,
        Polygon,
        Uninitialized
    };

    /**
     * Each geometry feature might translate into several render features.
     */
    struct RenderFeature {
        void initializeBuffers();

        RenderType type = RenderType::Uninitialized;
        GLuint vaoId = 0;
        GLuint vboId = 0;
        size_t nVertices = 0;
        bool isExtrusionFeature = false;

        /// Store the geodetic lat long coordinates of each vertex, so we can quickly
        /// recompute the height values for these points
        std::vector<Geodetic2> vertices;

        /// Keep the heights around
        std::vector<float> heights;
    };

    /**
     * Some extra data that we need for doing the rendering.
     */
    struct ExtraRenderData {
        float pointSizeScale;
        float lineWidthScale;
        PointRenderMode& pointRenderMode;
        rendering::helper::LightSourceRenderData& lightSourceData;
    };

    std::string key() const;

    void setOffsets(glm::vec3 offsets);

    void initializeGL(ghoul::opengl::ProgramObject* pointsProgram,
        ghoul::opengl::ProgramObject* linesAndPolygonsProgram);
    void deinitializeGL();
    bool isReady() const;
    bool isPoints() const;
    bool useHeightMap() const;

    void updateTexture(bool isInitializeStep = false);

    void createFromSingleGeosGeometry(const geos::geom::Geometry* geo, int index,
        bool ignoreHeights);

    // 2 pass rendering to get correct culling for polygons
    void render(const RenderData& renderData, int pass, float mainOpacity,
        const ExtraRenderData& extraRenderData);

    bool shouldUpdateDueToHeightMapChange() const;

    void update(bool dataIsDirty, bool preventHeightUpdates);
    void updateGeometry();
    void updateHeightsFromHeightMap();

private:
    void renderPoints(const RenderFeature& feature, const RenderData& renderData,
        const PointRenderMode& renderMode, float sizeScale) const;

    void renderLines(const RenderFeature& feature) const;

    void renderPolygons(const RenderFeature& feature, bool shouldRenderTwice,
        int renderPass) const;

    /**
     * Create the vertex information for any line parts of the feature. Returns the
     * resulting vertex positions, so we can use them for extrusion.
     */
    std::vector<std::vector<glm::vec3>> createLineGeometry();

    /**
     * Create the vertex information for any point parts of the feature. Also creates the
     * features for extruded lines for the points.
     */
    void createPointGeometry();

    /**
     * Create the triangle geometry for the extruded edges of lines/polygons.
     */
    void createExtrudedGeometry(const std::vector<std::vector<glm::vec3>>& edgeVertices);

    /**
     * Create the triangle geometry for the polygon part of the feature (the area
     * contained by the shape).
     */
    void createPolygonGeometry();

    void initializeRenderFeature(RenderFeature& feature,
        const std::vector<Vertex>& vertices);

    /**
     * Get the distance that shall be used for tessellation, based on the properties.
     */
    float tessellationStepSize() const;

    /**
     * Compute the heights to the surface at the reference points.
     */
    std::vector<double> getCurrentReferencePointsHeights() const;

    /**
     * Buffer the static data for the vertices.
     */
    void bufferVertexData(const RenderFeature& feature,
        const std::vector<Vertex>& vertexData);

    /**
     * Buffer the dynamic height data for the vertices, based on the height map.
     */
    void bufferDynamicHeightData(const RenderFeature& feature);

    GeometryType _type = GeometryType::Error;
    const RenderableGlobe& _globe;

    /// Coordinates for geometry. For polygons, the first is always the outer ring and any
    /// following are the inner rings (holes)
    std::vector<std::vector<Geodetic3>> _geoCoordinates;

    /// Coordinates for any triangles representing the geometry (only relevant for
    /// polygons)
    std::vector<Geodetic3> _triangleCoordinates;

    std::vector<RenderFeature> _renderFeatures;

    /// lat, long, distance (meters). Passed from parent on property change
    glm::vec3 _offsets = glm::vec3(0.f);

    std::string _key;
    const PropertySet _properties;

    std::vector<Geodetic3> _heightUpdateReferencePoints;
    std::vector<double> _lastControlHeights;
    std::chrono::system_clock::time_point _lastHeightUpdateTime;

    bool _hasTexture = false;
    std::unique_ptr<TextureComponent> _pointTexture;

    ghoul::opengl::ProgramObject* _linesAndPolygonsProgram = nullptr;
    ghoul::opengl::ProgramObject* _pointsProgram = nullptr;
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___GLOBEGEOMETRYFEATURE___H__
