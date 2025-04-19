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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___GLOBEGEOMETRYHELPER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___GLOBEGEOMETRYHELPER___H__

#include <ghoul/glm.h>
#include <vector>

namespace openspace {
    struct Geodetic2;
    struct Geodetic3;

    namespace globebrowsing { class RenderableGlobe; }
    namespace rendering::helper { struct VertexXYZNormal; }
} // namespace openspace

namespace geos::geom {
    class Coordinate;
    class Geometry;
} // namespace geos::geom

namespace openspace::globebrowsing::geometryhelper {

Geodetic3 toGeodetic(const geos::geom::Coordinate& c);

geos::geom::Coordinate toGeosCoord(const Geodetic3& gd);

std::vector<Geodetic3> coordsToGeodetic(
    const std::vector<geos::geom::Coordinate>& coords);

std::vector<Geodetic3> geometryCoordsAsGeoVector(const geos::geom::Geometry* geometry);

std::vector<Geodetic2> geodetic2FromVertexList(const RenderableGlobe& globe,
    const std::vector<rendering::helper::VertexXYZNormal>& verts);

std::vector<float> heightMapHeightsFromGeodetic2List(const RenderableGlobe& globe,
    const std::vector<Geodetic2>& list);

/**
 * Create triangle geometry for the extruded edge, given the provided edge vertices.
 */
std::vector<rendering::helper::VertexXYZNormal> createExtrudedGeometryVertices(
    const std::vector<std::vector<glm::vec3>>& edgeVertices);

/**
 * Get height contribution from reference surface of the globe, based on the height map.
 */
double getHeightToReferenceSurface(const Geodetic2& geo, const RenderableGlobe& globe);

/**
 * Compute model space cordinate from geodetic coordinate, and account for lat, long
 * offsets.
 */
glm::dvec3 computeOffsetedModelCoordinate(const Geodetic3& geo,
    const RenderableGlobe& globe, float latOffset, float lonOffset);


struct PosHeightPair {
    glm::vec3 position;
    double height;
};

/**
 * Subdivide line between position v0 and v1 so that it fullfils the maxDistance criteria.
 * Interpolate the height value from * h0 to h1, as well as add the given offset and
 * account for the height map if that should be done.
 *
 * \return Pairs of position and height values
 */
std::vector<PosHeightPair> subdivideLine(const glm::dvec3& v0, const glm::dvec3& v1,
    double h0, double h1, double maxDistance);

/**
 * Subdivide triangle consisting of vertex positions v0, v1 and v2, with height values
 * h0, h1 and h2 into smaller triangles. maxDistance specifies tha maximum distance
 * between two vertices in the subdivided mesh.
 */
std::vector<rendering::helper::VertexXYZNormal> subdivideTriangle(
    const glm::vec3& v0, const glm::vec3& v1, const glm::vec3& v2,
    double h0, double h1, double h2, double maxDistance, const RenderableGlobe& globe);

} // namespace openspace::globebrowsing::geometryhelper

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___GLOBEGEOMETRYHELPER___H__
