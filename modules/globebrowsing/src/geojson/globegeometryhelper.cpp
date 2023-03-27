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

#include <modules/globebrowsing/src/geojson/globegeometryhelper.h>

#include <modules/globebrowsing/src/renderableglobe.h>
#include <openspace/rendering/helper.h>
#include <openspace/util/updatestructures.h>
#include <geos/geom/Coordinate.h>
#include <geos/geom/GeometryFactory.h>
#include <geos/triangulate/DelaunayTriangulationBuilder.h>
#include <geos/triangulate/quadedge/QuadEdgeSubdivision.h>

namespace openspace::globebrowsing::geometryhelper {

Geodetic3 toGeodetic(const geos::geom::Coordinate& c) {
    Geodetic3 gd;
    gd.geodetic2.lon = glm::radians(c.x);
    gd.geodetic2.lat = glm::radians(c.y);
    gd.height = std::isnan(c.z) ? 0.0 : c.z;
    return gd;
}

geos::geom::Coordinate toGeosCoord(const Geodetic3& gd) {
    geos::geom::Coordinate c;
    c.x = glm::degrees(gd.geodetic2.lon);
    c.y = glm::degrees(gd.geodetic2.lat);
    c.z = gd.height;
    return c;
}

std::vector<Geodetic3>
coordsToGeodetic(const std::vector<geos::geom::Coordinate>& coords)
{
    std::vector<Geodetic3> res;
    res.reserve(coords.size());
    for (const geos::geom::Coordinate& c : coords) {
        res.push_back(toGeodetic(c));
    }
    return res;
}

std::vector<rendering::helper::VertexXYZNormal>
createExtrudedGeometryVertices(const std::vector<std::vector<glm::vec3>>& edgeVertices)
{
    std::vector<rendering::helper::VertexXYZNormal> vertices;

    size_t nVerts = 0;
    for (const auto& edge : edgeVertices) {
        nVerts += edge.size();
    }
    vertices.reserve(nVerts * 3);

    // Extrude polygon
    for (int nBound = 0; nBound < edgeVertices.size(); ++nBound) {
        const std::vector<glm::vec3>& boundary = edgeVertices[nBound];
        for (int i = 1; i < boundary.size(); ++i) {
            glm::vec3 v0 = boundary[i - 1];
            glm::vec3 v1 = boundary[i ];

            // Vertices close to globe (Based on origin which is the zero point here)
            // TEST: use center of globe (TODO: allow setting the height)
            glm::vec3 vOrigin = glm::vec3(0.f);

            // Outer boundary is the first one
            if (nBound == 0) {
                glm::vec3 n = glm::vec3(glm::normalize(glm::cross(v1 - vOrigin, v0 - vOrigin)));
                vertices.push_back({ vOrigin.x, vOrigin.y, vOrigin.z, n.x, n.y, n.z });
                vertices.push_back({ v1.x, v1.y, v1.z, n.x, n.y, n.z });
                vertices.push_back({ v0.x, v0.y, v0.z, n.x, n.y, n.z });
            }
            // Inner boundary
            else {
                // Flipped winding order and normal for inner rings
                glm::vec3 n = glm::normalize(glm::cross(v0 - vOrigin, v1 - vOrigin));
                vertices.push_back({ vOrigin.x, vOrigin.y, vOrigin.z, n.x, n.y, n.z });
                vertices.push_back({ v0.x, v0.y, v0.z, n.x, n.y, n.z });
                vertices.push_back({ v1.x, v1.y, v1.z, n.x, n.y, n.z });
            }

            // TODO: Fix faulty triangle directions and draw triangles with correct shading on
            // both sides of the mesh
        }
    }

    vertices.shrink_to_fit();
    return vertices;

    // TODO: extrude lines as a box shape
}

double getHeightToReferenceSurface(const Geodetic2& geo, const RenderableGlobe& globe,
                                   bool useHeightMap)
{
    // Compute model space coordinate, and potentially account for height map
    glm::dvec3 posModelSpaceZeroHeight =
        globe.ellipsoid().cartesianSurfacePosition(geo);

    double heightToSurface = 0.0;

    // Different height computation depending on the height mode
    if (useHeightMap) {
        const SurfacePositionHandle posHandle =
            globe.calculateSurfacePositionHandle(posModelSpaceZeroHeight);

        heightToSurface += posHandle.heightToSurface;
    }

    return heightToSurface;
}

glm::dvec3 adjustHeightOfModelCoordinate(const glm::dvec3& pos,
                                         double targetHeight,
                                         const RenderableGlobe& globe,
                                         bool useHeightMap)
{
    Geodetic2 geo2 = globe.ellipsoid().cartesianToGeodetic2(pos);
    double heightToSurface = getHeightToReferenceSurface(geo2, globe, useHeightMap);
    Geodetic3 heightAdjustedGeo = { geo2, heightToSurface + targetHeight };
    return globe.ellipsoid().cartesianPosition(heightAdjustedGeo);
}

glm::dvec3 computeOffsetedModelCoordinate(const Geodetic3& geo,
                                          const RenderableGlobe& globe,
                                          const glm::vec3& offsets, bool useHeightMap)
{
    // Account for lat long offset
    double offsetLatRadians = glm::radians(offsets.x);
    double offsetLonRadians = glm::radians(offsets.y);

    Geodetic3 adjusted = geo;
    adjusted.geodetic2.lat += offsetLatRadians;
    adjusted.geodetic2.lon += offsetLonRadians;

    double heightToSurface = offsets.z;
    heightToSurface += getHeightToReferenceSurface(geo.geodetic2, globe, useHeightMap);

    Geodetic3 heightAdjustedGeo = geo;
    heightAdjustedGeo.height += heightToSurface;
    return globe.ellipsoid().cartesianPosition(heightAdjustedGeo);
}

std::vector<glm::vec3> subdivideLine(const glm::dvec3& v0, const glm::dvec3& v1,
                                     double h0, double h1, double hOffset,
                                     double maxDistance, const RenderableGlobe& globe,
                                     bool useHeightMap)
{
    double edgeLength = glm::distance(v1, v0);
    int nSegments = static_cast<int>(std::ceil(edgeLength / maxDistance));

    std::vector<glm::vec3> positions;
    positions.reserve(nSegments + 1);

    for (int seg = 0; seg < nSegments; ++seg) {
        double t = static_cast<double>(seg) / static_cast<double>(nSegments);

        // Interpolate both position and height value
        glm::vec3 newV = glm::mix(v0, v1, t);
        double newHeight = glm::mix(h0, h1, t);

        // Get position with adjusted height value
        newV = static_cast<glm::vec3>(adjustHeightOfModelCoordinate(
            newV,
            newHeight + hOffset,
            globe,
            useHeightMap
        ));

        positions.push_back(newV);
    }

    // Add final position
    positions.push_back(static_cast<glm::vec3>(
        adjustHeightOfModelCoordinate(
            v1,
            h1 + hOffset,
            globe,
            useHeightMap
        )
    ));

    positions.shrink_to_fit();
    return positions;
}

std::vector<rendering::helper::VertexXYZNormal>
subdivideTriangle(const glm::vec3& v0, const glm::vec3& v1, const glm::vec3& v2,
                  double h0, double h1, double h2, double maxDistance,
                  const glm::vec3& offsets, const RenderableGlobe& globe,
                  bool useHeightMap)
{
    std::vector<rendering::helper::VertexXYZNormal> vertices;

    // Subdivide edges
    std::vector<glm::vec3> edge01 = geometryhelper::subdivideLine(
        v0, v1,
        h0, h1,
        offsets.z,
        maxDistance,
        globe,
        useHeightMap
    );

    std::vector<glm::vec3> edge02 = geometryhelper::subdivideLine(
        v0, v2,
        h0, h2,
        offsets.z,
        maxDistance,
        globe,
        useHeightMap
    );

    std::vector<glm::vec3> edge12 = geometryhelper::subdivideLine(
        v1, v2,
        h1, h2,
        offsets.z,
        maxDistance,
        globe,
        useHeightMap
    );

    size_t nSteps01 = edge01.size();
    size_t nSteps02 = edge02.size();
    size_t nSteps12 = edge12.size();
    size_t maxSteps = std::max(std::max(nSteps01, nSteps02), nSteps12);
    vertices.reserve(maxSteps * maxSteps);

    // Add points inside the triangle
    std::vector<Coordinate> pointCoords;
    pointCoords.reserve(3 * maxSteps + 1);

    const float lengthEdge01 = glm::length(v1 - v0);
    const float lengthEdge02 = glm::length(v2 - v0);
    for (size_t i = 1; i < nSteps01; ++i) {
        for (size_t j = 1; j < nSteps02; ++j) {
            glm::vec3 comp01 = edge01[i] - v0;
            glm::vec3 comp02 = edge02[j] - v0;

            float w1 = glm::length(comp01) / lengthEdge01;
            float w2 = glm::length(comp02) / lengthEdge02;

            if (w1 + w2 > 1.f - std::numeric_limits<float>::epsilon()) {
                continue; // Sum larger than 1.0 => Outside of triangle
            }

            glm::vec3 pos = v0 + comp01 + comp02;

            Geodetic2 geo2 = globe.ellipsoid().cartesianToGeodetic2(pos);
            // TODO: include height from pos
            Geodetic3 geo3 = { geo2, 0.0 };
            pointCoords.push_back(geometryhelper::toGeosCoord(geo3));
        }
    }

    // Add egde positions
    for (size_t i = 0; i < maxSteps; ++i) {
        if (i < edge01.size() - 1) {
            Geodetic2 geo2 = globe.ellipsoid().cartesianToGeodetic2(edge01[i]);
            // TODO: include height from pos
            Geodetic3 geo3 = { geo2, 0.0 };
            pointCoords.push_back(geometryhelper::toGeosCoord(geo3));
        }
        if (i < edge02.size() - 1) {
            Geodetic2 geo2 = globe.ellipsoid().cartesianToGeodetic2(edge02[i]);
            // TODO: include height from pos
            Geodetic3 geo3 = { geo2, 0.0 };
            pointCoords.push_back(geometryhelper::toGeosCoord(geo3));
        }
        if (i < edge12.size() - 1) {
            Geodetic2 geo2 = globe.ellipsoid().cartesianToGeodetic2(edge12[i]);
            // TODO: include height from pos
            Geodetic3 geo3 = { geo2, 0.0 };
            pointCoords.push_back(geometryhelper::toGeosCoord(geo3));
        }
    }

    // Also add the final position (not part of the subdivide step above)
    Geodetic2 geo2 = globe.ellipsoid().cartesianToGeodetic2(v2);
    // TODO: include height from pos
    Geodetic3 geo3 = { geo2, 0.0 };
    pointCoords.push_back(geometryhelper::toGeosCoord(geo3));

    pointCoords.shrink_to_fit();

    using namespace geos::geom;

    GeometryFactory::Ptr geometryFactory = GeometryFactory::create();
    MultiPoint* points = geometryFactory->createMultiPoint(pointCoords);

    // Create triangulation of points
    geos::triangulate::DelaunayTriangulationBuilder builder;
    builder.setSites(*points->getCoordinates());

    // Returns a list o triangles, as geos polygons
    GeometryCollection* triangleGeoms = builder.getTriangles(*geometryFactory).release();
    std::vector<Coordinate> triCoords;
    triangleGeoms->getCoordinates()->toVector(triCoords);

    vertices.reserve(vertices.size() + triCoords.size() + 1);

    int count = 0;
    for (const Coordinate& coord : triCoords) {
        count++;
        if (count == 4) {
            // Skip every 4th coord, as polygons have one extra coord per triangle.
            // Also, reset the counting at this point.
            count = 0;
            continue;
        }
        Geodetic3 geodetic = geometryhelper::toGeodetic(coord);
        glm::vec3 v = geometryhelper::computeOffsetedModelCoordinate(
            geodetic,
            globe,
            offsets,
            useHeightMap
        );
        vertices.push_back({ v.x, v.y, v.z, 0.f, 0.f, 0.f });

        // Every third set of coordinates is a triangle => update normal of previous
        // triangle vertices
        if (count == 3) {
            // Find previous vertices
            rendering::helper::VertexXYZNormal& vert0 = vertices[count - 3];
            rendering::helper::VertexXYZNormal& vert1 = vertices[count - 2];

            const glm::vec3 v0 = glm::vec3(vert0.xyz[0], vert0.xyz[1], vert0.xyz[2]);
            const glm::vec3 v1 = glm::vec3(vert1.xyz[0], vert1.xyz[1], vert1.xyz[2]);
            const glm::vec3 n = -glm::normalize(glm::cross(v1 - v0, v - v0));

            vert0.normal[0] = n.x;
            vert0.normal[1] = n.y;
            vert0.normal[2] = n.z;

            vert1.normal[0] = n.x;
            vert1.normal[1] = n.y;
            vert1.normal[2] = n.z;

            vertices.back().normal[0] = n.x;
            vertices.back().normal[1] = n.y;
            vertices.back().normal[2] = n.y;
        }
    }

    vertices.shrink_to_fit();
    return vertices;
}

} // namespace openspace::globebrowsing::geometryhelper
