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
#include <algorithm>
#include <filesystem>
#include <fstream>

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
    vertices.reserve(edgeVertices.size() * edgeVertices.size());

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

} // namespace openspace::globebrowsing::geometryhelper
