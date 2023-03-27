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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___GLOBEGEOMETRYHELPER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___GLOBEGEOMETRYHELPER___H__

#include <glm/glm.hpp>
#include <vector>

namespace openspace::globebrowsing {
    struct Geodetic2;
    struct Geodetic3;
    class RenderableGlobe;
}

namespace openspace::rendering::helper {
    struct VertexXYZNormal;
}

namespace geos::geom {
    class Coordinate;
}

namespace openspace::globebrowsing::geometryhelper {

Geodetic3 toGeodetic(const geos::geom::Coordinate& c);

geos::geom::Coordinate toGeosCoord(const Geodetic3& gd);

std::vector<Geodetic3> coordsToGeodetic(
    const std::vector<geos::geom::Coordinate>& coords);

/**
* Create triangle geometry for the extruded edge, given the provided edge vertices
*/
std::vector<rendering::helper::VertexXYZNormal> createExtrudedGeometryVertices(
    const std::vector<std::vector<glm::vec3>>& edgeVertices);

/**
* Get height contribution from reference surface of the globe. The contribution depends
* on whether the height map should be used or not.
*/
double getHeightToReferenceSurface(const Geodetic2& geo,
    const RenderableGlobe& globe, bool useHeightMap);

/**
* Get position in model space. Target height is target height above reference surface
* @TODO elaborate
*/
glm::dvec3 adjustHeightOfModelCoordinate(const glm::dvec3& pos, double targetHeight,
        const RenderableGlobe& globe, bool useHeightMap);

/**
* Compute model space cordinate from geodetic coordinate, and account for lat, long
* and height offsets and potentially the height map
*/
glm::dvec3 computeOffsetedModelCoordinate(const Geodetic3& geo,
    const RenderableGlobe& globe, const glm::vec3& offsets, bool useHeightMap);

} // namespace openspace::globebrowsing::geometryhelper

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___GLOBEGEOMETRYHELPER___H__
