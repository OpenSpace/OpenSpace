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

#ifndef __OPENSPACE_CORE___GEODETIC___H__
#define __OPENSPACE_CORE___GEODETIC___H__

#include <ghoul/glm.h>
#include <optional>

namespace openspace {

class SceneGraphNode;

struct Geodetic2 {
    double lat = 0.0; // in radians
    double lon = 0.0; // in radians
};

struct Geodetic3 {
    Geodetic2 geodetic2;
    double height = 0.0; // in meters
};

/**
 * Sets the camera position for the next frame to be at the location identified by the
 * provided geodetic position \p geo relative to the SceneGraphNode \p sgn. The current
 * altitude of the camera over the selected scene graph node is maintained.
 */
void goToGeodetic2(const SceneGraphNode& sgn, Geodetic2 geo);

/**
 * Sets the camera position for the next frame to be at the location identified by the
 * provided geodetic position \p geo relative to the SceneGraphNode \p sgn.
 */
void goToGeodetic3(const SceneGraphNode& sgn, Geodetic3 geo);

/**
 * Returns the Cartesian coordinate for the provided geodetic position of \p latitude,
 * \p longitude, and \p altitude relative to the SceneGraphNode \p sgn. If no altitude is
 * provided, the current altitude of the camera relative to the scene graph node is used.
 */
glm::vec3 cartesianCoordinatesFromGeo(const SceneGraphNode& sgn, double latitude,
    double longitude, std::optional<double> altitude = std::nullopt);

/**
 * Returns the position of the camera relative to the current anchor node as geodetic
 * coordinates. The returned value contains the latitude in the x coordinate, the
 * longitude in the y coordinate, and the altitude in the z coordinate. The longitude and
 * latitude are provided in degrees, the altitude is provided in meters.
 */
glm::dvec3 geoPositionFromCamera();

/**
 * Returns the height of the camera relative to the provided SceneGraphNode \p sgn in
 * meters. If \p useHeightMap is provided as `true` and \p sgn is a globe with an existing
 * height map, that height map is used to calculate the altitude.
 */
double altitudeFromCamera(const SceneGraphNode& sgn, bool useHeightMap = false);

} // namespace openspace

#endif // __OPENSPACE_CORE___GEODETIC___H__
