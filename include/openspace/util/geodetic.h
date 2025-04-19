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

double altitudeFromCamera(const SceneGraphNode& renderable, bool useHeightMap = false);

void goToGeo(const SceneGraphNode& globe, double latitude, double longitude);
void goToGeo(const SceneGraphNode& globe, double latitude, double longitude,
    double altitude);

void goToGeodetic2(const SceneGraphNode& globe, Geodetic2 geo2);
void goToGeodetic3(const SceneGraphNode& globe, Geodetic3 geo3);

glm::vec3 cartesianCoordinatesFromGeo(const SceneGraphNode& renderable, double latitude,
    double longitude, std::optional<double> altitude = std::nullopt);

glm::dvec3 geoPosition();

} // namespace openspace

#endif // __OPENSPACE_CORE___GEODETIC___H__
