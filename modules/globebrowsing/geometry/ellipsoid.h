/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___ELLIPSOID___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___ELLIPSOID___H__

#include <ghoul/glm.h>

#include <vector>

namespace openspace::globebrowsing {

struct Geodetic2;
struct Geodetic3;

/**
 * This class is based largely on the Ellipsoid class defined in the book
 * "3D Engine Design for Virtual Globes". Most planets or planetary objects are better
 * described using ellipsoids than spheres. All inputs and outputs to this class is
 * based on the WGS84 standard coordinate system where the x-axis points towards
 * geographic (lat = 0, lon = 0), the y-axis points towards (lat = 0, lon = 90deg) and the
 * z-axis points towards the north pole. For other globes than earth of course the radii
 * can differ.
 */
class Ellipsoid {
public:
    // Shadow configuration structure
    struct ShadowConfiguration {
        std::pair<std::string, double> source;
        std::pair<std::string, double> caster;
    };

    /**
     * \param radii defines three radii for the Ellipsoid
     */
    Ellipsoid(glm::dvec3 radii = glm::dvec3(1.0, 1.0, 1.0));

    /**
     * Scales a point along the geocentric normal and places it on the surface of the
     * Ellipsoid.
     * \param p is a point in the cartesian coordinate system to be placed on the surface
     * of the Ellipsoid
     */
    glm::dvec3 geocentricSurfaceProjection(const glm::dvec3& p) const;

    /**
     * Scales a point along the geodetic normal and places it on the surface of the
     * Ellipsoid.
     * \param p is a point in the cartesian coordinate system to be placed on the surface
     * of the Ellipsoid
     */
    glm::dvec3 geodeticSurfaceProjection(const glm::dvec3& p) const;

    glm::dvec3 geodeticSurfaceNormalForGeocentricallyProjectedPoint(
        const glm::dvec3& p) const;
    glm::dvec3 geodeticSurfaceNormal(const Geodetic2& geodetic2) const;

    const glm::dvec3& radii() const;
    const glm::dvec3& radiiSquared() const;
    const glm::dvec3& oneOverRadiiSquared() const;
    const glm::dvec3& radiiToTheFourth() const;

    double minimumRadius() const;
    double maximumRadius() const;
    double averageRadius() const;

    double longitudalDistance(double lat, double lon1, double lon2) const;
    double greatCircleDistance(const Geodetic2& p1, const Geodetic2& p2) const;

    Geodetic2 cartesianToGeodetic2(const glm::dvec3& p) const;
    glm::dvec3 cartesianSurfacePosition(const Geodetic2& geodetic2) const;
    glm::dvec3 cartesianPosition(const Geodetic3& geodetic3) const;

    void setShadowConfigurationArray(
        const std::vector<Ellipsoid::ShadowConfiguration>& shadowConfArray
    );
    std::vector<Ellipsoid::ShadowConfiguration> shadowConfigurationArray() const;
    bool hasEclipseShadows() const;

private:
    struct EllipsoidCache {
        glm::dvec3 _radiiSquared;
        glm::dvec3 _oneOverRadiiSquared;
        glm::dvec3 _radiiToTheFourth;
        double _minimumRadius;
        double _maximumRadius;
        double _medianRadius;
    } _cached;

    void updateInternalCache();

    glm::dvec3 _radii;

    // Eclipse shadows conf
    std::vector<Ellipsoid::ShadowConfiguration> _shadowConfArray;
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___ELLIPSOID___H__
