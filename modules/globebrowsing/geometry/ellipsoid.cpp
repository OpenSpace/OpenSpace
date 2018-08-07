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

#include <modules/globebrowsing/geometry/ellipsoid.h>

#include <modules/globebrowsing/geometry/geodetic2.h>
#include <modules/globebrowsing/geometry/geodetic3.h>

#include <algorithm>
#include <array>
#include <vector>

namespace {
    constexpr const size_t MaxIterations = 8;
}

namespace openspace::globebrowsing {

Ellipsoid::Ellipsoid(glm::dvec3 radii) : _radii(radii) {
    updateInternalCache();
}

void Ellipsoid::updateInternalCache() {
    _cached._radiiSquared = glm::dvec3(
        _radii.x * _radii.x,
        _radii.y * _radii.y,
        _radii.z * _radii.z
    );

    _cached._oneOverRadiiSquared = glm::dvec3(1.0) / _cached._radiiSquared;
    _cached._radiiToTheFourth = _cached._radiiSquared * _cached._radiiSquared;

    std::array<double, 3> radii = { _radii.x, _radii.y, _radii.z };
    std::sort(radii.begin(), radii.end());
    _cached._minimumRadius = radii[0];
    _cached._medianRadius = radii[1];
    _cached._maximumRadius = radii[2];
}

glm::dvec3 Ellipsoid::geocentricSurfaceProjection(const glm::dvec3& p) const {
    const double beta = 1.0 / sqrt(dot(p * p, _cached._oneOverRadiiSquared));
    return beta * p;
}

glm::dvec3 Ellipsoid::geodeticSurfaceProjection(const glm::dvec3& p) const {
    const double beta = 1.0 / sqrt(dot(p * p, _cached._oneOverRadiiSquared));
    const double n = glm::length(beta * p * _cached._oneOverRadiiSquared);
    double alpha = (1.0 - beta) * (glm::length(p) / n);

    const glm::dvec3 p2 = p * p;
    glm::dvec3 d;
    double s = 0.0;
    double dSdA = 1.0;

    double epsilon = 1e-10;

    size_t nIterations = 0;
    do {
        alpha -= (s / dSdA);

        d = glm::dvec3(1.0) + alpha * _cached._oneOverRadiiSquared;
        const glm::dvec3 d2 = d * d;
        const glm::dvec3 d3 = d * d2;

        s = glm::dot(p2 / (_cached._radiiSquared * d2), glm::dvec3(1.0)) - 1.0;

        dSdA = -2.0 * glm::dot(p2 / (_cached._radiiToTheFourth * d3), glm::dvec3(1.0));
        ++nIterations;
    }
    while (std::abs(s) > epsilon && nIterations < MaxIterations);

    return p / d;
}

glm::dvec3 Ellipsoid::geodeticSurfaceNormalForGeocentricallyProjectedPoint(
                                                                const glm::dvec3& p) const
{
    const glm::dvec3 normal = p * _cached._oneOverRadiiSquared;
    return glm::normalize(normal);
}

glm::dvec3 Ellipsoid::geodeticSurfaceNormal(const Geodetic2& geodetic2) const {
    const double cosLat = glm::cos(geodetic2.lat);
    //geodetic2.lon = geodetic2.lon > M_PI ? geodetic2.lon - M_PI * 2 : geodetic2.lon;
    return glm::dvec3(
        cosLat * cos(geodetic2.lon),
        cosLat * sin(geodetic2.lon),
        sin(geodetic2.lat)
    );
}

const glm::dvec3& Ellipsoid::radii() const {
    return _radii;
}

const glm::dvec3& Ellipsoid::radiiSquared() const {
    return _cached._radiiSquared;
}

const glm::dvec3& Ellipsoid::oneOverRadiiSquared() const {
    return _cached._oneOverRadiiSquared;
}

const glm::dvec3& Ellipsoid::radiiToTheFourth() const {
    return _cached._radiiToTheFourth;
}

double Ellipsoid::minimumRadius() const {
    return _cached._minimumRadius;
}

double Ellipsoid::maximumRadius() const {
    return _cached._maximumRadius;
}

double Ellipsoid::averageRadius() const {
    return (_radii.x + _radii.y + _radii.z) / 3.0;
}

double Ellipsoid::longitudalDistance(double lat, double lon1, double lon2) const {
    const glm::dvec2 ellipseRadii = glm::cos(lat) * glm::dvec2(_radii);
    // Approximating with the ellipse mean radius
    const double meanRadius = 0.5 * (ellipseRadii.x + ellipseRadii.y);
    return meanRadius * std::abs(lon2 - lon1);
}

double Ellipsoid::greatCircleDistance(const Geodetic2& p1, const Geodetic2& p2) const {
    // https://en.wikipedia.org/wiki/Meridian_arc
    // https://en.wikipedia.org/wiki/Great-circle_distance#Vector_version

    const glm::dvec3 n1 = geodeticSurfaceNormal(p1);
    const glm::dvec3 n2 = geodeticSurfaceNormal(p2);
    const double centralAngle = glm::atan(
        glm::length(glm::cross(n1, n2)) / glm::dot(n1, n2)
    );

    const Geodetic2 pMid = (p1 + p2) / 2;
    const glm::dvec3 centralNormal = cartesianSurfacePosition(pMid);

    return centralAngle * glm::length(centralNormal);
}

Geodetic2 Ellipsoid::cartesianToGeodetic2(const glm::dvec3& p) const {
    const glm::dvec3 normal = geodeticSurfaceNormalForGeocentricallyProjectedPoint(p);
    return Geodetic2(
        asin(normal.z / length(normal)), // Latitude
        atan2(normal.y, normal.x)        // Longitude
    );
}

glm::dvec3 Ellipsoid::cartesianSurfacePosition(const Geodetic2& geodetic2) const {
    // Position on surface : height = 0
    return cartesianPosition(Geodetic3({ geodetic2, 0 }));
}

glm::dvec3 Ellipsoid::cartesianPosition(const Geodetic3& geodetic3) const {
    const glm::dvec3 normal = geodeticSurfaceNormal(geodetic3.geodetic2);
    const glm::dvec3 k = _cached._radiiSquared * normal;
    const double gamma = sqrt(dot(k, normal));
    const glm::dvec3 rSurface = k / gamma;
    return rSurface + geodetic3.height * normal;
}

void Ellipsoid::setShadowConfigurationArray(
                       const std::vector<Ellipsoid::ShadowConfiguration>& shadowConfArray)
{
    _shadowConfArray = shadowConfArray;
}

std::vector<Ellipsoid::ShadowConfiguration> Ellipsoid::shadowConfigurationArray() const {
    return _shadowConfArray;
}

bool Ellipsoid::hasEclipseShadows() const {
    return !_shadowConfArray.empty();
}

} // namespace openspace::globebrowsing
