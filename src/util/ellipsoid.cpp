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

#include <openspace/util/ellipsoid.h>

#include <openspace/util/geodetic.h>
#include <algorithm>
#include <array>
#include <vector>

namespace {
    constexpr size_t MaxIterations = 8;
} // namespace

namespace openspace {

Ellipsoid::Ellipsoid(glm::dvec3 radii) : _radii(std::move(radii)) {
    // If the radii is equal to 0, all of the calculations below fail and result in nan's
    // being raised all over the place
    if (glm::length(radii) > 0.0) {
        updateInternalCache();
    }
}

void Ellipsoid::updateInternalCache() {
    _cached.radiiSquared = _radii * _radii;

    _cached.oneOverRadiiSquared = glm::dvec3(1.0) / _cached.radiiSquared;
    _cached.radiiToTheFourth = _cached.radiiSquared * _cached.radiiSquared;

    std::array<double, 3> radii = { _radii.x, _radii.y, _radii.z };
    std::sort(radii.begin(), radii.end());
    _cached.minimumRadius = radii[0];
    _cached.maximumRadius = radii[2];
}

glm::dvec3 Ellipsoid::geocentricSurfaceProjection(const glm::dvec3& p) const {
    const double beta = 1.0 / std::sqrt(glm::dot(p * p, _cached.oneOverRadiiSquared));
    return beta * p;
}

glm::dvec3 Ellipsoid::geodeticSurfaceProjection(const glm::dvec3& p) const {
    const double beta = 1.0 / std::sqrt(glm::dot(p * p, _cached.oneOverRadiiSquared));
    const double n = glm::length(beta * p * _cached.oneOverRadiiSquared);
    double alpha = (1.0 - beta) * (glm::length(p) / n);

    const glm::dvec3 p2 = p * p;
    glm::dvec3 d = glm::dvec3(0.0);
    double s = 0.0;
    double dSdA = 1.0;

    constexpr double Epsilon = 1e-10;

    size_t nIterations = 0;
    do {
        alpha -= (s / dSdA);

        d = glm::dvec3(1.0) + alpha * _cached.oneOverRadiiSquared;
        const glm::dvec3 d2 = d * d;
        const glm::dvec3 d3 = d * d2;

        s = glm::dot(p2 / (_cached.radiiSquared * d2), glm::dvec3(1.0)) - 1.0;

        dSdA = -2.0 * glm::dot(p2 / (_cached.radiiToTheFourth * d3), glm::dvec3(1.0));
        ++nIterations;
    }
    while (std::abs(s) > Epsilon && nIterations < MaxIterations);

    return p / d;
}

glm::dvec3 Ellipsoid::geodeticSurfaceNormalForGeocentricallyProjectedPoint(
                                                                const glm::dvec3& p) const
{
    const glm::dvec3 normal = p * _cached.oneOverRadiiSquared;
    return glm::length(normal) > 0.0 ? glm::normalize(normal) : normal;
}

glm::dvec3 Ellipsoid::geodeticSurfaceNormal(const Geodetic2& geodetic2) const {
    const double cosLat = std::cos(geodetic2.lat);
    //geodetic2.lon = geodetic2.lon > M_PI ? geodetic2.lon - M_PI * 2 : geodetic2.lon;
    return glm::dvec3(
        cosLat * std::cos(geodetic2.lon),
        cosLat * std::sin(geodetic2.lon),
        std::sin(geodetic2.lat)
    );
}

const glm::dvec3& Ellipsoid::radii() const {
    return _radii;
}

double Ellipsoid::minimumRadius() const {
    return _cached.minimumRadius;
}

double Ellipsoid::maximumRadius() const {
    return _cached.maximumRadius;
}

double Ellipsoid::longitudalDistance(double lat, double lon1, double lon2) const {
    const glm::dvec2 ellipseRadii = std::cos(lat) * glm::dvec2(_radii);
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

    const Geodetic2 pMid = {
        .lat = (p1.lat + p2.lat) / 2.0,
        .lon = (p1.lon + p2.lon) / 2.0
    };
    const glm::dvec3 centralNormal = cartesianSurfacePosition(pMid);

    return centralAngle * glm::length(centralNormal);
}

Geodetic2 Ellipsoid::cartesianToGeodetic2(const glm::dvec3& p) const {
    const glm::dvec3 normal = geodeticSurfaceNormalForGeocentricallyProjectedPoint(p);
    return glm::length(normal) ?
        Geodetic2 {
            std::asin(normal.z / glm::length(normal)),
            std::atan2(normal.y, normal.x)
        } :
        Geodetic2 { 0.0, 0.0 };
}

glm::dvec3 Ellipsoid::cartesianSurfacePosition(const Geodetic2& geodetic2) const {
    // Position on surface : height = 0
    return cartesianPosition(Geodetic3({ geodetic2, 0 }));
}

glm::dvec3 Ellipsoid::cartesianPosition(const Geodetic3& geodetic3) const {
    const glm::dvec3 normal = geodeticSurfaceNormal(geodetic3.geodetic2);
    const glm::dvec3 k = _cached.radiiSquared * normal;
    const double gamma = std::sqrt(glm::dot(k, normal));
    const glm::dvec3 rSurface = k / gamma;
    return rSurface + geodetic3.height * normal;
}

void Ellipsoid::setShadowConfigurationArray(
                              std::vector<Ellipsoid::ShadowConfiguration> shadowConfArray)
{
    _shadowConfArray = std::move(shadowConfArray);
}

const std::vector<Ellipsoid::ShadowConfiguration>&
Ellipsoid::shadowConfigurationArray() const
{
    return _shadowConfArray;
}

} // namespace openspace
