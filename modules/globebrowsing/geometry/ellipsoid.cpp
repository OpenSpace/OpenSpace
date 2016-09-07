/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#define _USE_MATH_DEFINES
#include <math.h>
#include <algorithm>

namespace {
    const std::string _loggerCat = "Ellipsoid";
}

namespace openspace {

    Ellipsoid::Ellipsoid() : Ellipsoid(1,1,1) {

    }

    Ellipsoid::Ellipsoid(Vec3 radii) : _radii(radii) {
        updateInternalCache();
    }

    Ellipsoid::Ellipsoid(Scalar x, Scalar y, Scalar z) : _radii(x, y, z) {
        updateInternalCache();
    }

    Ellipsoid::~Ellipsoid(){

    }

    void Ellipsoid::updateInternalCache() {
        _cached._radiiSquared = Vec3(
            (_radii.x * _radii.x),
            (_radii.y * _radii.y),
            (_radii.z * _radii.z));

        _cached._oneOverRadiiSquared = Vec3(1) / _cached._radiiSquared;
        _cached._radiiToTheFourth = _cached._radiiSquared * _cached._radiiSquared;
        
        std::vector<Scalar> radii = { _radii.x, _radii.y, _radii.z };
        std::sort(radii.begin(), radii.end());
        _cached._minimumRadius = radii[0];
        _cached._medianRadius = radii[1];
        _cached._maximumRadius = radii[2];
    }

    


    Vec3 Ellipsoid::geocentricSurfaceProjection(const Vec3& p) const {
        Scalar beta = 1.0 / sqrt(dot(p * p, _cached._oneOverRadiiSquared));
        return beta * p;
    }

    Vec3 Ellipsoid::geodeticSurfaceProjection(const Vec3& p) const {
        Scalar beta = 1.0 / sqrt(dot(p * p, _cached._oneOverRadiiSquared));
        Scalar n = glm::length(beta * p * _cached._oneOverRadiiSquared);
        Scalar alpha = (1.0 - beta) * (glm::length(p) / n);
        
        Vec3 p2 = p * p;
        Vec3 d, d2, d3;

        Scalar s = 0.0;
        Scalar dSdA = 1.0;

        Scalar epsilon = 1e-10;
        do {
            alpha -= (s / dSdA);

            d = Vec3(1.0) + alpha * _cached._oneOverRadiiSquared;
            d2 = d * d;
            d3 = d * d2;
            
            s = glm::dot(p2 / (_cached._radiiSquared * d2), Vec3(1.0)) - 1.0;

            dSdA = -2.0 * glm::dot(p2 / (_cached._radiiToTheFourth * d3), Vec3(1.0));
        }
        while (abs(s) > epsilon);
        return p / d;
    }

    Vec3 Ellipsoid::geodeticSurfaceNormalForGeocentricallyProjectedPoint(const Vec3& p) const {
        Vec3 normal = p * _cached._oneOverRadiiSquared;
        return glm::normalize(normal);
    }

    Vec3 Ellipsoid::geodeticSurfaceNormal(Geodetic2 geodetic2) const {
        Scalar cosLat = glm::cos(geodetic2.lat);
        //geodetic2.lon = geodetic2.lon > M_PI ? geodetic2.lon - M_PI * 2 : geodetic2.lon;
        return Vec3(
            cosLat * cos(geodetic2.lon),
            cosLat * sin(geodetic2.lon), 
            sin(geodetic2.lat));
    }

    const Vec3& Ellipsoid::radii() const {
        return _radii;
    }


    const Vec3& Ellipsoid::radiiSquared() const {
        return _cached._radiiSquared;
    }

    const Vec3& Ellipsoid::oneOverRadiiSquared() const {
        return _cached._oneOverRadiiSquared;
    }

    const Vec3& Ellipsoid::radiiToTheFourth() const {
        return _cached._radiiToTheFourth;
    }

    Scalar Ellipsoid::minimumRadius() const {
        return _cached._minimumRadius;
    }

    Scalar Ellipsoid::maximumRadius() const {
        return _cached._maximumRadius;
    }

    Scalar Ellipsoid::averageRadius() const {
        return (_radii.x + _radii.y + _radii.z) / 3.0;
    }

    Scalar Ellipsoid::longitudalDistance(Scalar lat, Scalar lon1, Scalar lon2) const {
        Vec2 ellipseRadii = glm::cos(lat) * _radii.xy();
        // Approximating with the ellipse mean radius
        Scalar meanRadius = 0.5 * (ellipseRadii.x + ellipseRadii.y);
        return meanRadius * std::abs(lon2 - lon1);
    }

    Scalar Ellipsoid::greatCircleDistance(const Geodetic2& p1, const Geodetic2& p2) const{
        // https://en.wikipedia.org/wiki/Meridian_arc
        // https://en.wikipedia.org/wiki/Great-circle_distance#Vector_version

        Vec3 n1 = geodeticSurfaceNormal(p1);
        Vec3 n2 = geodeticSurfaceNormal(p2);
        Scalar centralAngle = glm::atan(glm::length(glm::cross(n1, n2)) / glm::dot(n1, n2));

        Geodetic2 pMid = (p1 + p2) / 2;
        Vec3 centralNormal = cartesianSurfacePosition(pMid);

        return centralAngle * glm::length(centralNormal);
    }

    Geodetic2 Ellipsoid::cartesianToGeodetic2(const Vec3& p) const
    {
        Vec3 normal = geodeticSurfaceNormalForGeocentricallyProjectedPoint(p);
        return Geodetic2(
            asin(normal.z / length(normal)),    // Latitude
            atan2(normal.y, normal.x));            // Longitude
    }

    Vec3 Ellipsoid::cartesianSurfacePosition(const Geodetic2& geodetic2) const
    {
        // Position on surface : height = 0
        return cartesianPosition(Geodetic3({ geodetic2, 0 }));
    }

    Vec3 Ellipsoid::cartesianPosition(const Geodetic3& geodetic3) const
    {
        Vec3 normal = geodeticSurfaceNormal(geodetic3.geodetic2);
        Vec3 k = _cached._radiiSquared * normal;
        Scalar gamma = sqrt(dot(k, normal));
        Vec3 rSurface = k / gamma;
        return rSurface + geodetic3.height * normal;
    }

} // namespace openspace
