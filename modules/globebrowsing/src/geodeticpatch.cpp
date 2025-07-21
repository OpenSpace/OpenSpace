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

#include <modules/globebrowsing/src/geodeticpatch.h>

#include <modules/globebrowsing/src/tileindex.h>
#include <ghoul/misc/assert.h>

namespace {
    // Normalizes the angle to the interval [center - pi, center + pi[
    double normalizedAngleAround(double angle, double center) {
        angle -= center + glm::pi<double>();

        // this will cause angle to be in value range ]-2pi, 2pi[
        angle = fmod(angle, glm::two_pi<double>());

        // ensure _radians are positive, ie in value range [0, 2pi[
        if (angle < 0.0) {
            angle += glm::two_pi<double>();
        }

        angle += center - glm::pi<double>();
        return angle;
    }
} // namespace

namespace openspace::globebrowsing {

GeodeticPatch::GeodeticPatch(double centerLat, double centerLon, double halfSizeLat,
                             double halfSizeLon)
    : _center{ centerLat, centerLon }
    , _halfSize{ halfSizeLat, halfSizeLon }
{}

GeodeticPatch::GeodeticPatch(const Geodetic2& center, const Geodetic2& halfSize)
    : _center(center)
    , _halfSize(halfSize)
{}

GeodeticPatch::GeodeticPatch(const TileIndex& tileIndex) {
    const double deltaLat = (2 * glm::pi<double>()) /
                            (static_cast<double>(1 << tileIndex.level));
    const double deltaLon = (2 * glm::pi<double>()) /
                            (static_cast<double>(1 << tileIndex.level));
    const Geodetic2 nwCorner{
        glm::half_pi<double>() - deltaLat * tileIndex.y,
        -glm::pi<double>() + deltaLon * tileIndex.x
    };
    _halfSize = Geodetic2{ deltaLat / 2.0, deltaLon / 2.0 };
    _center = Geodetic2{ nwCorner.lat - _halfSize.lat, nwCorner.lon + _halfSize.lon };
}

void GeodeticPatch::setCenter(Geodetic2 center) {
    _center = std::move(center);
}

void GeodeticPatch::setHalfSize(Geodetic2 halfSize) {
    _halfSize = std::move(halfSize);
}

double GeodeticPatch::maximumTileLevel() const {
    // Numerator is just pi, not 2*pi, since we are dealing with HALF sizes
    return std::log2(glm::pi<double>() / std::min(_halfSize.lat, _halfSize.lon));
}

double GeodeticPatch::minimumTileLevel() const {
    // Numerator is just pi, not 2*pi, since we are dealing with HALF sizes
    return std::log2(glm::pi<double>() / std::max(_halfSize.lat, _halfSize.lon));
}

const Geodetic2& GeodeticPatch::center() const {
    return _center;
}

const Geodetic2& GeodeticPatch::halfSize() const {
    return _halfSize;
}

Geodetic2 GeodeticPatch::size() const {
    return {
        _halfSize.lat * 2.0,
        _halfSize.lon * 2.0
    };
}

Geodetic2 GeodeticPatch::corner(Quad q) const {
    switch (q) {
        case NORTH_WEST:  return Geodetic2{ maxLat(), minLon() }; // northWestCorner();
        case NORTH_EAST:  return Geodetic2{ maxLat(), maxLon() }; // northEastCorner();
        case SOUTH_WEST:  return Geodetic2{ minLat(), minLon() }; // southWestCorner();
        case SOUTH_EAST:  return Geodetic2{ minLat(), maxLon() }; // southEastCorner();
        default:          throw ghoul::MissingCaseException();
    }
}

double GeodeticPatch::minLat() const {
    return _center.lat - _halfSize.lat;
}

double GeodeticPatch::maxLat() const {
    return _center.lat + _halfSize.lat;
}

double GeodeticPatch::minLon() const {
    return _center.lon - _halfSize.lon;
}

double GeodeticPatch::maxLon() const {
    return _center.lon + _halfSize.lon;
}

bool GeodeticPatch::contains(const Geodetic2& p) const {
    const Geodetic2 diff = {
        .lat = _center.lat - p.lat,
        .lon = _center.lon - p.lon
    };
    return std::abs(diff.lat) <= _halfSize.lat && std::abs(diff.lon) <= _halfSize.lon;
}

double GeodeticPatch::edgeLatitudeNearestEquator() const {
    return _center.lat + _halfSize.lat * (isNorthern() ? -1.0 : 1.0);
}

bool GeodeticPatch::isNorthern() const {
    return _center.lat > 0.0;
}

Geodetic2 GeodeticPatch::clamp(const Geodetic2& p) const {
    // Normalize w.r.t. the center in order for the clamping to done correctly
    //
    // Example:
    //    centerLat = 0 deg, halfSize.lat = 10 deg, pointLat = 330 deg
    //        --> Just clamping pointLat would be clamp(330, -10, 10) = 10 // WRONG!
    //    Instead, if we first normalize 330 deg around 0, we get -30 deg
    //        --> clamp(-30, -10, 10) = -10 // CORRECT!
    const double pointLat = normalizedAngleAround(p.lat, _center.lat);
    const double pointLon = normalizedAngleAround(p.lon, _center.lon);

    return {
        std::clamp(pointLat, minLat(), maxLat()),
        std::clamp(pointLon, minLon(), maxLon())
    };
}

Geodetic2 GeodeticPatch::closestCorner(const Geodetic2& p) const {
    // LatLon vector from patch center to the point
    const Geodetic2 centerToPoint = {
        .lat = p.lat - _center.lat,
        .lon = p.lon - _center.lon
    };

    // Normalize the difference angles to be centered around 0.
    const double latDiff = normalizedAngleAround(centerToPoint.lat, 0.0);
    const double lonDiff = normalizedAngleAround(centerToPoint.lon, 0.0);

    // If latDiff > 0
    //    --> point p is north of the patch center
    //    --> the closest corner to the point must be a northern one
    //    --> set the corner's latitude coordinate to center.lat + halfSize.lat
    // else
    //    --> set corner's latidude coordinate to center.lat - halfSize.lat
    const double cornerLat = _center.lat + _halfSize.lat * (latDiff > 0.0 ? 1 : -1);

    // We then assigned the corner's longitude coordinate in a similar fashion
    const double cornerLon = _center.lon + _halfSize.lon * (lonDiff > 0.0 ? 1 : -1);

    return Geodetic2{ cornerLat, cornerLon };
}

Geodetic2 GeodeticPatch::closestPoint(const Geodetic2& p) const {
    // This method finds the closest point on the patch, to the provided
    // point p. As we are deali ng with latitude-longitude patches, distance in this
    // context refers to great-circle distance.
    // (https://en.wikipedia.org/wiki/Great-circle_distance)
    //
    // This uses a simple clamping approach to find the closest point on the
    // patch. A naive castesian clamp is not sufficient for this purpose,
    // as illustrated with an example below.

    // Example: (degrees are used for latidude, longitude)
    //    patchCenter = (0,0), patchHalfSize = (45,45), point = (5, 170)
    //    Note, the point and the patch are on opposite sides of the sphere
    //
    //    cartesian clamp:
    //       --> clampedPointLat = clamp(5, -45, 45) = 5
    //       --> clampedPointLon = clamp(170, -45, 45) = 45
    //       --> result: (5, 45)
    //       --> closest point is actually (45, 45)
    //       --> The error is significant
    //
    // This method simply adds an extra clamp on the latitude in these cases. In the
    // above example, that would be the following:
    //       --> clampedPointLat = clamp(180 - 5, -45, 45) = 45
    //
    // Just doing this actually makes points returned from this methods being the
    // true closest point, great-circle distance-wise.


    // Normalize point with respect to center. This is done because the point
    // will later be clamped. See LatLonPatch::clamp(const LatLon&) for explanation
    const double pointLat = normalizedAngleAround(p.lat, _center.lat);
    const double pointLon = normalizedAngleAround(p.lon, _center.lon);

    // Calculate the longitud difference between center and point. We normalize around
    // zero because we want the "shortest distance" difference, i.e the difference
    // should be in the interval [-180 deg, 180 deg]
    const double centerToPointLon = normalizedAngleAround(_center.lon - pointLon, 0.0);

    // Calculate the longitudinal distance to the closest patch edge
    const double lonDistanceToClosestPatch = std::abs(centerToPointLon) - _halfSize.lon;

    // If the longitude distance to the closest patch edge is larger than 90 deg
    // the latitude will have to be clamped to its closest corner, as explained in
    // the example above.
    const double clampedLat =
        lonDistanceToClosestPatch > glm::half_pi<double>() ?
        std::clamp(
            normalizedAngleAround(glm::pi<double>() - pointLat, _center.lat),
            minLat(),
            maxLat()) :
        std::clamp(pointLat, minLat(), maxLat());

    // Longitude is just clamped normally
    const double clampedLon = std::clamp(pointLon, minLon(), maxLon());

    return Geodetic2{ clampedLat, clampedLon };
}

} // namespace openspace::globebrowsing
