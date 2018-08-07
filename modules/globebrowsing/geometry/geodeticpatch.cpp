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

#include <modules/globebrowsing/geometry/geodeticpatch.h>

#include <modules/globebrowsing/geometry/angle.h>
#include <modules/globebrowsing/tile/tileindex.h>

#include <ghoul/misc/assert.h>

namespace openspace::globebrowsing {

GeodeticPatch::GeodeticPatch(double centerLat, double centerLon, double halfSizeLat,
                             double halfSizeLon)
    : _center(Geodetic2(centerLat, centerLon))
    , _halfSize(Geodetic2(halfSizeLat, halfSizeLon))
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
    const Geodetic2 nwCorner(
        glm::pi<double>() / 2 - deltaLat * tileIndex.y,
        -glm::pi<double>() + deltaLon * tileIndex.x
    );
    _halfSize = Geodetic2(deltaLat / 2, deltaLon / 2);
    _center = Geodetic2(nwCorner.lat - _halfSize.lat, nwCorner.lon + _halfSize.lon);
}

void GeodeticPatch::setCenter(Geodetic2 center) {
    _center = std::move(center);
}

void GeodeticPatch::setHalfSize(Geodetic2 halfSize) {
    _halfSize = std::move(halfSize);
}

double GeodeticPatch::maximumTileLevel() const {
    // Numerator is just pi, not 2*pi, since we are dealing with HALF sizes
    return log2(glm::pi<double>() / glm::min(_halfSize.lat, _halfSize.lon));
}

double GeodeticPatch::minimumTileLevel() const {
    // Numerator is just pi, not 2*pi, since we are dealing with HALF sizes
    return log2(glm::pi<double>() / glm::max(_halfSize.lat, _halfSize.lon));
}

const Geodetic2& GeodeticPatch::center() const {
    return _center;
}

const Geodetic2& GeodeticPatch::halfSize() const {
    return _halfSize;
}

Geodetic2 GeodeticPatch::size() const {
    return _halfSize * 2.0;
}

Geodetic2 GeodeticPatch::corner(Quad q) const {
    switch (q) {
        case NORTH_WEST: return Geodetic2(maxLat(), minLon());// northWestCorner();
        case NORTH_EAST: return Geodetic2(maxLat(), maxLon());// northEastCorner();
        case SOUTH_WEST: return Geodetic2(minLat(), minLon());// southWestCorner();
        case SOUTH_EAST: return Geodetic2(minLat(), maxLon());// southEastCorner();
        default:         throw ghoul::MissingCaseException();
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
    const Geodetic2 diff = _center - p;
    return glm::abs(diff.lat) <= _halfSize.lat && glm::abs(diff.lon) <= _halfSize.lon;
}

double GeodeticPatch::edgeLatitudeNearestEquator() const {
    return _center.lat + _halfSize.lat * (isNorthern() ? -1 : 1);
}

double GeodeticPatch::isNorthern() const {
    return _center.lat > 0.0;
}

Geodetic2 GeodeticPatch::clamp(const Geodetic2& p) const {
    using Ang = Angle<double>;

    // Convert to Angles for normalization
    const Ang centerLat = Ang::fromRadians(_center.lat);
    const Ang centerLon = Ang::fromRadians(_center.lon);
    Ang pointLat = Ang::fromRadians(p.lat);
    Ang pointLon = Ang::fromRadians(p.lon);

    // Normalize w.r.t. the center in order for the clamping to done correctly
    //
    // Example:
    //    centerLat = 0 deg, halfSize.lat = 10 deg, pointLat = 330 deg
    //        --> Just clamping pointLat would be clamp(330, -10, 10) = 10 // WRONG!
    //    Instead, if we first normalize 330 deg around 0, we get -30 deg
    //        --> clamp(-30, -10, 10) = -10 // CORRECT!
    pointLat.normalizeAround(centerLat);
    pointLon.normalizeAround(centerLon);

    return Geodetic2(
        glm::clamp(pointLat.asRadians(), minLat(), maxLat()),
        glm::clamp(pointLon.asRadians(), minLon(), maxLon())
    );
}

Geodetic2 GeodeticPatch::closestCorner(const Geodetic2& p) const {
    using Ang = Angle<double>;

    // LatLon vector from patch center to the point
    const Geodetic2 centerToPoint = p - _center;

    // Normalize the difference angles to be centered around 0.
    const Ang latDiff = Ang::fromRadians(centerToPoint.lat).normalizeAround(Ang::ZERO);
    const Ang lonDiff = Ang::fromRadians(centerToPoint.lon).normalizeAround(Ang::ZERO);

    // If latDiff > 0
    //    --> point p is north of the patch center
    //    --> the closest corner to the point must be a northern one
    //    --> set the corner's latitude coordinate to center.lat + halfSize.lat
    // else
    //    --> set corner's latidude coordinate to center.lat - halfSize.lat
    const double cornerLat = _center.lat + _halfSize.lat * (latDiff > Ang::ZERO ? 1 : -1);

    // We then assigned the corner's longitude coordinate in a similar fashion
    const double cornerLon = _center.lon + _halfSize.lon * (lonDiff > Ang::ZERO ? 1 : -1);

    return Geodetic2(cornerLat, cornerLon);
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

    using Ang = Angle<double>;

    // Convert to Angles for normalization
    const Ang centerLat = Ang::fromRadians(_center.lat);
    const Ang centerLon = Ang::fromRadians(_center.lon);
    Ang pointLat = Ang::fromRadians(p.lat);
    Ang pointLon = Ang::fromRadians(p.lon);

    // Normalize point with respect to center. This is done because the point
    // will later be clamped. See LatLonPatch::clamp(const LatLon&) for explanation
    pointLat.normalizeAround(centerLat);
    pointLon.normalizeAround(centerLon);

    // Calculate the longitud difference between center and point. We normalize around
    // zero because we want the "shortest distance" difference, i.e the difference
    // should be in the interval [-180 deg, 180 deg]
    const Ang centerToPointLon = (centerLon - pointLon).normalizeAround(Ang::ZERO);

    // Calculate the longitudinal distance to the closest patch edge
    const Ang longitudeDistanceToClosestPatchEdge = centerToPointLon.abs() -
                                                    Ang::fromRadians(_halfSize.lon);

    // If the longitude distance to the closest patch edge is larger than 90 deg
    // the latitude will have to be clamped to its closest corner, as explained in
    // the example above.
    const double clampedLat =
        longitudeDistanceToClosestPatchEdge > Ang::QUARTER ?
        glm::clamp(
            (Ang::HALF - pointLat).normalizeAround(centerLat).asRadians(),
            minLat(),
            maxLat()) :
        glm::clamp(pointLat.asRadians(), minLat(), maxLat());

    // Longitude is just clamped normally
    const double clampedLon = glm::clamp(pointLon.asRadians(), minLon(), maxLon());

    return Geodetic2(clampedLat, clampedLon);
}

} // namespace openspace::globebrowsing
