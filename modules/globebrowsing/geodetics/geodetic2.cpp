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

#include <modules/globebrowsing/geodetics/geodetic2.h>
#include <modules/globebrowsing/geodetics/angle.h>
#include <modules/globebrowsing/geodetics/ellipsoid.h>

#include <ghoul/misc/assert.h>

#define _USE_MATH_DEFINES
#include <math.h>

namespace {
    const std::string _loggerCat = "Geodetic2";
}

namespace openspace {
    //////////////////////////////////////////////////////////////////////////////////////
    //								   GEODETIC2										//
    //////////////////////////////////////////////////////////////////////////////////////

    Geodetic2::Geodetic2()
        : lat(0)
        , lon(0)
    {}

    Geodetic2::Geodetic2(Scalar latitude, Scalar longitude)
        : lat(latitude)
        , lon(longitude) 
    {
    
    }

    Geodetic2::Geodetic2(const Geodetic2& p) 
        : Geodetic2(p.lat, p.lon) 
    {
    
    }


    Vec2 Geodetic2::toLonLatVec2() const {
        return Vec2(lon, lat);
    }

    bool Geodetic2::operator==(const Geodetic2& other) const {
        return lat == other.lat && lon == other.lon;
    }

    Geodetic2 Geodetic2::operator+(const Geodetic2& other) const {
        return Geodetic2(lat + other.lat, lon + other.lon);
    }

    Geodetic2 Geodetic2::operator-(const Geodetic2& other) const {
        return Geodetic2(lat - other.lat, lon - other.lon);
    }

    Geodetic2 Geodetic2::operator*(Scalar scalar) const {
        return Geodetic2(lat * scalar, lon * scalar);
    }

    Geodetic2 Geodetic2::operator/(Scalar scalar) const {
        return Geodetic2(lat / scalar, lon / scalar);
    }





    //////////////////////////////////////////////////////////////////////////////////////
    //									TILE INDEX										//
    //////////////////////////////////////////////////////////////////////////////////////

    HashKey GeodeticTileIndex::hashKey() const{
        return x ^ (y << 16) ^ (level << 21);
    }

    bool GeodeticTileIndex::operator==(const GeodeticTileIndex& other) const {
        return x == other.x && y == other.y && level == other.level;
    }

    //////////////////////////////////////////////////////////////////////////////////////
    //							 	GEODETICPATCH										//
    //////////////////////////////////////////////////////////////////////////////////////

    GeodeticPatch::GeodeticPatch(
        Scalar centerLat,
        Scalar centerLon,
        Scalar halfSizeLat,
        Scalar halfSizeLon)
        : _center(Geodetic2(centerLat, centerLon))
        , _halfSize(Geodetic2(halfSizeLat, halfSizeLon))
    {
    
    }

    GeodeticPatch::GeodeticPatch(
        const Geodetic2& center,
        const Geodetic2& halfSize)
        : _center(center)
        , _halfSize(halfSize)
    {
    
    }

    GeodeticPatch::GeodeticPatch(const GeodeticPatch& patch)
        : _center(patch._center)
        , _halfSize(patch._halfSize)
    {
    
    }


    GeodeticPatch::GeodeticPatch(const GeodeticTileIndex& tileIndex) {
        Scalar deltaLat = (2*M_PI) / ((double)(1 << tileIndex.level));
        Scalar deltaLon = (2*M_PI) / ((double)(1 << tileIndex.level));
        Geodetic2 nwCorner(M_PI / 2 - deltaLat * tileIndex.y, -M_PI + deltaLon * tileIndex.x);
        _halfSize = Geodetic2(deltaLat / 2, deltaLon / 2);
        _center = Geodetic2(nwCorner.lat - _halfSize.lat, nwCorner.lon + _halfSize.lon);
    }




    void GeodeticPatch::setCenter(const Geodetic2& center) {
        _center = center;
    }

    void GeodeticPatch::setHalfSize(const Geodetic2& halfSize) {
        _halfSize = halfSize;
    }

    Scalar GeodeticPatch::minimalBoundingRadius(const Ellipsoid& ellipsoid) const {
        // TODO: THIS FUNCTION IS CURRENTLY ERROR PRONE SINCE THE PATCH IS NOW COVERING
        // A PART OF AN ELLIPSOID AND NOT A SPHERE!MUST CHECK IF THIS FUNCTION IS STILL
        // VALID.
        const Geodetic2& cornerNearEquator = _center.lat > 0 ? southWestCorner() : northWestCorner();
        return glm::length(ellipsoid.geodetic2ToCartesian(_center) - ellipsoid.geodetic2ToCartesian(cornerNearEquator));
    }
    /*
    Scalar GeodeticPatch::unitArea() const {
        Scalar deltaTheta = 2 * _halfSize.lon;
        Scalar phiMin = _center.lat - _halfSize.lat;
        Scalar phiMax = _center.lat + _halfSize.lat;
        return deltaTheta * (sin(phiMax) - sin(phiMin));
    }
    */
    const Geodetic2& GeodeticPatch::center() const {
        return _center;
    }

    const Geodetic2& GeodeticPatch::halfSize() const {
        return _halfSize;
    }

    Geodetic2 GeodeticPatch::size() const {
        return Geodetic2(2 * _halfSize.lat, 2 * _halfSize.lon);
    }

    Geodetic2 GeodeticPatch::northWestCorner() const{
        return Geodetic2(_center.lat + _halfSize.lat, _center.lon - _halfSize.lon);
    }
    
    Geodetic2 GeodeticPatch::northEastCorner() const{
        return Geodetic2(_center.lat + _halfSize.lat, _center.lon + _halfSize.lon);
    }
    
    Geodetic2 GeodeticPatch::southWestCorner() const{
        return Geodetic2(_center.lat - _halfSize.lat, _center.lon - _halfSize.lon);
    }
    
    Geodetic2 GeodeticPatch::southEastCorner() const{
        return Geodetic2(_center.lat - _halfSize.lat, _center.lon + _halfSize.lon);
    }


    Geodetic2 GeodeticPatch::clamp(const Geodetic2& p) const {
        using Ang = Angle<Scalar>;

        // Convert to Angles for normalization
        Ang centerLat = Ang::fromRadians(_center.lat);
        Ang centerLon = Ang::fromRadians(_center.lon);
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

        // get clamp bounds
        Geodetic2 max = northEastCorner();
        Geodetic2 min = southWestCorner();

        return Geodetic2(
            glm::clamp(pointLat.asRadians(), min.lat, max.lat),
            glm::clamp(pointLon.asRadians(), min.lon, max.lon)
            );
    }


    Geodetic2 GeodeticPatch::closestCorner(const Geodetic2& p) const {
        using Ang = Angle<Scalar>;

        // LatLon vector from patch center to the point
        Geodetic2 centerToPoint = p - _center;
    
        // Normalize the difference angles to be centered around 0.
        Ang latDiff = Ang::fromRadians(centerToPoint.lat).normalizeAround(Ang::ZERO);
        Ang lonDiff = Ang::fromRadians(centerToPoint.lon).normalizeAround(Ang::ZERO);
        
        // If latDiff > 0 
        //    --> point p is north of the patch center 
        //    --> the closest corner to the point must be a northern one
        //    --> set the corner's latitude coordinate to center.lat + halfSize.lat
        // else 
        //    --> set corner's latidude coordinate to center.lat - halfSize.lat
        Scalar cornerLat = _center.lat + _halfSize.lat * (latDiff > Ang::ZERO ? 1 : -1);

        // We then assigned the corner's longitude coordinate in a similar fashion
        Scalar cornerLon = _center.lon + _halfSize.lon * (lonDiff > Ang::ZERO ? 1 : -1);

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
    

        using Ang = Angle<Scalar>;

        // Convert to Angles for normalization
        Ang centerLat = Ang::fromRadians(_center.lat);
        Ang centerLon = Ang::fromRadians(_center.lon);
        Ang pointLat = Ang::fromRadians(p.lat);
        Ang pointLon = Ang::fromRadians(p.lon);

        // Normalize point with respect to center. This is done because the point 
        // will later be clamped. See LatLonPatch::clamp(const LatLon&) for explanation
        pointLat.normalizeAround(centerLat);
        pointLon.normalizeAround(centerLon);

        // Calculate the longitud difference between center and point. We normalize around 
        // zero because we want the "shortest distance" difference, i.e the difference 
        // should be in the interval [-180 deg, 180 deg]
        Ang centerToPointLon = (centerLon - pointLon).normalizeAround(Ang::ZERO);

        // Calculate the longitudinal distance to the closest patch edge 
        Ang longitudeDistanceToClosestPatchEdge = centerToPointLon.abs() - Ang::fromRadians(_halfSize.lon);

        // get clamp bounds
        Geodetic2 max = northEastCorner();
        Geodetic2 min = southWestCorner();

        // If the longitude distance to the closest patch edge is larger than 90 deg
        // the latitude will have to be clamped to its closest corner, as explained in
        // the example above.
        Scalar clampedLat = longitudeDistanceToClosestPatchEdge > Ang::QUARTER  ?
            clampedLat = glm::clamp((Ang::HALF - pointLat).normalizeAround(centerLat).asRadians(), min.lat, max.lat) :
            clampedLat = glm::clamp(pointLat.asRadians(), min.lat, max.lat);

        // Longitude is just clamped normally
        Scalar clampedLon = glm::clamp(pointLon.asRadians(), min.lon, max.lon);

        return Geodetic2(clampedLat, clampedLon);
    }

} // namespace openspace
