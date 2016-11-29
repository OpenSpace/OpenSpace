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

#ifndef __GEODETIC2_H__
#define __GEODETIC2_H__

#include <glm/glm.hpp>
#include <vector>
#include <memory>
#include <ostream>

#define _USE_MATH_DEFINES
#include <math.h>

#include <modules/globebrowsing/tile/tileindex.h>

#include <ghoul/misc/assert.h>

// Using double precision
typedef double Scalar;
typedef glm::dvec2 Vec2;
typedef glm::dvec3 Vec3;

namespace openspace {
namespace globebrowsing {

// Forward declaration
class Ellipsoid;

struct Geodetic2 {
    Geodetic2();
    Geodetic2(Scalar latitude, Scalar longitude);
    Geodetic2(const Geodetic2& src);
    
    //static Geodetic2 fromCartesian(const Vec3& v);
    //Vec3 asUnitCartesian() const;

    Vec2 toLonLatVec2() const;

    bool operator==(const Geodetic2& other) const;
    bool operator!=(const Geodetic2& other) const { return !(*this == (other)); }

    Geodetic2 operator+(const Geodetic2& other) const;
    Geodetic2 operator-(const Geodetic2& other) const;
    Geodetic2 operator*(Scalar scalar) const;
    Geodetic2 operator/(Scalar scalar) const;

    Scalar lat;
    Scalar lon;
};

struct Geodetic3 {
    Geodetic2 geodetic2;
    Scalar height;
};

//////////////////////////////////////////////////////////////////////////////////////
//                                 GEODETICPATCH                                        //
//////////////////////////////////////////////////////////////////////////////////////
class GeodeticPatch {
public:
    GeodeticPatch(
        Scalar centerLat,
        Scalar centerLon,
        Scalar halfSizeLat,
        Scalar halfSizeLon);

    GeodeticPatch(
        const Geodetic2& center,
        const Geodetic2& halfSize);

    GeodeticPatch(const GeodeticPatch& patch);

    GeodeticPatch(const TileIndex& tileIndex);

    void setCenter(const Geodetic2&);
    void setHalfSize(const Geodetic2&);    

    /**
        returns the latitude boundary which is closest to the equator
    */
    Scalar edgeLatitudeNearestEquator() const;

    /**
        Returns true if the center above the equator
    */
    Scalar isNorthern() const;

    Geodetic2 getCorner(Quad q) const;
    Geodetic2 getSize() const;

    Scalar minLat() const;
    Scalar maxLat() const;
    Scalar minLon() const;
    Scalar maxLon() const;

    /**
     * returns true if the specified coordinate is contained within the patch
     */
    bool contains(const Geodetic2& p) const;


    /**
     * Clamps a point to the patch region
     */
    Geodetic2 clamp(const Geodetic2& p) const;

    /**
     * Returns the corner of the patch that is closest to the given point p
     */
    Geodetic2 closestCorner(const Geodetic2& p) const;

    /**
     * Returns a point on the patch that minimizes the great-circle distance to
     * the given point p.
     */
    Geodetic2 closestPoint(const Geodetic2& p) const;

    /**
     * Returns the minimum tile level of the patch (based on largest side)
     */
    Scalar minimumTileLevel() const;

    /**
    * Returns the maximum level of the patch (based on smallest side)
    */
    Scalar maximumTileLevel() const;

    const Geodetic2& center() const;
    const Geodetic2& halfSize() const;
    Geodetic2 size() const;

private:
    Geodetic2 _center;
    Geodetic2 _halfSize;
};

} // namespace globebrowsing
} // namespace openspace

#endif // __GEODETIC2_H__
