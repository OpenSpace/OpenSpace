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

#ifndef __ELLIPSOID_H__
#define __ELLIPSOID_H__

#include <modules/globebrowsing/geometry/geodetic2.h>

namespace openspace {

    /**
    This class is based largely on the Ellipsoid class defined in the book
    "3D Engine Design for Virtual Globes". Most planets or planetary objects are better
    described using ellipsoids than spheres. All inputs and outputs to this class is
    in the WGS84 standard coordinate system where the x-axis points towards geographic
    (lat = 0, lon = 0), the y-axis points towards (lat = 0, lon = 90deg) and the
    z-axis points towards the north pole.
    */
class Ellipsoid {
public:

    Ellipsoid();

    /**
    \param radii defines three radii for the Ellipsoid
    */
    Ellipsoid(Vec3 radii);

    /**
    \param x defines the radius in x direction.
    \param y defines the radius in y direction.
    \param z defines the radius in z direction.
    */
    Ellipsoid(Scalar x, Scalar y, Scalar z);
    ~Ellipsoid();


    /**
    Scales a point along the geocentric normal and places it on the surface of the
    Ellipsoid.
    \param p is a point in the cartesian coordinate system to be placed on the surface
    of the Ellipsoid
    */
    Vec3 geocentricSurfaceProjection(const Vec3& p) const;


    /**
    Scales a point along the geodetic normal and places it on the surface of the
    Ellipsoid.
    \param p is a point in the cartesian coordinate system to be placed on the surface
    of the Ellipsoid
    */
    Vec3 geodeticSurfaceProjection(const Vec3& p) const;

    Vec3 geodeticSurfaceNormalForGeocentricallyProjectedPoint(const Vec3& p) const;
    Vec3 geodeticSurfaceNormal(Geodetic2 geodetic2) const;
    
    const Vec3& radii() const;
    const Vec3& radiiSquared() const;
    const Vec3& oneOverRadiiSquared() const;
    const Vec3& radiiToTheFourth() const;
    

    Scalar minimumRadius() const;
    Scalar maximumRadius() const;
    Scalar averageRadius() const;

    Geodetic2 cartesianToGeodetic2(const Vec3& p) const;
    Vec3 cartesianSurfacePosition(const Geodetic2& geodetic2) const;
    Vec3 cartesianPosition(const Geodetic3& geodetic3) const;

private:
    struct EllipsoidCache {
        Vec3 _radiiSquared;
        Vec3 _oneOverRadiiSquared;
        Vec3 _radiiToTheFourth;
        Scalar _minimumRadius;
        Scalar _maximumRadius;
        Scalar _medianRadius;
    } _cached;

    void updateInternalCache();

    Vec3 _radii;
};
} // namespace openspace

#endif // __ELLIPSOID_H__
