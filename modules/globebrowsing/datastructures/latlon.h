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

#ifndef __LATLON_H__
#define __LATLON_H__

#include <glm/glm.hpp>
#include <vector>
#include <memory>
#include <ostream>


// Using double precision
typedef double Scalar;
typedef glm::dvec2 Vec2;
typedef glm::dvec3 Vec3;

namespace openspace {

struct LatLon {
	LatLon();
	LatLon(Scalar latitude, Scalar longitude);
	LatLon(const LatLon& src);
	
	static LatLon fromCartesian(const Vec3& v);
	Vec3 asUnitCartesian() const;
	Vec2 toLonLatVec2() const;

	inline bool operator==(const LatLon& other);
	inline bool operator!=(const LatLon& other) { return !(*this == (other)); }

	Scalar lat;
	Scalar lon;
};




class LatLonPatch {
public:
	LatLonPatch(Scalar, Scalar, Scalar, Scalar);
	LatLonPatch(const LatLon& center, const LatLon& halfSize);
	LatLonPatch(const LatLonPatch& patch);


	void setCenter(const LatLon&);
	void setHalfSize(const LatLon&);
	

	// Returns the minimal bounding radius that together with the LatLonPatch's
	// center point represents a sphere in which the patch is completely contained
	Scalar minimalBoundingRadius() const;

	// Returns the area of the patch with unit radius
	Scalar unitArea() const;


	LatLon northWestCorner() const;
	LatLon northEastCorner() const;
	LatLon southWestCorner() const;
	LatLon southEastCorner() const;

	const LatLon& center() const;
	const LatLon& halfSize() const;

private:
	LatLon _center;
	LatLon _halfSize;

};

} // namespace openspace

#endif // __LATLON_H__
