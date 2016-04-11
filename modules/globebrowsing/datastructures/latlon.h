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
	LatLon(Scalar latitude, Scalar longitude);
	LatLon(const LatLon& src);
	
	static LatLon fromCartesian(const Vec3& v);
	Vec3 asUnitCartesian();

	inline bool operator==(const LatLon& other);
	inline bool operator!=(const LatLon& other) { return !(*this == (other)); }

	Scalar lat;
	Scalar lon;
};





struct LatLonPatch {

	LatLonPatch(Scalar, Scalar, Scalar, Scalar);
	LatLonPatch(const LatLon& center, const LatLon& halfSize);
	LatLonPatch(const LatLonPatch& patch);

	Scalar unitArea() const;

	LatLon northWestCorner() const;
	LatLon northEastCorner() const;
	LatLon southWestCorner() const;
	LatLon southEastCorner() const;

	LatLon center;
	LatLon halfSize;

};



class CachingLatLonPatch : private LatLonPatch {

public:

	CachingLatLonPatch(const LatLon& center, const LatLon& halfSize);
	CachingLatLonPatch(const LatLonPatch& patch);


	void setCenter(const LatLon&);
	void setHalfSize(const LatLon&);

	const LatLon& getCenter() const;
	const LatLon& getHalfSize() const;

	Scalar unitArea();


	const Vec3& cartesianUnitCenter();
	const Vec3& cartesianUnitHalfSize();

	LatLon northWestCorner() const;
	LatLon northEastCorner() const;
	LatLon southWestCorner() const;
	LatLon southEastCorner() const;

	const Vec3& CachingLatLonPatch::cartesianUnitNorthWestCorner();
	const Vec3& CachingLatLonPatch::cartesianUnitNorthEastCorner();
	const Vec3& CachingLatLonPatch::cartesianUnitSouthWestCorner();
	const Vec3& CachingLatLonPatch::cartesianUnitSouthEastCorner();

	
private:
	bool _hasCachedCartesianCenter;
	bool _hasCachedCartesianHalfSize;
	bool _hasCachedArea;

	bool _hasCachedCartesianNorthWestCorner;
	bool _hasCachedCartesianNorthEastCorner;
	bool _hasCachedCartesianSouthWestCorner;
	bool _hasCachedCartesianSouthEastCorner;

	Vec3 _cachedCartesianCenter;
	Vec3 _cachedCartesianHalfSize;
	Scalar _cachedArea;

	Vec3 _cachedCartesianNorthWestCorner;
	Vec3 _cachedCartesianNorthEastCorner;
	Vec3 _cachedCartesianSouthWestCorner;
	Vec3 _cachedCartesianSouthEastCorner;


};



} // namespace openspace



#endif // __LATLON_H__
