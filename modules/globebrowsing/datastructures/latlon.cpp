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

#include <ghoul/misc/assert.h>

#include <modules/globebrowsing/datastructures/chunknode.h>
#include <modules/globebrowsing/datastructures/latlon.h>

#include <modules/globebrowsing/rendering/chunklodglobe.h>
#include <modules/globebrowsing/util/converter.h>

namespace {
	const std::string _loggerCat = "LatLon";
}

namespace openspace {


	//////////////////////////////////////////////////////////////////////////////////////////
	//								   LATITUDE LONGITUDE									//
	//////////////////////////////////////////////////////////////////////////////////////////

	LatLon::LatLon(Scalar latitude, Scalar longitude)
		: lat(latitude)
		, lon(longitude) 
	{
	
	}

	LatLon::LatLon(const LatLon& p) 
		: LatLon(p.lat, p.lon) 
	{
	
	}

	
	LatLon LatLon::fromCartesian(const Vec3& v) {
		Scalar r = glm::length(v);
		return LatLon(glm::asin(v.z / r), atan2(v.y, v.x));
	}


	Vec3 LatLon::asUnitCartesian() {
		return Vec3(
			glm::cos(lat) * glm::cos(lon),
			glm::cos(lat) * glm::sin(lon),
			glm::sin(lat));
	}

	bool LatLon::operator==(const LatLon& other) {
		return lat == other.lat && lon == other.lon;
	}

	//////////////////////////////////////////////////////////////////////////////////////////
	//							 	LATITUDE LONGITUDE PATCH								//
	//////////////////////////////////////////////////////////////////////////////////////////

	LatLonPatch::LatLonPatch(Scalar centerLat, Scalar centerLon, Scalar halfSizeLat, Scalar halfSizeLon)
		: center(LatLon(centerLat, centerLon))
		, halfSize(LatLon(halfSizeLat, halfSizeLon)) 
	{
	
	}

	LatLonPatch::LatLonPatch(const LatLon& center, const LatLon& halfSize)
		: center(center)
		, halfSize(halfSize) 
	{
	
	}

	LatLonPatch::LatLonPatch(const LatLonPatch& patch)
		: center(patch.center)
		, halfSize(patch.halfSize) 
	{
	
	}


	Scalar LatLonPatch::unitArea() const {
		Scalar deltaTheta = 2 * halfSize.lon;
		Scalar phiMin = center.lat - halfSize.lat;
		Scalar phiMax = center.lat + halfSize.lat;
		return deltaTheta * (sin(phiMax) - sin(phiMin));
	}


	LatLon LatLonPatch::northWestCorner() const{
		return LatLon(center.lat + halfSize.lat, center.lon - halfSize.lon);
	}
	
	LatLon LatLonPatch::northEastCorner() const{
		return LatLon(center.lat + halfSize.lat, center.lon + halfSize.lon);
	}
	
	LatLon LatLonPatch::southWestCorner() const{
		return LatLon(center.lat - halfSize.lat, center.lon - halfSize.lon);
	}
	
	LatLon LatLonPatch::southEastCorner() const{
		return LatLon(center.lat - halfSize.lat, center.lon + halfSize.lon);
	}
	


	//////////////////////////////////////////////////////////////////////////////////////////
	//							CACHING LATITUDE LONGITUDE PATCH							//
	//////////////////////////////////////////////////////////////////////////////////////////

	CachingLatLonPatch::CachingLatLonPatch(const LatLon& center, const LatLon& halfSize) 
		: LatLonPatch(center, halfSize)
		, _hasCachedCartesianCenter(false)
		, _hasCachedCartesianHalfSize(false)
		, _hasCachedArea(false)
		, _hasCachedCartesianNorthWestCorner(false)
		, _hasCachedCartesianNorthEastCorner(false)
		, _hasCachedCartesianSouthWestCorner(false)
		, _hasCachedCartesianSouthEastCorner(false)
	{

	}

	CachingLatLonPatch::CachingLatLonPatch(const LatLonPatch& patch)
		: LatLonPatch(patch)
		, _hasCachedCartesianCenter(false)
		, _hasCachedCartesianHalfSize(false) 
		, _hasCachedArea(false)
		,_hasCachedCartesianNorthWestCorner(false)
		,_hasCachedCartesianNorthEastCorner(false)
		,_hasCachedCartesianSouthWestCorner(false)
		,_hasCachedCartesianSouthEastCorner(false)
	{ 
	
	}



	void CachingLatLonPatch::setCenter(const LatLon& newCenter) {
		if (center != newCenter) {
			_hasCachedCartesianCenter = false;
			_hasCachedArea = false;
			center = newCenter;
		}
	}

	void CachingLatLonPatch::setHalfSize(const LatLon& newHalfSize) {
		if (halfSize != newHalfSize) {
			_hasCachedCartesianHalfSize = false;
			_hasCachedArea = false;
			halfSize = newHalfSize;
		}
	}



	const LatLon& CachingLatLonPatch::getCenter() const {
		return center;
	}

	const LatLon& CachingLatLonPatch::getHalfSize() const {
		return halfSize;
	}



	const Vec3& CachingLatLonPatch::cartesianUnitCenter() {
		if (!_hasCachedCartesianCenter) {
			_cachedCartesianCenter = center.asUnitCartesian();
			_hasCachedCartesianCenter = true;
		}
		return _cachedCartesianCenter;
	}

	const Vec3& CachingLatLonPatch::cartesianUnitHalfSize() {
		if (!_hasCachedCartesianHalfSize) {
			_cachedCartesianHalfSize = halfSize.asUnitCartesian();
			_hasCachedCartesianHalfSize = true;
		}
		return _cachedCartesianHalfSize;
	}

	Scalar CachingLatLonPatch::unitArea() {
		if (!_hasCachedArea) {
			_hasCachedArea = true;
			_cachedArea = LatLonPatch::unitArea();
		}
		return _cachedArea;
	}


	LatLon CachingLatLonPatch::northWestCorner() const{
		return LatLonPatch::northWestCorner();
	}

	LatLon CachingLatLonPatch::northEastCorner() const{
		return LatLonPatch::northEastCorner();
	}

	LatLon CachingLatLonPatch::southWestCorner() const{
		return LatLonPatch::southWestCorner();
	}

	LatLon CachingLatLonPatch::southEastCorner() const{
		return LatLonPatch::southEastCorner();
	}





	const Vec3& CachingLatLonPatch::cartesianUnitNorthWestCorner() {
		if(!_hasCachedCartesianNorthWestCorner){
			_cachedCartesianNorthWestCorner = LatLonPatch::northWestCorner().asUnitCartesian();
			_hasCachedCartesianNorthWestCorner = true;
		}
		return _cachedCartesianNorthWestCorner;
	}

	const Vec3& CachingLatLonPatch::cartesianUnitNorthEastCorner() {
		if(!_hasCachedCartesianNorthEastCorner){
			_cachedCartesianNorthEastCorner = LatLonPatch::northEastCorner().asUnitCartesian();
			_hasCachedCartesianNorthEastCorner = true;
		}
		return _cachedCartesianNorthEastCorner;
	}

	const Vec3& CachingLatLonPatch::cartesianUnitSouthWestCorner() {
		if(!_hasCachedCartesianSouthWestCorner){
			_cachedCartesianSouthWestCorner = LatLonPatch::southWestCorner().asUnitCartesian();
			_hasCachedCartesianSouthWestCorner = true;
		}
		return _cachedCartesianSouthWestCorner;
	}

	const Vec3& CachingLatLonPatch::cartesianUnitSouthEastCorner() {
		if(!_hasCachedCartesianSouthEastCorner){
			_cachedCartesianSouthEastCorner = LatLonPatch::southEastCorner().asUnitCartesian();
			_hasCachedCartesianSouthEastCorner = true;
		}
		return _cachedCartesianSouthEastCorner;
	}



} // namespace openspace
