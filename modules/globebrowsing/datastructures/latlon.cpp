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
#include <modules/globebrowsing/datastructures/angle.h>

#define _USE_MATH_DEFINES
#include <math.h>

namespace {
	const std::string _loggerCat = "LatLon";
}

namespace openspace {
	//////////////////////////////////////////////////////////////////////////////////////////
	//								   LATITUDE LONGITUDE									//
	//////////////////////////////////////////////////////////////////////////////////////////

	LatLon::LatLon()
		: lat(0)
		, lon(0)
	{}

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


	Vec3 LatLon::asUnitCartesian() const{
		return Vec3(
			glm::cos(lat) * glm::cos(lon),
			glm::cos(lat) * glm::sin(lon),
			glm::sin(lat));
	}

	Vec2 LatLon::toLonLatVec2() const {
		return Vec2(lon, lat);
	}

	bool LatLon::operator==(const LatLon& other) const {
		return lat == other.lat && lon == other.lon;
	}

	LatLon LatLon ::operator+(const LatLon& other) const {
		return LatLon(lat - other.lat, lon - other.lon);
	}

	LatLon LatLon ::operator-(const LatLon& other) const {
		return LatLon(lat - other.lat, lon - other.lon);
	}

	LatLon LatLon::operator*(Scalar scalar) const {
		return LatLon(lat * scalar, lon * scalar);
	}

	LatLon LatLon::operator/(Scalar scalar) const {
		return LatLon(lat / scalar, lon / scalar);
	}


	//////////////////////////////////////////////////////////////////////////////////////////
	//							 	LATITUDE LONGITUDE PATCH								//
	//////////////////////////////////////////////////////////////////////////////////////////

	LatLonPatch::LatLonPatch(Scalar centerLat, Scalar centerLon, Scalar halfSizeLat, Scalar halfSizeLon)
		: _center(LatLon(centerLat, centerLon))
		, _halfSize(LatLon(halfSizeLat, halfSizeLon)) 
	{
	
	}

	LatLonPatch::LatLonPatch(const LatLon& center, const LatLon& halfSize)
		: _center(center)
		, _halfSize(halfSize) 
	{
	
	}

	LatLonPatch::LatLonPatch(const LatLonPatch& patch)
		: _center(patch._center)
		, _halfSize(patch._halfSize) 
	{
	
	}


	void LatLonPatch::setCenter(const LatLon& center) {
		_center = center;
	}

	void LatLonPatch::setHalfSize(const LatLon& halfSize) {
		_halfSize = halfSize;
	}

	Scalar LatLonPatch::minimalBoundingRadius() const {
		const LatLon& cornerNearEquator = _center.lat > 0 ? southWestCorner() : northWestCorner();
		return glm::length(_center.asUnitCartesian() - cornerNearEquator.asUnitCartesian());
	}

	Scalar LatLonPatch::unitArea() const {
		Scalar deltaTheta = 2 * _halfSize.lon;
		Scalar phiMin = _center.lat - _halfSize.lat;
		Scalar phiMax = _center.lat + _halfSize.lat;
		return deltaTheta * (sin(phiMax) - sin(phiMin));
	}

	const LatLon& LatLonPatch::center() const {
		return _center;
	}

	const LatLon& LatLonPatch::halfSize() const {
		return _halfSize;
	}

	LatLon LatLonPatch::size() const {
		return LatLon(2 * _halfSize.lat, 2 * _halfSize.lon);
	}

	LatLon LatLonPatch::northWestCorner() const{
		return LatLon(_center.lat + _halfSize.lat, _center.lon - _halfSize.lon);
	}
	
	LatLon LatLonPatch::northEastCorner() const{
		return LatLon(_center.lat + _halfSize.lat, _center.lon + _halfSize.lon);
	}
	
	LatLon LatLonPatch::southWestCorner() const{
		return LatLon(_center.lat - _halfSize.lat, _center.lon - _halfSize.lon);
	}
	
	LatLon LatLonPatch::southEastCorner() const{
		return LatLon(_center.lat - _halfSize.lat, _center.lon + _halfSize.lon);
	}


	LatLon LatLonPatch::clamp(const LatLon& p) const {
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
		LatLon max = northEastCorner();
		LatLon min = southWestCorner();

		return LatLon(
			glm::clamp(pointLat.asRadians(), min.lat, max.lat),
			glm::clamp(pointLon.asRadians(), min.lon, max.lon)
			);
	}


	LatLon LatLonPatch::closestCorner(const LatLon& p) const {
		using Ang = Angle<Scalar>;

		// LatLon vector from patch center to the point
		LatLon centerToPoint = p - _center;
	
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

		return LatLon(cornerLat, cornerLon);
	}

	
	LatLon LatLonPatch::closestPoint(const LatLon& p) const {
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
		LatLon max = northEastCorner();
		LatLon min = southWestCorner();

		// If the longitude distance to the closest patch edge is larger than 90 deg
		// the latitude will have to be clamped to its closest corner, as explained in
		// the example above.
		Scalar clampedLat = longitudeDistanceToClosestPatchEdge > Ang::QUARTER  ?
			clampedLat = glm::clamp((Ang::HALF - pointLat).normalizeAround(centerLat).asRadians(), min.lat, max.lat) :
			clampedLat = glm::clamp(pointLat.asRadians(), min.lat, max.lat);

		// Longitude is just clamped normally
		Scalar clampedLon = glm::clamp(pointLon.asRadians(), min.lon, max.lon);

		return LatLon(clampedLat, clampedLon);
	}

} // namespace openspace
