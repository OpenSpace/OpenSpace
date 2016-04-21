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

#include "gtest/gtest.h"

#include <modules/globebrowsing/datastructures/latlon.h>

#define _USE_MATH_DEFINES
#include <math.h>
#include <glm/glm.hpp>

class LatLonPatchTest : public testing::Test {};

using namespace openspace;

TEST_F(LatLonPatchTest, findCenterControlPoint) {

	LatLonPatch patch(0, 0, M_PI / 4, M_PI / 4);

}



TEST_F(LatLonPatchTest, TestFindClosestCorner) {

	Scalar piOver4 = M_PI / 4;
	LatLon halfSize(piOver4, piOver4);
	LatLon center(0, 0);
	LatLonPatch patch(center, halfSize);

	Scalar piOver3 = M_PI / 3;
	LatLon point(piOver3, piOver3);

	LatLon closestCorner = patch.closestCorner(point);
	LatLon northEastCorner = patch.northEastCorner();

	ASSERT_EQ(closestCorner.lat, northEastCorner.lat);
	ASSERT_EQ(closestCorner.lon, northEastCorner.lon);

}

TEST_F(LatLonPatchTest, TestFindClosestCorner2) {

	Scalar piOver6 = M_PI / 4;
	Scalar piOver3 = M_PI / 3;

	LatLon halfSize(1.1*piOver6, 1.1*piOver6);
	LatLon center(piOver6, piOver6);
	LatLonPatch patch(center, halfSize);

	LatLon point(0, 0);

	LatLon closestCorner = patch.closestCorner(point);
	LatLon expectedCorner = patch.southWestCorner();

	ASSERT_EQ(closestCorner.lat, expectedCorner.lat);
	ASSERT_EQ(closestCorner.lon, expectedCorner.lon);

}


TEST_F(LatLonPatchTest, TestSphericalClamp1) {
	LatLonPatch patch(0, 0, M_PI / 4, M_PI / 4);

	// inside patch latitude-wise, east of patch longitude-wise
	LatLon point(M_PI / 6, M_PI - 0.01);


	LatLon clampedPoint = patch.closestPoint(point);
	LatLon neCorner = patch.northEastCorner();
	ASSERT_EQ(clampedPoint.lat, neCorner.lat);
	ASSERT_EQ(clampedPoint.lon, neCorner.lon);
}


TEST_F(LatLonPatchTest, TestSphericalClamp2) {
	LatLonPatch patch(0, 0, M_PI / 4, M_PI / 4);

	// inside patch latitude-wise, west of patch longitude-wise
	LatLon point(M_PI / 6, M_PI + 0.01);

	LatLon clampedPoint = patch.closestPoint(point);
	LatLon nwCorner = patch.northWestCorner();
	ASSERT_EQ(clampedPoint.lat, nwCorner.lat);
	ASSERT_EQ(clampedPoint.lon, nwCorner.lon);
}

TEST_F(LatLonPatchTest, TestSphericalClamp3) {
	LatLonPatch patch(0, 0, M_PI / 4, M_PI / 4);

	// North east of patch
	LatLon point(M_PI / 3, M_PI - 0.01);

	LatLon clampedPoint = patch.closestPoint(point);
	LatLon neCorner = patch.northEastCorner();
	ASSERT_EQ(clampedPoint.lat, neCorner.lat);
	ASSERT_EQ(clampedPoint.lon, neCorner.lon);
}

TEST_F(LatLonPatchTest, TestSphericalClamp4) {
	LatLonPatch patch(0, 0, M_PI / 4, M_PI / 4);

	// South east of patch
	LatLon point(-M_PI / 3, M_PI - 0.01);

	LatLon clampedPoint = patch.closestPoint(point);
	LatLon seCorner = patch.southEastCorner();
	ASSERT_EQ(clampedPoint.lat, seCorner.lat);
	ASSERT_EQ(clampedPoint.lon, seCorner.lon);
}


TEST_F(LatLonPatchTest, TestSphericalClamp5) {
	LatLonPatch patch(0, 0, M_PI / 4, M_PI / 4);

	// South west of patch
	LatLon point(-M_PI / 3, 3*M_PI + 0.01);

	LatLon clampedPoint = patch.closestPoint(point);
	LatLon swCorner = patch.southWestCorner();
	ASSERT_EQ(clampedPoint.lat, swCorner.lat);
	ASSERT_EQ(clampedPoint.lon, swCorner.lon);
}

int radAsDeg(double rads) {
	return floorf(Angle<Scalar>::fromRadians(rads).asDegrees());
}

TEST_F(LatLonPatchTest, PrintingSphericalClamp) {
	LatLonPatch patch(0, 0, M_PI / 4, M_PI / 4);

	using Ang = Angle<Scalar>;
	Ang delta = Ang::fromDegrees(30);
	std::cout << "point lat, lon  -->  clamped lat, lon" << std::endl;
	for (Ang lat = Ang::fromDegrees(90); lat > -Ang::QUARTER; lat -= delta) {
		for (Ang lon = Ang::fromDegrees(180); lon > -Ang::HALF; lon -= delta) {

			LatLon point(lat.asRadians(), lon.asRadians());
			LatLon clamped = patch.closestPoint(point);
			std::cout 
				<< radAsDeg(point.lat) << ", " 
				<< radAsDeg(point.lon) << "  -->  " 
				<< radAsDeg(clamped.lat) << ", " 
				<< radAsDeg(clamped.lon) << std::endl;
		}
		std::cout << std::endl;
	}
}
