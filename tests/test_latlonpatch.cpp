/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#include "catch2/catch.hpp"

#include <modules/globebrowsing/src/basictypes.h>
#include <modules/globebrowsing/src/geodeticpatch.h>
#include <ghoul/glm.h>

TEST_CASE("LatLonPatch: findCenterControlPoint", "[latlonpatch]") {
    using namespace openspace::globebrowsing;

    GeodeticPatch patch(0, 0, glm::pi<float>() / 4.f, glm::pi<float>() / 4.f);
}

TEST_CASE("LatLonPatch: Find Closest Corner", "[latlonpatch]") {
    using namespace openspace::globebrowsing;

    constexpr float piOver4 = glm::pi<float>() / 4.f;
    Geodetic2 halfSize { piOver4, piOver4 };
    Geodetic2 center { 0, 0 };
    GeodeticPatch patch(center, halfSize);

    constexpr float piOver3 = glm::pi<float>() / 3.f;
    Geodetic2 point { piOver3, piOver3 };

    Geodetic2 closestCorner = patch.closestCorner(point);
    Geodetic2 northEastCorner = patch.corner(NORTH_EAST);

    CHECK(closestCorner.lat == northEastCorner.lat);
    CHECK(closestCorner.lon == northEastCorner.lon);
}

TEST_CASE("LatLonPatch: Find Closest Corner 2", "[latlonpatch]") {
    using namespace openspace::globebrowsing;

    constexpr float piOver6 = glm::pi<float>() / 4.f;

    Geodetic2 halfSize { 1.1 * piOver6, 1.1 * piOver6 };
    Geodetic2 center { piOver6, piOver6 };
    GeodeticPatch patch(center, halfSize);

    Geodetic2 point { 0, 0 };

    Geodetic2 closestCorner = patch.closestCorner(point);
    Geodetic2 expectedCorner = patch.corner(SOUTH_WEST);

    CHECK(closestCorner.lat == expectedCorner.lat);
    CHECK(closestCorner.lon == expectedCorner.lon);
}
