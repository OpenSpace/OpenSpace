/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <openspace/util/distanceconstants.h>
#include <openspace/util/distanceconversion.h>

using namespace openspace;

TEST_CASE("DistanceConversion: Convert to meters", "[distanceconversion]") {
    const double unit = 1.0;
    double res;

    res = convertDistance(unit, DistanceUnit::Nanometer, DistanceUnit::Meter);
    REQUIRE(res == Approx(1e-9));

    res = convertDistance(unit, DistanceUnit::Micrometer, DistanceUnit::Meter);
    REQUIRE(res == Approx(1e-6));

    res = convertDistance(unit, DistanceUnit::Millimeter, DistanceUnit::Meter);
    REQUIRE(res == Approx(1e-3));

    res = convertDistance(unit, DistanceUnit::Centimeter, DistanceUnit::Meter);
    REQUIRE(res == Approx(1e-2));

    res = convertDistance(unit, DistanceUnit::Decimeter, DistanceUnit::Meter);
    REQUIRE(res == Approx(1e-1));

    res = convertDistance(unit, DistanceUnit::Meter, DistanceUnit::Meter);
    REQUIRE(res == Approx(1.0));

    res = convertDistance(unit, DistanceUnit::Kilometer, DistanceUnit::Meter);
    REQUIRE(res == Approx(1000.0));

    res = convertDistance(unit, DistanceUnit::AU, DistanceUnit::Meter);
    REQUIRE(res == Approx(openspace::distanceconstants::AstronomicalUnit));

    res = convertDistance(unit, DistanceUnit::Lighthour, DistanceUnit::Meter);
    REQUIRE(res == Approx(openspace::distanceconstants::LightHour));

    res = convertDistance(unit, DistanceUnit::Lightday, DistanceUnit::Meter);
    REQUIRE(res == Approx(openspace::distanceconstants::LightDay));

    res = convertDistance(unit, DistanceUnit::Lightmonth, DistanceUnit::Meter);
    REQUIRE(res == Approx(openspace::distanceconstants::LightMonth));

    res = convertDistance(unit, DistanceUnit::Lightyear, DistanceUnit::Meter);
    REQUIRE(res == Approx(openspace::distanceconstants::LightYear));

    res = convertDistance(unit, DistanceUnit::Parsec, DistanceUnit::Meter);
    REQUIRE(res == Approx(openspace::distanceconstants::Parsec));

    res = convertDistance(unit, DistanceUnit::Kiloparsec, DistanceUnit::Meter);
    REQUIRE(res == Approx(1e3 * distanceconstants::Parsec));

    res = convertDistance(unit, DistanceUnit::Megaparsec, DistanceUnit::Meter);
    REQUIRE(res == Approx(1e6 * distanceconstants::Parsec));

    res = convertDistance(unit, DistanceUnit::Gigaparsec, DistanceUnit::Meter);
    REQUIRE(res == Approx(1e9 * distanceconstants::Parsec));

    res = convertDistance(unit, DistanceUnit::Thou, DistanceUnit::Meter);
    REQUIRE(res == Approx(1e-3 * distanceconstants::Inch));

    res = convertDistance(unit, DistanceUnit::Inch, DistanceUnit::Meter);
    REQUIRE(res == Approx(distanceconstants::Inch));

    res = convertDistance(unit, DistanceUnit::Foot, DistanceUnit::Meter);
    REQUIRE(res == Approx(distanceconstants::Foot));

    res = convertDistance(unit, DistanceUnit::Yard, DistanceUnit::Meter);
    REQUIRE(res == Approx(distanceconstants::Yard));

    res = convertDistance(unit, DistanceUnit::Chain, DistanceUnit::Meter);
    REQUIRE(res == Approx(distanceconstants::Chain));

    res = convertDistance(unit, DistanceUnit::Furlong, DistanceUnit::Meter);
    REQUIRE(res == Approx(10.0 * distanceconstants::Chain));

    res = convertDistance(unit, DistanceUnit::Mile, DistanceUnit::Meter);
    REQUIRE(res == Approx(distanceconstants::Mile));

    res = convertDistance(unit, DistanceUnit::League, DistanceUnit::Meter);
    REQUIRE(res == Approx(3.0 * distanceconstants::Mile));
}

TEST_CASE("DistanceConversion: Convert from meters", "[distanceconversion]") {
    const double meters = 1.0;
    double res;

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Nanometer);
    REQUIRE(res == Approx(meters / 1e-9));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Micrometer);
    REQUIRE(res == Approx(meters / 1e-6));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Millimeter);
    REQUIRE(res == Approx(meters / 1e-3));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Centimeter);
    REQUIRE(res == Approx(meters / 1e-2));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Decimeter);
    REQUIRE(res == Approx(meters / 1e-1));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Meter);
    REQUIRE(res == Approx(1.0));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Kilometer);
    REQUIRE(res == Approx(meters / 1000.0));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::AU);
    REQUIRE(res == Approx(meters / openspace::distanceconstants::AstronomicalUnit));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Lighthour);
    REQUIRE(res == Approx(meters / openspace::distanceconstants::LightHour));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Lightday);
    REQUIRE(res == Approx(meters / openspace::distanceconstants::LightDay));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Lightmonth);
    REQUIRE(res == Approx(meters / openspace::distanceconstants::LightMonth));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Lightyear);
    REQUIRE(res == Approx(meters / openspace::distanceconstants::LightYear));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Parsec);
    REQUIRE(res == Approx(meters / openspace::distanceconstants::Parsec));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Kiloparsec);
    REQUIRE(res == Approx(meters / (1e3 * distanceconstants::Parsec)));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Megaparsec);
    REQUIRE(res == Approx(meters / (1e6 * distanceconstants::Parsec)));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Gigaparsec);
    REQUIRE(res == Approx(meters / (1e9 * distanceconstants::Parsec)));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Thou);
    REQUIRE(res == Approx(meters / (1e-3 * distanceconstants::Inch)));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Inch);
    REQUIRE(res == Approx(meters / distanceconstants::Inch));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Foot);
    REQUIRE(res == Approx(meters / distanceconstants::Foot));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Yard);
    REQUIRE(res == Approx(meters / distanceconstants::Yard));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Chain);
    REQUIRE(res == Approx(meters / distanceconstants::Chain));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Furlong);
    REQUIRE(res == Approx(meters / (10.0 * distanceconstants::Chain)));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Mile);
    REQUIRE(res == Approx(meters / distanceconstants::Mile));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::League);
    REQUIRE(res == Approx(meters / (3.0 * distanceconstants::Mile)));
}


