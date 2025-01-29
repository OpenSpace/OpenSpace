/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>

#include <openspace/util/distanceconstants.h>
#include <openspace/util/distanceconversion.h>

using namespace openspace;

TEST_CASE("DistanceConversion: Convert to meters", "[distanceconversion]") {
    constexpr double Unit = 1.0;
    double res = 0.0;

    res = convertDistance(Unit, DistanceUnit::Nanometer, DistanceUnit::Meter);
    CHECK(res == Catch::Approx(1e-9));

    res = convertDistance(Unit, DistanceUnit::Micrometer, DistanceUnit::Meter);
    CHECK(res == Catch::Approx(1e-6));

    res = convertDistance(Unit, DistanceUnit::Millimeter, DistanceUnit::Meter);
    CHECK(res == Catch::Approx(1e-3));

    res = convertDistance(Unit, DistanceUnit::Centimeter, DistanceUnit::Meter);
    CHECK(res == Catch::Approx(1e-2));

    res = convertDistance(Unit, DistanceUnit::Decimeter, DistanceUnit::Meter);
    CHECK(res == Catch::Approx(1e-1));

    res = convertDistance(Unit, DistanceUnit::Meter, DistanceUnit::Meter);
    CHECK(res == Catch::Approx(1.0));

    res = convertDistance(Unit, DistanceUnit::Kilometer, DistanceUnit::Meter);
    CHECK(res == Catch::Approx(1000.0));

    res = convertDistance(Unit, DistanceUnit::AU, DistanceUnit::Meter);
    CHECK(res == Catch::Approx(1.495978707E11));

    res = convertDistance(Unit, DistanceUnit::Lighthour, DistanceUnit::Meter);
    CHECK(res == Catch::Approx(1.0799921E12));

    res = convertDistance(Unit, DistanceUnit::Lightday, DistanceUnit::Meter);
    CHECK(res == Catch::Approx(2.591981E13));

    res = convertDistance(Unit, DistanceUnit::Lightmonth, DistanceUnit::Meter);
    CHECK(res == Catch::Approx(7.8839421E14));

    res = convertDistance(Unit, DistanceUnit::Lightyear, DistanceUnit::Meter);
    CHECK(res == Catch::Approx(9.4607304725808E15));

    res = convertDistance(Unit, DistanceUnit::Parsec, DistanceUnit::Meter);
    CHECK(res == Catch::Approx(3.0856776E16));

    res = convertDistance(Unit, DistanceUnit::Kiloparsec, DistanceUnit::Meter);
    CHECK(res == Catch::Approx(1e3 * 3.0856776E16));

    res = convertDistance(Unit, DistanceUnit::Megaparsec, DistanceUnit::Meter);
    CHECK(res == Catch::Approx(1e6 * 3.0856776E16));

    res = convertDistance(Unit, DistanceUnit::Gigaparsec, DistanceUnit::Meter);
    CHECK(res == Catch::Approx(1e9 * 3.0856776E16));

    res = convertDistance(Unit, DistanceUnit::Thou, DistanceUnit::Meter);
    CHECK(res == Catch::Approx(1e-3 * 0.0254));

    res = convertDistance(Unit, DistanceUnit::Inch, DistanceUnit::Meter);
    CHECK(res == Catch::Approx(0.0254));

    res = convertDistance(Unit, DistanceUnit::Foot, DistanceUnit::Meter);
    CHECK(res == Catch::Approx(0.3048));

    res = convertDistance(Unit, DistanceUnit::Yard, DistanceUnit::Meter);
    CHECK(res == Catch::Approx(0.9144));

    res = convertDistance(Unit, DistanceUnit::Chain, DistanceUnit::Meter);
    CHECK(res == Catch::Approx(20.1168));

    res = convertDistance(Unit, DistanceUnit::Furlong, DistanceUnit::Meter);
    CHECK(res == Catch::Approx(10.0 * 20.1168));

    res = convertDistance(Unit, DistanceUnit::Mile, DistanceUnit::Meter);
    CHECK(res == Catch::Approx(1609.344));

    res = convertDistance(Unit, DistanceUnit::League, DistanceUnit::Meter);
    CHECK(res == Catch::Approx(3.0 * 1609.344));
}

TEST_CASE("DistanceConversion: Convert from meters", "[distanceconversion]") {
    constexpr double Meters = 1.0;
    double res = 0.0;

    res = convertDistance(Meters, DistanceUnit::Meter, DistanceUnit::Nanometer);
    CHECK(res == Catch::Approx(Meters / 1e-9));

    res = convertDistance(Meters, DistanceUnit::Meter, DistanceUnit::Micrometer);
    CHECK(res == Catch::Approx(Meters / 1e-6));

    res = convertDistance(Meters, DistanceUnit::Meter, DistanceUnit::Millimeter);
    CHECK(res == Catch::Approx(Meters / 1e-3));

    res = convertDistance(Meters, DistanceUnit::Meter, DistanceUnit::Centimeter);
    CHECK(res == Catch::Approx(Meters / 1e-2));

    res = convertDistance(Meters, DistanceUnit::Meter, DistanceUnit::Decimeter);
    CHECK(res == Catch::Approx(Meters / 1e-1));

    res = convertDistance(Meters, DistanceUnit::Meter, DistanceUnit::Meter);
    CHECK(res == Catch::Approx(1.0));

    res = convertDistance(Meters, DistanceUnit::Meter, DistanceUnit::Kilometer);
    CHECK(res == Catch::Approx(Meters / 1000.0));

    res = convertDistance(Meters, DistanceUnit::Meter, DistanceUnit::AU);
    CHECK(res == Catch::Approx(Meters / 1.495978707E11));

    res = convertDistance(Meters, DistanceUnit::Meter, DistanceUnit::Lighthour);
    CHECK(res == Catch::Approx(Meters / 1.0799921E12));

    res = convertDistance(Meters, DistanceUnit::Meter, DistanceUnit::Lightday);
    CHECK(res == Catch::Approx(Meters / 2.591981E13));

    res = convertDistance(Meters, DistanceUnit::Meter, DistanceUnit::Lightmonth);
    CHECK(res == Catch::Approx(Meters / 7.8839421E14));

    res = convertDistance(Meters, DistanceUnit::Meter, DistanceUnit::Lightyear);
    CHECK(res == Catch::Approx(Meters / 9.4607304725808E15));

    res = convertDistance(Meters, DistanceUnit::Meter, DistanceUnit::Parsec);
    CHECK(res == Catch::Approx(Meters / 3.0856776E16));

    res = convertDistance(Meters, DistanceUnit::Meter, DistanceUnit::Kiloparsec);
    CHECK(res == Catch::Approx(Meters / (1e3 * 3.0856776E16)));

    res = convertDistance(Meters, DistanceUnit::Meter, DistanceUnit::Megaparsec);
    CHECK(res == Catch::Approx(Meters / (1e6 * 3.0856776E16)));

    res = convertDistance(Meters, DistanceUnit::Meter, DistanceUnit::Gigaparsec);
    CHECK(res == Catch::Approx(Meters / (1e9 * 3.0856776E16)));

    res = convertDistance(Meters, DistanceUnit::Meter, DistanceUnit::Thou);
    CHECK(res == Catch::Approx(Meters / (1e-3 * 0.0254)));

    res = convertDistance(Meters, DistanceUnit::Meter, DistanceUnit::Inch);
    CHECK(res == Catch::Approx(Meters / 0.0254));

    res = convertDistance(Meters, DistanceUnit::Meter, DistanceUnit::Foot);
    CHECK(res == Catch::Approx(Meters / 0.3048));

    res = convertDistance(Meters, DistanceUnit::Meter, DistanceUnit::Yard);
    CHECK(res == Catch::Approx(Meters / 0.9144));

    res = convertDistance(Meters, DistanceUnit::Meter, DistanceUnit::Chain);
    CHECK(res == Catch::Approx(Meters / 20.1168));

    res = convertDistance(Meters, DistanceUnit::Meter, DistanceUnit::Furlong);
    CHECK(res == Catch::Approx(Meters / (10.0 * 20.1168)));

    res = convertDistance(Meters, DistanceUnit::Meter, DistanceUnit::Mile);
    CHECK(res == Catch::Approx(Meters / 1609.344));

    res = convertDistance(Meters, DistanceUnit::Meter, DistanceUnit::League);
    CHECK(res == Catch::Approx(Meters / (3.0 * 1609.344)));
}

TEST_CASE("DistanceConversion: Cross conversion", "[distanceconversion]") {
    constexpr double Unit = 1.0;
    double res = 0.0;

    res = convertDistance(Unit, DistanceUnit::Nanometer, DistanceUnit::Kilometer);
    CHECK(res == Catch::Approx(1e-12));

    res = convertDistance(Unit, DistanceUnit::Micrometer, DistanceUnit::Decimeter);
    CHECK(res == Catch::Approx(1e-5));

    res = convertDistance(Unit, DistanceUnit::Millimeter, DistanceUnit::Nanometer);
    CHECK(res == Catch::Approx(1e6));

    res = convertDistance(Unit, DistanceUnit::Centimeter, DistanceUnit::Micrometer);
    CHECK(res == Catch::Approx(1e4));

    res = convertDistance(Unit, DistanceUnit::Decimeter, DistanceUnit::Millimeter);
    CHECK(res == Catch::Approx(1e2));

    res = convertDistance(Unit, DistanceUnit::Kilometer, DistanceUnit::Centimeter);
    CHECK(res == Catch::Approx(1e5));

    res = convertDistance(Unit, DistanceUnit::AU, DistanceUnit::Parsec);
    CHECK(res == Catch::Approx(4.84813681e-6));

    res = convertDistance(Unit, DistanceUnit::Lighthour, DistanceUnit::Lightmonth);
    CHECK(res == Catch::Approx(1.36986305e-3));

    res = convertDistance(Unit, DistanceUnit::Lightday, DistanceUnit::Kiloparsec);
    CHECK(res == Catch::Approx(8.40003829e-7));

    res = convertDistance(Unit, DistanceUnit::Lightmonth, DistanceUnit::Lightday);
    CHECK(res == Catch::Approx(30.4166662487));

    res = convertDistance(Unit, DistanceUnit::Lightyear, DistanceUnit::Gigaparsec);
    CHECK(res == Catch::Approx(3.0660139e-10));

    res = convertDistance(Unit, DistanceUnit::Parsec, DistanceUnit::Lightyear);
    CHECK(res == Catch::Approx(3.26156379673));

    res = convertDistance(Unit, DistanceUnit::Kiloparsec, DistanceUnit::AU);
    CHECK(res == Catch::Approx(2.06264806E8));

    res = convertDistance(Unit, DistanceUnit::Megaparsec, DistanceUnit::Lighthour);
    CHECK(res == Catch::Approx(2.85712978826E10));

    res = convertDistance(Unit, DistanceUnit::Gigaparsec, DistanceUnit::Megaparsec);
    CHECK(res == Catch::Approx(1e3));

    res = convertDistance(Unit, DistanceUnit::Thou, DistanceUnit::Yard);
    CHECK(res == Catch::Approx(2.77777778e-5));

    res = convertDistance(Unit, DistanceUnit::Inch, DistanceUnit::Foot);
    CHECK(res == Catch::Approx(8.33333333e-2));

    res = convertDistance(Unit, DistanceUnit::Foot, DistanceUnit::Mile);
    CHECK(res == Catch::Approx(1.89393939e-4));

    res = convertDistance(Unit, DistanceUnit::Yard, DistanceUnit::Chain);
    CHECK(res == Catch::Approx(4.54545455e-2));

    res = convertDistance(Unit, DistanceUnit::Chain, DistanceUnit::League);
    CHECK(res == Catch::Approx(4.16666666e-3));

    res = convertDistance(Unit, DistanceUnit::Furlong, DistanceUnit::Thou);
    CHECK(res == Catch::Approx(7.92E6));

    res = convertDistance(Unit, DistanceUnit::Mile, DistanceUnit::Inch);
    CHECK(res == Catch::Approx(6.3360E4));

    res = convertDistance(Unit, DistanceUnit::League, DistanceUnit::Furlong);
    CHECK(res == Catch::Approx(24.0));
}
