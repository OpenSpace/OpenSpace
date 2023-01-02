/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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
    CHECK(res == Approx(1e-9));

    res = convertDistance(unit, DistanceUnit::Micrometer, DistanceUnit::Meter);
    CHECK(res == Approx(1e-6));

    res = convertDistance(unit, DistanceUnit::Millimeter, DistanceUnit::Meter);
    CHECK(res == Approx(1e-3));

    res = convertDistance(unit, DistanceUnit::Centimeter, DistanceUnit::Meter);
    CHECK(res == Approx(1e-2));

    res = convertDistance(unit, DistanceUnit::Decimeter, DistanceUnit::Meter);
    CHECK(res == Approx(1e-1));

    res = convertDistance(unit, DistanceUnit::Meter, DistanceUnit::Meter);
    CHECK(res == Approx(1.0));

    res = convertDistance(unit, DistanceUnit::Kilometer, DistanceUnit::Meter);
    CHECK(res == Approx(1000.0));

    res = convertDistance(unit, DistanceUnit::AU, DistanceUnit::Meter);
    CHECK(res == Approx(1.495978707E11));

    res = convertDistance(unit, DistanceUnit::Lighthour, DistanceUnit::Meter);
    CHECK(res == Approx(1.0799921E12));

    res = convertDistance(unit, DistanceUnit::Lightday, DistanceUnit::Meter);
    CHECK(res == Approx(2.591981E13));

    res = convertDistance(unit, DistanceUnit::Lightmonth, DistanceUnit::Meter);
    CHECK(res == Approx(7.8839421E14));

    res = convertDistance(unit, DistanceUnit::Lightyear, DistanceUnit::Meter);
    CHECK(res == Approx(9.4607304725808E15));

    res = convertDistance(unit, DistanceUnit::Parsec, DistanceUnit::Meter);
    CHECK(res == Approx(3.0856776E16));

    res = convertDistance(unit, DistanceUnit::Kiloparsec, DistanceUnit::Meter);
    CHECK(res == Approx(1e3 * 3.0856776E16));

    res = convertDistance(unit, DistanceUnit::Megaparsec, DistanceUnit::Meter);
    CHECK(res == Approx(1e6 * 3.0856776E16));

    res = convertDistance(unit, DistanceUnit::Gigaparsec, DistanceUnit::Meter);
    CHECK(res == Approx(1e9 * 3.0856776E16));

    res = convertDistance(unit, DistanceUnit::Thou, DistanceUnit::Meter);
    CHECK(res == Approx(1e-3 * 0.0254));

    res = convertDistance(unit, DistanceUnit::Inch, DistanceUnit::Meter);
    CHECK(res == Approx(0.0254));

    res = convertDistance(unit, DistanceUnit::Foot, DistanceUnit::Meter);
    CHECK(res == Approx(0.3048));

    res = convertDistance(unit, DistanceUnit::Yard, DistanceUnit::Meter);
    CHECK(res == Approx(0.9144));

    res = convertDistance(unit, DistanceUnit::Chain, DistanceUnit::Meter);
    CHECK(res == Approx(20.1168));

    res = convertDistance(unit, DistanceUnit::Furlong, DistanceUnit::Meter);
    CHECK(res == Approx(10.0 * 20.1168));

    res = convertDistance(unit, DistanceUnit::Mile, DistanceUnit::Meter);
    CHECK(res == Approx(1609.344));

    res = convertDistance(unit, DistanceUnit::League, DistanceUnit::Meter);
    CHECK(res == Approx(3.0 * 1609.344));
}

TEST_CASE("DistanceConversion: Convert from meters", "[distanceconversion]") {
    const double meters = 1.0;
    double res;

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Nanometer);
    CHECK(res == Approx(meters / 1e-9));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Micrometer);
    CHECK(res == Approx(meters / 1e-6));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Millimeter);
    CHECK(res == Approx(meters / 1e-3));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Centimeter);
    CHECK(res == Approx(meters / 1e-2));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Decimeter);
    CHECK(res == Approx(meters / 1e-1));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Meter);
    CHECK(res == Approx(1.0));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Kilometer);
    CHECK(res == Approx(meters / 1000.0));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::AU);
    CHECK(res == Approx(meters / 1.495978707E11));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Lighthour);
    CHECK(res == Approx(meters / 1.0799921E12));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Lightday);
    CHECK(res == Approx(meters / 2.591981E13));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Lightmonth);
    CHECK(res == Approx(meters / 7.8839421E14));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Lightyear);
    CHECK(res == Approx(meters / 9.4607304725808E15));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Parsec);
    CHECK(res == Approx(meters / 3.0856776E16));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Kiloparsec);
    CHECK(res == Approx(meters / (1e3 * 3.0856776E16)));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Megaparsec);
    CHECK(res == Approx(meters / (1e6 * 3.0856776E16)));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Gigaparsec);
    CHECK(res == Approx(meters / (1e9 * 3.0856776E16)));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Thou);
    CHECK(res == Approx(meters / (1e-3 * 0.0254)));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Inch);
    CHECK(res == Approx(meters / 0.0254));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Foot);
    CHECK(res == Approx(meters / 0.3048));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Yard);
    CHECK(res == Approx(meters / 0.9144));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Chain);
    CHECK(res == Approx(meters / 20.1168));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Furlong);
    CHECK(res == Approx(meters / (10.0 * 20.1168)));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::Mile);
    CHECK(res == Approx(meters / 1609.344));

    res = convertDistance(meters, DistanceUnit::Meter, DistanceUnit::League);
    CHECK(res == Approx(meters / (3.0 * 1609.344)));
}

TEST_CASE("DistanceConversion: Cross conversion", "[distanceconversion]") {
    const double unit = 1.0;
    double res;

    res = convertDistance(unit, DistanceUnit::Nanometer, DistanceUnit::Kilometer);
    CHECK(res == Approx(1e-12));

    res = convertDistance(unit, DistanceUnit::Micrometer, DistanceUnit::Decimeter);
    CHECK(res == Approx(1e-5));

    res = convertDistance(unit, DistanceUnit::Millimeter, DistanceUnit::Nanometer);
    CHECK(res == Approx(1e6));

    res = convertDistance(unit, DistanceUnit::Centimeter, DistanceUnit::Micrometer);
    CHECK(res == Approx(1e4));

    res = convertDistance(unit, DistanceUnit::Decimeter, DistanceUnit::Millimeter);
    CHECK(res == Approx(1e2));

    res = convertDistance(unit, DistanceUnit::Kilometer, DistanceUnit::Centimeter);
    CHECK(res == Approx(1e5));

    res = convertDistance(unit, DistanceUnit::AU, DistanceUnit::Parsec);
    CHECK(res == Approx(4.84813681e-6));

    res = convertDistance(unit, DistanceUnit::Lighthour, DistanceUnit::Lightmonth);
    CHECK(res == Approx(1.36986305e-3));

    res = convertDistance(unit, DistanceUnit::Lightday, DistanceUnit::Kiloparsec);
    CHECK(res == Approx(8.40003829e-7));

    res = convertDistance(unit, DistanceUnit::Lightmonth, DistanceUnit::Lightday);
    CHECK(res == Approx(30.4166662487));

    res = convertDistance(unit, DistanceUnit::Lightyear, DistanceUnit::Gigaparsec);
    CHECK(res == Approx(3.0660139e-10));

    res = convertDistance(unit, DistanceUnit::Parsec, DistanceUnit::Lightyear);
    CHECK(res == Approx(3.26156379673));

    res = convertDistance(unit, DistanceUnit::Kiloparsec, DistanceUnit::AU);
    CHECK(res == Approx(2.06264806E8));

    res = convertDistance(unit, DistanceUnit::Megaparsec, DistanceUnit::Lighthour);
    CHECK(res == Approx(2.85712978826E10));

    res = convertDistance(unit, DistanceUnit::Gigaparsec, DistanceUnit::Megaparsec);
    CHECK(res == Approx(1e3));

    res = convertDistance(unit, DistanceUnit::Thou, DistanceUnit::Yard);
    CHECK(res == Approx(2.77777778e-5));

    res = convertDistance(unit, DistanceUnit::Inch, DistanceUnit::Foot);
    CHECK(res == Approx(8.33333333e-2));

    res = convertDistance(unit, DistanceUnit::Foot, DistanceUnit::Mile);
    CHECK(res == Approx(1.89393939e-4));

    res = convertDistance(unit, DistanceUnit::Yard, DistanceUnit::Chain);
    CHECK(res == Approx(4.54545455e-2));

    res = convertDistance(unit, DistanceUnit::Chain, DistanceUnit::League);
    CHECK(res == Approx(4.16666666e-3));

    res = convertDistance(unit, DistanceUnit::Furlong, DistanceUnit::Thou);
    CHECK(res == Approx(7.92E6));

    res = convertDistance(unit, DistanceUnit::Mile, DistanceUnit::Inch);
    CHECK(res == Approx(6.3360E4));

    res = convertDistance(unit, DistanceUnit::League, DistanceUnit::Furlong);
    CHECK(res == Approx(24.0));
}
