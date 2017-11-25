/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <openspace/util/distanceconversion.h>

#include <openspace/util/distanceconstants.h>

namespace openspace {

std::pair<double, std::string> simplifyDistance(double meters) {
    if (meters > 1e-3 && meters < 1e3) {
        return { meters, "meter" };
    }

    if (meters < 1e-9) {
        return { meters / 1e-9, "nanometer" };
    }
    else if (meters < 1e-6) {
        return { meters / 1e-6, "micrometer" };
    }
    else if (meters < 1e-3) {
        return { meters / 1e-3, "millimeter" };
    }

    if (meters > (1e9 * distanceconstants::Parsec)) {
        return { meters / (1e9 * distanceconstants::Parsec) , "Gigaparsec" };
    }
    else if (meters > (1e6 * distanceconstants::Parsec)) {
        return { meters / (1e6 * distanceconstants::Parsec), "Megaparsec" };
    }
    else if (meters > (1e3 * distanceconstants::Parsec)) {
        return { meters / (1e3 * distanceconstants::Parsec), "Kiloparsec" };
    }
    else if (meters > distanceconstants::Parsec) {
        return { meters / distanceconstants::Parsec, "Parsec" };
    }
    else if (meters > distanceconstants::LightYear) {
        return { meters / distanceconstants::LightYear, "Lightyears" };
    }
    else if (meters > distanceconstants::LightMonth) {
        return { meters / distanceconstants::LightMonth, "Lightmonth" };
    }
    else if (meters > distanceconstants::LightDay) {
        return { meters / distanceconstants::LightDay, "Lightday" };
    }
    else if (meters > distanceconstants::LightHour) {
        return { meters / distanceconstants::LightDay, "Lighthour" };
    }
    else if (meters > distanceconstants::AstronomicalUnit) {
        return { meters / distanceconstants::AstronomicalUnit, "AU" };
    }
    else {
        return { meters / 1000.0, "km" };
    }
}

} // namespace openspace
