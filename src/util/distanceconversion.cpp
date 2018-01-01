/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <ghoul/glm.h>

namespace openspace {

std::pair<double, std::string> simplifyDistance(double meters) {
    double metersVal = glm::abs(meters);

    if (metersVal == 0.0) {
        return { 0.0, "meters"};
    }
    else if (metersVal > 1e-3 && metersVal < 1e3) {
        return { meters, meters == 1.0 ? "meter" : "meters" };
    }

    if (metersVal < 1e-9) {
        return { meters / 1e-9, (meters / 1e-9) == 1.0 ? "nanometer" : "nanometers" };
    }
    else if (metersVal < 1e-6) {
        return { meters / 1e-6, (meters / 1e-6) == 1.0 ? "micrometer" : "micrometers" };
    }
    else if (metersVal < 1e-3) {
        return { meters / 1e-3, (meters / 1e-3) == 1.0 ? "millimeter" : "millimeters" };
    }

    if (metersVal > (1e9 * distanceconstants::Parsec)) {
        double val = meters / (1e9 * distanceconstants::Parsec);
        return { val, val == 1.0 ? "Gigaparsec" : "Gigaparsecs" };
    }
    else if (metersVal > (1e6 * distanceconstants::Parsec)) {
        double val = meters / (1e6 * distanceconstants::Parsec);
        return { val, val == 1.0 ? "Megaparsec" : "Megaparsecs" };
    }
    else if (metersVal > (1e3 * distanceconstants::Parsec)) {
        double val = meters / (1e3 * distanceconstants::Parsec);
        return { val, val == 1.0 ? "Kiloparsec" : "Kiloparsecs" };
    }
    else if (metersVal > distanceconstants::Parsec) {
        double val = meters / distanceconstants::Parsec;
        return { val, val == 1.0 ? "Parsec" : "Parsecs" };
    }
    else if (metersVal > distanceconstants::LightYear) {
        double val = meters / distanceconstants::LightYear;
        return { val, val == 1.0 ? "Lightyear" : "Lightyears" };
    }
    else if (metersVal > distanceconstants::LightMonth) {
        double val = meters / distanceconstants::LightMonth;
        return { val, val == 1.0 ? "Lightmonth" : "Lightmonths" };
    }
    else if (metersVal > distanceconstants::LightDay) {
        double val = meters / distanceconstants::LightDay;
        return { val, val == 1.0 ? "Lightday" : "Lightdays" };
    }
    else if (metersVal > distanceconstants::LightHour) {
        double val = meters / distanceconstants::LightDay;
        return { val, val == 1.0 ? "Lighthour" : "Lighthours" };
    }
    else if (metersVal > distanceconstants::AstronomicalUnit) {
        return { meters / distanceconstants::AstronomicalUnit, "AU" };
    }
    else {
        return { meters / 1000.0, "km" };
    }
}

} // namespace openspace
