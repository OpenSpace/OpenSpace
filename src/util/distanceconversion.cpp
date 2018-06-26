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

#include <ghoul/glm.h>

namespace openspace {

std::pair<double, std::string> simplifyDistance(double meters, bool forceSingularForm) {
    constexpr const double GraceFactor = 0.5;

    const double metersVal = glm::abs(meters);

    if (metersVal == 0.0) {
        return { 0.0, forceSingularForm ? "meter" : "meters"};
    }
    else if (metersVal > 1e-3 && metersVal < 1e3) {
        return { meters, (meters == 1.0 || forceSingularForm) ? "meter" : "meters" };
    }

    if (metersVal < 1e-9) {
        const double val = meters / 1e-9;
        return { val, (val == 1.0 || forceSingularForm) ? "nanometer" : "nanometers" };
    }
    else if (metersVal < 1e-6) {
        const double val = meters / 1e-6;
        return { val, (val == 1.0 || forceSingularForm) ? "micrometer" : "micrometers" };
    }
    else if (metersVal < 1e-3) {
        const double val = meters / 1e-3;
        return { val, (val == 1.0 || forceSingularForm) ? "millimeter" : "millimeters" };
    }

    if (metersVal > (1e9 * GraceFactor * distanceconstants::Parsec)) {
        const double val = meters / (1e9 * distanceconstants::Parsec);
        return { val, (val == 1.0 || forceSingularForm) ? "Gigaparsec" : "Gigaparsecs" };
    }
    else if (metersVal > (1e6 * GraceFactor * distanceconstants::Parsec)) {
        const double val = meters / (1e6 * distanceconstants::Parsec);
        return { val, (val == 1.0 || forceSingularForm) ? "Megaparsec" : "Megaparsecs" };
    }
    else if (metersVal > (1e3 * GraceFactor * distanceconstants::Parsec)) {
        const double val = meters / (1e3 * distanceconstants::Parsec);
        return { val, (val == 1.0 || forceSingularForm) ? "Kiloparsec" : "Kiloparsecs" };
    }
    else if (metersVal > GraceFactor * distanceconstants::Parsec) {
        const double val = meters / distanceconstants::Parsec;
        return { val, (val == 1.0 || forceSingularForm) ? "Parsec" : "Parsecs" };
    }
    else if (metersVal > GraceFactor * distanceconstants::LightYear) {
        const double val = meters / distanceconstants::LightYear;
        return { val, (val == 1.0 || forceSingularForm) ? "Lightyear" : "Lightyears" };
    }
    else if (metersVal > GraceFactor * distanceconstants::LightMonth) {
        const double val = meters / distanceconstants::LightMonth;
        return { val, (val == 1.0 || forceSingularForm) ? "Lightmonth" : "Lightmonths" };
    }
    else if (metersVal > GraceFactor * distanceconstants::LightDay) {
        const double val = meters / distanceconstants::LightDay;
        return { val, (val == 1.0 || forceSingularForm) ? "Lightday" : "Lightdays" };
    }
    else if (metersVal > GraceFactor * distanceconstants::LightHour) {
        const double val = meters / distanceconstants::LightHour;
        return { val, (val == 1.0 || forceSingularForm) ? "Lighthour" : "Lighthours" };
    }
    else if (metersVal > GraceFactor * distanceconstants::AstronomicalUnit) {
        return { meters / distanceconstants::AstronomicalUnit, "AU" };
    }
    else {
        return { meters / 1000.0, "km" };
    }
}

} // namespace openspace
