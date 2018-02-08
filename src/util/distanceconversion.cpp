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
#include <ghoul/misc/assert.h>

namespace {
    constexpr bool equal(char const* lhs, char const* rhs) {
        while (*lhs || *rhs) {
            if (*lhs++ != *rhs++) {
                return false;
            }
        }
        return true;
    }
}

namespace openspace {

std::pair<double, std::string> simplifyDistance(double meters, bool forceSingularForm) {
    double metersVal = glm::abs(meters);

    if (metersVal == 0.0) {
        return { 0.0, forceSingularForm ? "meter" : "meters"};
    }
    else if (metersVal > 1e-3 && metersVal < 1e3) {
        return { meters, (meters == 1.0 || forceSingularForm) ? "meter" : "meters" };
    }

    if (metersVal < 1e-9) {
        double val = meters / 1e-9;
        return { val, (val == 1.0 || forceSingularForm) ? "nanometer" : "nanometers" };
    }
    else if (metersVal < 1e-6) {
        double val = meters / 1e-6;
        return { val, (val == 1.0 || forceSingularForm) ? "micrometer" : "micrometers" };
    }
    else if (metersVal < 1e-3) {
        double val = meters / 1e-3;
        return { val, (val == 1.0 || forceSingularForm) ? "millimeter" : "millimeters" };
    }

    if (metersVal > (1e9 * distanceconstants::Parsec)) {
        double val = meters / (1e9 * distanceconstants::Parsec);
        return { val, (val == 1.0 || forceSingularForm) ? "Gigaparsec" : "Gigaparsecs" };
    }
    else if (metersVal > (1e6 * distanceconstants::Parsec)) {
        double val = meters / (1e6 * distanceconstants::Parsec);
        return { val, (val == 1.0 || forceSingularForm) ? "Megaparsec" : "Megaparsecs" };
    }
    else if (metersVal > (1e3 * distanceconstants::Parsec)) {
        double val = meters / (1e3 * distanceconstants::Parsec);
        return { val, (val == 1.0 || forceSingularForm) ? "Kiloparsec" : "Kiloparsecs" };
    }
    else if (metersVal > distanceconstants::Parsec) {
        double val = meters / distanceconstants::Parsec;
        return { val, (val == 1.0 || forceSingularForm) ? "Parsec" : "Parsecs" };
    }
    else if (metersVal > distanceconstants::LightYear) {
        double val = meters / distanceconstants::LightYear;
        return { val, (val == 1.0 || forceSingularForm) ? "Lightyear" : "Lightyears" };
    }
    else if (metersVal > distanceconstants::LightMonth) {
        double val = meters / distanceconstants::LightMonth;
        return { val, (val == 1.0 || forceSingularForm) ? "Lightmonth" : "Lightmonths" };
    }
    else if (metersVal > distanceconstants::LightDay) {
        double val = meters / distanceconstants::LightDay;
        return { val, (val == 1.0 || forceSingularForm) ? "Lightday" : "Lightdays" };
    }
    else if (metersVal > distanceconstants::LightHour) {
        double val = meters / distanceconstants::LightHour;
        return { val, (val == 1.0 || forceSingularForm) ? "Lighthour" : "Lighthours" };
    }
    else if (metersVal > distanceconstants::AstronomicalUnit) {
        return { meters / distanceconstants::AstronomicalUnit, "AU" };
    }
    else {
        return { meters / 1000.0, "km" };
    }
}


double convertDistance(double meters, DistanceUnit requestedUnit) {
    switch (requestedUnit) {
        case DistanceUnit::Nanometer:
            return meters / 1e-9;
        case DistanceUnit::Micrometer:
            return meters / 1e-6;
        case DistanceUnit::Millimeter:
            return meters / 1e-3;
        case DistanceUnit::Meter:
            return meters;
        case DistanceUnit::Kilometer:
            return meters / 1000.0;
        case DistanceUnit::AU:
            return meters / distanceconstants::AstronomicalUnit;
        case DistanceUnit::Lighthour:
            return meters / distanceconstants::LightHour;
        case DistanceUnit::Lightday:
            return meters / distanceconstants::LightDay;
        case DistanceUnit::Lightmonth:
            return meters / distanceconstants::LightMonth;
        case DistanceUnit::Lightyear:
            return meters / distanceconstants::LightYear;
        case DistanceUnit::Parsec:
            return meters / distanceconstants::Parsec;
        case DistanceUnit::Kiloparsec:
            return meters / (1e3 * distanceconstants::Parsec);
        case DistanceUnit::Megaparsec:
            return meters / (1e6 * distanceconstants::Parsec);
        case DistanceUnit::Gigaparsec:
            return meters / (1e9 * distanceconstants::Parsec);
        default:
            throw ghoul::MissingCaseException();
    }
}

std::string nameForDistanceUnit(DistanceUnit unit, bool pluralForm) {
    switch (unit) {
        case DistanceUnit::Nanometer:
            return pluralForm ? "nanometers" : "nanometer";
        case DistanceUnit::Micrometer:
            return pluralForm ? "micrometers" : "micrometer";
        case DistanceUnit::Millimeter:
            return pluralForm ? "millimeters" : "millimeter";
        case DistanceUnit::Meter:
            return pluralForm ? "meters" : "meter";
        case DistanceUnit::Kilometer:
            return "km";
        case DistanceUnit::AU:
            return "AU";
        case DistanceUnit::Lighthour:
            return pluralForm ? "lighthours" : "lighthour";
        case DistanceUnit::Lightday:
            return pluralForm ? "lightdays" : "lightday";
        case DistanceUnit::Lightmonth:
            return pluralForm ? "lightmonths" : "lightmonth";
        case DistanceUnit::Lightyear:
            return pluralForm ? "lightyears" : "lightyear";
        case DistanceUnit::Parsec:
            return pluralForm ? "parsecs" : "parsec";
        case DistanceUnit::Kiloparsec:
            return pluralForm ? "kiloparsecs" : "kiloparsec";
        case DistanceUnit::Megaparsec:
            return pluralForm ? "megaparsecs" : "megaparsec";
        case DistanceUnit::Gigaparsec:
            return pluralForm ? "gigaparsecs" : "gigaparsec";
        default:
            throw ghoul::MissingCaseException();
    }
}


DistanceUnit distanceUnitFromString(const std::string& unit) {
    if (unit == "nanometer" || unit == "nanometers") {
        return DistanceUnit::Nanometer;
    }
    else if (unit == "micrometer" || unit == "micrometers") {
        return DistanceUnit::Micrometer;
    }
    else if (unit == "millimeter" || unit == "millimeters") {
        return DistanceUnit::Millimeter;
    }
    else if (unit == "meter" || unit == "meters") {
        return DistanceUnit::Meter;
    }
    else if (unit == "km") {
        return DistanceUnit::Kilometer;
    }
    else if (unit == "AU") {
        return DistanceUnit::AU;
    }
    else if (unit == "lighthour" || unit == "lighthours") {
        return DistanceUnit::Lighthour;
    }
    else if (unit == "lightday" || unit == "lightdays") {
        return DistanceUnit::Lightday;
    }
    else if (unit == "lightmonth" || unit == "lightmonths") {
        return DistanceUnit::Lightmonth;
    }
    else if (unit == "lightyear" || unit == "lightyears") {
        return DistanceUnit::Lightyear;
    }
    else if (unit == "parsec" || unit == "parsecs") {
        return DistanceUnit::Parsec;
    }
    else if (unit == "kiloparsec" || unit == "kiloparsecs") {
        return DistanceUnit::Kiloparsec;
    }
    else if (unit == "megaparsec" || unit == "megaparsecs") {
        return DistanceUnit::Megaparsec;
    }
    else if (unit == "gigaparsec" || unit == "gigaparsecs") {
        return DistanceUnit::Gigaparsec;
    }
    else {
        throw ghoul::MissingCaseException();
    }
}


} // namespace openspace
