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

#ifndef __OPENSPACE_CORE___DISTANCECONVERSION___H__
#define __OPENSPACE_CORE___DISTANCECONVERSION___H__

#include <openspace/util/distanceconstants.h>

#include <ghoul/misc/assert.h>
#include <ghoul/misc/constexpr.h>
#include <array>
#include <string>
#include <utility>

namespace openspace {

enum class DistanceUnit {
    Nanometer = 0,
    Micrometer,
    Millimeter,
    Meter,
    Kilometer,
    AU,
    Lighthour,
    Lightday,
    Lightmonth,
    Lightyear,
    Parsec,
    Kiloparsec,
    Megaparsec,
    Gigaparsec
};

// Assumption:  Unit names are sequential in memory
constexpr const char* DistanceUnitNanometer = "nanometer";
constexpr const char* DistanceUnitMicrometer = "micrometer";
constexpr const char* DistanceUnitMillimeter = "millimeter";
constexpr const char* DistanceUnitMeter = "meter";
constexpr const char* DistanceUnitKilometer = "km";
constexpr const char* DistanceUnitAU = "AU";
constexpr const char* DistanceUnitLighthour = "lighthour";
constexpr const char* DistanceUnitLightday = "lightday";
constexpr const char* DistanceUnitLightmonth = "lightmonth";
constexpr const char* DistanceUnitLightyear = "lightyear";
constexpr const char* DistanceUnitParsec = "parsec";
constexpr const char* DistanceUnitKiloparsec = "kiloparsec";
constexpr const char* DistanceUnitMegaparsec = "megaparsec";
constexpr const char* DistanceUnitGigaparsec = "gigaparsec";

// Assumption:  Unit names are sequential in memory
constexpr const char* DistanceUnitNanometers = "nanometers";
constexpr const char* DistanceUnitMicrometers = "micrometers";
constexpr const char* DistanceUnitMillimeters = "millimeters";
constexpr const char* DistanceUnitMeters = "meters";
constexpr const char* DistanceUnitKilometers = "km";
constexpr const char* DistanceUnitAUs = "AU";
constexpr const char* DistanceUnitLighthours = "lighthours";
constexpr const char* DistanceUnitLightdays = "lightdays";
constexpr const char* DistanceUnitLightmonths = "lightmonths";
constexpr const char* DistanceUnitLightyears = "lightyears";
constexpr const char* DistanceUnitParsecs = "parsecs";
constexpr const char* DistanceUnitKiloparsecs = "kiloparsecs";
constexpr const char* DistanceUnitMegaparsecs = "megaparsecs";
constexpr const char* DistanceUnitGigaparsecs = "gigaparsecs";

constexpr const std::array<DistanceUnit, static_cast<int>(DistanceUnit::Gigaparsec) + 1>
DistanceUnits = {
    DistanceUnit::Nanometer, DistanceUnit::Micrometer, DistanceUnit::Millimeter,
    DistanceUnit::Meter, DistanceUnit::Kilometer, DistanceUnit::AU,
    DistanceUnit::Lighthour, DistanceUnit::Lightday, DistanceUnit::Lightmonth,
    DistanceUnit::Lightyear, DistanceUnit::Parsec, DistanceUnit::Kiloparsec,
    DistanceUnit::Megaparsec, DistanceUnit::Gigaparsec
};

constexpr const std::array<const char*, static_cast<int>(DistanceUnit::Gigaparsec) + 1>
DistanceUnitNamesSingular = {
    DistanceUnitNanometer, DistanceUnitMicrometer, DistanceUnitMillimeter,
    DistanceUnitMeter, DistanceUnitKilometer, DistanceUnitAU, DistanceUnitLighthour,
    DistanceUnitLightday, DistanceUnitLightmonth, DistanceUnitLightyear,
    DistanceUnitParsec, DistanceUnitKiloparsec, DistanceUnitMegaparsec,
    DistanceUnitGigaparsec
};

constexpr const std::array<const char*, static_cast<int>(DistanceUnit::Gigaparsec) + 1>
DistanceUnitNamesPlural = {
    DistanceUnitNanometers, DistanceUnitMicrometers, DistanceUnitMillimeters,
    DistanceUnitMeters, DistanceUnitKilometers, DistanceUnitAUs, DistanceUnitLighthours,
    DistanceUnitLightdays, DistanceUnitLightmonths, DistanceUnitLightyears,
    DistanceUnitParsecs, DistanceUnitKiloparsecs, DistanceUnitMegaparsecs,
    DistanceUnitGigaparsecs
};

constexpr bool isValidDistanceUnitName(const char* name) {
    int i = 0;
    for (const char* val : DistanceUnitNamesSingular) {
        if (ghoul::equal(name, val)) {
            return true;
        }
        ++i;
    }

    i = 0;
    for (const char* val : DistanceUnitNamesPlural) {
        if (ghoul::equal(name, val)) {
            return true;
        }
        ++i;
    }
    return false;
}

constexpr const char* nameForDistanceUnit(DistanceUnit unit, bool pluralForm = false) {
    switch (unit) {
    case DistanceUnit::Nanometer:
    case DistanceUnit::Micrometer:
    case DistanceUnit::Millimeter:
    case DistanceUnit::Meter:
    case DistanceUnit::Kilometer:
    case DistanceUnit::AU:
    case DistanceUnit::Lighthour:
    case DistanceUnit::Lightday:
    case DistanceUnit::Lightmonth:
    case DistanceUnit::Lightyear:
    case DistanceUnit::Parsec:
    case DistanceUnit::Kiloparsec:
    case DistanceUnit::Megaparsec:
    case DistanceUnit::Gigaparsec:
        if (pluralForm) {
            return DistanceUnitNamesPlural[static_cast<int>(unit)];
        }
        else {
            return DistanceUnitNamesSingular[static_cast<int>(unit)];
        }
    default:
        throw ghoul::MissingCaseException();
    }
}

constexpr DistanceUnit distanceUnitFromString(const char* unitName) {
    int found = -1;
    int i = 0;
    for (const char* val : DistanceUnitNamesSingular) {
        if (ghoul::equal(unitName, val)) {
            found = i;
            break;
        }
        ++i;
    }

    i = 0;
    for (const char* val : DistanceUnitNamesPlural) {
        if (ghoul::equal(unitName, val)) {
            found = i;
            break;
        }
        ++i;
    }


    if (found != -1) {
        return static_cast<DistanceUnit>(found);
    }
    else {
        throw ghoul::MissingCaseException();
    }
}



std::pair<double, std::string> simplifyDistance(double meters,
    bool forceSingularForm = false);

constexpr double convertDistance(double meters, DistanceUnit requestedUnit) {
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

} // namespace openspace

#endif // __OPENSPACE_CORE___DISTANCECONVERSION___H__
