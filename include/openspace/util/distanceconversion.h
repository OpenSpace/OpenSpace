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

#ifndef __OPENSPACE_CORE___DISTANCECONVERSION___H__
#define __OPENSPACE_CORE___DISTANCECONVERSION___H__

#include <openspace/util/distanceconstants.h>

#include <ghoul/misc/assert.h>
#include <ghoul/misc/constexpr.h>
#include <algorithm>
#include <array>
#include <string>
#include <utility>

namespace openspace {

enum class DistanceUnit {
    Nanometer = 0,
    Micrometer,
    Millimeter,
    Centimeter,
    Decimeter,
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
    Gigaparsec,
    Gigalightyear,

    // Weird units
    Thou,
    Inch,
    Foot,
    Yard,
    Chain,
    Furlong,
    Mile,
    League,
    NauticalMile
};

struct DistanceUnitName {
    std::string_view singular;
    std::string_view plural;
    std::string_view abbreviation;
};

constexpr std::array<DistanceUnit, static_cast<int>(DistanceUnit::NauticalMile) + 1>
DistanceUnits = {
    DistanceUnit::Nanometer, DistanceUnit::Micrometer, DistanceUnit::Millimeter,
    DistanceUnit::Centimeter, DistanceUnit::Decimeter, DistanceUnit::Meter,
    DistanceUnit::Kilometer, DistanceUnit::AU, DistanceUnit::Lighthour,
    DistanceUnit::Lightday, DistanceUnit::Lightmonth, DistanceUnit::Lightyear,
    DistanceUnit::Parsec, DistanceUnit::Kiloparsec, DistanceUnit::Megaparsec,
    DistanceUnit::Gigaparsec, DistanceUnit::Gigalightyear, DistanceUnit::Thou,
    DistanceUnit::Inch, DistanceUnit::Foot, DistanceUnit::Yard, DistanceUnit::Chain,
    DistanceUnit::Furlong, DistanceUnit::Mile, DistanceUnit::League,
    DistanceUnit::NauticalMile
};

// Note that the syntax here is required when initializing constexpr std::arrays with
// structs
constexpr std::array<DistanceUnitName, static_cast<int>(DistanceUnit::NauticalMile) + 1>
DistanceUnitNames { {
    { "Nanometer", "Nanometers", "nm" },
    { "Micrometer", "Micrometers", "um" },
    { "Millimeter", "Millimeters", "mm" },
    { "Centimeter", "Centimeters", "cm" },
    { "Decimeter", "Decimeters", "dm" },
    { "Meter", "Meters", "m" },
    { "Kilometer", "Kilometers", "km" },
    { "AU", "AU", "au" },
    { "Lighthour", "Lighthours", "lh" },
    { "Lightday", "Lightdays", "ld" },
    { "Lightmonth", "Lightmonths", "lm" },
    { "Lightyear", "Lightyears", "ly" },
    { "Parsec", "Parsecs", "pc" },
    { "Kiloparsec", "Kiloparsecs", "kpc" },
    { "Megaparsec", "Megaparsecs", "Mpc" },
    { "Gigaparsec", "Gigaparsecs", "Gpc" },
    { "Gigalightyear", "Gigalightyears", "Gly" },
    { "Thou", "Thou", "th" },
    { "Inch", "Inches", "in" },
    { "Foot", "Feet", "ft" },
    { "Yard", "Yards", "yd" },
    { "Chain", "Chains", "ch" },
    { "Furlong", "Furlongs", "fur" },
    { "Mile", "Miles", "mi" },
    { "League", "Leagues", "league" },
    { "Nautical Mile", "Nautical Miles", "NM" }
}};

constexpr bool isValidDistanceUnitName(std::string_view name) {
    for (DistanceUnit unit : DistanceUnits) {
        const DistanceUnitName unitName = DistanceUnitNames[static_cast<int>(unit)];
        if (name == unitName.singular || name == unitName.plural ||
            name == unitName.abbreviation)
        {
            return true;
        }
    }
    return false;
}

constexpr std::string_view nameForDistanceUnit(DistanceUnit unit,
                                               bool usePluralForm = false)
{
    const DistanceUnitName unitName = DistanceUnitNames[static_cast<int>(unit)];
    return usePluralForm ? unitName.plural : unitName.singular;
}

constexpr std::string_view abbreviationForDistanceUnit(DistanceUnit unit) {
    return DistanceUnitNames[static_cast<int>(unit)].abbreviation;
}

constexpr DistanceUnit distanceUnitFromString(std::string_view unitName) {
    int i = 0;
    for (DistanceUnit unit : DistanceUnits) {
        const DistanceUnitName name = DistanceUnitNames[static_cast<int>(unit)];
        if (name.singular == unitName || name.plural == unitName ||
            name.abbreviation == unitName)
        {
            return static_cast<DistanceUnit>(i);
        }
        i++;
    }

    throw ghoul::MissingCaseException();
}

constexpr std::vector<std::string> distanceUnitList() {
    std::vector<std::string> res(DistanceUnits.size());
    std::transform(
        DistanceUnits.begin(),
        DistanceUnits.end(),
        res.begin(),
        [](DistanceUnit unit) {
            return std::string(nameForDistanceUnit(unit));
        }
    );
    return res;
}

std::pair<double, std::string_view> simplifyDistance(double meters,
    bool forceSingularForm = false);

constexpr double convertMeters(double meters, DistanceUnit requestedUnit) {
    switch (requestedUnit) {
        case DistanceUnit::Nanometer:
            return meters / 1e-9;
        case DistanceUnit::Micrometer:
            return meters / 1e-6;
        case DistanceUnit::Millimeter:
            return meters / 1e-3;
        case DistanceUnit::Centimeter:
            return meters / 1e-2;
        case DistanceUnit::Decimeter:
            return meters / 1e-1;
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
        case DistanceUnit::Gigalightyear:
            return meters / (1e9 * distanceconstants::LightYear);
        case DistanceUnit::Thou:
            return meters / (1e-3 * distanceconstants::Inch);
        case DistanceUnit::Inch:
            return meters / distanceconstants::Inch;
        case DistanceUnit::Foot:
            return meters / distanceconstants::Foot;
        case DistanceUnit::Yard:
            return meters / distanceconstants::Yard;
        case DistanceUnit::Chain:
            return meters / distanceconstants::Chain;
        case DistanceUnit::Furlong:
            return meters / (10.0 * distanceconstants::Chain);
        case DistanceUnit::Mile:
            return meters / distanceconstants::Mile;
        case DistanceUnit::League:
            return meters / (3.0 * distanceconstants::Mile);
        case DistanceUnit::NauticalMile:
            return meters / distanceconstants::NauticalMile;
        default:
            throw ghoul::MissingCaseException();
    }
}

constexpr double toMeter(DistanceUnit unit) {
    switch (unit) {
        case DistanceUnit::Nanometer:
            return 1e-9;
        case DistanceUnit::Micrometer:
            return 1e-6;
        case DistanceUnit::Millimeter:
            return 1e-3;
        case DistanceUnit::Centimeter:
            return 1e-2;
        case DistanceUnit::Decimeter:
            return 1e-1;
        case DistanceUnit::Meter:
            return 1.0;
        case DistanceUnit::Kilometer:
            return 1000.0;
        case DistanceUnit::AU:
            return distanceconstants::AstronomicalUnit;
        case DistanceUnit::Lighthour:
            return distanceconstants::LightHour;
        case DistanceUnit::Lightday:
            return distanceconstants::LightDay;
        case DistanceUnit::Lightmonth:
            return distanceconstants::LightMonth;
        case DistanceUnit::Lightyear:
            return distanceconstants::LightYear;
        case DistanceUnit::Parsec:
            return distanceconstants::Parsec;
        case DistanceUnit::Kiloparsec:
            return 1e3 * distanceconstants::Parsec;
        case DistanceUnit::Megaparsec:
            return 1e6 * distanceconstants::Parsec;
        case DistanceUnit::Gigaparsec:
            return 1e9 * distanceconstants::Parsec;
        case DistanceUnit::Gigalightyear:
            return 1e9 * distanceconstants::LightYear;
        case DistanceUnit::Thou:
            return 1e-3 * distanceconstants::Inch;
        case DistanceUnit::Inch:
            return distanceconstants::Inch;
        case DistanceUnit::Foot:
            return distanceconstants::Foot;
        case DistanceUnit::Yard:
            return distanceconstants::Yard;
        case DistanceUnit::Chain:
            return distanceconstants::Chain;
        case DistanceUnit::Furlong:
            return 10.0 * distanceconstants::Chain;
        case DistanceUnit::Mile:
            return distanceconstants::Mile;
        case DistanceUnit::League:
            return 3.0 * distanceconstants::Mile;
        case DistanceUnit::NauticalMile:
            return distanceconstants::NauticalMile;
        default:
            throw ghoul::MissingCaseException();
    }
}

constexpr double convertUnit(DistanceUnit fromUnit, DistanceUnit toUnit) {
    return convertMeters(toMeter(fromUnit), toUnit);
}

constexpr double convertDistance(double distance, DistanceUnit fromUnit,
                                 DistanceUnit toUnit)
{
    return distance * convertUnit(fromUnit, toUnit);
}

float convertMasPerYearToMeterPerSecond(float masPerYear, float parallax);

} // namespace openspace

#endif // __OPENSPACE_CORE___DISTANCECONVERSION___H__
