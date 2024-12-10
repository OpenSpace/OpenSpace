/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
    League
};

// Assumption:  Unit names are sequential in memory
constexpr std::string_view DistanceUnitNanometer = "Nanometer";
constexpr std::string_view DistanceUnitMicrometer = "Micrometer";
constexpr std::string_view DistanceUnitMillimeter = "Millimeter";
constexpr std::string_view DistanceUnitCentimeter = "Centimeter";
constexpr std::string_view DistanceUnitDecimeter = "Decimeter";
constexpr std::string_view DistanceUnitMeter = "Meter";
constexpr std::string_view DistanceUnitKilometer = "Kilometer";
constexpr std::string_view DistanceUnitAU = "AU";
constexpr std::string_view DistanceUnitLighthour = "Lighthour";
constexpr std::string_view DistanceUnitLightday = "Lightday";
constexpr std::string_view DistanceUnitLightmonth = "Lightmonth";
constexpr std::string_view DistanceUnitLightyear = "Lightyear";
constexpr std::string_view DistanceUnitParsec = "Parsec";
constexpr std::string_view DistanceUnitKiloparsec = "Kiloparsec";
constexpr std::string_view DistanceUnitMegaparsec = "Megaparsec";
constexpr std::string_view DistanceUnitGigaparsec = "Gigaparsec";
constexpr std::string_view DistanceUnitGigalightyear = "Gigalightyear";
constexpr std::string_view DistanceUnitThou = "Thou";
constexpr std::string_view DistanceUnitInch = "Inch";
constexpr std::string_view DistanceUnitFoot = "Foot";
constexpr std::string_view DistanceUnitYard = "Yard";
constexpr std::string_view DistanceUnitChain = "Chain";
constexpr std::string_view DistanceUnitFurlong = "Furlong";
constexpr std::string_view DistanceUnitMile = "Mile";
constexpr std::string_view DistanceUnitLeague = "League";


// Assumption:  Unit names are sequential in memory
constexpr std::string_view DistanceUnitNanometers = "Nanometers";
constexpr std::string_view DistanceUnitMicrometers = "Micrometers";
constexpr std::string_view DistanceUnitMillimeters = "Millimeters";
constexpr std::string_view DistanceUnitCentimeters = "Centimeters";
constexpr std::string_view DistanceUnitDecimeters = "Decimeters";
constexpr std::string_view DistanceUnitMeters = "Meters";
constexpr std::string_view DistanceUnitKilometers = "Kilometers";
constexpr std::string_view DistanceUnitAUs = "AU";
constexpr std::string_view DistanceUnitLighthours = "Lighthours";
constexpr std::string_view DistanceUnitLightdays = "Lightdays";
constexpr std::string_view DistanceUnitLightmonths = "Lightmonths";
constexpr std::string_view DistanceUnitLightyears = "Lightyears";
constexpr std::string_view DistanceUnitParsecs = "Parsecs";
constexpr std::string_view DistanceUnitKiloparsecs = "Kiloparsecs";
constexpr std::string_view DistanceUnitMegaparsecs = "Megaparsecs";
constexpr std::string_view DistanceUnitGigaparsecs = "Gigaparsecs";
constexpr std::string_view DistanceUnitGigalightyears = "Gigalightyears";
constexpr std::string_view DistanceUnitThous = "Thou";
constexpr std::string_view DistanceUnitInches = "Inches";
constexpr std::string_view DistanceUnitFeet = "Feet";
constexpr std::string_view DistanceUnitYards = "Yards";
constexpr std::string_view DistanceUnitChains = "Chains";
constexpr std::string_view DistanceUnitFurlongs = "Furlongs";
constexpr std::string_view DistanceUnitMiles = "Miles";
constexpr std::string_view DistanceUnitLeagues = "Leagues";

constexpr std::array<DistanceUnit, static_cast<int>(DistanceUnit::League) + 1>
DistanceUnits = {
    DistanceUnit::Nanometer, DistanceUnit::Micrometer, DistanceUnit::Millimeter,
    DistanceUnit::Centimeter, DistanceUnit::Decimeter, DistanceUnit::Meter,
    DistanceUnit::Kilometer, DistanceUnit::AU, DistanceUnit::Lighthour,
    DistanceUnit::Lightday, DistanceUnit::Lightmonth, DistanceUnit::Lightyear,
    DistanceUnit::Parsec, DistanceUnit::Kiloparsec, DistanceUnit::Megaparsec,
    DistanceUnit::Gigaparsec, DistanceUnit::Gigalightyear, DistanceUnit::Thou,
    DistanceUnit::Inch, DistanceUnit::Foot, DistanceUnit::Yard, DistanceUnit::Chain,
    DistanceUnit::Furlong, DistanceUnit::Mile, DistanceUnit::League
};

constexpr std::array<std::string_view, static_cast<int>(DistanceUnit::League) + 1>
DistanceUnitNamesSingular = {
    DistanceUnitNanometer, DistanceUnitMicrometer, DistanceUnitMillimeter,
    DistanceUnitCentimeter, DistanceUnitDecimeter, DistanceUnitMeter,
    DistanceUnitKilometer, DistanceUnitAU, DistanceUnitLighthour,
    DistanceUnitLightday, DistanceUnitLightmonth, DistanceUnitLightyear,
    DistanceUnitParsec, DistanceUnitKiloparsec, DistanceUnitMegaparsec,
    DistanceUnitGigaparsec, DistanceUnitGigalightyear, DistanceUnitThou, DistanceUnitInch,
    DistanceUnitFoot, DistanceUnitYard, DistanceUnitChain, DistanceUnitFurlong,
    DistanceUnitMile, DistanceUnitLeague
};

constexpr std::array<std::string_view, static_cast<int>(DistanceUnit::League) + 1>
DistanceUnitNamesPlural = {
    DistanceUnitNanometers, DistanceUnitMicrometers, DistanceUnitMillimeters,
    DistanceUnitCentimeters, DistanceUnitDecimeters, DistanceUnitMeters,
    DistanceUnitKilometers, DistanceUnitAUs, DistanceUnitLighthours,
    DistanceUnitLightdays, DistanceUnitLightmonths, DistanceUnitLightyears,
    DistanceUnitParsecs, DistanceUnitKiloparsecs, DistanceUnitMegaparsecs,
    DistanceUnitGigaparsecs, DistanceUnitGigalightyears, DistanceUnitThous,
    DistanceUnitInches, DistanceUnitFeet, DistanceUnitYards, DistanceUnitChains,
    DistanceUnitFurlongs, DistanceUnitMile, DistanceUnitLeague
};

constexpr bool isValidDistanceUnitName(std::string_view name) {
    for (std::string_view val : DistanceUnitNamesSingular) {
        if (val == name) {
            return true;
        }
    }

    for (std::string_view val : DistanceUnitNamesPlural) {
        if (val == name) {
            return true;
        }
    }
    return false;
}

constexpr std::string_view nameForDistanceUnit(DistanceUnit unit, bool pluralForm = false)
{
    switch (unit) {
        case DistanceUnit::Nanometer:
        case DistanceUnit::Micrometer:
        case DistanceUnit::Millimeter:
        case DistanceUnit::Centimeter:
        case DistanceUnit::Decimeter:
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
        case DistanceUnit::Gigalightyear:
        case DistanceUnit::Thou:
        case DistanceUnit::Inch:
        case DistanceUnit::Foot:
        case DistanceUnit::Yard:
        case DistanceUnit::Chain:
        case DistanceUnit::Furlong:
        case DistanceUnit::Mile:
        case DistanceUnit::League:
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

constexpr DistanceUnit distanceUnitFromString(std::string_view unitName) {
    int found = -1;
    int i = 0;
    for (std::string_view val : DistanceUnitNamesSingular) {
        if (val == unitName) {
            found = i;
            break;
        }
        i++;
    }

    i = 0;
    for (std::string_view val : DistanceUnitNamesPlural) {
        if (val == unitName) {
            found = i;
            break;
        }
        i++;
    }


    if (found != -1) {
        return static_cast<DistanceUnit>(found);
    }
    else {
        throw ghoul::MissingCaseException();
    }
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
