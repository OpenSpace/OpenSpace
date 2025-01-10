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

#include <openspace/util/coordinateconversion.h>

#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/lua/ghoul_lua.h>

namespace {
    constexpr std::string_view _loggerCat = "Coordinateconversion";

    // J2000 Galactic reference frame
    // Equatorial coordinates of the Galactic north pole
    constexpr double A0 = glm::radians(192.8595);
    constexpr double D0 = glm::radians(27.1284);
    // Galactic longitude of the equatorial north pole
    constexpr double L0 = glm::radians(122.9320);

    void parseString(const std::string& str, int& hoursOrDegrees, int& minutes,
                     double& seconds)
    {
        // Find hms or dms indicies
        const size_t hOrDIndex =
            (str.find('h') != std::string::npos) ? str.find('h') : str.find('d');
        const size_t mIndex = str.find('m');
        const size_t sIndex = str.find('s');
        if (hOrDIndex == std::string::npos || mIndex == std::string::npos ||
            sIndex == std::string::npos)
        {
            throw ghoul::lua::LuaRuntimeException(std::format(
                "Ra or Dec '{}' format is incorrect. Correct format is: Ra 'XhYmZs', "
                "and Dec 'XdYmZs'", str
            ));
        }

        // Construct the number strings
        const std::string sHoursOrDegrees = str.substr(0, hOrDIndex);
        const std::string sMinutes = str.substr(hOrDIndex + 1, mIndex - hOrDIndex - 1);
        const std::string sSeconds = str.substr(mIndex + 1, sIndex - mIndex - 1);

        // Convert the strings to numbers
        try {
            // Hours or degrees must be an integer
            double temp = std::stod(sHoursOrDegrees);
            if (std::floor(temp) != temp) {
                throw ghoul::lua::LuaRuntimeException(std::format(
                    "Ra or Dec '{}' format is incorrect. Correct format is: Ra 'XhYmZs', "
                    "and Dec 'XdYmZs', where X must be an integer", str
                ));
            }
            hoursOrDegrees = std::stoi(sHoursOrDegrees);

            // Minutes must be an integer
            temp = std::stod(sMinutes);
            if (std::floor(temp) != temp) {
                throw ghoul::lua::LuaRuntimeException(std::format(
                    "Ra or Dec '{}' format is incorrect. Correct format is: Ra 'XhYmZs', "
                    "and Dec 'XdYmZs', where Y must be an integer", str
                ));
            }
            minutes = std::stoi(sMinutes);

            // Seconds is a double
            seconds = std::stod(sSeconds);
        }
        catch (const std::invalid_argument&) {
            throw ghoul::lua::LuaRuntimeException(std::format(
                "Ra or Dec '{}' format is incorrect. Correct format is: Ra 'XhYmZs', "
                "and Dec 'XdYmZs'", str
            ));
        }
    }

    void parseRa(const std::string& ra, int& hours, int& minutes, double& seconds) {
        if (ra.find('d') != std::string::npos) {
            throw ghoul::lua::LuaRuntimeException(std::format(
                "Ra '{}' format is incorrect. Correct format is: 'XhYmZs'", ra
            ));
        }
        parseString(ra, hours, minutes, seconds);
    }

    void parseDec(const std::string& dec, int& degrees, int& minutes, double& seconds) {
        if (dec.find('h') != std::string::npos) {
            throw ghoul::lua::LuaRuntimeException(std::format(
                "Dec '{}' format is incorrect. Correct format is: 'XdYmZs'", dec
            ));
        }
        parseString(dec, degrees, minutes, seconds);
    }

    bool isRaDecValid(int raH, int raM, double raS, int decD, int decM, double decS) {
        // Ra
        if (raH < 0.0 || raH >= 24.0) {
            LWARNING(std::format(
                "Right ascension hours '{}' is outside the allowed range of 0 to 24 "
                "hours (exclusive)", raH
            ));
            return false;
        }
        if (raM < 0.0 || raM >= 60.0) {
            LWARNING(std::format(
                "Right ascension minutes '{}' is outside the allowed range of 0 to 60 "
                "minutes (exclusive)", raM
            ));
            return false;
        }
        if (raS < 0.0 || raS >= 60.0) {
            LWARNING(std::format(
                "Right ascension seconds '{}' is outside the allowed "
                "range of 0 to 60 seconds (exclusive)", raS
            ));
            return false;
        }

        // Dec
        if (decD < -90.0 || decD > 90.0) {
            LWARNING(std::format("Declination degrees '{}' is outside the allowed range "
                "of -90 to 90 degrees (inclusive)", decD
            ));
            return false;
        }
        else if ((decD == -90.0 || decD == 90.0) && (decM != 0 || decS != 0)) {
            LWARNING(
                "Total declination is outside the allowed range of -90 to 90 degrees "
                "(inclusive)"
            );
            return false;
        }
        if (decM < 0.0 || decM >= 60.0) {
            LWARNING(std::format(
                "Declination minutes '{}' is outside the allowed range of 0 to 60 "
                "minutes (exclusive)", decM
            ));
            return false;
        }
        if (decS < 0.0 || decS >= 60.0) {
            LWARNING(std::format(
                "Declination seconds '{}' is outside the allowed range of 0 to 60 "
                "seconds (exclusive)", decS
            ));
            return false;
        }

        return true;
    }
} // namespace

namespace openspace {

// Convert Equatorial coordinates ICRS right ascension and declination (a, d)
// into Galactic coordinates (l, b)
// Reference:
// https://www.atnf.csiro.au/people/Tobias.Westmeier/tools_coords.php
glm::dvec3 icrsToGalacticCartesian(double ra, double dec, double distance) {
    // (Ra, Dec) -> (a, d)
    const double a = glm::radians(ra);
    const double d = glm::radians(dec);

    // Convert to galactic reference frame
    const double l = L0 - std::atan2(
        std::cos(d) * std::sin(a - A0),
        std::sin(d) * std::cos(D0) - std::cos(d) * std::sin(D0) * std::cos(a - A0)
    );
    const double b = std::asin(
        std::sin(d) * std::sin(D0) + std::cos(d) * std::cos(D0) * std::cos(a - A0)
    );

    // Convert to cartesian
    const glm::dvec3 rGalactic = glm::dvec3(
        std::cos(b) * std::cos(l),
        std::cos(b) * std::sin(l),
        std::sin(b)
    );

    return distance * rGalactic;
}

// Ra format 'XhYmZs', where X and Y are positive integers and Z is a positive double
// Dec format 'XdYmZs', where X is a signed integer, Y is a positive integer and Z is a
// positive double
// Reference:
// https://math.stackexchange.com/questions/15323/how-do-i-calculate-the-cartesian-
// coordinates-of-stars
glm::dvec2 icrsToDecimalDegrees(const std::string& ra, const std::string& dec) {
    if (ra.size() < 6 || dec.size() < 6) {
        throw ghoul::lua::LuaRuntimeException(std::format(
            "Ra '{}' or Dec '{}' format is incorrect. Correct format is: Ra 'XhYmZs', "
            "and Dec 'XdYmZs'", ra, dec
        ));
    }

    // Parse right ascension
    int raHours = 0;
    int raMinutes = 0;
    double raSeconds = 0.0;
    parseRa(ra, raHours, raMinutes, raSeconds);

    // Parse declination
    int decDegrees = 0;
    int decMinutes = 0;
    double decSeconds = 0.0;
    parseDec(dec, decDegrees, decMinutes, decSeconds);

    const bool isValid = isRaDecValid(raHours,
        raMinutes,
        raSeconds,
        decDegrees,
        decMinutes,
        decSeconds
    );

    if (!isValid) {
        LWARNING(std::format(
            "Ra '{}' or Dec '{}' is outside the allowed range, result may be incorrect",
            ra, dec
        ));
    }

    // Convert from hours/degrees, minutes, seconds to decimal degrees
    const double sign = std::signbit(static_cast<float>(decDegrees)) ? -1.0 : 1.0;
    const double raDeg = (raHours * 15.0) +
        (raMinutes * 15.0 / 60.0) +
        (raSeconds * 15.0 / 3600.0);

    const double decDeg = (std::abs(decDegrees) +
        (decMinutes / 60.0) +
        (decSeconds / 3600.0)) * sign;

    return glm::dvec2(raDeg, decDeg);
}

// Convert Galactic coordinates (x, y, z) or (l, b) into Equatorial coordinates ICRS
// right ascension and declination in decimal degrees (a, d) plus distance
// References:
// https://www.atnf.csiro.au/people/Tobias.Westmeier/tools_coords.php,
// https://en.wikipedia.org/wiki/Celestial_coordinate_system
glm::dvec3 galacticCartesianToIcrs(double x, double y, double z) {
    // Normalize
    const double distance = std::sqrt(x*x + y*y + z*z);
    double const nX = x / distance;
    const double nY = y / distance;
    const double nZ = z / distance;

    // Convert from cartesian
    // (x, y, z) -> (l, b)
    const double l = std::atan2(nY, nX);
    const double b = std::asin(nZ);

    // Convert to equatorial reference frame
    const double a = std::atan2(
        std::cos(b) * std::sin(L0 - l),
        std::sin(b) * std::cos(D0) - std::cos(b) * std::sin(D0) * std::cos(L0 - l)
    ) + A0;
    const double d = std::asin(
        std::sin(b) * std::sin(D0) + std::cos(b) * std::cos(D0) * std::cos(L0 - l)
    );

    return glm::dvec3(glm::degrees(a), glm::degrees(d), distance);
}

// Return a pair with two formatted strings from the decimal degrees ra and dec
// References:
// https://www.rapidtables.com/convert/number/degrees-to-degrees-minutes-seconds.html,
// https://math.stackexchange.com/questions/15323/how-do-i-calculate-the-cartesian-
// coordinates-of-stars
std::pair<std::string, std::string> decimalDegreesToIcrs(double ra, double dec) {
    // Check input
    if (ra < 0.0 || ra > 360.0 || dec < -90.0 || dec > 90.0) {
        LWARNING(std::format(
            "Ra '{}' or Dec '{}' is outside the allowed range, result may be incorrect",
            ra, dec
        ));
    }

    // Calculate Ra
    const int raHours = static_cast<int>(std::trunc(ra) / 15.0);
    const double raMinutesFull = (ra - raHours * 15.0) * 60.0 / 15.0;
    const int raMinutes = static_cast<int>(std::trunc(raMinutesFull));
    const double raSeconds = (raMinutesFull - raMinutes) * 60.0;

    // Calculate Dec
    const int decDegrees = static_cast<int>(std::trunc(dec));
    const double decMinutesFull = (std::abs(dec) - std::abs(decDegrees)) * 60.0;
    const int decMinutes = static_cast<int>(std::trunc(decMinutesFull));
    const double decSeconds = (decMinutesFull - decMinutes) * 60.0;

    // Construct strings
    std::pair<std::string, std::string> result;
    result.first = std::to_string(raHours) + 'h' +
        std::to_string(raMinutes) + 'm' +
        std::to_string(raSeconds) + 's';

    result.second = std::to_string(decDegrees) + 'd' +
        std::to_string(decMinutes) + 'm' +
        std::to_string(decSeconds) + 's';

    // Check results
    const bool isValid = isRaDecValid(raHours,
        raMinutes,
        raSeconds,
        decDegrees,
        decMinutes,
        decSeconds
    );

    if (!isValid) {
        LWARNING(std::format(
            "Resulting Ra '{}' or Dec '{}' is outside the allowed range, result may be "
            "incorrect", result.first, result.second
        ));
    }
    return result;
}

} // namespace openspace
