/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <ghoul/fmt.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/lua/ghoul_lua.h>

namespace {
    constexpr const char* _loggerCat = "Coordinateconversion";

    void parseString(const std::string& str, int& hours_or_degrees, int& minutes,
                     double& seconds)
    {
        // Find hms or dms indicies
        size_t h_or_d_index;
        if (str.find('h') != std::string::npos) {
            h_or_d_index = str.find('h');
        }
        else {
            h_or_d_index = str.find('d');
        }
        size_t m_index = str.find('m');
        size_t s_index = str.find('s');
        if (h_or_d_index == std::string::npos || m_index == std::string::npos ||
            s_index == std::string::npos)
        {
            throw(ghoul::lua::LuaRuntimeException(fmt::format(
                "Ra or Dec '{}' format is incorrect. Correct format is: Ra 'XhYmZs', "
                "and Dec 'XdYmZs'", str))
            );
        }

        // Construct the number strings
        std::string s_hours_or_degrees = str.substr(0, h_or_d_index);
        std::string s_minutes = str.substr(h_or_d_index + 1, m_index - h_or_d_index - 1);
        std::string s_seconds = str.substr(m_index + 1, s_index - m_index - 1);

        // Convert the strings to numbers
        try {
            // Hours or degrees must be an integer
            double temp = std::stod(s_hours_or_degrees);
            std::cout << "double h: " << temp << std::endl;
            if (std::floor(temp) != temp) {
                throw(ghoul::lua::LuaRuntimeException(fmt::format(
                    "Ra or Dec '{}' format is incorrect. Correct format is: Ra 'XhYmZs', "
                    "and Dec 'XdYmZs', where X must be an integer", str))
                );
            }
            hours_or_degrees = std::stoi(s_hours_or_degrees);

            // Minutes must be an integer
            temp = std::stod(s_minutes);
            std::cout << "double m: " << temp << std::endl;
            if (std::floor(temp) != temp) {
                throw(ghoul::lua::LuaRuntimeException(fmt::format(
                    "Ra or Dec '{}' format is incorrect. Correct format is: Ra 'XhYmZs', "
                    "and Dec 'XdYmZs', where Y must be an integer", str))
                    );
            }
            minutes = std::stoi(s_minutes);

            // Seconds is a double
            seconds = std::stod(s_seconds);
        } catch (const std::invalid_argument& ia) {
            throw(ghoul::lua::LuaRuntimeException(fmt::format(
                "Ra or Dec '{}' format is incorrect. Correct format is: Ra 'XhYmZs', "
                "and Dec 'XdYmZs'", str))
            );
        }
    }

    void parseRa(const std::string& ra, int& hours, int& minutes, double& seconds) {
        if (ra.find('d') != std::string::npos) {
            throw(ghoul::lua::LuaRuntimeException(fmt::format(
                "Ra '{}' format is incorrect. Correct format is: 'XhYmZs'", ra))
            );
        }
        parseString(ra, hours, minutes, seconds);
    }

    void parseDec(const std::string& dec, int& degrees, int& minutes,
                  double& seconds)
    {
        if (dec.find('h') != std::string::npos) {
            throw(ghoul::lua::LuaRuntimeException(fmt::format(
                "Dec '{}' format is incorrect. Correct format is: "
                "Dec 'XXdYYmZZs', '-XXdYYmZZs', 'XdYmZs' or '-XdYmZs'", dec))
            );
        }
        parseString(dec, degrees, minutes, seconds);
    }

    bool isRaDecValid(int ra_h, int ra_m, double ra_s, int dec_d,
                      int dec_m, double dec_s)
    {
        // Ra
        if (ra_h < 0.0 || ra_h >= 24.0) {
            LWARNING(fmt::format("Right ascension hours '{}' is outside the allowed "
                "range of 0 to 24 hours (exclusive)", ra_h)
            );
            return false;
        }
        if (ra_m < 0.0 || ra_m >= 60.0) {
            LWARNING(fmt::format("Right ascension minutes '{}' is outside the allowed "
                "range of 0 to 60 minutes (exclusive)", ra_m)
            );
            return false;
        }
        if (ra_s < 0.0 || ra_s >= 60.0) {
            LWARNING(fmt::format("Right ascension seconds '{}' is outside the allowed "
                "range of 0 to 60 seconds (exclusive)", ra_s)
            );
            return false;
        }

        // Dec
        if (dec_d < -90.0 || dec_d > 90.0) {
            LWARNING(fmt::format("Declination degrees '{}' is outside the allowed range "
                "of -90 to 90 degrees (inclusive)", dec_d)
            );
            return false;
        }
        else if ((dec_d == -90.0 || dec_d == 90.0) && (dec_m != 0 || dec_s != 0)) {
            LWARNING("Total declination is outside the allowed range of -90 to 90 "
                "degrees (inclusive)"
            );
            return false;
        }
        if (dec_m < 0.0 || dec_m >= 60.0) {
            LWARNING(fmt::format("Declination minutes '{}' is outside the allowed range "
                "of 0 to 60 minutes (exclusive)", dec_m)
            );
            return false;
        }
        if (dec_s < 0.0 || dec_s >= 60.0) {
            LWARNING(fmt::format("Declination seconds '{}' is outside the allowed range "
                "of 0 to 60 seconds (exclusive)", dec_s)
            );
            return false;
        }

        return true;
    }
} // namespace

namespace openspace {

// Convert Equatorial coordinates ICRS right ascension and declination (a, d)
// into Galactic coordinates (l, b)
glm::dvec3 icrsToGalacticCartesian(double ra, double dec, double distance) {
    // Reference:
    // https://www.atnf.csiro.au/people/Tobias.Westmeier/tools_coords.php

    // (Ra, Dec) -> (a, d)
    double a = glm::radians(ra);
    double d = glm::radians(dec);

    // J2000 Galactic reference frame
    constexpr double a0 = glm::radians(192.8595); // Equatorial coordinates of the Galactic north pole
    constexpr double d0 = glm::radians(27.1284);
    constexpr double l0 = glm::radians(122.9320); // Galactic longitude of the equatorial north pole

    // Convert to galactic reference frame
    double l = l0 - atan2(
        cos(d) * sin(a - a0),
        sin(d) * cos(d0) - cos(d) * sin(d0) * cos(a - a0)
    );
    double b = asin(sin(d) * sin(d0) + cos(d) * cos(d0) * cos(a - a0));

    // Convert to cartesian
    glm::dvec3 rGalactic = glm::dvec3(
        cos(b) * cos(l),
        cos(b) * sin(l),
        sin(b)
    );

    return distance * rGalactic;
}

// Ra format "XXhYYmZZs" or "XhYmZs"
// Dec format "XXdYYmZZs", "-XXdYYmZZs", "XdYmZs" or "-XdYmZs"
glm::dvec2 icrsToDecimalDegrees(const std::string& ra, const std::string& dec) {
    // Reference:
    // https://math.stackexchange.com/questions/15323/how-do-i-calculate-the-cartesian-coordinates-of-stars
    if (ra.size() < 6 || dec.size() < 6) {
        throw(ghoul::lua::LuaRuntimeException(fmt::format(
            "Ra '{}' or Dec '{}' format is incorrect. Correct format is: Ra 'XhYmZs', "
            "and Dec 'XdYmZs'", ra, dec)
        ));
    }

    // Parse right ascension
    int ra_hours, ra_minutes;
    double ra_seconds;
    parseRa(ra, ra_hours, ra_minutes, ra_seconds);

    // Parse declination
    int dec_degrees, dec_minutes;
    double dec_seconds;
    parseDec(dec, dec_degrees, dec_minutes, dec_seconds);

    if (!isRaDecValid(ra_hours, ra_minutes, ra_seconds, dec_degrees, dec_minutes,
        dec_seconds))
    {
        LWARNING(fmt::format("Ra '{}' or Dec '{}' is outside the allowed range, "
            "result may be incorrect", ra, dec)
        );
    }

    // Convert from hours, minutes, seconds to decimal degrees
    double sign = signbit(static_cast<float>(dec_degrees)) ? -1.0 : 1.0;
    double ra_deg = (ra_hours * 15.0) +
        (ra_minutes * 15.0 / 60.0) +
        (ra_seconds * 15.0 / 3600.0);

    double dec_deg = (abs(dec_degrees) +
        (dec_minutes / 60.0) +
        (dec_seconds / 3600.0)) * sign;

    return glm::dvec2(ra_deg, dec_deg);
}

// Convert Galactic coordinates (x, y, z) or (l, b) into
// Equatorial coordinates ICRS right ascension and declination (a, d) plus distance
glm::dvec3 galacticCartesianToIcrs(double x, double y, double z) {
    // References:
    // https://www.atnf.csiro.au/people/Tobias.Westmeier/tools_coords.php,
    // https://en.wikipedia.org/wiki/Celestial_coordinate_system

    // Normalize
    double distance = sqrt(x * x + y * y + z * z);
    double n_x = x / distance;
    double n_y = y / distance;
    double n_z = z / distance;

    // Convert from cartesian
    // (x, y, z) -> (l, b)
    double l = atan2(n_y, n_x);
    double b = asin(n_z);

    // J2000 Galactic reference frame
    constexpr double a0 = glm::radians(192.8595); // Equatorial coordinates of the Galactic north pole
    constexpr double d0 = glm::radians(27.1284);
    constexpr double l0 = glm::radians(122.9320); // Galactic longitude of the equatorial north pole

    // Convert to equatorial reference frame
    double a = atan2(
        cos(b) * sin(l0 - l),
        sin(b) * cos(d0) - cos(b) * sin(d0) * cos(l0 - l)
    ) + a0;
    double d = asin(sin(b) * sin(d0) + cos(b) * cos(d0) * cos(l0 - l));

    glm::dvec3 rEquatorial = glm::dvec3(a, d, distance);

    return rEquatorial;
}

// Return a pair with two formatted strings from the decimal degrees ra and dec
std::pair<std::string, std::string> decimalDegreesToIcrs(double ra, double dec) {
    // References:
    // https://www.rapidtables.com/convert/number/degrees-to-degrees-minutes-seconds.html,
    // https://math.stackexchange.com/questions/15323/how-do-i-calculate-the-cartesian-coordinates-of-stars

    // Radians to degrees
    double ra_deg = glm::degrees(ra);
    double dec_deg = glm::degrees(dec);

    // Check input
    if (ra_deg < 0 || ra_deg > 360 || dec_deg < -90 || dec_deg > 90) {
        LWARNING(fmt::format("Given Ra '{}' or Dec '{}' is outside the allowed range, "
            "result may be incorrect", ra, dec)
        );
    }

    // Calculate Ra
    int ra_hours = std::trunc(ra_deg) / 15.0;
    double ra_minutes_full = (ra_deg - ra_hours * 15.0) * 60.0 / 15.0;
    int ra_minutes = std::trunc(ra_minutes_full);
    double ra_seconds = (ra_minutes_full - ra_minutes) * 60.0;

    // Calculate Dec
    int dec_degrees = std::trunc(dec_deg);
    double dec_minutes_full = (abs(dec_deg) - abs(dec_degrees)) * 60.0;
    int dec_minutes = std::trunc(dec_minutes_full);
    double dec_seconds = (dec_minutes_full - dec_minutes) * 60.0;

    // Construct strings
    std::pair<std::string, std::string> result;
    result.first = std::to_string(ra_hours) + 'h' +
        std::to_string(ra_minutes) + 'm' +
        std::to_string(ra_seconds) + 's';

    result.second = std::to_string(dec_degrees) + 'd' +
        std::to_string(dec_minutes) + 'm' +
        std::to_string(dec_seconds) + 's';

    // Check results
    if (!isRaDecValid(ra_hours, ra_minutes, ra_seconds, dec_degrees, dec_minutes,
        dec_seconds))
    {
        LWARNING(fmt::format("Resulting Ra '{}' or Dec '{}' is outside the allowed range, "
            "result may be incorrect", result.first, result.second)
        );
    }
    return result;
}

} // namespace openspace
