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

    bool isStringNumber(const std::string& str) {
        for (size_t i = 0; i < str.size(); ++i) {
            if (!isdigit(str[i])) {
                if (i == 0 && str.size() > 1) {
                    if (str[i] == '-' || str[i] == '+') {
                        continue;
                    }
                    else {
                        return false;
                    }
                }
                else {
                    return false;
                }
            }
        }
        return true;
    }

    void parseString(const std::string& str, double& hours_or_degrees, double& minutes,
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
                "Ra or Dec '{}' format is incorrect. Correct format is: Ra 'XXhYYmZZs' "
                "or 'XhYmZs', and Dec 'XXdYYmZZs', '-XXdYYmZZs', 'XdYmZs' or '-XdYmZs'",
                str)
            ));
        }

        // Construct the number strings
        std::string s_hours_or_degrees = str.substr(0, h_or_d_index);
        std::string s_minutes = str.substr(h_or_d_index + 1, m_index - h_or_d_index - 1);
        std::string s_seconds = str.substr(m_index + 1, s_index - m_index - 1);
        if (!isStringNumber(s_hours_or_degrees) || !isStringNumber(s_minutes) ||
            !isStringNumber(s_seconds))
        {
            throw(ghoul::lua::LuaRuntimeException(fmt::format(
                "Ra or Dec '{}' format is incorrect. Correct format is: Ra 'XXhYYmZZs' "
                "or 'XhYmZs', and Dec 'XXdYYmZZs', '-XXdYYmZZs', 'XdYmZs' or '-XdYmZs'",
                str)
            ));
        }

        // Convert the strings to numbers
        hours_or_degrees = std::stod(s_hours_or_degrees);
        minutes = std::stod(s_minutes);
        seconds = std::stod(s_seconds);
    }

    void parseRa(const std::string& ra, double& hours, double& minutes, double& seconds) {
        if (ra.find('d') != std::string::npos) {
            throw(ghoul::lua::LuaRuntimeException(fmt::format(
                "Ra '{}' format is incorrect. Correct format is: Ra 'XXhYYmZZs' or "
                "'XhYmZs'", ra)
            ));
        }
        parseString(ra, hours, minutes, seconds);
    }

    void parseDec(const std::string& dec, double& degrees, double& minutes,
                  double& seconds)
    {
        if (dec.find('h') != std::string::npos) {
            throw(ghoul::lua::LuaRuntimeException(fmt::format(
                "Dec '{}' format is incorrect. Correct format is: "
                "Dec 'XXdYYmZZs', '-XXdYYmZZs', 'XdYmZs' or '-XdYmZs'", dec)
            ));
        }
        parseString(dec, degrees, minutes, seconds);
    }

    bool isRaDecValid(double ra_h, double ra_m, double ra_s, double dec_d,
                      double dec_m, double dec_s)
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
    // https://www.atnf.csiro.au/people/Tobias.Westmeier/tools_coords.php,

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

// ra format "XXhYYmZZs" or "XhYmZs"
// dec format "XXdYYmZZs", "-XXdYYmZZs", "XdYmZs" or "-XdYmZs"
glm::dvec2 icrsToDecimalDegrees(const std::string& ra, const std::string& dec) {
    if (ra.size() > 9 || ra.size() < 6 || dec.size() > 10 || dec.size() < 6) {
        throw(ghoul::lua::LuaRuntimeException(fmt::format(
            "Ra '{}' or Dec '{}' format is incorrect. Correct format is: Ra 'XXhYYmZZs' "
            "or 'XhYmZs', and Dec 'XXdYYmZZs', '-XXdYYmZZs', 'XdYmZs' or '-XdYmZs'",
            ra, dec)
        ));
    }

    // Parse right ascension
    double ra_hours, ra_minutes, ra_seconds;
    parseRa(ra, ra_hours, ra_minutes, ra_seconds);

    // Parse declination
    double dec_degrees, dec_minutes, dec_seconds;
    parseDec(dec, dec_degrees, dec_minutes, dec_seconds);

    if (!isRaDecValid(ra_hours, ra_minutes, ra_seconds, dec_degrees, dec_minutes,
        dec_seconds))
    {
        LWARNING(fmt::format("Ra '{}' or Dec '{}' is outside the allowed range",
            ra, dec)
        );
    }

    // Convert from hours, minutes, seconds to decimal degrees
    double sign = signbit(dec_degrees) ? -1.0 : 1.0;
    double ra_deg = (ra_hours * 15.0) +
        (ra_minutes * 15.0 / 60.0) +
        (ra_seconds * 15.0 / 3600.0);

    double dec_deg = (abs(dec_degrees) +
        (dec_minutes / 60.0) +
        (dec_seconds / 3600.0)) * sign;

    return glm::dvec2(ra_deg, dec_deg);
}

} // namespace openspace
