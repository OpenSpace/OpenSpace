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

namespace openspace {

// Convert Equatorial coordinates ICRS right ascension and declination (a, d)
// into Galactic coordinates (l, b)
glm::dvec3 icrsToGalacticCartesian(float ra, float dec, double distance) {
    // Reference:
    // https://www.atnf.csiro.au/people/Tobias.Westmeier/tools_coords.php,

    // (Ra, Dec) -> (a, d)
    float a = glm::radians(ra);
    float d = glm::radians(dec);

    // J2000 Galactic reference frame
    constexpr float a0 = glm::radians(192.8595f); // Equatorial coordinates of the Galactic north pole
    constexpr float d0 = glm::radians(27.1284f);
    constexpr float l0 = glm::radians(122.9320f); // Galactic longitude of the equatorial north pole

    // Convert to galactic reference frame
    float l = l0 - atan2(
        cos(d) * sin(a - a0),
        sin(d) * cos(d0) - cos(d) * sin(d0) * cos(a - a0)
    );
    float b = asin(sin(d) * sin(d0) + cos(d) * cos(d0) * cos(a - a0));

    // Convert to cartesian
    glm::dvec3 rGalactic = glm::dvec3(
        cos(b) * cos(l),
        cos(b) * sin(l),
        sin(b)
    );

    return distance * rGalactic;
}

// ra format "XXhYYmZZs" or "XhYmZs"
// dec format "+XXdYYmZZs", "-XXdYYmZZs" or "-XdYmZs"
glm::dvec2 icrsToDecimalDegrees(std::string ra, std::string dec) {
    if (ra.size() > 9 || ra.size() < 6 || dec.size() > 10 || dec.size() < 7) {
        // Stirngs too big or small
    }

    // Parse right ascension
    size_t ra_h_index = ra.find('h');
    size_t ra_m_index = ra.find('m');
    size_t ra_s_index = ra.find('s');
    if (ra_h_index == std::string::npos || ra_m_index == std::string::npos ||
        ra_s_index == std::string::npos)
    {
        // Format not correct
    }

    float ra_hours = std::stof(ra.substr(0, ra_h_index));
    float ra_minutes = std::stof(ra.substr(ra_h_index + 1, ra_m_index - ra_h_index - 1));
    float ra_seconds = std::stof(ra.substr(ra_m_index + 1, ra_s_index - ra_m_index - 1));

    // Parse declination
    size_t dec_d_index = dec.find('d');
    size_t dec_m_index = dec.find('m');
    size_t dec_s_index = dec.find('s');
    if (dec_d_index == std::string::npos || dec_m_index == std::string::npos ||
        dec_s_index == std::string::npos)
    {
        // Format not correct
    }

    float dec_degrees = std::stof(dec.substr(0, dec_d_index));
    float dec_minutes = std::stof(dec.substr(dec_d_index + 1, dec_m_index - dec_d_index - 1));
    float dec_seconds = std::stof(dec.substr(dec_m_index + 1, dec_s_index - dec_m_index - 1));

    // Convert from hours, minutes, sewconds to degrees
    float sign = signbit(dec_degrees) ? -1.f : 1.f;
    float ra_deg = (ra_hours * 15.0) + (ra_minutes * 15 / 60.0) + (ra_seconds * 15 / 3600.0);
    float dec_deg = (abs(dec_degrees) + (dec_minutes / 60.0) + (dec_seconds / 3600.0)) * sign;

    return glm::dvec2(ra_deg, dec_deg);
}

} // namespace openspace
