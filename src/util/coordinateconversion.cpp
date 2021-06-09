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

} // namespace openspace
