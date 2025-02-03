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

#ifndef __OPENSPACE_CORE___COORDINATECONVERSION___H__
#define __OPENSPACE_CORE___COORDINATECONVERSION___H__

#include <ghoul/glm.h>
#include <string>

namespace openspace {

/**
 * Converts from ICRS decimal degrees coordinates to galactic cartesian coordinates.
 *
 * \param ra Right ascension, given in decimal degrees
 * \param dec Declination, given in decimal degrees
 * \param distance The distance, or radius, to the position given in any unit
 * \return A position in galactic cartesian coordinates, given in the same unit as the
 *         distance parameter
 */
glm::dvec3 icrsToGalacticCartesian(double ra, double dec, double distance);

/**
 * Converts from ICRS (hms and dms) coordinates to decimal degrees.
 *
 * \param ra Right ascension, given as a string in format 'XhYmZs'
 * \param dec Declination, given as a string in format 'XdYmZs'
 * \return The decimal degrees coordinate in degrees
 */
glm::dvec2 icrsToDecimalDegrees(const std::string& ra, const std::string& dec);

/**
 * Converts from galactic cartesian coordinates to ICRS decimal degrees coordinates
 * and distance.
 *
 * \param x, y, z X, Y, and Z coordinates
 * \return A vector with the ra and dec decimal degrees in degrees and distance
 */
glm::dvec3 galacticCartesianToIcrs(double x, double y, double z);

/**
 * Converts from ICRS decimal degrees coordinates to ICRS hms and dms coordinates.
 *
 * \param ra Right ascension, given in decimal degrees
 * \param dec Declination, given in decimal degrees
 * \return A pair with the ra and dec strings in hms and dms format
 */
std::pair<std::string, std::string> decimalDegreesToIcrs(double ra, double dec);

} // namespace openspace

#endif // __OPENSPACE_CORE___COORDINATECONVERSION___H__
