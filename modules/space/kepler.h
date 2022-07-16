/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#ifndef __OPENSPACE_MODULE_SPACE___KEPLER___H__
#define __OPENSPACE_MODULE_SPACE___KEPLER___H__

#include <filesystem>
#include <string>
#include <vector>

namespace openspace {

struct SatelliteKeplerParameters {
    std::string name;
    // International Designator (launch year + launch number of the year + piece of launch
    std::string id;

    double inclination = 0.0;
    double semiMajorAxis = 0.0;
    double ascendingNode = 0.0;
    double eccentricity = 0.0;
    double argumentOfPeriapsis = 0.0;
    double meanAnomaly = 0.0;
    double epoch = 0.0;
    double period = 0.0;
};

double epochFromSubstring(const std::string& epochString);
double epochFromYMDdSubstring(const std::string& epochString);

/**
 * Reads the satellite information from the provided \p file and returns them as
 * individual values.
 *
 * \param file The file to the TLE file. This file must be a valid file
 * \return Information about all of the contained satellite information in the \p file
 *
 * \pre \p file must be a file and must exist
 * \throw ghoul::RuntimeError If the provided \p file is not a valid TLE file
 */
std::vector<SatelliteKeplerParameters> readTleFile(std::filesystem::path file);

/**
 * Reads the satellite information from the provided \p file and returns them as
 * individual values.
 *
 * \param file The file to the OMM file. This file must be a valid file
 * \return Information about all of the contained satellite information in the \p file
 *
 * \pre \p file must be a file and must exist
 * \throw ghoul::RuntimeError If the provided \p file is not a valid OMM file
 */
std::vector<SatelliteKeplerParameters> readOmmFile(std::filesystem::path file);

} // namespace openspace

#endif // __OPENSPACE_MODULE_SPACE___KEPLER___H__
