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

#ifndef __OPENSPACE_MODULE_SPACE___KEPLER___H__
#define __OPENSPACE_MODULE_SPACE___KEPLER___H__

#include <filesystem>
#include <string>
#include <vector>

namespace openspace::kepler {

struct Parameters {
    /// Some human-readable name for the object represented by this kepler parameter set
    std::string name;

    /// Some form of unique identifier for the object represented by this data
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

/**
 * Reads the object information from the provided \p file and returns them as individual
 * values.
 *
 * \param file The file to the TLE file. This file must be a valid file
 * \return Information about all of the contained objects in the \p file
 *
 * \pre \p file must be a file and must exist
 * \throw ghoul::RuntimeError If the provided \p file is not a valid TLE file
 */
std::vector<Parameters> readTleFile(const std::filesystem::path& file);

/**
 * Reads the object information from the provided \p file and returns them as individual
 * values.
 *
 * \param file The file to the OMM file. This file must be a valid file
 * \return Information about all of the contained objects in the \p file
 *
 * \pre \p file must be a file and must exist
 * \throw ghoul::RuntimeError If the provided \p file is not a valid OMM file
 */
std::vector<Parameters> readOmmFile(const std::filesystem::path& file);

/**
 * Reads the object information from a CSV file following JPL's Small Body Database
 * format, which provides the Epoch, eccentricity, semi-major axis (in AU), inclination,
 * ascending node, argument of periapsis, mean anomaly, and period in that order.
 *
 * \param file The CSV file containing the information about the objects
 * \return Information about all of the contained objects in the \p file
 *
 * \pre \p file must be a file and must exist
 * \throw ghoul::RuntimeError If the provided \p file is not a valid JPL SBDB CSV format
 */
std::vector<Parameters> readSbdbFile(const std::filesystem::path& file);

/**
 * Reads the object information from a data file provided by the Minor Planet Center. Any
 * possible header in the file is ignored.
 *
 * \param file The DAT file contained the ephemerides information
 * \return Information about all of the contained objects in the \p file
 *
 * \pre \p file must be a file and must exist
 * \throw ghoul::RuntimeError If the provided \p file is not a valid MPC DAT file
 */
std::vector<Parameters> readMpcFile(const std::filesystem::path& file);

/**
 * The different formats that the readFile function is capable of loading.
 */
enum class Format {
    TLE,  //< Two-line elements
    OMM,  //< Orbit Mean-Elements Message
    SBDB, //< Small-Body Database
    MPC   //< Minor Planet Center 
};
/**
 * Reads the object information from the provided file.
 *
 * \param file The file containing the information about the objects
 * \param format The format of the provided \p file
 * \return Information about all of the contained objects in the \p file
 *
 * \pre \p file must be a file and must exist
 * \throw ghoul::RuntimeError If the provided \p is not in the provided file
 */
std::vector<Parameters> readFile(std::filesystem::path file, Format format);

} // namespace openspace::kepler

#endif // __OPENSPACE_MODULE_SPACE___KEPLER___H__
