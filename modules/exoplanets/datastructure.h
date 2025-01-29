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

#ifndef __OPENSPACE_MODULE_EXOPLANETS___DATASTRUCTURE___H__
#define __OPENSPACE_MODULE_EXOPLANETS___DATASTRUCTURE___H__

#include <openspace/documentation/documentation.h>
#include <ghoul/glm.h>
#include <ghoul/misc/dictionary.h>
#include <optional>
#include <string>
#include <vector>

namespace openspace::exoplanets {

struct ExoplanetDataEntry {
    /// Orbital semi-major axis in AU
    float a = std::numeric_limits<float>::quiet_NaN();
    /// Upper uncertainty of orbital semi-major axis
    double aUpper = std::numeric_limits<double>::quiet_NaN();
    /// Lower uncertainty of orbital semi-major axis
    double aLower = std::numeric_limits<double>::quiet_NaN();

    /// Longitude of ascending node in degrees
    float bigOmega = std::numeric_limits<float>::quiet_NaN();
    /// Upper uncertainty of longitude of ascending node
    float bigOmegaUpper = std::numeric_limits<float>::quiet_NaN();
    /// Lower uncertainty of longitude of ascending node
    float bigOmegaLower = std::numeric_limits<float>::quiet_NaN();

    /// Star known to be binary?
    bool binary = false;

    /// Star B − V color
    float bmv = std::numeric_limits<float>::quiet_NaN();

    /// Orbital eccentricity
    float ecc = std::numeric_limits<float>::quiet_NaN();
    /// Upper uncertainty of orbital eccentricity
    float eccUpper = std::numeric_limits<float>::quiet_NaN();
    /// Lower uncertainty of orbital eccentricity
    float eccLower = std::numeric_limits<float>::quiet_NaN();

    /// Orbital inclination in degrees (for transiting systems only)
    float i = std::numeric_limits<float>::quiet_NaN();
    /// Upper uncertainty of orbital inclination
    float iUpper = std::numeric_limits<float>::quiet_NaN();
    /// Lower uncertainty of orbital inclination
    float iLower = std::numeric_limits<float>::quiet_NaN();

    /// Number of known planets in the planetary system
    int nPlanets = std::numeric_limits<int>::quiet_NaN();

    /// Number of stars in the planetary system
    int nStars = std::numeric_limits<int>::quiet_NaN();

    /// Argument of periastron in degrees
    float omega = std::numeric_limits<float>::quiet_NaN();
    /// Upper uncertainty of argument of periastron
    float omegaUpper = std::numeric_limits<float>::quiet_NaN();
    /// Lower uncertainty of argument of periastron
    float omegaLower = std::numeric_limits<float>::quiet_NaN();

    /// Orbital period in days
    double per = std::numeric_limits<double>::quiet_NaN();
    /// Upper uncertainty of period
    float perUpper = std::numeric_limits<float>::quiet_NaN();
    /// Lower uncertainty of period
    float perLower = std::numeric_limits<float>::quiet_NaN();

    /// Radius of the planet in Jupiter radii
    double r = std::numeric_limits<double>::quiet_NaN();
    /// Upper uncertainty of radius of the planet
    double rUpper = std::numeric_limits<double>::quiet_NaN();
    /// Lower uncertainty of radius of the planet
    double rLower = std::numeric_limits<double>::quiet_NaN();

    /// Estimated radius of the star in solar radii
    float rStar = std::numeric_limits<float>::quiet_NaN();
    /// Upper uncertainty of estimated star radius
    float rStarUpper = std::numeric_limits<float>::quiet_NaN();
    /// Lower uncertainty of estimated star radius
    float rStarLower = std::numeric_limits<float>::quiet_NaN();

    /// Star luminosity, in units of solar luminosities
    float luminosity = std::numeric_limits<float>::quiet_NaN();
    /// Upper uncertainty of star luminosity
    float luminosityUpper = std::numeric_limits<float>::quiet_NaN();
    /// Lower uncertainty of star luminosity
    float luminosityLower = std::numeric_limits<float>::quiet_NaN();

    /// Star's effective temperature in Kelvin
    float teff = std::numeric_limits<float>::quiet_NaN();
    /// Upper uncertainty of effective temperature
    float teffUpper = std::numeric_limits<float>::quiet_NaN();
    /// Lower uncertainty of effective temperature
    float teffLower = std::numeric_limits<float>::quiet_NaN();

    /// Epoch of transit center in HJD-2440000
    double tt = std::numeric_limits<double>::quiet_NaN();
    /// Upper uncertainty of epoch of transit center
    float ttUpper = std::numeric_limits<float>::quiet_NaN();
    /// Lower uncertainty of epoch of transit center
    float ttLower = std::numeric_limits<float>::quiet_NaN();

    /// Star position's X-coordinate in parsec
    float positionX = std::numeric_limits<float>::quiet_NaN();
    /// Star position's Y-coordinate in parsec
    float positionY = std::numeric_limits<float>::quiet_NaN();
    /// Star position's Z-coordinate in parsec
    float positionZ = std::numeric_limits<float>::quiet_NaN();
};

struct StarData {
    glm::vec3 position = glm::vec3(std::numeric_limits<float>::quiet_NaN()); // In parsec
    float radius = std::numeric_limits<float>::quiet_NaN(); // In solar radii
    float bv = std::numeric_limits<float>::quiet_NaN();
    float teff = std::numeric_limits<float>::quiet_NaN(); // In Kelvin
    float luminosity = std::numeric_limits<float>::quiet_NaN(); // In solar luminosities
};

struct ExoplanetSystem {
    std::string starName;
    StarData starData;
    std::vector<std::string> planetNames;
    std::vector<ExoplanetDataEntry> planetsData;

    ghoul::Dictionary toDataDictionary() const;
    static documentation::Documentation Documentation();
};

} // namespace openspace::exoplanets

#endif // __OPENSPACE_MODULE_EXOPLANETS___DATASTRUCTURE___H__
