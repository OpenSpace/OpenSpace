/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#ifndef __OPENSPACE_MODULE_EXOPLANETS___EXOPLANETSHELPER___H__
#define __OPENSPACE_MODULE_EXOPLANETS___EXOPLANETSHELPER___H__

#include <ghoul/glm.h>
#include <optional>
#include <string>
#include <vector>

namespace {
    constexpr float fNaN = std::numeric_limits<float>::quiet_NaN();
    constexpr float dNaN = std::numeric_limits<double>::quiet_NaN();
    constexpr float iNaN = std::numeric_limits<int>::quiet_NaN();
} // namespace

namespace openspace::exoplanets {

struct ExoplanetDataEntry {
    float a = fNaN;            // Orbital semi-major axis in AU
    double aUpper = dNaN;      // Upper uncertainty of orbital semi-major axis
    double aLower = dNaN;      // Lower uncertainty of orbital semi-major axis
    float bigOmega = fNaN;     // Longitude of ascending node in degrees
    float bigOmegaUpper = fNaN; // Upper uncertainty of longitude of ascending node
    float bigOmegaLower = fNaN; // Lower uncertainty of longitude of ascending node
    bool binary;                // Star known to be binary?
    float bmv = fNaN;          // Star B − V color
    float ecc = fNaN;          // Orbital eccentricity
    float eccUpper = fNaN;     // Upper uncertainty of orbital eccentricity
    float eccLower = fNaN;     // Lower uncertainty of orbital eccentricity
    float i = fNaN;            // Orbital inclination in degrees (for transiting systems only)
    float iUpper = fNaN;       // Upper uncertainty of orbital inclination
    float iLower = fNaN;       // Lower uncertainty of orbital inclination
    int nPlanets = iNaN;       // Number of known planets in the planetary system
    int nStars = iNaN;         // Number of stars in the planetary system
    float omega = fNaN;        // Argument of periastron in degrees
    float omegaUpper = fNaN;   // Upper uncertainty of argument of periastron
    float omegaLower = fNaN;   // Lower uncertainty of argument of periastron
    double per = dNaN;         // Orbital period in days
    float perUpper = fNaN;     // Upper uncertainty of period
    float perLower = fNaN;     // Lower uncertainty of period
    double r = dNaN;           // Radius of the planet in Jupiter radii
    double rUpper = dNaN;      // Upper uncertainty of radius of the planet
    double rLower = dNaN;      // Lower uncertainty of radius of the planet
    float rStar = fNaN;        // Estimated radius of the star in solar radii
    float rStarUpper = fNaN;   // Upper uncertainty of estimated star radius
    float rStarLower = fNaN;   // Lower uncertainty of estimated star radius
    float luminosity = fNaN;   // Star luminosity, in units of solar luminosities
    float luminosityUpper = fNaN; // Upper uncertainty of star luminosity
    float luminosityLower = fNaN; // Lower uncertainty of star luminosity
    float teff = fNaN;         // Star's effective temperature in Kelvin
    float teffUpper = fNaN;    // Upper uncertainty of effective temperature
    float teffLower = fNaN;    // Lower uncertainty of effective temperature
    double tt = dNaN;          // Epoch of transit center in HJD-2440000
    float ttUpper = fNaN;      // Upper uncertainty of epoch of transit center
    float ttLower = fNaN;      // Lower uncertainty of epoch of transit center

    float positionX = fNaN;    // Star position's X-coordinate in parsec
    float positionY = fNaN;    // Star position's Y-coordinate in parsec
    float positionZ = fNaN;    // Star position's Z-coordinate in parsec
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
};

bool isValidPosition(const glm::vec3& pos);

// Check if the exoplanet p has sufficient data for visualization
bool hasSufficientData(const ExoplanetDataEntry& p);

// Compute star color in RGB from b-v color index
glm::vec3 computeStarColor(float bv);

glm::dmat4 computeOrbitPlaneRotationMatrix(float i, float bigom = 180.f,
    float omega = 90.f);

// Rotate the original coordinate system (where x is pointing to First Point of Aries)
// so that x is pointing from star to the sun.
glm::dmat3 computeSystemRotation(glm::dvec3 starPosition);

void sanitizeNameString(std::string& s);

// Star data should not vary between planets, but one data entry might lack data
// for the host star while another does not. So for every planet, update star data
// if needed
void updateStarDataFromNewPlanet(StarData& starData, const ExoplanetDataEntry& p);

} // namespace openspace::exoplanets

#endif // __OPENSPACE_MODULE_EXOPLANETS___EXOPLANETSHELPER___H__
