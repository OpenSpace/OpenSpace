/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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
#include <string>
#include <vector>

namespace openspace::exoplanets {

struct ExoplanetDataEntry {
    float a;            // Orbital semi-major axis in AU
    double aUpper;      // Upper uncertainty of orbital semi-major axis
    double aLower;      // Lower uncertainty of orbital semi-major axis
    float bigOmega;     // Longitude of ascending node in degrees
    float bigOmegaUpper; // Upper uncertainty of longitude of ascending node
    float bigOmegaLower; // Lower uncertainty of longitude of ascending node
    bool binary;        // Star known to be binary?
    float bmv;          // B − V color
    float ecc;          // Orbital eccentricity
    float eccUpper;     // Upper uncertainty of orbital eccentricity
    float eccLower;     // Lower uncertainty of orbital eccentricity
    float i;            // Orbital inclination in degrees (for transiting systems only)
    float iUpper;       // Upper uncertainty of orbital inclination
    float iLower;       // Lower uncertainty of orbital inclination
    int nComp;          // Number of planetary companions known
    float omega;        // Argument of periastron in degrees
    float omegaUpper;   // Upper uncertainty of argument of periastron
    float omegaLower;   // Lower uncertainty of argument of periastron
    double per;         // Orbital period in days
    float perUpper;     // Upper uncertainty of period
    float perLower;     // Lower uncertainty of period
    double r;           // Radius of the planet in Jupiter radii
    double rUpper;      // Upper uncertainty of radius of the planet
    double rLower;      // Lower uncertainty of radius of the planet
    float rStar;        // Estimated radius of the star in solar radii
    float rStarUpper;   // Upper uncertainty of estimated star radius
    float rStarLower;   // Lower uncertainty of estimated star radius
    double tt;          // Epoch of transit center in HJD-2440000
    float ttUpper;      // Upper uncertainty of epoch of transit center
    float ttLower;      // Lower uncertainty of epoch of transit center
    float positionX;    // Star position's X-coordinate in parsec
    float positionY;    // Star position's Y-coordinate in parsec
    float positionZ;    // Star position's Z-coordinate in parsec
};

struct StarData {
    glm::vec3 position; // In parsec
    float radius;       // In solar radii
    float bvColorIndex;
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
glm::vec3 starColor(float bv);

glm::dmat4 computeOrbitPlaneRotationMatrix(float i, float bigom, float omega);

// Rotate the original coordinate system (where x is pointing to First Point of Aries)
// so that x is pointing from star to the sun.
glm::dmat3 computeSystemRotation(glm::dvec3 starPosition);

// Create an identifier without whitespaces
std::string createIdentifier(std::string name);

void sanitizeNameString(std::string& s);

} // namespace openspace::exoplanets

#endif // __OPENSPACE_MODULE_EXOPLANETS___EXOPLANETSHELPER___H__
