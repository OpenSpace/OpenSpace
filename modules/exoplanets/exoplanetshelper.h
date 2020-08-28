/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#ifndef __OPENSPACE_MODULE_EXOPLANETS___EXOPLANET_HELPER___H__
#define __OPENSPACE_MODULE_EXOPLANETS___EXOPLANET_HELPER___H__

#include <string>

namespace openspace::exoplanets {

struct Exoplanet {
    float A;            // Orbital semi-major axis in AU
    double AUPPER;      // Upper uncertainty of orbital semi-major axis
    double ALOWER;      // Lower uncertainty of orbital semi-major axis
    double UA;          // Uncertainty of orbital semi-major axis
    float BIGOM;        // Longitude of ascending node in degrees
    float BIGOMUPPER;   // Upper uncertainty of longitude of ascending node
    float BIGOMLOWER;   // Lower uncertainty of longitude of ascending node
    float UBIGOM;       // Uncertainty of longitude of ascending node
    bool BINARY;        // Star known to be binary?
    float BMV;          // B − V color
    float ECC;          // Orbital eccentricity
    float ECCUPPER;     // Upper uncertainty of orbital eccentricity
    float ECCLOWER;     // Lower uncertainty of orbital eccentricity
    float UECC;         // Uncertainty of orbital eccentricity
    float I;            // Orbital inclination in degrees (for transiting systems only)
    float IUPPER;       // Upper uncertainty of orbital inclination
    float ILOWER;       // Lower uncertainty of orbital inclination
    float UI;           // Uncertainty of orbital inclination
    int NCOMP;          // Number of planetary companions known
    float OM;           // Argument of periastron in degrees
    float OMUPPER;      // Upper uncertainty of argument of periastron
    float OMLOWER;      // Lower uncertainty of argument of periastron
    float UOM;          // Uncertainty of argument of periastron
    double PER;         // Orbital period in days
    float PERUPPER;     // Upper uncertainty of period
    float PERLOWER;     // Lower uncertainty of period
    float UPER;         // Uncertainty of period
    double R;           // Radius of the planet in Jupiter radii
    double RUPPER;      // Upper uncertainty of radius of the planet
    double RLOWER;      // Lower uncertainty of radius of the planet
    double UR;          // Uncertainty of radius of the planet
    float RSTAR;        // Estimated radius of the star in solar radii
    float RSTARUPPER;   // Upper uncertainty of estimated star radius
    float RSTARLOWER;   // Lower uncertainty of estimated star radius
    float URSTAR;       // Uncertainty of estimated star radius
    double TT;          // Epoch of transit center in HJD-2440000
    float TTUPPER;      // Upper uncertainty of epoch of transit center
    float TTLOWER;      // Lower uncertainty of epoch of transit center
    float UTT;          // Uncertainty of epoch of transit center
    float POSITIONX;    // Star position's X-coordinate in parsec
    float POSITIONY;    // Star position's Y-coordinate in parsec
    float POSITIONZ;    // Star position's Z-coordinate in parsec
};

// Convert csv-file specific names to the corresponding name in the speck data file
std::string getSpeckStarName(std::string name);

// Convert speck-file specific names to the corresponding name in the csv data file
std::string getCsvStarName(std::string name);

} // namespace openspace::exoplanets

#endif // __OPENSPACE_MODULE_EXOPLANETS___EXOPLANETSMODULE___H__
