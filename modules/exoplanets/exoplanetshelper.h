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

#ifndef __OPENSPACE_MODULE_EXOPLANETS___EXOPLANETSHELPER___H__
#define __OPENSPACE_MODULE_EXOPLANETS___EXOPLANETSHELPER___H__

#include <ghoul/glm.h>
#include <ghoul/misc/dictionary.h>
#include <string>

namespace openspace::exoplanets {

struct ExoplanetDataEntry;
struct StarData;

bool isValidPosition(const glm::vec3& pos);

/**
 * Check if the exoplanet p has sufficient data for visualization.
 */
bool hasSufficientData(const ExoplanetDataEntry& p);

/**
 * Compute star color in RGB from b-v color index.
 */
glm::vec3 computeStarColor(float bv);

glm::dmat4 computeOrbitPlaneRotationMatrix(float i, float bigom = 180.f,
    float omega = 90.f);

/**
 * Rotate the original coordinate system (where x is pointing to First Point of Aries) so
 * that x is pointing from star to the Sun.
 */
glm::dmat3 computeSystemRotation(const glm::dvec3& starPosition);

void sanitizeNameString(std::string& s);

/**
 * Star data should not vary between planets, but one data entry might lack data for the
 * host star while another does not. So for every planet, update star data if needed.
 */
void updateStarDataFromNewPlanet(StarData& starData, const ExoplanetDataEntry& p);

} // namespace openspace::exoplanets

#endif // __OPENSPACE_MODULE_EXOPLANETS___EXOPLANETSHELPER___H__
