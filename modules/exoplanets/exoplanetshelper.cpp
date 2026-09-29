/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <modules/exoplanets/exoplanetshelper.h>

#include <modules/exoplanets/datastructure.h>
#include <modules/exoplanets/exoplanetsmodule.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/util/distanceconstants.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/timeconstants.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/stringhelper.h>
#include <glm/gtx/transform.hpp>
#include <algorithm>
#include <cmath>
#include <filesystem>
#include <fstream>
#include <memory>
#include <sstream>

namespace {
    constexpr std::string_view _loggerCat = "ExoplanetsModule";
} // namespace

namespace openspace {

bool isValidPosition(const glm::vec3& pos) {
    return !glm::any(glm::isnan(pos));
}

bool hasSufficientData(const ExoplanetDataEntry& p) {
    const glm::vec3 starPosition = glm::vec3(p.positionX, p.positionY, p.positionZ);

    const bool validStarPosition = isValidPosition(starPosition);
    const bool hasSemiMajorAxis = !std::isnan(p.a);
    const bool hasOrbitalPeriod = !std::isnan(p.per);

    return validStarPosition && hasSemiMajorAxis && hasOrbitalPeriod;
}

glm::vec3 computeStarColor(float bv) {
    const ExoplanetsModule* module = global::moduleEngine->module<ExoplanetsModule>();
    const std::filesystem::path bvColormapPath = module->bvColormapPath();

    std::ifstream colorMap = std::ifstream(absPath(bvColormapPath), std::ios::in);

    if (!colorMap.good()) {
        LERROR(std::format("Failed to open colormap data file '{}'", bvColormapPath));
        return glm::vec3(0.f);
    }

    // Interpret the colormap cmap file
    std::string line;
    while (ghoul::getline(colorMap, line)) {
        if (line.empty() || (line[0] == '#')) {
            continue;
        }
        break;
    }

    // The first line is the width of the image, i.e number of values
    std::istringstream ss = std::istringstream(line);
    int nValues = 0;
    ss >> nValues;

    // Find the line matching the input B-V value (B-V is in [-0.4,2.0])
    const int t = static_cast<int>(round(((bv + 0.4) / (2.0 + 0.4)) * (nValues - 1)));
    std::string color;
    for (int i = 0; i < t + 1; i++) {
        ghoul::getline(colorMap, color);
    }

    std::istringstream colorStream = std::istringstream(color);
    glm::vec3 rgb;
    colorStream >> rgb.r >> rgb.g >> rgb.b;
    return rgb;
}

glm::dmat4 computeOrbitPlaneRotationMatrix(float i, float bigom, float omega) {
    // Exoplanet defined inclination changed to be used as Kepler defined inclination
    const glm::dvec3 ascendingNodeAxisRot = glm::dvec3(0.0, 0.0, 1.0);
    const glm::dvec3 inclinationAxisRot = glm::dvec3(1.0, 0.0, 0.0);
    const glm::dvec3 argPeriapsisAxisRot = glm::dvec3(0.0, 0.0, 1.0);

    const double asc = glm::radians(bigom);
    const double inc = glm::radians(i);
    const double per = glm::radians(omega);

    const glm::dmat4 orbitPlaneRotation =
        glm::rotate(asc, glm::dvec3(ascendingNodeAxisRot)) *
        glm::rotate(inc, glm::dvec3(inclinationAxisRot)) *
        glm::rotate(per, glm::dvec3(argPeriapsisAxisRot));

    return orbitPlaneRotation;
}

std::pair<float, glm::vec2> computeStellarInclination(float vsini, float vsiniLower,
                                                      float vsiniUpper, float radius,
                                                      float radiusLower,
                                                      float radiusUpper,
                                                      float rotationPeriod,
                                                      float rotLower,
                                                      float rotUpper)
{
    if (std::isnan(vsini) || std::isnan(radius) || std::isnan(rotationPeriod) ||
        vsini <= 0.f || radius <= 0.f || rotationPeriod <= 0.f)
    {
        return {
            std::numeric_limits<float>::quiet_NaN(),
            glm::vec2(std::numeric_limits<float>::quiet_NaN())
        };
    }

    // Radius in km: (radius * SolarRadius) / 1000.0
    // Period in seconds: rotationPeriod * SecondsPerDay
    // Equatorial velocity veq in km/s = 2 * pi * radius_km / period_s
    const double radiusKm =
        (static_cast<double>(radius) * distanceconstants::SolarRadius) / 1000.0;
    const double periodSeconds =
        static_cast<double>(rotationPeriod) * timeconstants::SecondsPerDay;
    const double veq = (2.0 * glm::pi<double>() * radiusKm) / periodSeconds;

    if (veq <= 0.0) {
        return {
            std::numeric_limits<float>::quiet_NaN(),
            glm::vec2(std::numeric_limits<float>::quiet_NaN())
        };
    }

    const double sinI = static_cast<double>(vsini) / veq;
    const double clampedSinI = std::clamp(sinI, 0.0, 1.0);
    const float inclination = static_cast<float>(std::asin(clampedSinI));

    // Uncertainty propagation on sin(i) = (vsini * P_rot) / (2 * pi * R_*):
    // Fractional uncertainties add in quadrature for each asymmetric bound:
    // - Upper bound combines vsiniUpper, rotUpper, and radiusLower (increases sin(i))
    // - Lower bound combines vsiniLower, rotLower, and radiusUpper (decreases sin(i))
    auto squaredError = [](float value, float error) -> double {
        return std::pow(static_cast<double>(error) / static_cast<double>(value), 2.0);
    };

    double fUpperSq = 0.0;
    bool hasUpperError = false;
    if (!std::isnan(vsiniUpper) && vsiniUpper > 0.f) {
        fUpperSq += squaredError(vsini, vsiniUpper);
        hasUpperError = true;
    }
    if (!std::isnan(rotUpper) && rotUpper > 0.f) {
        fUpperSq += squaredError(rotationPeriod, rotUpper);
        hasUpperError = true;
    }
    if (!std::isnan(radiusLower) && radiusLower > 0.f) {
        fUpperSq += squaredError(radius, radiusLower);
        hasUpperError = true;
    }

    double fLowerSq = 0.0;
    bool hasLowerError = false;
    if (!std::isnan(vsiniLower) && vsiniLower > 0.f) {
        fLowerSq +=
            squaredError(vsini, vsiniLower);
        hasLowerError = true;
    }
    if (!std::isnan(rotLower) && rotLower > 0.f) {
        fLowerSq += squaredError(rotationPeriod, rotLower);
        hasLowerError = true;
    }
    if (!std::isnan(radiusUpper) && radiusUpper > 0.f) {
        fLowerSq += squaredError(radius, radiusUpper);
        hasLowerError = true;
    }

    glm::vec2 incError = glm::vec2(std::numeric_limits<float>::quiet_NaN());
    if (hasLowerError) {
        const double sigmaSinLower = sinI * std::sqrt(fLowerSq);
        const double sinILower = std::clamp(sinI - sigmaSinLower, 0.0, 1.0);
        incError.x = static_cast<float>(inclination - std::asin(sinILower));
    }
    if (hasUpperError) {
        const double sigmaSinUpper = sinI * std::sqrt(fUpperSq);
        const double sinIUpper = std::clamp(sinI + sigmaSinUpper, 0.0, 1.0);
        incError.y = static_cast<float>(std::asin(sinIUpper) - inclination);
    }

    return { inclination, incError };
}

glm::dmat3 computeSystemRotation(const glm::dvec3& starPosition) {
    const glm::dvec3 sunPosition = glm::dvec3(0.0);
    const glm::dvec3 starToSunVec = glm::normalize(sunPosition - starPosition);
    const glm::dvec3 galacticNorth = glm::dvec3(0.0, 0.0, 1.0);

    const glm::dmat3 galacticToCelestialMatrix =
        SpiceManager::ref().positionTransformMatrix("GALACTIC", "J2000", 0.0);

    const glm::dvec3 celestialNorth = glm::normalize(
        galacticToCelestialMatrix * galacticNorth
    );

    // Earth's north vector projected onto the skyplane, the plane perpendicular to the
    // viewing vector (starToSunVec)
    const float celestialAngle = static_cast<float>(glm::dot(
        celestialNorth,
        starToSunVec
    ));
    const glm::dvec3 northProjected = glm::normalize(
        celestialNorth - (celestialAngle / glm::length(starToSunVec)) * starToSunVec
    );

    const glm::dvec3 beta = glm::normalize(glm::cross(starToSunVec, northProjected));

    return glm::dmat3(
        northProjected.x,
        northProjected.y,
        northProjected.z,
        beta.x,
        beta.y,
        beta.z,
        starToSunVec.x,
        starToSunVec.y,
        starToSunVec.z
    );
}

void sanitizeNameString(std::string& s) {
    // We want to avoid quotes and apostrophes in names, since they cause problems when a
    // string is translated to a script call
    s.erase(remove(s.begin(), s.end(), '\"'), s.end());
    s.erase(remove(s.begin(), s.end(), '\''), s.end());
}

void updateStarDataFromNewPlanet(StarData& starData, const ExoplanetDataEntry& p) {
    const glm::vec3 pos = glm::vec3(p.positionX, p.positionY, p.positionZ);
    if (starData.position != pos && isValidPosition(pos)) {
        starData.position = pos;
    }
    if (starData.radius != p.rStar && !std::isnan(p.rStar)) {
        starData.radius = p.rStar;
    }
    if (starData.bv != p.bmv && !std::isnan(p.bmv)) {
        starData.bv = p.bmv;
    }
    if (starData.teff != p.teff && !std::isnan(p.teff)) {
        starData.teff = p.teff;
    }
    if (starData.luminosity != p.luminosity && !std::isnan(p.luminosity)) {
        starData.luminosity = p.luminosity;
    }
    if (starData.rotationPeriod != p.starRotationPeriod && !std::isnan(p.starRotationPeriod)) {
        starData.rotationPeriod = p.starRotationPeriod;
    }
    if (starData.vsini != p.starVsini && !std::isnan(p.starVsini)) {
        starData.vsini = p.starVsini;
    }

    if (!std::isnan(starData.vsini) && !std::isnan(starData.radius) &&
        !std::isnan(starData.rotationPeriod))
    {
        const float vLower = !std::isnan(p.starVsiniLower) ? p.starVsiniLower :
            std::numeric_limits<float>::quiet_NaN();
        const float vUpper = !std::isnan(p.starVsiniUpper) ? p.starVsiniUpper :
            std::numeric_limits<float>::quiet_NaN();
        const float rLower = !std::isnan(p.rStarLower) ? p.rStarLower :
            std::numeric_limits<float>::quiet_NaN();
        const float rUpper = !std::isnan(p.rStarUpper) ? p.rStarUpper :
            std::numeric_limits<float>::quiet_NaN();
        const float pLower = !std::isnan(p.starRotationPeriodLower) ? p.starRotationPeriodLower :
            std::numeric_limits<float>::quiet_NaN();
        const float pUpper = !std::isnan(p.starRotationPeriodUpper) ? p.starRotationPeriodUpper :
            std::numeric_limits<float>::quiet_NaN();

        auto [inc, incErr] = computeStellarInclination(
            starData.vsini, vLower, vUpper,
            starData.radius, rLower, rUpper,
            starData.rotationPeriod, pLower, pUpper
        );
        starData.inclination = inc;
        starData.inclinationError = incErr;
    }
}

} // namespace openspace
