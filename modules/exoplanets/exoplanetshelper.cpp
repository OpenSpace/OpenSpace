/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <modules/exoplanets/exoplanetsmodule.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/scene/scene.h>
#include <openspace/util/distanceconstants.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>
#include <openspace/util/timeconversion.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/stringhelper.h>
#include <glm/gtx/quaternion.hpp>
#include <glm/gtx/transform.hpp>
#include <string_view>
#include <fstream>
#include <sstream>

namespace {
    constexpr std::string_view _loggerCat = "ExoplanetsModule";
} // namespace

namespace openspace::exoplanets {

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

    std::ifstream colorMap(absPath(bvColormapPath), std::ios::in);

    if (!colorMap.good()) {
        LERROR(std::format(
            "Failed to open colormap data file '{}'", absPath(bvColormapPath)
        ));
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
    std::istringstream ss(line);
    int nValues = 0;
    ss >> nValues;

    // Find the line matching the input B-V value (B-V is in [-0.4,2.0])
    const int t = static_cast<int>(round(((bv + 0.4) / (2.0 + 0.4)) * (nValues - 1)));
    std::string color;
    for (int i = 0; i < t + 1; i++) {
        ghoul::getline(colorMap, color);
    }

    std::istringstream colorStream(color);
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

glm::dmat3 computeSystemRotation(const glm::dvec3& starPosition) {
    const glm::dvec3 sunPosition = glm::dvec3(0.0, 0.0, 0.0);
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
    // We want to avoid quotes and apostrophes in names, since they cause problems
    // when a string is translated to a script call
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
}

// KeplerTranslation requires angles in range [0, 360]
float validAngle(float angle, float defaultValue, bool& usedDefault) {
    if (std::isnan(angle)) {
        usedDefault = true;
        return defaultValue;
    }
    if (angle < 0.f) { return angle + 360.f; }
    if (angle > 360.f) { return angle - 360.f; }
    return angle;
};

ghoul::Dictionary toDataDictionary(const ExoplanetSystem& system) {
    ghoul_assert(
        system.planetNames.size() == system.planetsData.size(),
        "The length of the planet names list must match the planet data list"
    );

    ghoul::Dictionary res;

    res.setValue("SystemId", makeIdentifier(system.starName));

    const glm::dvec3 starPosInParsec = static_cast<glm::dvec3>(system.starData.position);

    if (!isValidPosition(starPosInParsec)) {
        LERROR(std::format(
            "Insufficient data available for exoplanet system '{}'. Could not "
            "determine star position", system.starName
        ));
        return ghoul::Dictionary();
    }

    const glm::dvec3 starPos = starPosInParsec * distanceconstants::Parsec;
    res.setValue("Position", starPos);

    res.setValue("NumPlanets", static_cast<double>(system.planetNames.size()));

    std::string starName = system.starName;
    sanitizeNameString(starName);
    res.setValue("StarName", starName);

    if (!std::isnan(system.starData.radius)) {
        double radiusInMeter = distanceconstants::SolarRadius * system.starData.radius;
        res.setValue("StarRadius", radiusInMeter);
    }

    if (!std::isnan(system.starData.bv)) {
        res.setValue("StarColor", glm::dvec3(computeStarColor(system.starData.bv)));
    }

    if (!std::isnan(system.starData.teff)) {
        res.setValue("StarTeff", static_cast<double>(system.starData.teff));
    }

    if (!std::isnan(system.starData.luminosity)) {
        res.setValue("StarLuminosity", static_cast<double>(system.starData.luminosity));
    }

    ghoul::Dictionary planets;

    for (size_t i = 0; i < system.planetNames.size(); i++) {
        const ExoplanetDataEntry& data = system.planetsData[i];
        const std::string& name = system.planetNames[i];
        const std::string id = makeIdentifier(name);

        ghoul::Dictionary planet;

        planet.setValue("Id", id);
        planet.setValue("Name", name);

        bool canVisualize = hasSufficientData(data);
        planet.setValue("HasEnoughData", canVisualize);

        bool hasUsedDefaultValues = false;

        if (!canVisualize) {
            continue;
        }

        if (!std::isnan(data.r)) {
            double planetRadius = data.r * distanceconstants::JupiterRadius;
            planet.setValue("Radius", planetRadius);
        }

        // Orbit data
        {
            planet.setValue(
                "SemiMajorAxis",
                static_cast<double>(data.a) * distanceconstants::AstronomicalUnit
            );

            if (!std::isnan(data.ecc)) {
                planet.setValue("Eccentricity", static_cast<double>(data.ecc));
            }
            else {
                planet.setValue("Eccentricity", 0.0);
                hasUsedDefaultValues = true;
            }

            float inclination = validAngle(data.i, 90.f, hasUsedDefaultValues);
            float bigOmega = validAngle(data.bigOmega, 180.f, hasUsedDefaultValues);
            float omega = validAngle(data.omega, 90.f, hasUsedDefaultValues);

            planet.setValue("Inclination", static_cast<double>(inclination));
            planet.setValue("AscendingNode", static_cast<double>(bigOmega));
            planet.setValue("ArgumentOfPeriapsis", static_cast<double>(omega));

            std::string sEpoch;
            if (!std::isnan(data.tt)) {
                Time epoch;
                epoch.setTime("JD " + std::to_string(data.tt));
                sEpoch = std::string(epoch.ISO8601());
            }
            else {
                hasUsedDefaultValues = true;
                sEpoch = "2009-05-19T07:11:34.080";
            }
            planet.setValue("Epoch", sEpoch);
            planet.setValue("Period", data.per);

            bool hasUpperAUncertainty = !std::isnan(data.aUpper);
            bool hasLowerAUncertainty = !std::isnan(data.aLower);

            if (hasUpperAUncertainty && hasLowerAUncertainty) {
                const float lowerOffset = static_cast<float>(data.aLower / data.a);
                const float upperOffset = static_cast<float>(data.aUpper / data.a);
                planet.setValue(
                    "SemiMajorAxisUncertainty",
                    glm::dvec2(lowerOffset, lowerOffset)
                );
            }

            const glm::dmat4 rotation = computeOrbitPlaneRotationMatrix(
                inclination,
                bigOmega,
                omega
            );
            const glm::dmat3 rotationMat3 = static_cast<glm::dmat3>(rotation);
            planet.setValue("OrbitPlaneRotationMatrix", rotationMat3);

            planet.setValue("HasUsedDefaultValues", hasUsedDefaultValues);
        }

        planets.setValue(id, planet);
    }

    res.setValue("Planets", planets);

    // Extra stuff that is useful for rendering
    const glm::dmat3 exoplanetSystemRotation = computeSystemRotation(starPos);
    res.setValue("SystemRotation", exoplanetSystemRotation);

    float meanInclination = 0.f;
    for (const ExoplanetDataEntry& p : system.planetsData) {
        // Compute a valid inclination value (same as used for the dictionary)
        bool usedDefault = false; // Dummy value
        meanInclination += validAngle(p.i, 90.f, usedDefault);
    }
    meanInclination /= static_cast<float>(system.planetsData.size());
    const glm::dmat4 rotation = computeOrbitPlaneRotationMatrix(meanInclination);
    const glm::dmat3 meanOrbitPlaneRotationMatrix = static_cast<glm::dmat3>(rotation);

    res.setValue("MeanOrbitRotation", meanOrbitPlaneRotationMatrix);

    double distanceToOurSystem = glm::length(starPosInParsec) *
        distanceconstants::Parsec / distanceconstants::LightYear;

    res.setValue("DistanceToUs", distanceToOurSystem);

    return res;
}

} // namespace openspace::exoplanets
