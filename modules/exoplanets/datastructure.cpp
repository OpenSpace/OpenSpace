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

#include <modules/exoplanets/datastructure.h>

#include <modules/exoplanets/exoplanetshelper.h>
#include <openspace/scene/scene.h>
#include <openspace/util/distanceconstants.h>
#include <openspace/util/time.h>
#include <openspace/util/timeconversion.h>
#include <ghoul/logging/logmanager.h>
#include <string_view>

namespace {
    constexpr std::string_view _loggerCat = "ExoplanetsModule";

    // This is the documentation for the data object that is returned from the
    // `openspace.exoplanets.systemData` function in the Scripting API.
    //
    // The data object provides the information needed to create the scene graph nodes
    // for an exoplanet system. However, depending on what data is available for the
    // specific planets and star, some values may be undefined.
    struct [[codegen::Dictionary(ExoplanetSystemData)]] Parameters {
        // The identifier of the system, which equals the identifier of the host star
        // created based on the star name.
        std::string systemId;

        // The position of the host star, expressed in meters.
        glm::dvec3 position;

        // The number of known planets in the system that have enough data to be
        // visualized.
        int numPlanets;

        // The name of the star.
        std::string starName;

        // The radius of the star, in meters, if data exists.
        std::optional<double> starRadius;

        // An RGB color for the host star, computed from the star's B-V value, if data
        // exists.
        std::optional<glm::dvec3> starColor [[codegen::color()]];

        // The effective temperature of the star, in Kelvin, if data exists.
        std::optional<double> starTeff;

        // The luminosity for the star, in units of solar luminosities [log10(Solar)], if
        // data exists.
        std::optional<double> starLuminosity;

        struct Planet {
            // An identifier to use for the planet, created based on the planet name.
            std::string id;

            // The name of the exoplanet.
            std::string name;

            // The radius of the planet, in meters, if data exists.
            std::optional<double> starRadius;

            // The semi-major axis of the planet's orbit, in meters. That is, the orbit
            // size. This is required to be able to be visualize the planetary orbit.
            double semiMajorAxis;

            // The uncertainty of the semi-major axis, given as an asymmetric uncertainty
            // with two values: the lower and upper uncertainty for the orbit size. The
            // values are given as values relative to the size of the `SemiMajorAxis`. For
            // example, the lower (first) value is computed as the lower uncertainty
            // value divided by the value for the semi-major axis.
            //
            // If no value is given, there is no uncertainty and the semi-major axis value
            // is exact.
            std::optional<glm::dvec2> semiMajorAxisUncertainty;

            // The eccentricity of the planet's orbit. If it does not exist in the data,
            // it is given a default value of 0.
            double eccentricity;

            // The inclination of the planet's orbit, given as an angle in degrees in the
            // range 0-360. If it does not exist in the data, it is given a default value
            // of 90 degrees.
            double inclination;

            // The longitude of ascending node, that is, the angle for the direction of
            // the point where the orbit passes through a reference plane. Also
            // known as the right ascension of the ascending node. The angle is given as
            // degrees in the range 0-360. If it does not exist in the data, it is given
            // a default value of 180 degrees.
            double ascendingNode;

            // The argument of periapsis of the orbit, given as an angle in degrees in
            // the range 0-360. It is the angle between the planet's periapsis (the point
            // where it is closest to its star) and its ascending node, that is, where it
            // passes through its reference plane. If it does not exist in the data, it
            // is given a default value of 90 degrees.
            double argumentOfPeriapsis;

            // The epoch to use when computing the planet's initial position in its orbit,
            // given as an ISO 8601-like date string of the format YYYY-MM-DDTHH:MN:SS.
            //
            // Computed based on the \"Time of Conjunction (Transit Midpoint)\" value in
            // the data from the exoplanet archive.
            std::string epoch;

            // Time the planet takes to make a complete orbit around the host star or
            // system, in days.
            double period;

            // A rotation matrix that represents the rotation of the plane of the orbit.
            // It is computed based on the inclination, ascending node, and argument of
            // periapsis values. Hence, it may be derived from default values.
            glm::dmat3 orbitPlaneRotationMatrix;

            // True, if default values have been used for any of the orbit parameters, due
            // to missing data.
            bool hasUsedDefaultValues;
        };

        // A list of data tables for the planets in the system (that have enough data to
        // be visualized), with data about the planet and its orbit.
        std::vector<Planet> planets;

        // A rotation matrix to use for the entire system's rotation. Broadly speaking, it
        // rotates the system so that the reference plane is perpendicular to the
        // line-of-sight from Earth (and so that a 90 degree orbit inclination leads to
        // an orbit that passes in front of its star, relative to the line-of-sight).
        glm::dmat3 systemRotation;

        // A rotation matrix that represents the average of all the orbit planes in the
        // system. Computed using the planets' average inclinations, ascending nodes, and
        // arguments of periapsis.
        glm::dmat3 meanOrbitRotation;

        // The distance from this system to our solar system, in light years.
        double distance;
    };
#include "datastructure_codegen.cpp"
} // namespace

namespace openspace::exoplanets {

ghoul::Dictionary ExoplanetSystem::toDataDictionary() const {
    ghoul_assert(
        planetNames.size() == planetsData.size(),
        "The length of the planet names list must match the planet data list"
    );

    ghoul::Dictionary res;

    res.setValue("SystemId", makeIdentifier(starName));

    const glm::dvec3 starPosInParsec = static_cast<glm::dvec3>(starData.position);

    if (!isValidPosition(starPosInParsec)) {
        LERROR(std::format(
            "Insufficient data available for exoplanet system '{}'. Could not "
            "determine star position", starName
        ));
        return ghoul::Dictionary();
    }

    const glm::dvec3 starPos = starPosInParsec * distanceconstants::Parsec;
    res.setValue("Position", starPos);

    res.setValue("NumPlanets", static_cast<int>(planetNames.size()));

    std::string sanitizedStarName = starName;
    sanitizeNameString(sanitizedStarName);
    res.setValue("StarName", sanitizedStarName);

    if (!std::isnan(starData.radius)) {
        double radiusInMeter = distanceconstants::SolarRadius * starData.radius;
        res.setValue("StarRadius", radiusInMeter);
    }

    if (!std::isnan(starData.bv)) {
        res.setValue("StarColor", glm::dvec3(computeStarColor(starData.bv)));
    }

    if (!std::isnan(starData.teff)) {
        res.setValue("StarTeff", static_cast<double>(starData.teff));
    }

    if (!std::isnan(starData.luminosity)) {
        res.setValue("StarLuminosity", static_cast<double>(starData.luminosity));
    }

    ghoul::Dictionary planets;
    std::vector<float> inclinations;
    inclinations.reserve(planetNames.size());

    for (size_t i = 0; i < planetNames.size(); i++) {
        const ExoplanetDataEntry& data = planetsData[i];
        const std::string& name = planetNames[i];
        const std::string id = makeIdentifier(name);

        ghoul::Dictionary planet;

        planet.setValue("Id", id);
        planet.setValue("Name", name);

        bool hasUsedDefaultValues = false;

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

            bool hasUpperAUncertainty = !std::isnan(data.aUpper);
            bool hasLowerAUncertainty = !std::isnan(data.aLower);

            if (hasUpperAUncertainty && hasLowerAUncertainty) {
                const float lowerOffset = static_cast<float>(data.aLower / data.a);
                const float upperOffset = static_cast<float>(data.aUpper / data.a);
                planet.setValue(
                    "SemiMajorAxisUncertainty",
                    glm::dvec2(lowerOffset, upperOffset)
                );
            }

            if (!std::isnan(data.ecc)) {
                planet.setValue("Eccentricity", static_cast<double>(data.ecc));
            }
            else {
                planet.setValue("Eccentricity", 0.0);
                hasUsedDefaultValues = true;
            }

            // KeplerTranslation requires angles in range [0, 360]
            auto validAngle = [&hasUsedDefaultValues](float angle, float defaultValue) {
                if (std::isnan(angle)) {
                    hasUsedDefaultValues = true;
                    return defaultValue;
                }
                while (angle < 0.f) {
                    angle += 360.f;
                }
                while (angle > 360.f) {
                    angle = -360.f;
                }
                return angle;
            };

            float inclination = validAngle(data.i, 90.f);
            float bigOmega = validAngle(data.bigOmega, 180.f);
            float omega = validAngle(data.omega, 90.f);

            inclinations.push_back(inclination);

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
    for (const float& i : inclinations) {
        meanInclination += i / static_cast<float>(inclinations.size());
    }
    const glm::dmat4 rotation = computeOrbitPlaneRotationMatrix(meanInclination);
    const glm::dmat3 meanOrbitPlaneRotationMatrix = static_cast<glm::dmat3>(rotation);

    res.setValue("MeanOrbitRotation", meanOrbitPlaneRotationMatrix);

    double distanceToOurSystem = glm::length(starPosInParsec) *
        distanceconstants::Parsec / distanceconstants::LightYear;

    res.setValue("Distance", distanceToOurSystem);

    return res;
}

documentation::Documentation ExoplanetSystem::Documentation() {
    return codegen::doc<Parameters>("exoplanets_exoplanet_system_data");
}

} // namespace openspace::exoplanets
