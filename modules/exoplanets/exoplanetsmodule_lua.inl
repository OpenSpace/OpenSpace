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

#include <modules/exoplanets/exoplanetshelper.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/distanceconstants.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/timeconversion.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/glm.h>
#include <ghoul/misc/assert.h>
#include <glm/gtc/quaternion.hpp>
#include <glm/gtx/quaternion.hpp>
#include <glm/gtx/transform.hpp>
#include <fstream>
#include <iostream>
#include <sstream>

namespace openspace::exoplanets::luascriptfunctions {

constexpr const char* ExoplanetsGUIPath = "/Milky Way/Exoplanets/Exoplanets Systems/";

std::string getStarColor(float bv, std::ifstream& colormap) {
    const int t = round(((bv + 0.4) / (2.0 + 0.4)) * 255);
    std::string color;
    for (size_t i = 0; i < t + 12; i++) {
        getline(colormap, color);
    }
    colormap.close();

    std::istringstream colorStream(color);
    std::string r, g, b;
    getline(colorStream, r, ' ');
    getline(colorStream, g, ' ');
    getline(colorStream, b, ' ');

    return "{" + r + ", " + g + ", " + b + "}";
}

glm::dmat4 computeOrbitPlaneRotationMatrix(float i, float bigom, 
                                           float om, glm::dmat3 rot) 
{
    // Exoplanet defined inclination changed to be used as Kepler defined inclination
    const glm::dvec3 ascendingNodeAxisRot = rot * glm::dvec3(0.f, 0.f, 1.f);
    const glm::dvec3 inclinationAxisRot = rot * glm::dvec3(1.f, 0.f, 0.f );
    const glm::vec3 argPeriapsisAxisRot = rot * glm::dvec3( 0.f, 0.f, 1.f );

    const double asc = glm::radians(bigom);
    const double inc = glm::radians(i);
    const double per = glm::radians(om);

    glm::dmat4 orbitPlaneRotation =
        glm::rotate(asc, glm::dvec3(ascendingNodeAxisRot)) *
        glm::rotate(inc, glm::dvec3(inclinationAxisRot)) *
        glm::rotate(per, glm::dvec3(argPeriapsisAxisRot));

    return orbitPlaneRotation;
}

// Rotate the original coordinate system (where x is pointing to First Point of Aries)
// so that x is pointing from star to the sun.
// Modified from http://www.opengl-tutorial.org/intermediate-tutorials/tutorial-17-quaternions/#how-do-i-find-the-rotation-between-2-vectors- 
glm::dmat3 getExoplanetSystemRotation(glm::dvec3 start, glm::dvec3 end) {
    glm::quat rotationQuat;
    glm::dvec3 rotationAxis;
    const float cosTheta = dot(start, end);
    constexpr float Epsilon = 1E-3f;

    if (cosTheta < -1.f + Epsilon) {
        // special case when vectors in opposite directions:
        // there is no "ideal" rotation axis
        // So guess one; any will do as long as it's perpendicular to start vector
        rotationAxis = cross(glm::dvec3(0.0, 0.0, 1.0), start);
        if (length2(rotationAxis) < 0.01f) // bad luck, they were parallel, try again!
            rotationAxis = cross(glm::dvec3(1.0, 0.0, 0.0), start);

        rotationAxis = normalize(rotationAxis);
        rotationQuat = glm::quat(glm::radians(180.f), rotationAxis);
        return glm::dmat3(toMat4(rotationQuat));
    }

    rotationAxis = cross(start, end);

    const float s = sqrt((1.f + cosTheta) * 2.f);
    const float invs = 1.f / s;

    rotationQuat = glm::quat(
        s * 0.5f,
        rotationAxis.x * invs,
        rotationAxis.y * invs,
        rotationAxis.z * invs
    );

    return glm::dmat3(toMat4(rotationQuat));
}

// Create an identifier without whitespaces
std::string createIdentifier(const std::string& name) {
    std::string res = name;
    std::replace(res.begin(), res.end(), ' ', '_');
    return res;
}

int addExoplanetSystem(lua_State* L) {
    const int StringLocation = -1;
    const std::string starName = luaL_checkstring(L, StringLocation);

    // If user have given name as in EOD, change it to speck-name
    std::string starNameSpeck = getSpeckStarName(starName);

    std::ifstream data(
        absPath("${MODULE_EXOPLANETS}/expl_data.bin"), 
        std::ios::in | std::ios::binary
    );

    if (!data.good()) {
        return ghoul::lua::luaError(L, "Failed to open exoplanets data file");
    }

    std::ifstream lut(absPath("${MODULE_EXOPLANETS}/lookup.txt"));
    if (!lut.good()) {
        return ghoul::lua::luaError(L, "Failed to open exoplanets look-up table file");
    }

    //1. search lut for the starname and return the corresponding location
    //2. go to that location in the data file
    //3. read sizeof(exoplanet) bytes into an exoplanet object.
    Exoplanet p;
    std::string line;
    bool found = false;

    std::vector<Exoplanet> planetSystem;
    std::vector<std::string> planetNames;

    while (getline(lut, line)) {
        std::istringstream ss(line);
        std::string name;
        getline(ss, name, ',');

        if (name.compare(0, name.length() - 2, starNameSpeck) == 0) {
            std::string location_s;
            getline(ss, location_s);
            long location = std::stol(location_s.c_str());

            data.seekg(location);
            data.read((char*)&p, sizeof(Exoplanet));

            planetNames.push_back(name);
            planetSystem.push_back(p);
            found = true;
        }
    }
    
    data.close();
    lut.close();

    if (!found || isnan(p.POSITIONX) || isnan(p.A) || isnan(p.PER)) { // || p.BINARY
        return ghoul::lua::luaError(
            L, 
            "No star with that name or not enough data about it."
        ); 
    }

    Time epoch;
    glm::dvec3 starPosition = glm::dvec3(
        p.POSITIONX * distanceconstants::Parsec, 
        p.POSITIONY * distanceconstants::Parsec,
        p.POSITIONZ * distanceconstants::Parsec
    );
       
    glm::dvec3 sunPosition = glm::dvec3(0.0, 0.0, 0.0);
    glm::dvec3 starToSunVec = normalize(sunPosition - starPosition);
    glm::dvec3 galacticNorth = glm::dvec3(0.0, 0.0, 1.0);

    glm::dmat3 galaxticToCelectialMatrix = SpiceManager::ref().positionTransformMatrix( // galaxtic north in celectial coordinates, or just celectial north?
        "GALACTIC",
        "J2000",
        0.0
    );
    glm::dvec3 celestialNorth = normalize(galaxticToCelectialMatrix * galacticNorth);

    // Earth's north vector projected onto the skyplane, the plane perpendicular to the viewing vector (starToSunVec)
    const float celestialAngle = dot(celestialNorth, starToSunVec);
    glm::dvec3 northProjected = glm::normalize(
        celestialNorth - (celestialAngle / glm::length(starToSunVec)) * starToSunVec
    );

    glm::dvec3 beta = glm::normalize(glm::cross(starToSunVec, northProjected));

    const glm::dmat3 exoplanetSystemRotation = glm::dmat3(
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

    std::string starIdentifier = createIdentifier(starNameSpeck);

    const std::string starParent = "{"
        "Identifier = '" + starIdentifier + "',"
        "Parent = 'SolarSystemBarycenter'," 
        "Transform = {"
            "Rotation = {"
                "Type = 'StaticRotation',"
                "Rotation = " + ghoul::to_string(exoplanetSystemRotation) + ""
            "},"
            "Translation = {"
                "Type = 'StaticTranslation',"
                "Position = " + ghoul::to_string(starPosition) + ""
            "}"
        "},"
        "GUI = {"
            "Name = '" + starNameSpeck + "',"
            "Path = '" + ExoplanetsGUIPath + starNameSpeck + "',"
        "}"
    "}";

    openspace::global::scriptEngine.queueScript(
        "openspace.addSceneGraphNode(" + starParent + ");",
        openspace::scripting::ScriptEngine::RemoteScripting::Yes
    );

    float starRadius = p.RSTAR;
    if (!isnan(starRadius)) {
        std::ifstream colorMap(
            absPath("${SYNC}/http/stars_colormap/2/colorbv.cmap"), 
            std::ios::in
        );

        if (!colorMap.good()) {
            ghoul::lua::luaError(L, "Failed to open colormap data file");
        }

        std::string color = getStarColor(p.BMV, colorMap);
        Exoplanet firstPlanet = planetSystem[0];

        if (isnan(firstPlanet.ECC)) {
            firstPlanet.ECC = 0.f;
        }
        if (isnan(firstPlanet.I)) {
            firstPlanet.I = 90.f;
        }
        if (isnan(firstPlanet.BIGOM)) {
            firstPlanet.BIGOM = 180.f;
        }
        if (isnan(firstPlanet.OM)) {
            firstPlanet.OM = 90.f;
        }
        std::string sEpochStar;
        if (!isnan(firstPlanet.TT)) {
            epoch.setTime("JD " + std::to_string(firstPlanet.TT));
            sEpochStar = epoch.ISO8601();
        }
        else {
            sEpochStar = "2009-05-19T07:11:34.080";
        }

        const float period = firstPlanet.PER * static_cast<float>(SecondsPerDay);
        const float radiusInMeter = starRadius * distanceconstants::SolarRadius;

        const std::string starGlobeNode = "{"
            "Identifier = '" + starIdentifier + "_Globe',"
            "Parent = '" + starIdentifier + "',"
            "Renderable = {"
                "Type = 'RenderableGlobe',"
                "Radii = " + std::to_string(radiusInMeter) + ","
                "SegmentsPerPatch = 64,"
                "PerformShading = false,"
                "Layers = {"
                    "ColorLayers = {"
                        "{"
                            "Identifier = 'StarColor',"
                            "Type = 'SolidColor',"
                            "Color = " + color + ","
                            "BlendMode = 'Normal',"
                            "Enabled = true"
                        "},"
                        "{"
                            "Identifier = 'StarTexture',"
                            "FilePath = openspace.absPath('${MODULE_EXOPLANETS}/sun.jpg'),"
                            "BlendMode = 'Color',"
                            "Enabled = true"
                        "}"
                    "}"
                "}"
            "},"
            "Transform = {"
                "Scale = {"
                    "Type = 'StaticScale',"
                    "Scale = 1.0,"
                "},"
                "Translation = {"
                    "Type = 'KeplerTranslation',"
                    "Eccentricity = " + std::to_string(firstPlanet.ECC) + "," //ECC
                    "SemiMajorAxis = 0," 
                    "Inclination = " + std::to_string(firstPlanet.I) + "," //I
                    "AscendingNode  = " + std::to_string(firstPlanet.BIGOM) + "," //BIGOM
                    "ArgumentOfPeriapsis  = " + std::to_string(firstPlanet.OM) + "," //OM
                    "MeanAnomaly = 180.0,"
                    "Epoch = '" + sEpochStar + "'," //TT. JD to YYYY MM DD hh:mm:ss
                    "Period = " + std::to_string(period) + ","
                "}"
            "},"
            "GUI = {"
                "Name = '" + starNameSpeck + " Globe',"
                "Path = '" + ExoplanetsGUIPath + starNameSpeck + "',"
            "}"
        "}";

        openspace::global::scriptEngine.queueScript(
            "openspace.addSceneGraphNode(" + starGlobeNode + ");",
            openspace::scripting::ScriptEngine::RemoteScripting::Yes
        );
    }

    for (size_t i = 0; i < planetSystem.size(); i++) {
        Exoplanet planet = planetSystem[i];
        std::string planetName = planetNames[i];

        if (isnan(planet.ECC)) {
            planet.ECC = 0.f;
        }
        if (isnan(planet.I)) {
            planet.I = 90.f;
        }
        if (isnan(planet.BIGOM)) {
            planet.BIGOM = 180.f;
        }
        if (isnan(planet.OM)) {
            planet.OM = 90.f;
        }
        std::string sEpoch;
        if (!isnan(planet.TT)) {
            epoch.setTime("JD " + std::to_string(planet.TT));
            sEpoch = epoch.ISO8601();
        }
        else {
            sEpoch = "2009-05-19T07:11:34.080";
        }

        float planetRadius;
        std::string enabled = "";

        if (isnan(planet.R)) {
            if (isnan(planet.RSTAR)) {
                planetRadius = planet.A * 0.001f * distanceconstants::AstronomicalUnit;
            }
            else {
                planetRadius = planet.RSTAR * 0.1f * distanceconstants::SolarRadius;
            }
            enabled = "false";
        }
        else {
            planetRadius = planet.R * distanceconstants::JupiterRadius;
            enabled = "true";
        }

        const float period = planet.PER * static_cast<float>(SecondsPerDay);
        const float semiMajorAxisInMeter = planet.A * distanceconstants::AstronomicalUnit;
        const float semiMajorAxisInKm = semiMajorAxisInMeter * 0.001f;

        std::string planetIdentifier = createIdentifier(planetName);

        const std::string planetNode = "{"
            "Identifier = '" + planetIdentifier + "',"
            "Parent = '" + starIdentifier + "',"
            "Enabled = true,"
            "Renderable = {"
                "Type = 'RenderableGlobe',"
                "Enabled = " + enabled + ","
                "Radii = " + std::to_string(planetRadius) + "," //R. in meters. 
                "SegmentsPerPatch = 64,"
                "PerformShading = false,"
                "Layers = {"
                    "ColorLayers = {"
                        "{"
                            "Identifier = 'ExoplanetTexture',"
                            "FilePath = openspace.absPath('${DATA}/test3.jpg'),"
                            "Enabled = true"
                        "}"
                    "}"
                "}"
            "},"
            "Transform = {"
                "Scale = {"
                    "Type = 'StaticScale',"
                    "Scale = 1.0,"
                "},"
                "Translation = {"
                    "Type = 'KeplerTranslation',"
                    "Eccentricity = " + std::to_string(planet.ECC) + "," //ECC
                    "SemiMajorAxis = " + std::to_string(semiMajorAxisInKm) + ","
                    "Inclination = " + std::to_string(planet.I) + "," //I
                    "AscendingNode = " + std::to_string(planet.BIGOM) + "," //BIGOM
                    "ArgumentOfPeriapsis = " + std::to_string(planet.OM) + "," //OM
                    "MeanAnomaly = 0.0,"
                    "Epoch = '" + sEpoch + "'," //TT. JD to YYYY MM DD hh:mm:ss
                    "Period = " + std::to_string(period) + ","
                "},"
            "},"
            "GUI = {"
                "Name = '" + planetName + "',"
                "Path = '" + ExoplanetsGUIPath + starNameSpeck + "',"
            "}"
        "}";

        openspace::global::scriptEngine.queueScript(
            "openspace.addSceneGraphNode(" + planetNode + ");",
            openspace::scripting::ScriptEngine::RemoteScripting::Yes
        );

        const std::string planetTrailNode = "{"
            "Identifier = '" + planetIdentifier + "_Trail',"
            "Parent = '" + starIdentifier + "',"
            "Enabled = true,"
            "Renderable = {"
                "Type = 'RenderableTrailOrbit',"
                "Period = " + std::to_string(planet.PER) + ","
                "Resolution = 1000,"
                "Translation = {"
                    "Type = 'KeplerTranslation',"
                    "Eccentricity = " + std::to_string(planet.ECC) + "," //ECC 
                    "SemiMajorAxis = " + std::to_string(semiMajorAxisInKm) + "," 
                    "Inclination = " + std::to_string(planet.I) + "," //I
                    "AscendingNode  = " + std::to_string(planet.BIGOM) + "," //BIGOM
                    "ArgumentOfPeriapsis  = " + std::to_string(planet.OM) + "," //OM
                    "MeanAnomaly = 0.0,"
                    "Epoch = '" + sEpoch + "'," //TT. JD to YYYY MM DD hh:mm:ss
                    "Period = " + std::to_string(period) + ","
                "},"
                "Color = { 1, 1, 1 }"
            "},"
            "GUI = {"
                "Name = '" + planetName + " Trail',"
                "Path = '" + ExoplanetsGUIPath + starNameSpeck + "',"
            "}"
        "}";

        openspace::global::scriptEngine.queueScript(
            "openspace.addSceneGraphNode(" + planetTrailNode + ");",
            openspace::scripting::ScriptEngine::RemoteScripting::Yes
        );

        bool hasUpperAUncertainty = !isnan(planet.AUPPER);
        bool hasLowerAUncertainty = !isnan(planet.ALOWER);

        if (hasUpperAUncertainty && hasLowerAUncertainty)
        {
            // Get the orbit plane that the trail orbit and planet have from the KeplerTranslation
            glm::dmat4 orbitPlaneRotationMatrix = computeOrbitPlaneRotationMatrix(
                planet.I, 
                planet.BIGOM, 
                planet.OM, 
                exoplanetSystemRotation
            );
            glm::dmat3 rotation = orbitPlaneRotationMatrix;
            const std::string discNode = "{"
                "Identifier = '" + planetIdentifier + "_Disc',"
                "Parent = '" + starIdentifier + "',"
                "Enabled = true,"
                "Renderable = {"
                    "Type = 'RenderableOrbitdisc',"
                    "Texture = openspace.absPath('${MODULE_EXOPLANETS}/disc3.png'),"
                    "Size = " + std::to_string(semiMajorAxisInMeter) + ","
                    "Eccentricity = " + std::to_string(planet.ECC) + ","
                    "Offset = { " +
                        std::to_string(planet.ALOWER) + ", " +
                        std::to_string(planet.AUPPER) + 
                    " }," //min / max extend
                    "Opacity = 0.5"
                "},"
                "Transform = {"
                    "Rotation = {"
                        "Type = 'StaticRotation',"
                        "Rotation = " + ghoul::to_string(rotation) + ","
                    "}"
                "},"
                "GUI = {"
                    "Name = '" + planetName + " Disc',"
                    "Path = '" + ExoplanetsGUIPath + starNameSpeck + "',"
                "}"
            "}";

            openspace::global::scriptEngine.queueScript(
                "openspace.addSceneGraphNode(" + discNode + ");",
                openspace::scripting::ScriptEngine::RemoteScripting::Yes
            );

            // Eccentricity uncertainty
            bool hasLowerEccUncertainty = !isnan(planet.ECCLOWER) && planet.ECCLOWER > 0.0;
            bool hasUpperEccUncertainty = !isnan(planet.ECCUPPER) && planet.ECCUPPER > 0.0;

            if (hasLowerEccUncertainty && hasUpperEccUncertainty)
            {
                double lowerEccentricity = planet.ECC - planet.ECCLOWER;
                if (lowerEccentricity < 0.0) {
                    lowerEccentricity = 0.0;
                }

                const std::string discLowerEccentricityNode = "{"
                    "Identifier = '" + planetIdentifier + "_Disc_ECC_LOWER',"
                    "Parent = '" + starIdentifier + "',"
                    "Renderable = {"
                        "Type = 'RenderableOrbitdisc',"
                        "Texture = openspace.absPath('${MODULE_EXOPLANETS}/discL.png'),"
                        "Size = " + std::to_string(semiMajorAxisInMeter) + ","
                        "Eccentricity = " + std::to_string(lowerEccentricity) + ","
                        "Offset = { " +
                            std::to_string(planet.ALOWER) + ", " +
                            std::to_string(planet.AUPPER) +
                        " }," //min / max extend
                        "Opacity = 0.98,"
                        "Enabled = false"
                    "},"
                    "Transform = {"
                        "Rotation = {"
                            "Type = 'StaticRotation',"
                            "Rotation = " + ghoul::to_string(rotation) + ","
                        "}"
                    "},"
                    // @TODO: GUI, if we make it work 
                "}";

                openspace::global::scriptEngine.queueScript(
                    "openspace.addSceneGraphNode(" + discLowerEccentricityNode + ");",
                    openspace::scripting::ScriptEngine::RemoteScripting::Yes
                );

                double upperEccentricity = planet.ECC + planet.ECCUPPER;
                if (upperEccentricity > 1.0) {
                    upperEccentricity = 1.0;
                }

                const std::string discUpperEccentricityNode = "{"
                    "Identifier = '" + planetIdentifier + "_Disc_ECC_UPPER',"
                    "Parent = '" + starIdentifier + "',"
                    "Renderable = {"
                        "Type = 'RenderableOrbitdisc',"
                        "Texture = openspace.absPath('${MODULE_EXOPLANETS}/discU.png'),"
                        "Size = " + std::to_string(semiMajorAxisInMeter) + ","
                        "Eccentricity = " + std::to_string(upperEccentricity) + ","
                        "Offset = { " +
                            std::to_string(planet.ALOWER) + ", " +
                            std::to_string(planet.AUPPER) +
                        " }," //min / max extend    
                        "Opacity = 0.98,"
                        "Enabled = false"
                    "},"
                    "Transform = {"
                        "Rotation = {"
                            "Type = 'StaticRotation',"
                            "Rotation = " + ghoul::to_string(rotation) + ","
                        "}"
                    "},"
                    // @TODO: GUI, if we make it work 
                "}";

                openspace::global::scriptEngine.queueScript(
                    "openspace.addSceneGraphNode(" + discUpperEccentricityNode + ");",
                    openspace::scripting::ScriptEngine::RemoteScripting::Yes
                );
            }
        }
    }

    return 0;
}

int removeExoplanetSystem(lua_State* L) {
    const int StringLocation = -1;
    const std::string starName = luaL_checkstring(L, StringLocation);
    std::string starNameSpeck = getSpeckStarName(starName);
    std::string starIdentifier = createIdentifier(starNameSpeck);

    openspace::global::scriptEngine.queueScript(
        "openspace.removeSceneGraphNode('" + starIdentifier + "');",
        scripting::ScriptEngine::RemoteScripting::Yes
    );

    return 0;
}
} //namespace openspace::exoplanets::luascriptfunctions
