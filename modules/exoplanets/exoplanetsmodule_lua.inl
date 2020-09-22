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

#include <modules/exoplanets/exoplanetshelper.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/query/query.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/distanceconstants.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/timeconversion.h>
#include <openspace/util/timemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/fmt.h>
#include <ghoul/glm.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <glm/gtc/quaternion.hpp>
#include <glm/gtx/quaternion.hpp>
#include <glm/gtx/transform.hpp>
#include <fstream>
#include <iostream>
#include <sstream>

namespace openspace::exoplanets::luascriptfunctions {

constexpr const char* _loggerCat = "ExoplanetsModule";

constexpr const char* ExoplanetsGuiPath = "/Milky Way/Exoplanets/Exoplanet Systems/";

constexpr const char* LookUpTablePath = "${SYNC}/http/exoplanets_data/1/lookup.txt";
constexpr const char* ExoplanetsDataPath = 
    "${SYNC}/http/exoplanets_data/1/exoplanets_data.bin";

constexpr const char* StarTextureFile = "${SYNC}/http/exoplanets_textures/1/sun.jpg";
constexpr const char* DiscTextureFile = 
    "${SYNC}/http/exoplanets_textures/1/disc_texture.png";

constexpr const char* BvColormapPath = "${SYNC}/http/stars_colormap/2/colorbv.cmap";

std::string starColor(float bv, std::ifstream& colormap) {
    const int t = static_cast<int>(round(((bv + 0.4) / (2.0 + 0.4)) * 255));
    std::string color;
    for (int i = 0; i < t + 12; i++) {
        getline(colormap, color);
    }
    colormap.close();

    std::istringstream colorStream(color);
    std::string r, g, b;
    getline(colorStream, r, ' ');
    getline(colorStream, g, ' ');
    getline(colorStream, b, ' ');

    return fmt::format("{{ {}, {}, {} }}", r, g, b);
}

glm::dmat4 computeOrbitPlaneRotationMatrix(float i, float bigom, float om) {
    // Exoplanet defined inclination changed to be used as Kepler defined inclination
    const glm::dvec3 ascendingNodeAxisRot = glm::dvec3(0.0, 0.0, 1.0);
    const glm::dvec3 inclinationAxisRot =  glm::dvec3(1.0, 0.0, 0.0);
    const glm::dvec3 argPeriapsisAxisRot = glm::dvec3(0.0, 0.0, 1.0);

    const double asc = glm::radians(bigom);
    const double inc = glm::radians(i);
    const double per = glm::radians(om);

    const glm::dmat4 orbitPlaneRotation =
        glm::rotate(asc, glm::dvec3(ascendingNodeAxisRot)) *
        glm::rotate(inc, glm::dvec3(inclinationAxisRot)) *
        glm::rotate(per, glm::dvec3(argPeriapsisAxisRot));

    return orbitPlaneRotation;
}

// Rotate the original coordinate system (where x is pointing to First Point of Aries)
// so that x is pointing from star to the sun.
// Modified from "http://www.opengl-tutorial.org/intermediate-tutorials/
// tutorial-17-quaternions/ #how-do-i-find-the-rotation-between-2-vectors"
glm::dmat3 exoplanetSystemRotation(glm::dvec3 start, glm::dvec3 end) {
    glm::quat rotationQuat;
    glm::dvec3 rotationAxis;
    const float cosTheta = static_cast<float>(glm::dot(start, end));
    constexpr float Epsilon = 1E-3f;

    if (cosTheta < -1.f + Epsilon) {
        // special case when vectors in opposite directions:
        // there is no "ideal" rotation axis
        // So guess one; any will do as long as it's perpendicular to start vector
        rotationAxis = glm::cross(glm::dvec3(0.0, 0.0, 1.0), start);
        if (length2(rotationAxis) < 0.01f) {
            // bad luck, they were parallel, try again!
            rotationAxis = glm::cross(glm::dvec3(1.0, 0.0, 0.0), start);
        }

        rotationAxis = glm::normalize(rotationAxis);
        rotationQuat = glm::quat(glm::radians(180.f), rotationAxis);
        return glm::dmat3(toMat4(rotationQuat));
    }

    rotationAxis = glm::cross(start, end);

    const float s = sqrt((1.f + cosTheta) * 2.f);
    const float invs = 1.f / s;

    rotationQuat = glm::quat(
        s * 0.5f,
        rotationAxis.x * invs,
        rotationAxis.y * invs,
        rotationAxis.z * invs
    );

    return glm::dmat3(glm::toMat4(rotationQuat));
}

// Create an identifier without whitespaces
std::string createIdentifier(std::string name) {
    std::replace(name.begin(), name.end(), ' ', '_');
    return name;
}

int addExoplanetSystem(lua_State* L) {
    const int StringLocation = -1;
    const std::string starName = luaL_checkstring(L, StringLocation);

    // If user have given name as in EOD, change it to speck-name
    const std::string starNameSpeck = std::string(speckStarName(starName));

    const std::string starIdentifier = createIdentifier(starNameSpeck);
    const std::string guiPath = ExoplanetsGuiPath + starNameSpeck;

    SceneGraphNode* existingStarNode = sceneGraphNode(starIdentifier);
    if (existingStarNode) {
        return ghoul::lua::luaError(
            L, 
            "Adding of exoplanet system failed. The system has already been added."
        );
    }

    std::ifstream data(absPath(ExoplanetsDataPath), std::ios::in | std::ios::binary);

    if (!data.good()) {
        return ghoul::lua::luaError(L, "Failed to open exoplanets data file");
    }

    std::ifstream lut(absPath(LookUpTablePath));
    if (!lut.good()) {
        return ghoul::lua::luaError(L, "Failed to open exoplanets look-up table file");
    }

    // 1. search lut for the starname and return the corresponding location
    // 2. go to that location in the data file
    // 3. read sizeof(exoplanet) bytes into an exoplanet object.
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
            data.read(reinterpret_cast<char*>(&p), sizeof(Exoplanet));

            planetNames.push_back(name);
            planetSystem.push_back(p);
            found = true;
        }
    }
    
    data.close();
    lut.close();

    bool notEnoughData = isnan(p.positionX) || isnan(p.a) || isnan(p.per);

    if (!found || notEnoughData) {
        return ghoul::lua::luaError(
            L, 
            "No star with that name or not enough data about it."
        ); 
    }

    const glm::dvec3 starPosition = glm::dvec3(
        p.positionX * distanceconstants::Parsec,
        p.positionY * distanceconstants::Parsec,
        p.positionZ * distanceconstants::Parsec
    );
       
    const glm::dvec3 sunPosition = glm::dvec3(0.0, 0.0, 0.0);
    const glm::dvec3 starToSunVec = glm::normalize(sunPosition - starPosition);
    const glm::dvec3 galacticNorth = glm::dvec3(0.0, 0.0, 1.0);

    const glm::dmat3 galaxticToCelestialMatrix = 
        SpiceManager::ref().positionTransformMatrix("GALACTIC", "J2000", 0.0);

    const glm::dvec3 celestialNorth = glm::normalize(
        galaxticToCelestialMatrix * galacticNorth
    );

    // Earth's north vector projected onto the skyplane, the plane perpendicular to the 
    // viewing vector (starToSunVec)
    const float celestialAngle = static_cast<float>(glm::dot(
        celestialNorth, 
        starToSunVec
    ));
    glm::dvec3 northProjected = glm::normalize(
        celestialNorth - (celestialAngle / glm::length(starToSunVec)) * starToSunVec
    );

    const glm::dvec3 beta = glm::normalize(glm::cross(starToSunVec, northProjected));

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

    // Star renderable globe, if we have a radius
    std::string starGlobeRenderableString;
    const float starRadius = p.rStar;
    if (!isnan(starRadius)) {
        std::ifstream colorMap(absPath(BvColormapPath), std::ios::in);

        if (!colorMap.good()) {
            ghoul::lua::luaError(L, "Failed to open colormap data file");
        }

        const std::string color = starColor(p.bmv, colorMap);
        const float radiusInMeter = starRadius * static_cast<float>(distanceconstants::SolarRadius);

        starGlobeRenderableString = "Renderable = {"
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
                        "FilePath = openspace.absPath('" + StarTextureFile +"'),"
                        "BlendMode = 'Color',"
                        "Enabled = true"
                    "}"
                "}"
            "}"
        "},";
    }

    const std::string starParent = "{"
        "Identifier = '" + starIdentifier + "',"
        "Parent = 'SolarSystemBarycenter',"  
        "" + starGlobeRenderableString + ""
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
            "Name = '" + starNameSpeck + " (Star)',"
            "Path = '" + guiPath + "'"
        "}"
    "}";

    openspace::global::scriptEngine.queueScript(
        "openspace.addSceneGraphNode(" + starParent + ");",
        openspace::scripting::ScriptEngine::RemoteScripting::Yes
    );

    // Planets
    for (size_t i = 0; i < planetSystem.size(); i++) {
        Exoplanet planet = planetSystem[i];
        const std::string planetName = planetNames[i];

        if (isnan(planet.ecc)) {
            planet.ecc = 0.f;
        }
        if (isnan(planet.i)) {
            planet.i = 90.f;
        }
        if (isnan(planet.bigOm)) {
            planet.bigOm = 180.f;
        }
        if (isnan(planet.om)) {
            planet.om = 90.f;
        }
        Time epoch;
        std::string sEpoch;
        if (!isnan(planet.tt)) {
            epoch.setTime("JD " + std::to_string(planet.tt));
            sEpoch = std::string(epoch.ISO8601());
        }
        else {
            sEpoch = "2009-05-19T07:11:34.080";
        }

        float planetRadius;
        std::string enabled;

        const float astronomicalUnit = static_cast<float>(distanceconstants::AstronomicalUnit);
        const float solarRadius = static_cast<float>(distanceconstants::SolarRadius);
        const float jupiterRadius = static_cast<float>(distanceconstants::JupiterRadius);

        if (isnan(planet.r)) {
            if (isnan(planet.rStar)) {
                planetRadius = planet.a * 0.001f * astronomicalUnit;
            }
            else {
                planetRadius = planet.rStar * 0.1f * solarRadius;
            }
            enabled = "false";
        }
        else {
            planetRadius = planet.r * jupiterRadius;
            enabled = "true";
        }

        const float period = planet.per * static_cast<float>(SecondsPerDay);
        const float semiMajorAxisInMeter = planet.a * astronomicalUnit;
        const float semiMajorAxisInKm = semiMajorAxisInMeter * 0.001f;

        const std::string planetIdentifier = createIdentifier(planetName);

        const std::string planetKeplerTranslation = "{"
            "Type = 'KeplerTranslation',"
            "Eccentricity = " + std::to_string(planet.ecc) + "," //ECC 
            "SemiMajorAxis = " + std::to_string(semiMajorAxisInKm) + ","
            "Inclination = " + std::to_string(planet.i) + "," //I
            "AscendingNode = " + std::to_string(planet.bigOm) + "," //BIGOM
            "ArgumentOfPeriapsis = " + std::to_string(planet.om) + "," //OM
            "MeanAnomaly = 0.0,"
            "Epoch = '" + sEpoch + "'," //TT. JD to YYYY MM DD hh:mm:ss
            "Period = " + std::to_string(period) + ""
        "}";

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
                "Layers = {}"
            "},"
            "Transform = { "
                "Translation = " + planetKeplerTranslation + ""
            "},"
            "GUI = {"
                "Name = '" + planetName + "',"
                "Path = '" + guiPath + "'"
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
                "Period = " + std::to_string(planet.per) + ","
                "Resolution = 1000,"
                "Translation = " + planetKeplerTranslation + ","
                "Color = { 1, 1, 1 }"
            "},"
            "GUI = {"
                "Name = '" + planetName + " Trail',"
                "Path = '" + guiPath + "'"
            "}"
        "}";

        openspace::global::scriptEngine.queueScript(
            "openspace.addSceneGraphNode(" + planetTrailNode + ");",
            openspace::scripting::ScriptEngine::RemoteScripting::Yes
        );

        bool hasUpperAUncertainty = !isnan(planet.aUpper);
        bool hasLowerAUncertainty = !isnan(planet.aLower);

        if (hasUpperAUncertainty && hasLowerAUncertainty) {
            // Get the orbit plane of the planet trail orbit from the KeplerTranslation
            const glm::dmat4 orbitPlaneRotationMatrix = computeOrbitPlaneRotationMatrix(
                planet.i,
                planet.bigOm,
                planet.om
            );
            const glm::dmat3 rotation = orbitPlaneRotationMatrix;

            const std::string discNode = "{"
                "Identifier = '" + planetIdentifier + "_Disc',"
                "Parent = '" + starIdentifier + "',"
                "Enabled = true,"
                "Renderable = {"
                    "Type = 'RenderableOrbitDisc',"
                    "Texture = openspace.absPath('" + DiscTextureFile + "'),"
                    "Size = " + std::to_string(semiMajorAxisInMeter) + ","
                    "Eccentricity = " + std::to_string(planet.ecc) + ","
                    "Offset = { " +
                        std::to_string(planet.aLower) + ", " +
                        std::to_string(planet.aUpper) + 
                    "}," //min / max extend
                    "Opacity = 0.3"
                "},"
                "Transform = {"
                    "Rotation = {"
                        "Type = 'StaticRotation',"
                        "Rotation = " + ghoul::to_string(rotation) + ""
                    "}"
                "},"
                "GUI = {"
                    "Name = '" + planetName + " Disc',"
                    "Path = '" + guiPath + "'"
                "}"
            "}";

            openspace::global::scriptEngine.queueScript(
                "openspace.addSceneGraphNode(" + discNode + ");",
                openspace::scripting::ScriptEngine::RemoteScripting::Yes
            );
        }
    }

    return 0;
}

int removeExoplanetSystem(lua_State* L) {
    const int StringLocation = -1;
    const std::string starName = luaL_checkstring(L, StringLocation);
    const std::string starNameSpeck = std::string(speckStarName(starName));
    const std::string starIdentifier = createIdentifier(starNameSpeck);

    openspace::global::scriptEngine.queueScript(
        "openspace.removeSceneGraphNode('" + starIdentifier + "');",
        scripting::ScriptEngine::RemoteScripting::Yes
    );

    return 0;
}

int listAvailableExoplanetSystems(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::listAvailableExoplanetSystems");

    std::ifstream file(absPath(LookUpTablePath));

    if (!file.good()) {
        return ghoul::lua::luaError(
            L,
            fmt::format("Failed to open file '{}'", LookUpTablePath)
        );
    }

    std::vector<std::string> names;
    // As of 2020 there are about 4000 confirmed exoplanets, so use this number
    // as a guess for the vector size
    const int nExoplanetsGuess = 4000;
    names.reserve(nExoplanetsGuess);

    std::string line;
    while (getline(file, line)) {
        std::stringstream ss(line);
        std::string name;
        getline(ss, name, ',');
        // Remove the last two characters, that specify the planet
        name = name.substr(0, name.size() - 2);

        names.push_back(name);
    }

    // For easier read, sort by names and remove duplicates
    std::sort(names.begin(), names.end());
    names.erase(std::unique(names.begin(), names.end()), names.end());

    std::string output;
    for (auto it = names.begin(); it != names.end(); ++it) {
        if (it != names.end()) {
            output += *it + ", ";
        }
    }

    LINFO(fmt::format(
        "There is data available for the following {} exoplanet systems: {}", 
        names.size(),
        output
    ));

    return 0;
}
} //namespace openspace::exoplanets::luascriptfunctions
