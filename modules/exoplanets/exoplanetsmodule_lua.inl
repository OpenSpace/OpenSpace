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
#include <openspace/query/query.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/distanceconstants.h>
#include <openspace/util/timeconversion.h>
#include <openspace/util/timemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/fmt.h>
#include <ghoul/glm.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <fstream>
#include <sstream>

namespace {
    constexpr const char* _loggerCat = "ExoplanetsModule";
} // namespace

namespace openspace::exoplanets::luascriptfunctions {

constexpr const char* ExoplanetsGuiPath = "/Milky Way/Exoplanets/Exoplanet Systems/";

constexpr const char* LookUpTablePath = "${SYNC}/http/exoplanets_data/1/lookup.txt";
constexpr const char* ExoplanetsDataPath =
    "${SYNC}/http/exoplanets_data/1/exoplanets_data.bin";

constexpr const char* StarTextureFile = "${SYNC}/http/exoplanets_textures/1/sun.jpg";
constexpr const char* DiscTextureFile =
    "${SYNC}/http/exoplanets_textures/1/disc_texture.png";

void createExoplanetSystem(std::string_view starName) {
    // If user have given name as in EOD, change it to speck-name
    const std::string starNameSpeck = std::string(speckStarName(starName));

    const std::string starIdentifier = createIdentifier(starNameSpeck);
    const std::string guiPath = ExoplanetsGuiPath + starNameSpeck;

    SceneGraphNode* existingStarNode = sceneGraphNode(starIdentifier);
    if (existingStarNode) {
        LERROR(fmt::format(
            "Adding of exoplanet system '{}' failed. The system has already been added.",
            starName
        ));
        return;
    }

    std::ifstream data(absPath(ExoplanetsDataPath), std::ios::in | std::ios::binary);

    if (!data.good()) {
        LERROR(fmt::format(
            "Failed to open exoplanets data file: '{}'", absPath(ExoplanetsDataPath)
        ));
        return;
    }

    std::ifstream lut(absPath(LookUpTablePath));
    if (!lut.good()) {
        LERROR(fmt::format(
            "Failed to open exoplanets look-up table: '{}'", absPath(LookUpTablePath)
        ));
        return;
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

            if (!hasSufficientData(p)) {
                LERROR(fmt::format(
                    "Insufficient data available for visualizion of exoplanet system: '{}'",
                    starName
                ));
                return;
            }
        }
    }

    data.close();
    lut.close();

    if (!found) {
        LERROR(fmt::format("No star with the provided name was found: '{}'", starName));
        return;
    }

    const glm::dvec3 starPosition = glm::dvec3(
        p.positionX * distanceconstants::Parsec,
        p.positionY * distanceconstants::Parsec,
        p.positionZ * distanceconstants::Parsec
    );

    const glm::dmat3 exoplanetSystemRotation = computeSystemRotation(starPosition);

    // Star renderable globe, if we have a radius and bv color index
    std::string starGlobeRenderableString;
    if (!std::isnan(p.rStar)) {
        const float radiusInMeter =
            p.rStar * static_cast<float>(distanceconstants::SolarRadius);

        std::string layers = "";
        if (!std::isnan(p.bmv)) {
            // @TODO (emmbr, 2020-10-12) should also check the bv value for the siblings.
            // The data on the planets is derived from different sources, so while this
            // planet has a nan value, another might not
            const std::string color = starColor(p.bmv);

            if (color.empty()) {
                LERROR("Error occurred when computing star color");
                return;
            }

            layers = "ColorLayers = {"
                "{"
                    "Identifier = 'StarColor',"
                    "Type = 'SolidColor',"
                    "Color = " + color + ","
                    "BlendMode = 'Normal',"
                    "Enabled = true"
                "},"
                "{"
                    "Identifier = 'StarTexture',"
                    "FilePath = openspace.absPath('" + StarTextureFile + "'),"
                    "BlendMode = 'Color',"
                    "Enabled = true"
                "}"
            "}";
        }

        starGlobeRenderableString = "Renderable = {"
            "Type = 'RenderableGlobe',"
            "Radii = " + std::to_string(radiusInMeter) + ","
            "SegmentsPerPatch = 64,"
            "PerformShading = false,"
            "Layers = {" + layers + "}"
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
        "Tag = {'exoplanet_system'},"
        "GUI = {"
            "Name = '" + starNameSpeck + " (Star)',"
            "Path = '" + guiPath + "'"
        "}"
    "}";

    openspace::global::scriptEngine.queueScript(
        "openspace.addSceneGraphNode(" + starParent + ");",
        scripting::ScriptEngine::RemoteScripting::Yes
    );

    // Planets
    for (size_t i = 0; i < planetSystem.size(); i++) {
        Exoplanet planet = planetSystem[i];
        const std::string planetName = planetNames[i];

        if (std::isnan(planet.ecc)) {
            planet.ecc = 0.f;
        }

        // KeplerTranslation requires angles in range [0, 360]
        auto validAngle = [](float angle, float defaultValue) {
            if (std::isnan(angle)) { return defaultValue; }
            if (angle < 0.f) { return angle + 360.f; }
            if (angle > 360.f) { return angle - 360.f; }
        };

        planet.i = validAngle(planet.i, 90.f);
        planet.bigOmega = validAngle(planet.bigOmega, 180.f);
        planet.omega = validAngle(planet.omega, 90.f);

        Time epoch;
        std::string sEpoch;
        if (!std::isnan(planet.tt)) {
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

        if (std::isnan(planet.r)) {
            if (std::isnan(planet.rStar)) {
                planetRadius = planet.a * 0.001f * astronomicalUnit;
            }
            else {
                planetRadius = planet.rStar * 0.1f * solarRadius;
            }
            enabled = "false";
        }
        else {
            planetRadius = static_cast<float>(planet.r) * jupiterRadius;
            enabled = "true";
        }

        const float period = static_cast<float>(planet.per * SecondsPerDay);
        const float semiMajorAxisInMeter = planet.a * astronomicalUnit;
        const float semiMajorAxisInKm = semiMajorAxisInMeter * 0.001f;

        const std::string planetIdentifier = createIdentifier(planetName);

        const std::string planetKeplerTranslation = "{"
            "Type = 'KeplerTranslation',"
            "Eccentricity = " + std::to_string(planet.ecc) + "," //ECC
            "SemiMajorAxis = " + std::to_string(semiMajorAxisInKm) + ","
            "Inclination = " + std::to_string(planet.i) + "," //I
            "AscendingNode = " + std::to_string(planet.bigOmega) + "," //BIGOM
            "ArgumentOfPeriapsis = " + std::to_string(planet.omega) + "," //OM
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

        int trailResolution = 1000;

        // increase the resolution for highly eccentric orbits
        const float eccentricityThreshold = 0.85f;
        if (planet.ecc > eccentricityThreshold) {
            trailResolution *= 2;
        }

        openspace::global::scriptEngine.queueScript(
            "openspace.addSceneGraphNode(" + planetNode + ");",
            scripting::ScriptEngine::RemoteScripting::Yes
        );

        const std::string planetTrailNode = "{"
            "Identifier = '" + planetIdentifier + "_Trail',"
            "Parent = '" + starIdentifier + "',"
            "Enabled = true,"
            "Renderable = {"
                "Type = 'RenderableTrailOrbit',"
                "Period = " + std::to_string(planet.per) + ","
                "Resolution = " + std::to_string(trailResolution) + ","
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

        bool hasUpperAUncertainty = !std::isnan(planet.aUpper);
        bool hasLowerAUncertainty = !std::isnan(planet.aLower);

        if (hasUpperAUncertainty && hasLowerAUncertainty) {
            // Get the orbit plane of the planet trail orbit from the KeplerTranslation
            const glm::dmat4 orbitPlaneRotationMatrix = computeOrbitPlaneRotationMatrix(
                planet.i,
                planet.bigOmega,
                planet.omega
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
                scripting::ScriptEngine::RemoteScripting::Yes
            );
        }
    }
}

int addExoplanetSystem(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::addExoplanetSystem");

    const int t = lua_type(L, 1);
    if (t == LUA_TSTRING) {
        // The user provided a single name
        const std::string& starName = ghoul::lua::value<std::string>(L, 1);
        createExoplanetSystem(starName);
    }
    else if (t == LUA_TTABLE) {
        // A list of names was provided
        ghoul::Dictionary d;
        ghoul::lua::luaDictionaryFromState(L, d);

        for (size_t i = 1; i <= d.size(); ++i) {
            if (!d.hasKeyAndValue<std::string>(std::to_string(i))) {
                return ghoul::lua::luaError(
                    L, fmt::format("List item {} is of invalid type", i)
                );
            }
            const std::string& starName = d.value<std::string>(std::to_string(i));
            createExoplanetSystem(starName);
        }
        lua_pop(L, 1);
    }
    else {
        return ghoul::lua::luaError(L, "Invalid input");
    }

    lua_settop(L, 0);
    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int removeExoplanetSystem(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::removeExoplanetSystem");

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

std::vector<std::string> readHostStarNames(std::ifstream& lookupTableFile) {
    std::vector<std::string> names;
    std::string line;

    // Read number of lines
    int nExoplanets = 0;
    while (getline(lookupTableFile, line)) {
        ++nExoplanets;
    }
    lookupTableFile.clear();
    lookupTableFile.seekg(0);
    names.reserve(nExoplanets);

    while (getline(lookupTableFile, line)) {
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

    return names;
}

int getListOfExoplanets(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::getListOfExoplanets");

    std::ifstream file(absPath(LookUpTablePath));
    if (!file.good()) {
        return ghoul::lua::luaError(
            L, fmt::format("Failed to open file '{}'", LookUpTablePath
        ));
    }

    std::vector<std::string> names = readHostStarNames(file);

    lua_newtable(L);
    int number = 1;
    for (const std::string& s : names) {
        lua_pushstring(L, s.c_str());
        lua_rawseti(L, -2, number);
        ++number;
    }

    return 1;
}

int listAvailableExoplanetSystems(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::listAvailableExoplanetSystems");

    std::ifstream file(absPath(LookUpTablePath));
    if (!file.good()) {
        return ghoul::lua::luaError(
            L, fmt::format("Failed to open file '{}'", LookUpTablePath
        ));
    }

    std::vector<std::string> names = readHostStarNames(file);

    std::string output;
    for (auto it = names.begin(); it != names.end(); ++it) {
        if (it != names.end()) {
            output += *it + ", ";
        }
    }

    LINFO(fmt::format(
        "There is data available for the following {} exoplanet systems: {}",
        names.size(), output
    ));

    return 0;
}
} //namespace openspace::exoplanets::luascriptfunctions
