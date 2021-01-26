/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

constexpr const char* LookUpTablePath = "${SYNC}/http/exoplanets_data/2/lookup.txt";
constexpr const char* ExoplanetsDataPath =
    "${SYNC}/http/exoplanets_data/2/exoplanets_data.bin";

constexpr const char* StarTextureFile = "${SYNC}/http/exoplanets_textures/1/sun.jpg";
constexpr const char* NoDataTextureFile =
    "${SYNC}/http/exoplanets_textures/1/grid-32.png";
constexpr const char* DiscTextureFile =
    "${SYNC}/http/exoplanets_textures/1/disc_texture.png";
constexpr const char* HabitableZoneTextureFile =
    "${SYNC}/http/habitable_zone_textures/1/green_faded.png";

constexpr const float AU = static_cast<float>(distanceconstants::AstronomicalUnit);
constexpr const float SolarRadius = static_cast<float>(distanceconstants::SolarRadius);
constexpr const float JupiterRadius = static_cast<float>(distanceconstants::JupiterRadius);

ExoplanetSystem findExoplanetSystemInData(std::string_view starName) {
    ExoplanetSystem system;

    std::ifstream data(absPath(ExoplanetsDataPath), std::ios::in | std::ios::binary);
    if (!data.good()) {
        LERROR(fmt::format(
            "Failed to open exoplanets data file: '{}'", absPath(ExoplanetsDataPath)
        ));
        return ExoplanetSystem();
    }

    std::ifstream lut(absPath(LookUpTablePath));
    if (!lut.good()) {
        LERROR(fmt::format(
            "Failed to open exoplanets look-up table: '{}'", absPath(LookUpTablePath)
        ));
        return ExoplanetSystem();
    }

    // 1. search lut for the starname and return the corresponding location
    // 2. go to that location in the data file
    // 3. read sizeof(exoplanet) bytes into an exoplanet object.
    ExoplanetDataEntry p;
    std::string line;
    while (std::getline(lut, line)) {
        std::istringstream ss(line);
        std::string name;
        std::getline(ss, name, ',');

        if (name.substr(0, name.length() - 2) == starName) {
            std::string location_s;
            std::getline(ss, location_s);
            long location = std::stol(location_s.c_str());

            data.seekg(location);
            data.read(reinterpret_cast<char*>(&p), sizeof(ExoplanetDataEntry));

            sanitizeNameString(name);

            if (!hasSufficientData(p)) {
                LWARNING(fmt::format("Insufficient data for exoplanet: '{}'", name));
                continue;
            }

            system.planetNames.push_back(name);
            system.planetsData.push_back(p);

            // Star data - Should not vary between planets, but one data entry might
            // lack data for the host star while another does not. So for every planet,
            // update star data if needed
            const glm::vec3 pos{ p.positionX, p.positionY, p.positionZ };
            if (system.starData.position != pos && isValidPosition(pos)) {
                system.starData.position = pos;
            }
            if (system.starData.radius != p.rStar && !std::isnan(p.rStar)) {
                system.starData.radius = p.rStar;
            }
            if (system.starData.bv != p.bmv && !std::isnan(p.bmv)) {
                system.starData.bv = p.bmv;
            }
            if (system.starData.teff != p.teff && !std::isnan(p.teff)) {
                system.starData.teff = p.teff;
            }
            if (system.starData.luminosity != p.luminosity && !std::isnan(p.luminosity)) {
                system.starData.luminosity = p.luminosity;
            }
        }
    }

    system.starName = starName;
    return system;
}

void createExoplanetSystem(const std::string& starName) {
    const std::string starIdentifier = createIdentifier(starName);

    std::string sanitizedStarName = starName;
    sanitizeNameString(sanitizedStarName);

    const std::string guiPath = ExoplanetsGuiPath + sanitizedStarName;

    SceneGraphNode* existingStarNode = sceneGraphNode(starIdentifier);
    if (existingStarNode) {
        LERROR(fmt::format(
            "Adding of exoplanet system '{}' failed. The system has already been added",
            starName
        ));
        return;
    }

    ExoplanetSystem system = findExoplanetSystemInData(starName);
    if (system.planetsData.empty()) {
        LERROR(fmt::format("Exoplanet system '{}' could not be found", starName));
        return;
    }

    const glm::vec3 starPosInParsec = system.starData.position;
    if (!isValidPosition(starPosInParsec)) {
        LERROR(fmt::format(
            "Insufficient data available for exoplanet system: '{}'. "
            "Could not determine star position", starName
        ));
        return;
    }

    const glm::dvec3 starPos =
        static_cast<glm::dvec3>(starPosInParsec) * distanceconstants::Parsec;
    const glm::dmat3 exoplanetSystemRotation = computeSystemRotation(starPos);

    // Star
    float radiusInMeter = SolarRadius;
    if (!std::isnan(system.starData.radius)) {
        radiusInMeter *= system.starData.radius;
    }

    std::string colorLayers;
    const float bv = system.starData.bv;

    if (!std::isnan(bv)) {
        const glm::vec3 color = starColor(bv);
        colorLayers =
            "{"
                "Identifier = 'StarColor',"
                "Type = 'SolidColor',"
                "Color = " + ghoul::to_string(color) + ","
                "BlendMode = 'Normal',"
                "Enabled = true"
            "},"
            "{"
                "Identifier = 'StarTexture',"
                "FilePath = " +
                    fmt::format("openspace.absPath('{}')", StarTextureFile) + ","
                "BlendMode = 'Color',"
                "Enabled = true"
            "}";
    }
    else {
        colorLayers =
            "{"
                "Identifier = 'NoDataStarTexture',"
                "FilePath = " +
                    fmt::format("openspace.absPath('{}')", NoDataTextureFile) + ","
                "BlendMode = 'Color',"
                "Enabled = true"
            "}";
    }

    const std::string starGlobeRenderableString = "Renderable = {"
        "Type = 'RenderableGlobe',"
        "Radii = " + std::to_string(radiusInMeter) + ","
        "SegmentsPerPatch = 64,"
        "PerformShading = false,"
        "Layers = {"
            "ColorLayers = { " + colorLayers + "}"
        "}"
    "},";

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
                "Position = " + ghoul::to_string(starPos) + ""
            "}"
        "},"
        "Tag = {'exoplanet_system'},"
        "GUI = {"
            "Name = '" + sanitizedStarName + " (Star)',"
            "Path = '" + guiPath + "'"
        "}"
    "}";

    openspace::global::scriptEngine->queueScript(
        "openspace.addSceneGraphNode(" + starParent + ");",
        scripting::ScriptEngine::RemoteScripting::Yes
    );

    // Planets
    for (size_t i = 0; i < system.planetNames.size(); i++) {
        ExoplanetDataEntry& planet = system.planetsData[i];
        const std::string planetName = system.planetNames[i];

        if (std::isnan(planet.ecc)) {
            planet.ecc = 0.f;
        }

        // KeplerTranslation requires angles in range [0, 360]
        auto validAngle = [](float angle, float defaultValue) {
            if (std::isnan(angle)) { return defaultValue; }
            if (angle < 0.f) { return angle + 360.f; }
            if (angle > 360.f) { return angle - 360.f; }
            return angle;
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
        if (std::isnan(planet.r)) {
            if (std::isnan(planet.rStar)) {
                planetRadius = planet.a * 0.001f * AU;
            }
            else {
                planetRadius = planet.rStar * 0.1f * SolarRadius;
            }
            enabled = "false";
        }
        else {
            planetRadius = static_cast<float>(planet.r) * JupiterRadius;
            enabled = "true";
        }

        const float periodInSeconds = static_cast<float>(planet.per * SecondsPerDay);
        const float semiMajorAxisInMeter = planet.a * AU;
        const float semiMajorAxisInKm = semiMajorAxisInMeter * 0.001f;

        const std::string planetIdentifier = createIdentifier(planetName);

        const std::string planetKeplerTranslation = "{"
            "Type = 'KeplerTranslation',"
            "Eccentricity = " + std::to_string(planet.ecc) + ","
            "SemiMajorAxis = " + std::to_string(semiMajorAxisInKm) + ","
            "Inclination = " + std::to_string(planet.i) + ","
            "AscendingNode = " + std::to_string(planet.bigOmega) + ","
            "ArgumentOfPeriapsis = " + std::to_string(planet.omega) + ","
            "MeanAnomaly = 0.0,"
            "Epoch = '" + sEpoch + "'," //TT. JD to YYYY MM DD hh:mm:ss
            "Period = " + std::to_string(periodInSeconds) + ""
        "}";

        const std::string planetNode = "{"
            "Identifier = '" + planetIdentifier + "',"
            "Parent = '" + starIdentifier + "',"
            "Enabled = true,"
            "Renderable = {"
                "Type = 'RenderableGlobe',"
                "Enabled = " + enabled + ","
                "Radii = " + std::to_string(planetRadius) + "," // in meters
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

        // Increase the resolution for highly eccentric orbits
        const float eccentricityThreshold = 0.85f;
        if (planet.ecc > eccentricityThreshold) {
            trailResolution *= 2;
        }

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

        openspace::global::scriptEngine->queueScript(
            "openspace.addSceneGraphNode(" + planetTrailNode + ");"
            "openspace.addSceneGraphNode(" + planetNode + ");",
            scripting::ScriptEngine::RemoteScripting::Yes
        );

        bool hasUpperAUncertainty = !std::isnan(planet.aUpper);
        bool hasLowerAUncertainty = !std::isnan(planet.aLower);

        if (hasUpperAUncertainty && hasLowerAUncertainty) {
            const glm::dmat4 rotation = computeOrbitPlaneRotationMatrix(
                planet.i,
                planet.bigOmega,
                planet.omega
            );
            const glm::dmat3 rotationMat3 = static_cast<glm::dmat3>(rotation);

            const float lowerOffset = static_cast<float>(planet.aLower / planet.a);
            const float upperOffset = static_cast<float>(planet.aUpper / planet.a);

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
                        std::to_string(lowerOffset) + ", " +
                        std::to_string(upperOffset) +
                    "}," //min / max extend
                    "Opacity = 0.3"
                "},"
                "Transform = {"
                    "Rotation = {"
                        "Type = 'StaticRotation',"
                        "Rotation = " + ghoul::to_string(rotationMat3) + ""
                    "}"
                "},"
                "GUI = {"
                    "Name = '" + planetName + " Disc',"
                    "Path = '" + guiPath + "'"
                "}"
            "}";

            openspace::global::scriptEngine->queueScript(
                "openspace.addSceneGraphNode(" + discNode + ");",
                scripting::ScriptEngine::RemoteScripting::Yes
            );
        }
    }

    // Habitable Zone
    bool hasTeff = !std::isnan(system.starData.teff);
    bool hasLuminosity = !std::isnan(system.starData.luminosity);

    if (hasTeff && hasLuminosity) {
        float meanInclination = 0.f;
        for (const ExoplanetDataEntry& p : system.planetsData) {
            meanInclination += p.i;
        }
        meanInclination /= static_cast<float>(system.planetsData.size());
        const glm::dmat4 rotation = computeOrbitPlaneRotationMatrix(meanInclination);
        const glm::dmat3 rotationMat3 = static_cast<glm::dmat3>(rotation);

        constexpr const char* description =
            "The habitable zone is the region around a star in which an Earth-like "
            "planet can potentially have liquid water on its surface."
            "<br><br>"
            "The inner boundary is where the greenhouse gases in the atmosphere "
            "would trap any incoming infrared radiation, leading to the planet "
            "surface becoming so hot that water boils away. The outer boundary is where "
            "the greenhouse effect would not be able to maintain surface temperature "
            "above freezing anywhere on the planet.";

        const std::string zoneDiscNode = "{"
            "Identifier = '" + starIdentifier + "_HZ_Disc',"
            "Parent = '" + starIdentifier + "',"
            "Enabled = true,"
            "Renderable = {"
                "Type = 'RenderableHabitableZone',"
                "Texture = openspace.absPath('" + HabitableZoneTextureFile + "'),"
                "Luminosity = " + std::to_string(system.starData.luminosity) + ","
                "EffectiveTemperature = " + std::to_string(system.starData.teff) + ","
                "Opacity = 0.07"
            "},"
            "Transform = {"
                "Rotation = {"
                    "Type = 'StaticRotation',"
                    "Rotation = " + ghoul::to_string(rotationMat3) + ""
                "}"
            "},"
            "GUI = {"
                "Name = '" + starName + " Habitable Zone',"
                "Path = '" + guiPath + "',"
                "Description = '" + description + "'"
            "}"
        "}";

        openspace::global::scriptEngine->queueScript(
            "openspace.addSceneGraphNode(" + zoneDiscNode + ");",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
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
            if (!d.hasValue<std::string>(std::to_string(i))) {
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
    const std::string starIdentifier = createIdentifier(starName);

    openspace::global::scriptEngine->queueScript(
        "openspace.removeSceneGraphNode('" + starIdentifier + "');",
        scripting::ScriptEngine::RemoteScripting::Yes
    );

    return 0;
}

std::vector<std::string> hostStarsWithSufficientData() {
    std::vector<std::string> names;
    std::string line;

    std::ifstream lookupTableFile(absPath(LookUpTablePath));
    if (!lookupTableFile.good()) {
        LERROR(fmt::format("Failed to open lookup table file '{}'", LookUpTablePath));
        return {};
    }

    std::ifstream data(absPath(ExoplanetsDataPath), std::ios::in | std::ios::binary);
    if (!data.good()) {
        LERROR(fmt::format("Failed to open data file '{}'", ExoplanetsDataPath));
        return {};
    }

    // Read number of lines
    int nExoplanets = 0;
    while (std::getline(lookupTableFile, line)) {
        ++nExoplanets;
    }
    lookupTableFile.clear();
    lookupTableFile.seekg(0);
    names.reserve(nExoplanets);

    ExoplanetDataEntry p;
    while (std::getline(lookupTableFile, line)) {
        std::stringstream ss(line);
        std::string name;
        std::getline(ss, name, ',');
        // Remove the last two characters, that specify the planet
        name = name.substr(0, name.size() - 2);

        // Don't want to list systems where there is not enough data to visualize.
        // So, test if there is before adding the name to the list.
        std::string location_s;
        std::getline(ss, location_s);
        long location = std::stol(location_s.c_str());

        data.seekg(location);
        data.read(reinterpret_cast<char*>(&p), sizeof(ExoplanetDataEntry));

        if (hasSufficientData(p)) {
            names.push_back(name);
        }
    }

    // For easier read, sort by names and remove duplicates
    std::sort(names.begin(), names.end());
    names.erase(std::unique(names.begin(), names.end()), names.end());

    return names;
}

int getListOfExoplanets(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::getListOfExoplanets");

    std::vector<std::string> names = hostStarsWithSufficientData();

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

    std::vector<std::string> names = hostStarsWithSufficientData();

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
