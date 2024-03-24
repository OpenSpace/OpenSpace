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

#include <openspace/scene/scene.h>
#include <ghoul/misc/csvreader.h>
#include <algorithm>
#include <map>
#include <string>
#include <string_view>

namespace {

constexpr std::string_view _loggerCat = "ExoplanetsModule";

constexpr std::string_view ExoplanetsGuiPath = "/Milky Way/Exoplanets/Exoplanet Systems/";

// Lua cannot handle backslashes, so replace these with forward slashes
std::string formatPathToLua(const std::string& path) {
    std::string resPath = path;
    std::replace(resPath.begin(), resPath.end(), '\\', '/');
    return resPath;
}

openspace::exoplanets::ExoplanetSystem findExoplanetSystemInData(
                                                                std::string_view starName)
{
    using namespace openspace;
    using namespace exoplanets;

    const ExoplanetsModule* module = global::moduleEngine->module<ExoplanetsModule>();

    const std::string binPath = module->exoplanetsDataPath();
    std::ifstream data(absPath(binPath), std::ios::in | std::ios::binary);
    if (!data.good()) {
        LERROR(std::format("Failed to open exoplanets data file '{}'", binPath));
        return ExoplanetSystem();
    }

    const std::string lutPath = module->lookUpTablePath();
    std::ifstream lut(absPath(lutPath));
    if (!lut.good()) {
        LERROR(std::format("Failed to open exoplanets look-up table '{}'", lutPath));
        return ExoplanetSystem();
    }

    ExoplanetSystem system;

    // 1. search lut for the starname and return the corresponding location
    // 2. go to that location in the data file
    // 3. read sizeof(exoplanet) bytes into an exoplanet object.
    ExoplanetDataEntry p;
    std::string line;
    while (std::getline(lut, line)) {
        std::istringstream ss(line);
        std::string name;
        std::getline(ss, name, ',');

        if (name.substr(0, name.length() - 2) != starName) {
            continue;
        }

        std::string location_s;
        std::getline(ss, location_s);
        long location = std::stol(location_s.c_str());

        data.seekg(location);
        data.read(reinterpret_cast<char*>(&p), sizeof(ExoplanetDataEntry));

        sanitizeNameString(name);

        if (!hasSufficientData(p)) {
            LWARNING(std::format("Insufficient data for exoplanet '{}'", name));
            continue;
        }

        system.planetNames.push_back(name);
        system.planetsData.push_back(p);

        updateStarDataFromNewPlanet(system.starData, p);
    }

    system.starName = starName;
    return system;
}

void createExoplanetSystem(const std::string& starName,
                           openspace::exoplanets::ExoplanetSystem system)
{
    using namespace openspace;
    using namespace exoplanets;

    const std::string starIdentifier = makeIdentifier(starName);

    std::string sanitizedStarName = starName;
    sanitizeNameString(sanitizedStarName);

    const std::string guiPath = std::format("{}{}", ExoplanetsGuiPath, sanitizedStarName);

    SceneGraphNode* existingStarNode = sceneGraphNode(starIdentifier);
    if (existingStarNode) {
        LERROR(std::format(
            "Adding of exoplanet system '{}' failed. The system has already been added",
            starName
        ));
        return;
    }

    const glm::vec3 starPosInParsec = system.starData.position;
    if (!isValidPosition(starPosInParsec)) {
        LERROR(std::format(
            "Insufficient data available for exoplanet system '{}'. Could not determine "
            "star position", starName
        ));
        return;
    }

    const ExoplanetsModule* module = global::moduleEngine->module<ExoplanetsModule>();

    const glm::dvec3 starPos =
        static_cast<glm::dvec3>(starPosInParsec) * distanceconstants::Parsec;
    const glm::dmat3 exoplanetSystemRotation = computeSystemRotation(starPos);

    // Star
    double radiusInMeter = distanceconstants::SolarRadius;
    if (!std::isnan(system.starData.radius)) {
        radiusInMeter *= system.starData.radius;
    }

    std::string colorLayers;
    std::optional<glm::vec3> starColor = std::nullopt;
    const float bv = system.starData.bv;

    if (!std::isnan(bv)) {
        starColor = computeStarColor(bv);
        const std::string starTexture = module->starTexturePath();
        colorLayers =
            "{"
                "Identifier = 'StarColor',"
                "Type = 'SolidColor',"
                "Color = " + ghoul::to_string(*starColor) + ","
                "BlendMode = 'Normal',"
                "Enabled = true"
            "},"
            "{"
                "Identifier = 'StarTexture',"
                "FilePath = openspace.absPath('" + formatPathToLua(starTexture) + "'),"
                "BlendMode = 'Color',"
                "Enabled = true"
            "}";
    }
    else {
        const std::string noDataTexture = module->noDataTexturePath();
        colorLayers =
            "{"
                "Identifier = 'NoDataStarTexture',"
                "FilePath = openspace.absPath('" + formatPathToLua(noDataTexture) + "'),"
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

    // No sync or send because this is already inside a Lua script, therefor it has
    // already been synced and sent to the connected nodes and peers
    global::scriptEngine->queueScript(
        "openspace.addSceneGraphNode(" + starParent + ");",
        scripting::ScriptEngine::ShouldBeSynchronized::No,
        scripting::ScriptEngine::ShouldSendToRemote::No
    );

    // Planets
    for (size_t i = 0; i < system.planetNames.size(); i++) {
        // Note that we are here overriding some invalid parameters in the planet data.
        // Use a reference, so that it is changed down the line
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

        double planetRadius;
        std::string enabled;
        if (std::isnan(planet.r)) {
            if (std::isnan(planet.rStar)) {
                planetRadius = planet.a * 0.001 * distanceconstants::AstronomicalUnit;
            }
            else {
                planetRadius = planet.rStar * 0.1 * distanceconstants::SolarRadius;
            }
            enabled = "false";
        }
        else {
            planetRadius = planet.r * distanceconstants::JupiterRadius;
            enabled = "true";
        }

        float periodInSeconds = static_cast<float>(planet.per * SecondsPerDay);
        double semiMajorAxisInMeter = planet.a * distanceconstants::AstronomicalUnit;
        double semiMajorAxisInKm = semiMajorAxisInMeter * 0.001;

        const std::string planetIdentifier = makeIdentifier(planetName);

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

        // No sync or send because this is already inside a Lua script, therefor it has
        // already been synced and sent to the connected nodes and peers
        global::scriptEngine->queueScript(
            "openspace.addSceneGraphNode(" + planetTrailNode + ");"
            "openspace.addSceneGraphNode(" + planetNode + ");",
            scripting::ScriptEngine::ShouldBeSynchronized::No,
            scripting::ScriptEngine::ShouldSendToRemote::No
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

            const std::string discTexture = module->orbitDiscTexturePath();

            bool isDiscEnabled = module->showOrbitUncertainty();

            const std::string discNode = "{"
                "Identifier = '" + planetIdentifier + "_Disc',"
                "Parent = '" + starIdentifier + "',"
                "Renderable = {"
                    "Type = 'RenderableOrbitDisc',"
                    "Enabled = " + (isDiscEnabled ? "true" : "false") +  ","
                    "Texture = openspace.absPath('" +
                        formatPathToLua(discTexture) +
                    "'),"
                    "Size = " + std::to_string(semiMajorAxisInMeter) + ","
                    "Eccentricity = " + std::to_string(planet.ecc) + ","
                    "Offset = { " +
                        std::to_string(lowerOffset) + ", " +
                        std::to_string(upperOffset) +
                    "}," //min / max extend
                    "Opacity = 0.25"
                "},"
                "Transform = {"
                    "Rotation = {"
                        "Type = 'StaticRotation',"
                        "Rotation = " + ghoul::to_string(rotationMat3) + ""
                    "}"
                "},"
                "Tag = {'exoplanet_uncertainty_disc'},"
                "GUI = {"
                    "Name = '" + planetName + " Disc',"
                    "Path = '" + guiPath + "'"
                "}"
            "}";

            // No sync or send because this is already inside a Lua script, therefor it
            // has already been synced and sent to the connected nodes and peers
            global::scriptEngine->queueScript(
                "openspace.addSceneGraphNode(" + discNode + ");",
                scripting::ScriptEngine::ShouldBeSynchronized::No,
                scripting::ScriptEngine::ShouldSendToRemote::No
            );
        }
    }

    float meanInclination = 0.f;
    for (const ExoplanetDataEntry& p : system.planetsData) {
        meanInclination += p.i;
    }
    meanInclination /= static_cast<float>(system.planetsData.size());
    const glm::dmat4 rotation = computeOrbitPlaneRotationMatrix(meanInclination);
    const glm::dmat3 meanOrbitPlaneRotationMatrix = static_cast<glm::dmat3>(rotation);

    bool isCircleEnabled = module->showComparisonCircle();
    glm::vec3 circleColor = module->comparisonCircleColor();

    // 1 AU Size Comparison Circle
    const std::string circle = "{"
        "Identifier = '" + starIdentifier + "_1AU_Circle',"
        "Parent = '" + starIdentifier + "',"
        "Renderable = {"
            "Type = 'RenderableRadialGrid',"
            "Enabled = " + (isCircleEnabled ? "true" : "false") + ","
            "Radii = { 0.0, 1.0 },"
            "Color = " + ghoul::to_string(circleColor) + ","
            "CircleSegments = 64,"
            "LineWidth = 2.0,"
        "},"
        "Transform = {"
            "Rotation = {"
                "Type = 'StaticRotation',"
                "Rotation = " + ghoul::to_string(meanOrbitPlaneRotationMatrix) + ""
            "},"
            "Scale = {"
                "Type = 'StaticScale',"
                "Scale = " + std::to_string(distanceconstants::AstronomicalUnit) + ""
            "}"
        "},"
        "Tag = {'exoplanet_1au_ring'},"
        "GUI = {"
            "Name = '1 AU Size Comparison Circle',"
            "Path = '" + guiPath + "'"
        "}"
    "}";

    // No sync or send because this is already inside a Lua script, therefor it has
    // already been synced and sent to the connected nodes and peers
    global::scriptEngine->queueScript(
        "openspace.addSceneGraphNode(" + circle + ");",
        scripting::ScriptEngine::ShouldBeSynchronized::No,
        scripting::ScriptEngine::ShouldSendToRemote::No
    );

    // Habitable Zone
    bool hasTeff = !std::isnan(system.starData.teff);
    bool hasLuminosity = !std::isnan(system.starData.luminosity);

    if (hasTeff && hasLuminosity) {
        constexpr std::string_view description =
            "The habitable zone is the region around a star in which an Earth-like "
            "planet can potentially have liquid water on its surface."
            "<br><br>"
            "The inner boundary is where the greenhouse gases in the atmosphere "
            "would trap any incoming infrared radiation, leading to the planet "
            "surface becoming so hot that water boils away. The outer boundary is where "
            "the greenhouse effect would not be able to maintain surface temperature "
            "above freezing anywhere on the planet";

        const std::string hzTexture = module->habitableZoneTexturePath();
        bool isHzEnabled = module->showHabitableZone();
        bool useOptimistic = module->useOptimisticZone();
        float opacity = module->habitableZoneOpacity();

        const std::string zoneDiscNode = "{"
            "Identifier = '" + starIdentifier + "_HZ_Disc',"
            "Parent = '" + starIdentifier + "',"
            "Renderable = {"
                "Type = 'RenderableHabitableZone',"
                "Enabled = " + (isHzEnabled ? "true" : "false") + ","
                "Texture = openspace.absPath('" + formatPathToLua(hzTexture) + "'),"
                "Luminosity = " + std::to_string(system.starData.luminosity) + ","
                "EffectiveTemperature = " + std::to_string(system.starData.teff) + ","
                "Optimistic = " + (useOptimistic ? "true" : "false") + ","
                "Opacity = " + std::to_string(opacity) + ""
            "},"
            "Transform = {"
                "Rotation = {"
                    "Type = 'StaticRotation',"
                    "Rotation = " + ghoul::to_string(meanOrbitPlaneRotationMatrix) + ""
                "}"
            "},"
            "Tag = {'exoplanet_habitable_zone'},"
            "GUI = {"
                "Name = '" + starName + " Habitable Zone',"
                "Path = '" + guiPath + "',"
                "Description = '" + std::string(description) + "'"
            "}"
        "}";

        // No sync or send because this is already inside a Lua script, therefor it has
        // already been synced and sent to the connected nodes and peers
        global::scriptEngine->queueScript(
            "openspace.addSceneGraphNode(" + zoneDiscNode + ");",
            scripting::ScriptEngine::ShouldBeSynchronized::No,
            scripting::ScriptEngine::ShouldSendToRemote::No
        );

        // Star glare
        if (starColor.has_value()) {
            // This is a little magic to make the size of the glare dependent on the
            // size and the temperature of the star. It's kind of based on the fact that
            // the luminosity of a star is proportional to: (radius^2)*(temperature^4)
            // Maybe a better option would be to compute the size based on the aboslute
            // magnitude or star luminosity, but for now this looks good enough.
            double size = 59.0 * radiusInMeter;
            if (hasTeff) {
                constexpr float sunTeff = 5780.f;
                size *= std::pow(system.starData.teff / sunTeff, 2.0);
            }

            const std::string glareTexture = module->starGlareTexturePath();

            const std::string starGlare = "{"
                "Identifier = '" + starIdentifier + "_Glare',"
                "Parent = '" + starIdentifier + "',"
                "Renderable = {"
                    "Type = 'RenderablePlaneImageLocal',"
                    "Size = " + ghoul::to_string(size) + ","
                    "Origin = 'Center',"
                    "Billboard = true,"
                    "Texture = openspace.absPath('"
                        + formatPathToLua(glareTexture) +
                    "'),"
                    "BlendMode = 'Additive',"
                    "Opacity = 0.65,"
                    "MultiplyColor = " + ghoul::to_string(*starColor) + ""
                "},"
                "GUI = {"
                    "Name = '" + sanitizedStarName + " Glare',"
                    "Path = '" + guiPath + "'"
                "}"
            "}";

            // No sync or send because this is already inside a Lua script, therefor it
            // has already been synced and sent to the connected nodes and peers
            global::scriptEngine->queueScript(
                "openspace.addSceneGraphNode(" + starGlare + ");",
                scripting::ScriptEngine::ShouldBeSynchronized::No,
                scripting::ScriptEngine::ShouldSendToRemote::No
            );
        }
    }
}

std::vector<std::string> hostStarsWithSufficientData() {
    using namespace openspace;
    using namespace exoplanets;
    const ExoplanetsModule* module = global::moduleEngine->module<ExoplanetsModule>();

    if (!module->hasDataFiles()) {
        // If no data file path has been configured at all, we just bail out early here
        LINFO("No data path was configured for the exoplanets");
        return {};
    }

    const std::string lutPath = module->lookUpTablePath();
    std::ifstream lookupTableFile(absPath(lutPath));
    if (!lookupTableFile.good()) {
        LERROR(std::format("Failed to open lookup table file '{}'", lutPath));
        return {};
    }

    const std::string binPath = module->exoplanetsDataPath();
    std::ifstream data(absPath(binPath), std::ios::in | std::ios::binary);
    if (!data.good()) {
        LERROR(std::format("Failed to open data file '{}'", binPath));
        return {};
    }

    std::vector<std::string> names;
    std::string line;

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

/**
 * Add one or multiple exoplanet systems to the scene, as specified by the input. An input
 * string should be the name of the system host star.
 */
[[codegen::luawrap]] void addExoplanetSystem(
                            std::variant<std::string, std::vector<std::string>> starNames)
{
    std::vector<std::string> starsToAdd;

    if (std::holds_alternative<std::string>(starNames)) {
        // The user provided a single name
        const std::string starName = std::get<std::string>(starNames);
        starsToAdd.push_back(starName);
    }
    else {
        starsToAdd = std::get<std::vector<std::string>>(starNames);
    }

    for (const std::string& starName : starsToAdd) {
        openspace::exoplanets::ExoplanetSystem systemData =
            findExoplanetSystemInData(starName);

        if (systemData.planetsData.empty()) {
            LERROR(std::format("Exoplanet system '{}' could not be found", starName));
            return;
        }

        createExoplanetSystem(starName, systemData);
    }

}

[[codegen::luawrap]] void removeExoplanetSystem(std::string starName) {
    using namespace openspace;
    using namespace exoplanets;
    const std::string starIdentifier = makeIdentifier(std::move(starName));

    // No sync or send because this is already inside a Lua script, therefor it has
    // already been synced and sent to the connected nodes and peers
    global::scriptEngine->queueScript(
        "openspace.removeSceneGraphNode('" + starIdentifier + "');",
        scripting::ScriptEngine::ShouldBeSynchronized::No,
        scripting::ScriptEngine::ShouldSendToRemote::No
    );
}

/**
 * Returns a list with names of the host star of all the exoplanet systems
 * that have sufficient data for generating a visualization, based on the
 * module's loaded data file.
 */
[[codegen::luawrap]] std::vector<std::string> listOfExoplanets() {
    std::vector<std::string> names = hostStarsWithSufficientData();
    return names;
}

/**
 * Deprecated in favor of 'listOfExoplanets'
 */
[[codegen::luawrap("getListOfExoplanets")]] std::vector<std::string>
listOfExoplanetsDeprecated()
{
    LWARNINGC(
        "Deprecation",
        "'getListOfExoplanets' function is deprecated and should be replaced with "
        "'listOfExoplanets'"
    );
    return listOfExoplanets();
}

[[codegen::luawrap]] void listAvailableExoplanetSystems() {
    std::vector<std::string> names = hostStarsWithSufficientData();

    std::string output;
    for (const std::string& name : names) {
        output += name + ", ";
    }
    output.pop_back();
    output.pop_back();

    LINFO(std::format(
        "There is data available for the following {} exoplanet systems: {}",
        names.size(), output
    ));
}

/**
 * Load a set of exoplanets based on custom data, in the form of a CSV file, and add
 * them to the rendering. Can be used to load custom datasets, or more recent planets
 * than what are included in the internal data file that is released with OpenSpace.
 *
 * The format and column names in the CSV sould be the same as the ones provided by the
 * NASA Exoplanet Archive. https://exoplanetarchive.ipac.caltech.edu/
 *
 * We recommend downloading the file from the Exoplanet Archive's Composite data table,
 * where multiple sources are combined into one row per planet.
 * https://exoplanetarchive.ipac.caltech.edu
 * /cgi-bin/TblView/nph-tblView?app=ExoTbls&config=PSCompPars
 *
 * Please remember to include all columns in the file download, as missing data columns
 * may lead to an incomplete visualization.
 *
 * Also, avoid loading too large files of planets, as each added system will affect the
 * rendering performance.
 */
[[codegen::luawrap]] void loadExoplanetsFromCsv(std::string csvFile) {
    using namespace openspace;
    using namespace exoplanets;

    using PlanetData = ExoplanetsDataPreparationTask::PlanetData;

    std::ifstream inputDataFile(csvFile);
    if (!inputDataFile.good()) {
        LERROR(std::format("Failed to open input file '{}'", csvFile));
        return;
    }

    std::vector<std::string> columnNames =
        ExoplanetsDataPreparationTask::readFirstDataRow(inputDataFile);

    const ExoplanetsModule* module = global::moduleEngine->module<ExoplanetsModule>();
    const std::string teffBvConversionPath = module->teffToBvConversionFilePath();

    std::map<std::string, ExoplanetSystem> hostNameToSystemDataMap;

    // Parse the file line by line to compose system information
    std::string row;
    while (std::getline(inputDataFile, row)) {
        PlanetData planetData = ExoplanetsDataPreparationTask::parseDataRow(
            row,
            columnNames,
            "",
            module->teffToBvConversionFilePath()
        );

        LINFO(std::format("Reading data for planet '{}'", planetData.name));

        if (!hasSufficientData(planetData.dataEntry)) {
            LWARNING(std::format(
                "Insufficient data for exoplanet '{}'", planetData.name
            ));
            continue;
        }

        auto found = hostNameToSystemDataMap.find(planetData.host);
        if (found != hostNameToSystemDataMap.end()) {
            // Found a match. Add the planet to the system data
            ExoplanetSystem& system = found->second;
            sanitizeNameString(planetData.name);
            system.planetNames.push_back(planetData.name);
            system.planetsData.push_back(planetData.dataEntry);
            updateStarDataFromNewPlanet(system.starData, planetData.dataEntry);
        }
        else {
            // No host found. Add a new one
            ExoplanetSystem system;
            sanitizeNameString(planetData.name);
            system.planetNames.push_back(planetData.name);
            system.planetsData.push_back(planetData.dataEntry);
            updateStarDataFromNewPlanet(system.starData, planetData.dataEntry);

            hostNameToSystemDataMap[planetData.host] = system;
        }
    }

    // Add all the added exoplanet systems
    using K = const std::string;
    using V = ExoplanetSystem;
    for (const std::pair<K, V>& entry : hostNameToSystemDataMap) {
        const std::string& hostName = entry.first;
        const ExoplanetSystem& data = entry.second;
        createExoplanetSystem(hostName, data);
    }

    LINFO(std::format(
        "Read data for {} exoplanet systems from CSV file: {}. Please wait until "
        "they are all finished initializing. You may have to reload the user interface.",
        hostNameToSystemDataMap.size(), csvFile
    ));
}

#include "exoplanetsmodule_lua_codegen.cpp"

} // namespace
