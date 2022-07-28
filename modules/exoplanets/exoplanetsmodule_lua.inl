/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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
        LERROR(fmt::format("Failed to open exoplanets data file: '{}'", binPath));
        return ExoplanetSystem();
    }

    const std::string lutPath = module->lookUpTablePath();
    std::ifstream lut(absPath(lutPath));
    if (!lut.good()) {
        LERROR(fmt::format("Failed to open exoplanets look-up table: '{}'", lutPath));
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
            LWARNING(fmt::format("Insufficient data for exoplanet: '{}'", name));
            continue;
        }

        system.planetNames.push_back(name);
        system.planetsData.push_back(p);

        // Star data - Should not vary between planets, but one data entry might lack data
        // for the host star while another does not. So for every planet, update star data
        // if needed
        const glm::vec3 pos = glm::vec3(p.positionX, p.positionY, p.positionZ);
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

    system.starName = starName;
    return system;
}

void createExoplanetSystem(const std::string& starName) {
    using namespace openspace;
    using namespace exoplanets;

    const std::string starIdentifier = createIdentifier(starName);

    std::string sanitizedStarName = starName;
    sanitizeNameString(sanitizedStarName);

    const std::string guiPath = fmt::format("{}{}", ExoplanetsGuiPath, sanitizedStarName);

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
            "Insufficient data available for exoplanet system: '{}'. Could not determine "
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

    global::scriptEngine->queueScript(
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

        global::scriptEngine->queueScript(
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

            const std::string discTexture = module->orbitDiscTexturePath();

            const std::string discNode = "{"
                "Identifier = '" + planetIdentifier + "_Disc',"
                "Parent = '" + starIdentifier + "',"
                "Renderable = {"
                    "Type = 'RenderableOrbitDisc',"
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
                "GUI = {"
                    "Name = '" + planetName + " Disc',"
                    "Path = '" + guiPath + "'"
                "}"
            "}";

            global::scriptEngine->queueScript(
                "openspace.addSceneGraphNode(" + discNode + ");",
                scripting::ScriptEngine::RemoteScripting::Yes
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
    const std::string isCircleEnabledString = isCircleEnabled ? "true" : "false";

    // 1 AU Size Comparison Circle
    const std::string circle = "{"
        "Identifier = '" + starIdentifier + "_1AU_Circle',"
        "Parent = '" + starIdentifier + "',"
        "Renderable = {"
            "Type = 'RenderableRadialGrid',"
            "Enabled = " + isCircleEnabledString + ","
            "Radii = { 0.0, 1.0 },"
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
        "GUI = {"
            "Name = '1 AU Size Comparison Circle',"
            "Path = '" + guiPath + "'"
        "}"
    "}";

    global::scriptEngine->queueScript(
        "openspace.addSceneGraphNode(" + circle + ");",
        scripting::ScriptEngine::RemoteScripting::Yes
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
        const std::string isHzEnabledString = isHzEnabled ? "true" : "false";

        bool useOptimistic = module->useOptimisticZone();
        const std::string useOptimisticString = useOptimistic ? "true" : "false";

        float opacity = module->habitableZoneOpacity();

        const std::string zoneDiscNode = "{"
            "Identifier = '" + starIdentifier + "_HZ_Disc',"
            "Parent = '" + starIdentifier + "',"
            "Renderable = {"
                "Type = 'RenderableHabitableZone',"
                "Enabled = " + isHzEnabledString + ","
                "Texture = openspace.absPath('" + formatPathToLua(hzTexture) + "'),"
                "Luminosity = " + std::to_string(system.starData.luminosity) + ","
                "EffectiveTemperature = " + std::to_string(system.starData.teff) + ","
                "Optimistic = " + useOptimisticString + ","
                "Opacity = " + std::to_string(opacity) + ""
            "},"
            "Transform = {"
                "Rotation = {"
                    "Type = 'StaticRotation',"
                    "Rotation = " + ghoul::to_string(meanOrbitPlaneRotationMatrix) + ""
                "}"
            "},"
            "GUI = {"
                "Name = '" + starName + " Habitable Zone',"
                "Path = '" + guiPath + "',"
                "Description = '" + std::string(description) + "'"
            "}"
        "}";

        global::scriptEngine->queueScript(
            "openspace.addSceneGraphNode(" + zoneDiscNode + ");",
            scripting::ScriptEngine::RemoteScripting::Yes
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

            global::scriptEngine->queueScript(
                "openspace.addSceneGraphNode(" + starGlare + ");",
                scripting::ScriptEngine::RemoteScripting::Yes
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
        LERROR(fmt::format("Failed to open lookup table file '{}'", lutPath));
        return {};
    }

    const std::string binPath = module->exoplanetsDataPath();
    std::ifstream data(absPath(binPath), std::ios::in | std::ios::binary);
    if (!data.good()) {
        LERROR(fmt::format("Failed to open data file '{}'", binPath));
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
    if (std::holds_alternative<std::string>(starNames)) {
        // The user provided a single name
        std::string starName = std::get<std::string>(starNames);
        createExoplanetSystem(starName);
    }
    else {
        std::vector<std::string> sns = std::get<std::vector<std::string>>(starNames);
        for (const std::string& starName : sns) {
            createExoplanetSystem(starName);
        }
    }
}

[[codegen::luawrap]] void removeExoplanetSystem(std::string starName) {
    using namespace openspace;
    using namespace exoplanets;
    const std::string starIdentifier = createIdentifier(std::move(starName));
    global::scriptEngine->queueScript(
        "openspace.removeSceneGraphNode('" + starIdentifier + "');",
        scripting::ScriptEngine::RemoteScripting::Yes
    );
}

[[codegen::luawrap]] std::vector<std::string> getListOfExoplanets() {
    std::vector<std::string> names = hostStarsWithSufficientData();
    return names;
}

[[codegen::luawrap]] void listAvailableExoplanetSystems() {
    std::vector<std::string> names = hostStarsWithSufficientData();

    std::string output;
    for (auto it = names.begin(); it != names.end(); ++it) {
        output += *it + ", ";
    }
    output.pop_back();
    output.pop_back();

    LINFO(fmt::format(
        "There is data available for the following {} exoplanet systems: {}",
        names.size(), output
    ));
}

#include "exoplanetsmodule_lua_codegen.cpp"

} // namespace
