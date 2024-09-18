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
#include <ghoul/misc/stringhelper.h>
#include <algorithm>
#include <map>
#include <string>
#include <string_view>

namespace {

constexpr std::string_view _loggerCat = "ExoplanetsModule";

constexpr std::string_view ExoplanetsGuiPath = "/Milky Way/Exoplanets/Exoplanet Systems/";

// Lua cannot handle backslashes, so replace these with forward slashes
std::string formatPathToLua(const std::filesystem::path& path) {
    std::string resPath = path.string();
    std::replace(resPath.begin(), resPath.end(), '\\', '/');
    return resPath;
}

openspace::exoplanets::ExoplanetSystem findExoplanetSystemInData(
                                                                std::string_view starName)
{
    using namespace openspace;
    using namespace exoplanets;

    const ExoplanetsModule* module = global::moduleEngine->module<ExoplanetsModule>();

    const std::filesystem::path binPath = module->exoplanetsDataPath();
    std::ifstream data(absPath(binPath), std::ios::in | std::ios::binary);
    if (!data.good()) {
        LERROR(std::format("Failed to open exoplanets data file '{}'", binPath));
        return ExoplanetSystem();
    }

    const std::filesystem::path lutPath = module->lookUpTablePath();
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
    while (ghoul::getline(lut, line)) {
        std::istringstream ss(line);
        std::string name;
        ghoul::getline(ss, name, ',');

        if (name.substr(0, name.length() - 2) != starName) {
            continue;
        }

        std::string location_s;
        ghoul::getline(ss, location_s);
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

void queueAddSceneGraphNodeScript(const std::string& sgnTableAsString) {
    using namespace openspace;
    // No sync or send because this will already be called inside a Lua script,
    // therefor it has already been synced and sent to the connected nodes and peers
    global::scriptEngine->queueScript({
        .code = std::format("openspace.addSceneGraphNode({})", sgnTableAsString),
        .synchronized = scripting::ScriptEngine::Script::ShouldBeSynchronized::No,
        .sendToRemote = scripting::ScriptEngine::Script::ShouldSendToRemote::No
    });
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
        const std::filesystem::path starTexture = module->starTexturePath();

        if (!starTexture.empty() && !std::filesystem::is_regular_file(starTexture)) {
            LWARNING(std::format(
                "Could not find specified star texture set in {} module: '{}'",
                module->guiName(), starTexture
            ));
        }

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
        const std::filesystem::path noDataTexture = module->noDataTexturePath();
        colorLayers =
            "{"
                "Identifier = 'NoDataStarTexture',"
                "FilePath = openspace.absPath('" + formatPathToLua(noDataTexture) + "'),"
                "BlendMode = 'Color',"
                "Enabled = true"
            "}";
    }

    // @TODO (2024-09-16, emmbr) Compose a more extensive summary of the system,
    // based on more data in the archive (like number of stars) and the planets.
    // Also include more data on the star itself (like spectral type)
    float distanceToOurSystem = glm::length(starPosInParsec) *
        distanceconstants::Parsec / distanceconstants::LightYear;

    size_t nPlanets = system.planetNames.size();
    const std::string starDescription = std::format(
        "The star {} is the host star of an exoplanet system with {} {} that {}  "
        "enough data to be visualized. It has a size of {:.2f} solar radii and an "
        "effective temperature of {:.0f} Kelvin. The system is located at a "
        "distance of {:.0f} light-years from Earth.",
        sanitizedStarName, nPlanets,
        nPlanets > 1 ? "planets" : "planet",
        nPlanets > 1 ? "have" : "has",
        radiusInMeter / distanceconstants::SolarRadius,
        system.starData.teff, distanceToOurSystem
    );

    const std::string starGlobeRenderableString = "Renderable = {"
        "Type = 'RenderableGlobe',"
        "Radii = " + std::to_string(radiusInMeter) + ","
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
            "Path = '" + guiPath + "',"
            "Description = \"" + starDescription + "\""
        "}"
    "}";

    queueAddSceneGraphNodeScript(starParent);

    // Add a label for the star.
    // The fade values are set based on the values for the Sun label
    const std::string starLabel = "{"
        "Identifier = '" + starIdentifier + "_Label',"
        "Parent = '" + starIdentifier + "',"
        "Renderable = {"
          "Type = 'RenderableLabel',"
          "Enabled = false,"
          "Text = '" + sanitizedStarName + "',"
          "FontSize = 70.0,"
          "Size = 14.17,"
          "MinMaxSize = { 1, 50 },"
          "EnableFading = true,"
          "FadeUnit = 'pc',"
          "FadeDistances = { 1.33, 15.0 },"
          "FadeWidths = {1.0, 20.0}"
        "},"
        "Tag = {'exoplanet_system_labels'},"
        "GUI = {"
            "Name = '" + sanitizedStarName + " Label',"
            "Path = '" + guiPath + "'"
        "}"
    "}";

    queueAddSceneGraphNodeScript(starLabel);

    // Planets

    const std::filesystem::path planetTexture = module->planetDefaultTexturePath();
    if (!planetTexture.empty() && !std::filesystem::is_regular_file(planetTexture)) {
        LWARNING(std::format(
            "Could not find specified planet default texture set in {} module: '{}'",
            module->guiName(), planetTexture
        ));
    }

    for (size_t i = 0; i < system.planetNames.size(); i++) {
        // Note that we are here overriding some invalid parameters in the planet data.
        // Use a reference, so that it is changed down the line
        ExoplanetDataEntry& planet = system.planetsData[i];
        const std::string planetName = system.planetNames[i];

        bool hasDefaultValuesForOrbitShape = false;
        bool hasDefaultValuesForOrbitTime = false;

        if (std::isnan(planet.ecc)) {
            planet.ecc = 0.f;
            hasDefaultValuesForOrbitShape = true;
        }

        // KeplerTranslation requires angles in range [0, 360]
        auto validAngle = [](float angle, float defaultValue) {
            if (std::isnan(angle)) { return defaultValue; }
            if (angle < 0.f) { return angle + 360.f; }
            if (angle > 360.f) { return angle - 360.f; }
            return angle;
        };

        if (std::isnan(planet.i)) {
            hasDefaultValuesForOrbitShape = true;
        }

        planet.i = validAngle(planet.i, 90.f);
        planet.bigOmega = validAngle(planet.bigOmega, 180.f);
        planet.omega = validAngle(planet.omega, 90.f);

        Time epoch;
        std::string sEpoch;
        if (!std::isnan(planet.tt)) {
            epoch.setTime("JD " + std::to_string(planet.tt));
            sEpoch = std::string(epoch.ISO8601());
            hasDefaultValuesForOrbitTime = true;
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

        std::string planetLayers;
        std::string planetTypeDesc;

        // Constant for different categories of sizes of planets (in Earth radii)
        // Source: https://www.nasa.gov/image-article/sizes-of-known-exoplanets/
        constexpr float TerrestrialMaxR = 1.25f;
        constexpr float SuperEarthMaxR = 2.f;
        constexpr float NeptuneLikeMaxR = 6.f;

        const std::string TerrestrialDesc = std::format(
            "Terrestrial planets (R < {} Earth radii)",
            TerrestrialMaxR
        );

        const std::string SuperEarthDesc = std::format(
            "Super-Earths ({} < R < {} Earth radii)",
            TerrestrialMaxR, SuperEarthMaxR
        );

        const std::string NeptuneLikeDesc = std::format(
            "Neptune-like planets ({} < R < {} Earth radii)",
            SuperEarthMaxR, NeptuneLikeMaxR
        );

        const std::string GasGiantDesc = std::format(
            "Gas giants or larger planets (R > {} Earth radii)",
            NeptuneLikeMaxR
        );

        // Add a color layer with a fixed single color that represent the planet size,
        // that is, approximately what type of planet it is.
        // @TODO (2024-09-10, emmbr) Ideally the user should be able to edit this
        if (!std::isnan(planet.r)) {
            float rInMeter = static_cast<float>(planetRadius);
            glm::vec3 colorFromSize = glm::vec3(0.f);

            if (rInMeter < TerrestrialMaxR * distanceconstants::EarthRadius) {
                // Terrestrial
                colorFromSize = glm::vec3(0.32f, 0.2f, 0.1f); // Brown
                planetTypeDesc = TerrestrialDesc;
            }
            else if (rInMeter < SuperEarthMaxR * distanceconstants::EarthRadius) {
                // Super-Earths
                colorFromSize = glm::vec3(1.f, 0.76f, 0.65f); // Beige
                planetTypeDesc = SuperEarthDesc;
            }
            else if (rInMeter < NeptuneLikeMaxR * distanceconstants::EarthRadius) {
                // Neptune-like
                colorFromSize = glm::vec3(0.22f, 0.49f, 0.50f); // Blue
                planetTypeDesc = NeptuneLikeDesc;
            }
            else {
                // Gas Giants (Saturn and Jupiter size, and much larger!)
                colorFromSize = glm::vec3(0.55f, 0.34f, 0.39f); // Wine red
                planetTypeDesc = GasGiantDesc;
            }

            const std::string description = std::format(
                "This layer gives a fixed color to the planet surface based on the "
                "planet radius. The planets are split into four categories based on "
                "their radius (in Earth radii). 1) {} are Brown, 2) {} are Beige, 3) "
                "{} are Blue, and 4) {} are Wine red.",
                TerrestrialDesc, SuperEarthDesc, NeptuneLikeDesc, GasGiantDesc
            );

            planetLayers += "{"
                "Identifier = 'ColorFromSize',"
                "Name = 'Color From Size',"
                "Type = 'SolidColor',"
                "Color = " + ghoul::to_string(colorFromSize) + ","
                "Enabled = true,"
                "Description = \"" + description + "\""
            "}";
        }

        if (!planetTexture.empty()) {
            planetLayers += ",{"
                "Identifier = 'PlanetTexture',"
                "Name = 'Planet Texture',"
                "FilePath = openspace.absPath('" + formatPathToLua(planetTexture) + "'),"
                "Enabled = true"
            "}";
        }

        const std::string planetDescription = std::format(
            "The exoplanet {} falls into the category of {}. Some key facts: "
            "Radius: {:.2f} Earth radii, {:.2f} Jupiter radii. "
            "Orbit Period: {:.1f} (Earth) days. "
            "Orbit Semi-major axis: {:.2f} (AU). "
            "Orbit Eccentricity: {:.2f}.",
            planetName, planetTypeDesc,
            planetRadius / distanceconstants::EarthRadius,
            planetRadius / distanceconstants::JupiterRadius,
            planet.per, planet.a, planet.ecc
        );

        // Use a higher ambient intensity when only the color from size is used, so that
        // the color is more clearly visible from any direction
        float ambientIntensity = planetTexture.empty() ? 0.5f : 0.15f;

        const std::string planetNode = "{"
            "Identifier = '" + planetIdentifier + "',"
            "Parent = '" + starIdentifier + "',"
            "Renderable = {"
                "Type = 'RenderableGlobe',"
                "Enabled = " + enabled + ","
                "Radii = " + std::to_string(planetRadius) + "," // in meters
                "PerformShading = true,"
                "Layers = {"
                    "ColorLayers = {" + planetLayers + "}"
                "},"
                "LightSourceNode = '" + starIdentifier + "',"
                "AmbientIntensity = " + std::to_string(ambientIntensity) +
            "},"
            "Tag = { 'exoplanet_planet' }, "
            "Transform = { "
                "Translation = " + planetKeplerTranslation + ""
            "},"
            "Tag = {'exoplanet_planet', 'exoplanet', 'exoplanet_globe'},"
            "GUI = {"
                "Name = '" + planetName + "',"
                "Path = '" + guiPath + "',"
                "Description = [[" + planetDescription + "]]"
            "}"
        "}";

        int trailResolution = 1000;

        // Increase the resolution for highly eccentric orbits
        constexpr float EccentricityThreshold = 0.85f;
        if (planet.ecc > EccentricityThreshold) {
            trailResolution *= 2;
        }

        std::string extraTags = "";
        if (hasDefaultValuesForOrbitShape) {
            extraTags += ", 'defaultvalues_shape'";
        }
        if (hasDefaultValuesForOrbitTime) {
            extraTags += ", 'defaultvalues_time'";
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
            "Tag = {'exoplanet', 'exoplanet_orbit'" + extraTags + "},"
            "GUI = {"
                "Name = '" + planetName + " Trail',"
                "Path = '" + guiPath + "'"
            "}"
        "}";

        queueAddSceneGraphNodeScript(planetTrailNode);
        queueAddSceneGraphNodeScript(planetNode);

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

            const std::filesystem::path discTexture = module->orbitDiscTexturePath();

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
                "Tag = {'exoplanet_uncertainty_disc', 'exoplanet', 'exoplanet_orbit_disc'},"
                "GUI = {"
                    "Name = '" + planetName + " Disc',"
                    "Path = '" + guiPath + "',"
                    "Description = \"The width of this disc around the planet's orbit "
                        "marks the uncertainty of the orbit (based on the uncertainty of "
                        "the semi-major axis and eccentricity measures). The wider the "
                        "disc, the more uncertain the orbit is.\""
                "}"
            "}";

            queueAddSceneGraphNodeScript(discNode);
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
            "Path = '" + guiPath + "',"
            "Description = \"A circle with a radius of 1 Astronomical Unit. That is, its "
                "size corresponds to the size of Earth's orbit.\""
        "}"
    "}";

    queueAddSceneGraphNodeScript(circle);

    float meanSemimajorAxisInAu = 0.f;
    float maxSemimajorAxisInAu = 0.f;
    for (const ExoplanetDataEntry& p : system.planetsData) {
        meanSemimajorAxisInAu += p.a;
        maxSemimajorAxisInAu = std::max(p.a, maxSemimajorAxisInAu);
    }
    meanSemimajorAxisInAu /= static_cast<float>(system.planetsData.size());

    // 90 degrees inclination plane
    const glm::dmat4 inclinationPlaneRotation = computeOrbitPlaneRotationMatrix(90.f);
    const glm::dmat3 inclinationPlaneRotationMatrix =
        static_cast<glm::dmat3>(inclinationPlaneRotation);

    const float planeSize = 2.f * meanSemimajorAxisInAu
        * static_cast<float>(distanceconstants::AstronomicalUnit);

    const std::string inclinationPlane = "{"
        "Identifier = '" + starIdentifier + "_EdgeOnInclinationPlane',"
        "Parent = '" + starIdentifier + "',"
        "Renderable = {"
            "Type = 'RenderableGrid',"
            "Enabled = false," // TODO: make property if we want to keep this around
            "Size = { " + std::format("{0}, {0}", planeSize) + "}, "
            "Color = { 0.4, 0.4, 0.4 }, "
            "Segments = { 10, 10 },"
            "LineWidth = 1.0,"
        "},"
        "Transform = {"
            "Rotation = {"
                "Type = 'StaticRotation',"
                "Rotation = " + ghoul::to_string(inclinationPlaneRotationMatrix) + ""
            "},"
        "},"
        "Tag = {'exoplanet_inclination_plane'},"
        "GUI = {"
            "Name = 'Edge-on Inclination Plane (" + sanitizedStarName + ")',"
            "Path = '" + guiPath + "'"
        "}"
    "}";

    global::scriptEngine->queueScript({
        .code = "openspace.addSceneGraphNode(" + inclinationPlane + ")",
        .synchronized = scripting::ScriptEngine::Script::ShouldBeSynchronized::No,
        .sendToRemote = scripting::ScriptEngine::Script::ShouldSendToRemote::No
    });

    // Arrow pointing to Earth
    const float maxSemiMajor = maxSemimajorAxisInAu
        * static_cast<float>(distanceconstants::AstronomicalUnit);

    const std::string toEarthArrow = "{"
        "Identifier = '" + starIdentifier + "_EarthDirectionArrow',"
        "Renderable = {"
            "Type = 'RenderableNodeArrow',"
            "StartNode = '" + starIdentifier + "',"
            "EndNode = 'Earth',"
            "Offset = " + std::to_string(1.5 * maxSemiMajor) + ","
            "Length = " + std::to_string(0.5 * maxSemiMajor) + ","
            "Width = " + std::to_string(0.015f * maxSemiMajor) + ","
        "},"
        "Tag = {'exoplanet_earth_arrow'},"
        "GUI = {"
            "Name = 'Earth Direction Arrow (" + sanitizedStarName + ")',"
            "Path = '" + guiPath + "'"
        "}"
    "}";

    global::scriptEngine->queueScript({
        .code = "openspace.addSceneGraphNode(" + toEarthArrow + ")",
        .synchronized = scripting::ScriptEngine::Script::ShouldBeSynchronized::No,
        .sendToRemote = scripting::ScriptEngine::Script::ShouldSendToRemote::No
    });

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

        const std::filesystem::path hzTexture = module->habitableZoneTexturePath();
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
            "Tag = {'exoplanet_habitable_zone', 'exoplanet_system_habitable_zone'},"
            "GUI = {"
                "Name = '" + starName + " Habitable Zone',"
                "Path = '" + guiPath + "',"
                "Description = '" + std::string(description) + "'"
            "}"
        "}";

        queueAddSceneGraphNodeScript(zoneDiscNode);

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

            const std::filesystem::path glareTexture = module->starGlareTexturePath();

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

            queueAddSceneGraphNodeScript(starGlare);
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

    const std::filesystem::path lutPath = module->lookUpTablePath();
    std::ifstream lookupTableFile(absPath(lutPath));
    if (!lookupTableFile.good()) {
        LERROR(std::format("Failed to open lookup table file '{}'", lutPath));
        return {};
    }

    const std::filesystem::path binPath = module->exoplanetsDataPath();
    std::ifstream data(absPath(binPath), std::ios::in | std::ios::binary);
    if (!data.good()) {
        LERROR(std::format("Failed to open data file '{}'", binPath));
        return {};
    }

    std::vector<std::string> names;
    std::string line;

    // Read number of lines
    int nExoplanets = 0;
    while (ghoul::getline(lookupTableFile, line)) {
        ++nExoplanets;
    }
    lookupTableFile.clear();
    lookupTableFile.seekg(0);
    names.reserve(nExoplanets);

    ExoplanetDataEntry p;
    while (ghoul::getline(lookupTableFile, line)) {
        std::stringstream ss(line);
        std::string name;
        ghoul::getline(ss, name, ',');
        // Remove the last two characters, that specify the planet
        name = name.substr(0, name.size() - 2);

        // Don't want to list systems where there is not enough data to visualize.
        // So, test if there is before adding the name to the list.
        std::string location_s;
        ghoul::getline(ss, location_s);
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
    global::scriptEngine->queueScript({
        .code = "openspace.removeSceneGraphNode('" + starIdentifier + "');",
        .synchronized = scripting::ScriptEngine::Script::ShouldBeSynchronized::No,
        .sendToRemote = scripting::ScriptEngine::Script::ShouldSendToRemote::No
    });
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
    const std::filesystem::path
        teffBvConversionPath = module->teffToBvConversionFilePath();

    std::map<std::string, ExoplanetSystem> hostNameToSystemDataMap;

    // Parse the file line by line to compose system information
    std::string row;
    while (ghoul::getline(inputDataFile, row)) {
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
