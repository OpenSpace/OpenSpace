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
#include <modules/exoplanets/tasks/exoplanetsdatapreparationtask.h>
#include <openspace/scene/scene.h>
#include <ghoul/lua/lua_helper.h>
#include <ghoul/misc/csvreader.h>
#include <ghoul/misc/stringhelper.h>
#include <algorithm>
#include <map>
#include <string>
#include <string_view>

namespace {

constexpr std::string_view _loggerCat = "ExoplanetsModule";

openspace::exoplanets::ExoplanetSystem findSystemInData(std::string_view starName) {
    using namespace openspace;
    using namespace exoplanets;

    const ExoplanetsModule* module = global::moduleEngine->module<ExoplanetsModule>();

    const std::filesystem::path binPath = module->exoplanetsDataPath();
    std::ifstream data(absPath(binPath), std::ios::in | std::ios::binary);
    if (!data.good()) {
        throw ghoul::lua::LuaError(std::format(
            "Failed to open exoplanets data file '{}'", binPath
        ));
    }

    const std::filesystem::path lutPath = module->lookUpTablePath();
    std::ifstream lut(absPath(lutPath));
    if (!lut.good()) {
        throw ghoul::lua::LuaError(std::format(
            "Failed to open exoplanets look-up table '{}'", lutPath
        ));
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

std::vector<std::string> hostStarsWithSufficientData() {
    using namespace openspace;
    using namespace exoplanets;
    const ExoplanetsModule* module = global::moduleEngine->module<ExoplanetsModule>();

    if (!module->hasDataFiles()) {
        // If no data file path has been configured at all, we just bail out early here
        throw ghoul::lua::LuaError("No data path was configured for the exoplanets");
    }

    const std::filesystem::path lutPath = module->lookUpTablePath();
    std::ifstream lookupTableFile(absPath(lutPath));
    if (!lookupTableFile.good()) {
        throw ghoul::lua::LuaError(std::format(
            "Failed to open lookup table file '{}'", lutPath
        ));
    }

    const std::filesystem::path binPath = module->exoplanetsDataPath();
    std::ifstream data(absPath(binPath), std::ios::in | std::ios::binary);
    if (!data.good()) {
        throw ghoul::lua::LuaError(std::format("Failed to open data file '{}'", binPath));
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
 * Return an object containing the information needed to add a specific exoplanet system.
 * The data is retrieved from the module's prepared datafile for exoplanets. This file is
 * in a binary format, for fast retrieval during runtime.
 *
 * \param starName The name of the star to get the information for.
 *
 * \return An object of the type [ExoplanetSystemData](#exoplanets_exoplanet_system_data)
 *         that can be used to create the scene graph nodes for the exoplanet system
 */
[[codegen::luawrap]] ghoul::Dictionary systemData(std::string starName){
    using namespace openspace;

    exoplanets::ExoplanetSystem systemData = findSystemInData(starName);

    if (systemData.planetsData.empty()) {
        throw ghoul::lua::LuaError(std::format(
            "Exoplanet system '{}' could not be found", starName
        ));
    }

    return systemData.toDataDictionary();
}

/**
 * Remove a loaded exoplanet system.
 *
 * \param starName The name of the host star for the system to remove.
 */
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
 *
 * \return A list of exoplanet host star names.
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

/**
 * Lists the names of the host stars of all exoplanet systems that have sufficient
 * data for generating a visualization, and prints the list to the console.
 */
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
  * Load a set of exoplanet information based on custom data in the form of a CSV file.
  *
  * The format and column names in the CSV should be the same as the ones provided by the
  * [NASA Exoplanet Archive](https://exoplanetarchive.ipac.caltech.edu/).
  *
  * When dowloading the data from the archive we recommend including all columns, since a
  * few required ones are not selected by default.
  *
  * \param csvFile A path to the CSV file to load the data from.
  *
  * \return A list of objects of the type
  *         [ExoplanetSystemData](#exoplanets_exoplanet_system_data), that can be used to
  *         create the scene graph nodes for the exoplanet systems
  */
[[codegen::luawrap]] std::vector<ghoul::Dictionary> loadSystemDataFromCsv(
                                                                      std::string csvFile)
{
    using namespace openspace;
    using namespace exoplanets;

    using PlanetData = ExoplanetsDataPreparationTask::PlanetData;

    std::ifstream inputDataFile(csvFile);
    if (!inputDataFile.good()) {
        throw ghoul::lua::LuaError(std::format(
            "Failed to open input file '{}'", csvFile
        ));
    }

    std::vector<std::string> columnNames =
        ExoplanetsDataPreparationTask::readFirstDataRow(inputDataFile);

    const ExoplanetsModule* module = global::moduleEngine->module<ExoplanetsModule>();
    const std::filesystem::path
        teffBvConversionPath = module->teffToBvConversionFilePath();

    std::map<std::string, ExoplanetSystem> hostNameToSystemDataMap;

    LINFO(std::format("Reading exoplanet data from file '{}'", csvFile));

    // Parse the file line by line to compose system information
    std::string row;
    while (ghoul::getline(inputDataFile, row)) {
        PlanetData planetData = ExoplanetsDataPreparationTask::parseDataRow(
            row,
            columnNames,
            "",
            module->teffToBvConversionFilePath()
        );

        if (!hasSufficientData(planetData.dataEntry)) {
            LWARNING(std::format(
                "Insufficient data for exoplanet '{}'", planetData.name
            ));
            continue;
        }

        LINFO(std::format("Loaded data for planet '{}'", planetData.name));

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
            system.starName = planetData.host;
            system.planetNames.push_back(planetData.name);
            system.planetsData.push_back(planetData.dataEntry);
            updateStarDataFromNewPlanet(system.starData, planetData.dataEntry);

            hostNameToSystemDataMap[planetData.host] = system;
        }
    }

    std::vector<ghoul::Dictionary> result;
    result.reserve(hostNameToSystemDataMap.size());

    for (auto const& [_, system]: hostNameToSystemDataMap) {
        result.push_back(system.toDataDictionary());
    }
    return result;
}

#include "exoplanetsmodule_lua_codegen.cpp"

} // namespace
