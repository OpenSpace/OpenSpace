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

#include <modules/exoplanetsexperttool/dataloader.h>

#include <modules/exoplanetsexperttool/datahelper.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/scene/scene.h>
#include <openspace/util/coordinateconversion.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/csvreader.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/stringhelper.h>
#include <ghoul/logging/logmanager.h>
#include <json/json.hpp>
#include <scn/scan.h>
#include <charconv>
#include <cmath>
#include <fstream>
#include <limits>
#include <string>
#include <string_view>

namespace {
    constexpr std::string_view _loggerCat = "ExoplanetsDataLoader";

    // @TODO: naturally, this path should not be hardcoded
    constexpr std::string_view DataSettingsPath = "scripts/datasettings.json";
    constexpr std::string_view BasePath = "${MODULES}/exoplanetsexperttool";

    bool hasEnding(std::string const& fullString, std::string const& ending) {
        if (fullString.length() >= ending.length()) {
            int comp = fullString.compare(
                fullString.length() - ending.length(),
                ending.length(),
                ending
            );
            return (0 == comp);
        }
        else {
            return false;
        }
    }

    bool isNumber(const std::string& s) {
        return !s.empty() && std::find_if(s.begin(),
            s.end(), [](unsigned char c) { return !std::isdigit(c); }) == s.end();
    }

    void from_json(const nlohmann::json& j, openspace::exoplanets::DataSettings& s) {
        j.at("datafile").get_to(s.dataFile);

        const nlohmann::json dataMapping = j.at("data_mapping");
        dataMapping.at("position_ra").get_to(s.dataMapping.positionRa);
        dataMapping.at("position_dec").get_to(s.dataMapping.positionDec);
        dataMapping.at("position_distance").get_to(s.dataMapping.positionDistance);
        dataMapping.at("name").get_to(s.dataMapping.name);
        dataMapping.at("hostName").get_to(s.dataMapping.hostName);
        dataMapping.at("ring_size").get_to(s.dataMapping.ringSize);
        dataMapping.at("reference_link").get_to(s.dataMapping.referenceLink);

        const nlohmann::json columnInfo = j.at("column_info");
        for (auto& [key, value] : columnInfo.items()) {
            openspace::exoplanets::DataSettings::ColumnInfo info;
            value.at("name").get_to(info.name);
            if (value.contains("format")) {
                std::string temp;
                value.at("format").get_to(temp);
                info.format = temp;
            }
            if (value.contains("desc")) {
                std::string temp;
                value.at("desc").get_to(temp);
                info.description = temp;
            }
            if (value.contains("isText")) {
                bool temp = false;
                value.at("isText").get_to(temp);
                info.isText = temp;
            }
            s.columnInfo[key] = info;
        }
    }

} // namespace

namespace openspace::exoplanets {

DataLoader::DataLoader() {}

DataSettings DataLoader::loadDataSettingsFromJson() {
    // For some reason, this token does not exist yet.
    //std::filesystem::path basePath = absPath("${MODULE_EXOPLANETSEXPERTTOOL}");
    std::filesystem::path basePath = absPath(BasePath);
    std::filesystem::path settingsPath = basePath / std::filesystem::path(DataSettingsPath);

    std::ifstream datasetConfigFile(settingsPath);
    const nlohmann::json j = nlohmann::json::parse(datasetConfigFile);

    DataSettings settings;
    from_json(j, settings);
    return settings;
}

std::vector<ExoplanetItem> DataLoader::loadData(const DataSettings& settings) {
    std::filesystem::path csvFilePath = absPath(BasePath) / settings.dataFile;
    std::ifstream exoplanetsCsvFile(csvFilePath);

    if (!exoplanetsCsvFile.good()) {
        LERROR(std::format("Failed to open input file '{}'", csvFilePath));
        return std::vector<ExoplanetItem>();
    }

    LINFO("Reading Exoplanets CSV");

    std::vector<std::vector<std::string>> csvContent = ghoul::loadCSVFile(
        csvFilePath,
        true
    );

    if (csvContent.empty()) {
        LERROR(
            std::format("Could not read CSV data from file '{}'", csvFilePath)
        );
        return std::vector<ExoplanetItem>();
    }

    // Write exoplanet records to file
    std::vector<std::string> columns = csvContent[0];

    const int nRows = static_cast<int>(csvContent.size());

    std::vector<ExoplanetItem> planets;
    planets.reserve(nRows);

    int nDataColumns = -1;

    for (int row = 1; row < nRows; row++) {
        ExoplanetItem p;

        p.id = row - 1;

        for (int col = 0; col < columns.size(); col++) {
            const std::string& column = columns[col];
            const std::string& data = csvContent[row][col];

            // Get special values first
            if (column == settings.dataMapping.name) {
                p.name = data;
            }
            else if (column == settings.dataMapping.hostName) {
                p.hostName = data;
            }
            else if (column == settings.dataMapping.ringSize) {
                p.sizeValue = data::parseFloatData(data);
            }
            else if (column == settings.dataMapping.positionRa) {
                p.ra.value = data::parseFloatData(data);
            }
            else if (column == settings.dataMapping.positionDec) {
                p.dec.value = data::parseFloatData(data);
            }
            else if (column == settings.dataMapping.positionDistance) {
                p.distance.value = data::parseFloatData(data);
            }

            // Parse data column values

            float parsedNumeric = data::parseFloatData(data);

            bool hasIsTextInfo = settings.columnInfo.contains(column) &&
                settings.columnInfo.at(column).isText.has_value();

            bool shouldEnforceString = hasIsTextInfo ? *settings.columnInfo.at(column).isText : false;

            if (data.empty()) {
                // All columns should have empty string for missing values
                p.dataColumns[column] = "";
            }
            else if (!std::isnan(parsedNumeric) && !shouldEnforceString) {
                p.dataColumns[column] = parsedNumeric;
            }
            else {
                // Non empty string value
                p.dataColumns[column] = data;
            }

            // Molecules in atmosphere
            // Note that molecules are separated with '&' signs. We replace those
  /*          else if (column == "molecule_detection") {
                auto molecules = ghoul::tokenizeString(data, '&');
                p.moleculesDetection = ghoul::join(molecules, ", ");
            }
            else if (column == "molecule_upperLimit") {
                auto molecules = ghoul::tokenizeString(data, '&');
                p.moleculesUpperLimit = ghoul::join(molecules, ", ");
            }
            else if (column == "molecule_noDetection") {
                auto molecules = ghoul::tokenizeString(data, '&');
                p.moleculesNoDetection = ghoul::join(molecules, ", ");
            }*/
        }

        // Compute galactic position of item
        bool hasPos = p.ra.hasValue() && p.dec.hasValue() && p.distance.hasValue();
        if (hasPos) {
            const float ra = p.ra.value;
            const float dec = p.dec.value;
            p.position = icrsToGalacticCartesian(ra, dec, p.distance.value);
        }

        // Check if water has been detected
        // TODO: move to python
        // 1 = yes, 0 = maybe, -1 = no
        //constexpr const char WaterKey[] = "H2O";
        //if (p.moleculesDetection.find(WaterKey) != std::string::npos) {
        //    p.waterDetection = 1.f;
        //}
        //else if (p.moleculesUpperLimit.find(WaterKey) != std::string::npos) {
        //    p.waterDetection = 0.f;
        //}
        //else if (p.moleculesNoDetection.find(WaterKey) != std::string::npos) {
        //    p.waterDetection = -1.f;
        //}

        //// If unknown, compute planet mass
        //// TODO: move to python
        //if ((!p.mass.hasValue()) && p.radius.hasValue()) {
        //    float r = p.radius.value;

        //    // Mass radius relationship from Chen & Kipping (2017)
        //    // See eq. (2) in https://arxiv.org/pdf/1805.03671.pdf

        //    if (r < 1.23f) { // Terran
        //        p.mass.value = 0.9718f * glm::pow(r, 3.58f);
        //    }
        //    else if (r < 14.26) { // Neptunian
        //        p.mass.value = 1.436f * glm::pow(r, 1.70f);
        //    }
        //    // TODO: constant for larger planets (Jovian & Stellar)
        //    // Use their python package!
        //    // Their paper: https://iopscience.iop.org/article/10.3847/1538-4357/834/1/17
        //}

        // TODO: move to python
        //if (p.radius.hasValue() && p.mass.hasValue()) {
        //    constexpr const double G = 6.67430e-11;
        //    const double r = static_cast<double>(p.radius.value) * EarthRadius;
        //    const double M = static_cast<double>(p.mass.value) * EarthMass;
        //    p.surfaceGravity.value = static_cast<float>((G * M) / (r * r));
        //}

        // Virification related to "other columns"
        if (nDataColumns == -1) {
            nDataColumns = static_cast<int>(p.dataColumns.size());
        }
        else {
            if (p.dataColumns.size() != nDataColumns) {
                throw; // TODO: throw something meaningful
            }
        }

        planets.push_back(p);
    }
    planets.shrink_to_fit();

    if (planets.empty()) {
        return planets;
    }

    // Handle missing values

    std::map<std::string, bool> colIsNumeric;
    auto firstDataValues = planets.front().dataColumns;

    // Determine the type of all other columns
    for (const auto& [key, value] : firstDataValues) {
        // Default is text
        colIsNumeric[key] = false;

        // Find first entry with a value and use that to determine category
        for (auto p : planets) {
            std::variant<std::string, float> value = p.dataColumns[key];
            if (std::holds_alternative<std::string>(value) &&
                std::get<std::string>(value).empty())
            {
                continue; // do nothing
            }
            else {
                colIsNumeric[key] = std::holds_alternative<float>(value);
                break;
            }
        }
    }

    // Replace all the missing values in the numeric columns with NaN values
    for (const auto& [key, value] : firstDataValues) {
        if (!colIsNumeric[key]) {
            continue;
        }

        // Find first entry with a value and use that to determine category
        for (ExoplanetItem& p : planets) {
            std::variant<std::string, float> value = p.dataColumns[key];
            if (std::holds_alternative<std::string>(value) &&
                std::get<std::string>(value).empty())
            {
                p.dataColumns[key] = std::numeric_limits<float>::quiet_NaN();
            }
        }
    }
    // At this stage all values in the data columns should have the correct type

    std::map<std::string, std::vector<size_t>> hostIdToPlanetsMap;
    for (int i = 0; i < planets.size(); i++) {
        hostIdToPlanetsMap[makeIdentifier(planets[i].hostName)].push_back(i);
    }

    // Fill planets internal indices in system (based on whatever column is used for size)
    std::map<std::string, std::vector<size_t>>::iterator it;
    for (it = hostIdToPlanetsMap.begin(); it != hostIdToPlanetsMap.end(); it++) {
        std::vector<size_t> planetIds = it->second;

        std::sort(
            planetIds.begin(),
            planetIds.end(),
            [&planets](const size_t a, const size_t b) -> bool {
                float v1 = planets[a].sizeValue;
                float v2 = planets[b].sizeValue;
                return data::compareValues(v1, v2);
            }
        );
        for (int i = 0; i < static_cast<int>(planetIds.size()); ++i) {
            planets[planetIds[i]].indexInSystem = i;
        }
    }

    return planets;
}

} // namespace openspace::exoplanets
