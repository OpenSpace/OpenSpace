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

#include <modules/exoplanetsexperttool/dataloader.h>

#include <modules/exoplanetsexperttool/datahelper.h>
#include <modules/exoplanetsexperttool/exoplanetsexperttoolmodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/scene/scene.h>
#include <openspace/util/coordinateconversion.h>
#include <openspace/util/progressbar.h>
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
#include <variant>

namespace {
    constexpr std::string_view _loggerCat = "ExoplanetsDataLoader";

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

    bool isNan(const std::string& s) {
        if (s.empty()) {
            return true;
        }
        std::string lc = ghoul::toLowerCase(s);
        return (lc == "null") || (lc == "nan");
    }

    void from_json(const nlohmann::json& j, openspace::exoplanets::DataSettings& s) {
        using namespace openspace::exoplanets;

        try {
            std::string file;
            j.at("datafile").get_to(file);
            s.dataFile = file;

            const nlohmann::json dataMapping = j.at("data_mapping");
            dataMapping.at("position_ra").get_to(s.dataMapping.positionRa);
            dataMapping.at("position_dec").get_to(s.dataMapping.positionDec);
            dataMapping.at("position_distance").get_to(s.dataMapping.positionDistance);

            if (dataMapping.contains("name")) {
                dataMapping.at("name").get_to(s.dataMapping.name);
            }
            if (dataMapping.contains("hostName")) {
               dataMapping.at("hostName").get_to(s.dataMapping.hostName);
            }
            if (dataMapping.contains("ring_size")) {
                dataMapping.at("ring_size").get_to(s.dataMapping.ringSize);
            }
            if (dataMapping.contains("reference_name")) {
                dataMapping.at("reference_name").get_to(s.dataMapping.referenceName);
            }
            if (dataMapping.contains("reference_link")) {
                dataMapping.at("reference_link").get_to(s.dataMapping.referenceLink);
            }

            if (j.contains("default_colormapping")) {
                const nlohmann::json cmapping = j.at("default_colormapping");
                if (!cmapping.empty()) {
                    DataSettings::CmapInfo cmap;
                    cmapping.at("column").get_to(cmap.column);
                    cmapping.at("min").get_to(cmap.min);
                    cmapping.at("max").get_to(cmap.max);
                    s.defaultColormapping = cmap;
                }
            }

            const nlohmann::json columnInfo = j.at("column_info");
            for (auto& [key, value] : columnInfo.items()) {
                DataSettings::ColumnInfo info;
                value.at("name").get_to(info.name);
                if (value.contains("format")) {
                    value.at("format").get_to(info.format);
                }
                if (value.contains("desc")) {
                    value.at("desc").get_to(info.description);
                }
                if (value.contains("isText")) {
                    bool temp = false;
                    value.at("isText").get_to(temp);
                    info.isText = temp;
                }
                s.columnInfo[key] = info;
            }

            const std::vector<nlohmann::json> quickFilterGroups = j.at("quick_filters");
            s.quickFilterGroups.reserve(quickFilterGroups.size());

            for (const nlohmann::json& groupInfo : quickFilterGroups) {
                DataSettings::QuickFilterGroup group;

                groupInfo.at("group_title").get_to(group.title);

                if (groupInfo.contains("type")) {
                    if (ghoul::toUpperCase(groupInfo.at("type")) == "OR") {
                        group.type = DataSettings::QuickFilterGroup::Type::Or;
                    }
                    else if (ghoul::toUpperCase(groupInfo.at("type")) == "AND") {
                        group.type = DataSettings::QuickFilterGroup::Type::And;
                    }
                    else {
                        LERROR("Invalid quick filter group type. Expected 'AND' or 'OR'");
                    }

                }
                groupInfo.at("same_line").get_to(group.showOnSameLine);

                const std::vector<nlohmann::json> quickFilters = groupInfo.at("filters");

                for (const nlohmann::json& filterInfo : quickFilters) {
                    DataSettings::QuickFilter quickFilter;

                    filterInfo.at("name").get_to(quickFilter.name);

                    if (filterInfo.at("filter").is_array()) {
                        const std::vector<nlohmann::json>& filters = filterInfo.at("filter");
                        quickFilter.filters.reserve(filters.size());

                        for (const nlohmann::json& f : filters) {
                            DataSettings::QuickFilter::Filter filter;
                            f.at("column").get_to(filter.column);
                            f.at("query").get_to(filter.query);
                            quickFilter.filters.push_back(filter);
                        }
                    }
                    else if (filterInfo.at("filter").is_object()) {
                        const nlohmann::json f = filterInfo.at("filter");

                        DataSettings::QuickFilter::Filter filter;
                        f.at("column").get_to(filter.column);
                        f.at("query").get_to(filter.query);
                        quickFilter.filters.push_back(filter);
                    }

                    if (filterInfo.contains("desc")) {
                        filterInfo.at("desc").get_to(quickFilter.description);
                    }

                    group.quickFilters.push_back(quickFilter);
                }

                s.quickFilterGroups.push_back(group);
            }
        }
        catch (const nlohmann::json::out_of_range& e) {
            LERROR(std::format("When reading data settings .json: {}", e.what()));
        }
    }

} // namespace

namespace openspace::exoplanets {

DataLoader::DataLoader() {}

DataSettings DataLoader::loadDataSettingsFromJson() {
    auto mod = global::moduleEngine->module<ExoplanetsExpertToolModule>();
    std::filesystem::path settingsPath = absPath(mod->dataConfigFile());

    if (settingsPath.empty()) {
        LERROR("No dataset settings (.json) file was provided.");
        return DataSettings();
    }
    else if (!std::filesystem::is_regular_file(settingsPath)) {
        LERROR(std::format(
            "Could not find dataset settings (.json) file: '{}'", settingsPath
        ));
        return DataSettings();
    }

    std::ifstream datasetConfigFile(settingsPath);

    if (!datasetConfigFile.good()) {
        LERROR(std::format(
            "Failed reading dataset settings file: '{}'", settingsPath
        ));
        return DataSettings();
    }

    try {
        const nlohmann::json j = nlohmann::json::parse(datasetConfigFile);
        DataSettings settings;
        from_json(j, settings);
        return settings;
    }
    catch (...) {
        LERROR(std::format(
            "Failed parsing dataset settings file: '{}'", settingsPath
        ));
        return DataSettings();
    }
}

std::vector<ExoplanetItem> DataLoader::loadData(const DataSettings& settings) {
    std::filesystem::path csvFilePath = absPath("${MODULE_EXOPLANETSEXPERTTOOL}") / settings.dataFile;

    if (!std::filesystem::is_regular_file(csvFilePath)) {
        LERROR(std::format("Could not find input data file '{}'", csvFilePath));
        return std::vector<ExoplanetItem>();
    }

    std::ifstream csvFile(csvFilePath);
    if (!csvFile.good()) {
        LERROR(std::format("Failed to open input data file '{}'", csvFilePath));
        return std::vector<ExoplanetItem>();
    }

    LINFO(std::format("Reading CSV file: '{}'", csvFilePath));

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

    LINFO("Parsing content of CSV file");

    // Write exoplanet records to file
    std::vector<std::string> columns = csvContent[0];

    const int nRows = static_cast<int>(csvContent.size());

    std::vector<ExoplanetItem> planets;
    planets.reserve(nRows);

    ProgressBar progressBar(100);
    auto printProgress = [&progressBar](float progress) {
        progressBar.print(static_cast<int>(progress * 100.f));
    };

    int nDataColumns = -1;

    printProgress(0.f);

    for (int row = 1; row < nRows; row++) {
        ExoplanetItem p;

        p.id = row - 1;

        DataPoint ra;
        DataPoint dec;
        DataPoint distance;

        if (settings.dataMapping.name.empty()) {
            p.name = std::format("Item {}", row);
            p.dataColumns["name"] = p.name;
        }

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
                ra.value = data::parseFloatData(data);
            }
            else if (column == settings.dataMapping.positionDec) {
                dec.value = data::parseFloatData(data);
            }
            else if (column == settings.dataMapping.positionDistance) {
                distance.value = data::parseFloatData(data);
            }
            else if (column == settings.dataMapping.referenceName) {
                p.referenceName = data;
            }
            else if (column == settings.dataMapping.referenceLink) {
                p.referenceUrl= data;
            }

            // Parse data column values

            float parsedNumeric = data::parseFloatData(data);

            bool hasIsTextInfo = settings.columnInfo.contains(column) &&
                settings.columnInfo.at(column).isText.has_value();

            bool shouldEnforceString =
                hasIsTextInfo ? *settings.columnInfo.at(column).isText : false;

            if (data.empty() && isNan(data)) {
                // All columns should have empty string for missing values
                p.dataColumns[column] = "";
            }
            else if (!std::isnan(parsedNumeric) && !shouldEnforceString) {
                p.dataColumns[column] = parsedNumeric;
            }
            else {
                // Non empty string value
                p.dataColumns[column] = isNan(data) ? "" : data;
            }
        }

        // Compute galactic position of item
        bool hasPos = ra.hasValue() && dec.hasValue() && distance.hasValue();
        if (hasPos) {
            p.position = icrsToGalacticCartesian(ra.value, dec.value, distance.value);
        }

        // Verification related to "other columns". All rows should have the same number
        // of columns
        if (nDataColumns == -1) {
            nDataColumns = static_cast<int>(p.dataColumns.size());
        }
        else {
            if (p.dataColumns.size() != nDataColumns) {
                throw; // TODO: throw something meaningful
            }
        }

        planets.push_back(p);

        printProgress(static_cast<float>(row) / static_cast<float>(nRows));
    }
    planets.shrink_to_fit();
    printProgress(1.f);

    if (planets.empty()) {
        return planets;
    }

    // Handle missing values

    LINFO("Modifing data based on missing values and detecting column data types...");

    std::map<std::string, bool> colIsNumeric;
    auto firstDataValues = planets.front().dataColumns;

    // Determine the type of all other columns
    for (const auto& [key, _] : firstDataValues) {
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
    for (const auto& [key, _] : firstDataValues) {
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

    if (!settings.dataMapping.hostName.empty()) {
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
                    return data::compareValuesWithNan(v1, v2);
                }
            );
            for (int i = 0; i < static_cast<int>(planetIds.size()); ++i) {
                planets[planetIds[i]].indexInSystem = i;
            }
        }
    }

    LINFO("Done loading dataset!");

    return planets;
}

void DataLoader::saveData(const std::filesystem::path& targetPath,
                          const std::vector<ExoplanetItem>& allItems,
                          const std::vector<size_t>& indices,
                          const std::vector<ColumnKey>& columns)
{
    // TODO: verify csv ending

    std::fstream f(targetPath, std::ios::out);

    if (!f.is_open()) {
        LERROR(std::format("Failed writing data to file: '{}'", targetPath));
        return;
    }

    if (!columns.empty()) {
        f << ghoul::join(columns, ",") << std::endl;
    }
    else {
        std::string line;
        for (auto [key, _] : allItems.front().dataColumns) {
            line += key + ",";
        }
        f << line.substr(0, line.size() - 1) << std::endl;
    }

    for (const size_t& i : indices) {
        const ExoplanetItem& item = allItems[i];

        std::string line;
        for (auto [_, value] : item.dataColumns) {
            if (std::holds_alternative<float>(value)) {
                float v = std::get<float>(value);
                line += std::isnan(v) ? "" : ghoul::to_string(v);
            }
            else {
                line += std::get<std::string>(value);
            }
            line += ",";
        }
        f << line.substr(0, line.size() - 1) << std::endl;
    }

    // TODO: handle indices empty
}

} // namespace openspace::exoplanets
