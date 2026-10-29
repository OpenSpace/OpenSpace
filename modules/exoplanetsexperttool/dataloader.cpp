/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <modules/exoplanetsexperttool/exoplanetsexperttoolmodule.h>
#include <modules/exoplanetsexperttool/util/datahelper.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/filesystem/filesystem.h>
#include <openspace/json.h>
#include <openspace/logging/logmanager.h>
#include <openspace/misc/csvreader.h>
#include <openspace/misc/dictionary.h>
#include <openspace/misc/stringhelper.h>
#include <openspace/scene/scene.h>
#include <openspace/util/coordinateconversion.h>
#include <openspace/util/distanceconstants.h>
#include <openspace/util/progressbar.h>

#include <algorithm>
#include <array>
#include <cmath>
#include <fstream>
#include <limits>
#include <set>
#include <string>
#include <string_view>
#include <stdexcept>
#include <variant>

namespace {
    constexpr std::string_view _loggerCat = "ExoplanetsDataLoader";

    bool isNan(const std::string& s) {
        if (s.empty()) {
            return true;
        }
        std::string lc = openspace::toLowerCase(s);
        return (lc == "null") || (lc == "nan");
    }

    void from_json(const nlohmann::json& j, openspace::exoplanets::DataSettings& s) {
        using namespace openspace;
        using namespace openspace::exoplanets;

        if (!j.is_object()) {
            throw std::invalid_argument("Dataset settings must be a JSON object");
        }
        if (j.contains("datafile")) {
            std::string file;
            j.at("datafile").get_to(file);
            s.dataFile = file.find("${") == std::string::npos ?
                std::filesystem::path(file) : absPath(file);
        }

        nlohmann::json dataMapping = nlohmann::json::object();
        if (j.contains("data_mapping")) {
            dataMapping = j.at("data_mapping");
            if (!dataMapping.is_object()) {
                throw std::invalid_argument("'data_mapping' must be a JSON object");
            }
        }
        auto readMapping = [&dataMapping](std::string_view key, ColumnKey& value) {
            if (dataMapping.contains(key)) {
                dataMapping.at(key).get_to(value);
            }
        };
        readMapping("position_ra", s.dataMapping.positionRa);
        readMapping("position_dec", s.dataMapping.positionDec);
        readMapping("position_distance", s.dataMapping.positionDistance);
        readMapping("position_x", s.dataMapping.positionX);
        readMapping("position_y", s.dataMapping.positionY);
        readMapping("position_z", s.dataMapping.positionZ);
        if (dataMapping.contains("position_type")) {
            const std::string positionType = dataMapping.at("position_type").get<std::string>();
            if (positionType == "equatorial") {
                s.dataMapping.positionType = DataSettings::DataMapping::PositionType::Equatorial;
            }
            else if (positionType == "galactic_cartesian") {
                s.dataMapping.positionType =
                    DataSettings::DataMapping::PositionType::GalacticCartesian;
            }
            else {
                throw std::invalid_argument(std::format(
                    "Invalid position_type '{}'; expected 'equatorial' or 'galactic_cartesian'",
                    positionType
                ));
            }
        }
        if (dataMapping.contains("position_unit")) {
            const std::string unit = dataMapping.at("position_unit").get<std::string>();
            bool foundUnit = false;
            for (DistanceUnit candidate : DistanceUnits) {
                const DistanceUnitName unitName =
                    DistanceUnitNames[static_cast<int>(candidate)];
                if (unit == unitName.singular || unit == unitName.plural ||
                    unit == unitName.abbreviation)
                {
                    s.dataMapping.positionUnit = candidate;
                    foundUnit = true;
                    break;
                }
            }
            if (!foundUnit) {
                throw std::invalid_argument(std::format(
                    "Invalid position_unit '{}'; use a supported distance unit name or abbreviation",
                    unit
                ));
            }
        }
        readMapping("name", s.dataMapping.name);
        readMapping("hostName", s.dataMapping.hostName);
        readMapping("ring_size", s.dataMapping.ringSize);
        readMapping("reference_name", s.dataMapping.referenceName);
        readMapping("reference_link", s.dataMapping.referenceLink);
        readMapping("metallicity_ratio", s.dataMapping.metallicityRatio);
        readMapping("discovery_method", s.dataMapping.discoveryMethod);
        readMapping("inclination", s.dataMapping.inclination);
        readMapping("stellar_inclination", s.dataMapping.stellarInclination);

            if (j.contains("system_view")) {
                const nlohmann::json systemView = j.at("system_view");
                if (systemView.contains("system_columns")) {
                    systemView.at("system_columns").get_to(s.systemView.systemColumns);
                }
                if (systemView.contains("star_columns")) {
                    systemView.at("star_columns").get_to(s.systemView.starColumns);
                }
                if (systemView.contains("planet_columns")) {
                    systemView.at("planet_columns").get_to(s.systemView.planetColumns);
                }
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

            const nlohmann::json columnInfo = j.value(
                "column_info",
                nlohmann::json::object()
            );
            if (!columnInfo.is_object()) {
                throw std::invalid_argument("'column_info' must be a JSON object");
            }
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

            const std::vector<nlohmann::json> quickFilterGroups = j.value(
                "quick_filters",
                std::vector<nlohmann::json>()
            );
            s.quickFilterGroups.reserve(quickFilterGroups.size());

            for (const nlohmann::json& groupInfo : quickFilterGroups) {
                DataSettings::QuickFilterGroup group;

                groupInfo.at("group_title").get_to(group.title);

                if (groupInfo.contains("type")) {
                    if (toUpperCase(groupInfo.at("type").get<std::string>()) == "OR") {
                        group.type = DataSettings::QuickFilterGroup::Type::Or;
                    }
                    else if (toUpperCase(groupInfo.at("type").get<std::string>()) == "AND") {
                        group.type = DataSettings::QuickFilterGroup::Type::And;
                    }
                    else {
                        LERROR("Invalid quick filter group type. Expected 'AND' or 'OR'");
                    }

                }
                if (groupInfo.contains("per_line")) {
                    groupInfo.at("per_line").get_to(group.itemsPerLine);
                }

                const std::vector<nlohmann::json> quickFilters =
                    groupInfo.at("filters").get<std::vector<nlohmann::json>>();

                for (const nlohmann::json& filterInfo : quickFilters) {
                    DataSettings::QuickFilter quickFilter;

                    filterInfo.at("name").get_to(quickFilter.name);

                    if (filterInfo.at("filter").is_array()) {
                        const std::vector<nlohmann::json>& filters =
                            filterInfo.at("filter").get<std::vector<nlohmann::json>>();
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

} // namespace

namespace openspace::exoplanets {

DataLoader::DataLoader() {}

std::vector<std::string> DataLoader::csvColumnNames(const std::filesystem::path& path) {
    if (!std::filesystem::is_regular_file(path)) {
        throw std::runtime_error(std::format("Could not find input data file '{}'", path));
    }
    std::ifstream csvFile(path);
    if (!csvFile.good()) {
        throw std::runtime_error(std::format("Failed to open input data file '{}'", path));
    }

    const std::vector<std::vector<std::string>> csvContent = loadCSVFile(path, true);
    if (csvContent.empty() || csvContent.front().empty()) {
        throw std::runtime_error(std::format("CSV '{}' has no header row", path));
    }

    const std::vector<std::string>& columns = csvContent.front();
    std::set<std::string> uniqueColumns;
    for (const std::string& column : columns) {
        if (column.empty()) {
            throw std::runtime_error(std::format("CSV '{}' contains an empty column name", path));
        }
        if (!uniqueColumns.insert(column).second) {
            throw std::runtime_error(std::format(
                "CSV '{}' contains duplicate column name '{}'", path, column
            ));
        }
    }

    for (size_t row = 1; row < csvContent.size(); row++) {
        if (csvContent[row].size() != columns.size()) {
            throw std::runtime_error(std::format(
                "CSV '{}' row {} has {} fields; expected {}",
                path,
                row + 1,
                csvContent[row].size(),
                columns.size()
            ));
        }
    }

    return columns;
}

DataSettings DataLoader::loadDataSettingsFromJson() {
    auto mod = global::moduleEngine->module<ExoplanetsExpertToolModule>();
    const std::filesystem::path settingsPath = absPath(mod->dataConfigFile());

    if (settingsPath.empty()) {
        throw std::runtime_error("No dataset settings (.json) file was provided");
    }
    return loadDataSettingsFromJson(settingsPath);
}

DataSettings DataLoader::loadDataSettingsFromJson(const std::filesystem::path& path) {
    const std::filesystem::path settingsPath = absPath(path.string());
    if (!std::filesystem::is_regular_file(settingsPath)) {
        throw std::runtime_error(std::format(
            "Could not find dataset settings (.json) file '{}'", settingsPath
        ));
    }
    std::ifstream datasetConfigFile(settingsPath);
    if (!datasetConfigFile.good()) {
        throw std::runtime_error(std::format(
            "Failed reading dataset settings file '{}'", settingsPath
        ));
    }

    LINFO(std::format("Loading settings from file: '{}'", settingsPath));

    try {
        const nlohmann::json j = nlohmann::json::parse(datasetConfigFile);
        DataSettings settings;
        from_json(j, settings);
        if (!settings.dataFile.empty() && settings.dataFile.is_relative() &&
            settings.dataFile.string().find("${") == std::string::npos)
        {
            settings.dataFile = settingsPath.parent_path() / settings.dataFile;
        }
        return settings;
    }
    catch (const std::exception& e) {
        throw std::runtime_error(std::format(
            "Failed parsing dataset settings file '{}': {}", settingsPath, e.what()
        ));
    }
}

std::vector<ExoplanetItem> DataLoader::loadData(const DataSettings& settings) {
    std::filesystem::path csvFilePath = settings.dataFile;

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

    std::vector<std::vector<std::string>> csvContent = loadCSVFile(
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
    const DataSettings::DataMapping& mapping = settings.dataMapping;

    const std::set<std::string> availableColumns(columns.begin(), columns.end());
    std::set<std::string> uniqueColumns;
    for (const std::string& column : columns) {
        if (column.empty()) {
            throw std::runtime_error(std::format(
                "CSV '{}' contains an empty column name", csvFilePath
            ));
        }
        if (!uniqueColumns.insert(column).second) {
            throw std::runtime_error(std::format(
                "CSV '{}' contains duplicate column name '{}'", csvFilePath, column
            ));
        }
    }
    auto requireColumn = [&availableColumns, &csvFilePath, &columns](
        const ColumnKey& key,
        std::string_view role
    ) {
        if (key.empty()) {
            throw std::invalid_argument(std::format(
                "No CSV column is mapped for {}", role
            ));
        }
        if (!availableColumns.contains(key)) {
            throw std::invalid_argument(std::format(
                "Mapped {} column '{}' is not present in '{}', available columns: {}",
                role,
                key,
                csvFilePath,
                join(columns, ", ")
            ));
        }
    };
    const bool isEquatorial = mapping.positionType ==
        DataSettings::DataMapping::PositionType::Equatorial;
    if (isEquatorial) {
        requireColumn(mapping.positionRa, "right ascension");
        requireColumn(mapping.positionDec, "declination");
        requireColumn(mapping.positionDistance, "distance");
    }
    else {
        requireColumn(mapping.positionX, "Galactic X");
        requireColumn(mapping.positionY, "Galactic Y");
        requireColumn(mapping.positionZ, "Galactic Z");
    }

    if (!settings.dataMapping.nameFromRowIndex && !settings.dataMapping.name.empty() &&
        !availableColumns.contains(settings.dataMapping.name))
    {
        throw std::invalid_argument(std::format(
            "Mapped object-name column '{}' is not present in '{}', available columns: {}",
            settings.dataMapping.name,
            csvFilePath,
            join(columns, ", ")
        ));
    }

    const std::array<std::pair<const ColumnKey*, std::string_view>, 8> optionalMappings = {{
        { &mapping.hostName, "host name" },
        { &mapping.ringSize, "ring size" },
        { &mapping.referenceName, "reference name" },
        { &mapping.referenceLink, "reference link" },
        { &mapping.metallicityRatio, "metallicity ratio" },
        { &mapping.discoveryMethod, "discovery method" },
        { &mapping.inclination, "inclination" },
        { &mapping.stellarInclination, "stellar inclination" }
    }};
    for (const auto& [key, role] : optionalMappings) {
        if (!key->empty() && !availableColumns.contains(*key)) {
            LWARNING(std::format(
                "Ignoring missing {} column '{}' in '{}'", role, *key, csvFilePath
            ));
        }
    }

    const int nRows = static_cast<int>(csvContent.size());
    if (nRows < 2) {
        throw std::runtime_error(std::format("CSV '{}' contains a header but no data rows", csvFilePath));
    }
    for (int row = 1; row < nRows; row++) {
        if (csvContent[row].size() != columns.size()) {
            throw std::runtime_error(std::format(
                "CSV '{}' row {} has {} fields; expected {}",
                csvFilePath,
                row + 1,
                csvContent[row].size(),
                columns.size()
            ));
        }
    }

    std::vector<ExoplanetItem> planets;
    planets.reserve(nRows);

    std::vector<bool> columnIsNumeric(columns.size(), false);
    for (size_t col = 0; col < columns.size(); col++) {
        const std::string& column = columns[col];
        const bool isPositionColumn = isEquatorial ?
            (column == mapping.positionRa || column == mapping.positionDec ||
             column == mapping.positionDistance) :
            (column == mapping.positionX || column == mapping.positionY ||
             column == mapping.positionZ);
        const bool isIdentifierColumn = column == mapping.name ||
            column == mapping.hostName || column == mapping.referenceName ||
            column == mapping.referenceLink;
        const bool forceText = !isPositionColumn && (isIdentifierColumn ||
            (settings.columnInfo.contains(column) &&
             settings.columnInfo.at(column).isText.value_or(false)));

        if (isPositionColumn) {
            columnIsNumeric[col] = true;
            continue;
        }
        if (forceText) {
            continue;
        }

        bool sawValue = false;
        bool allValuesNumeric = true;
        for (int row = 1; row < nRows; row++) {
            const std::string& value = csvContent[row][col];
            if (isNan(value)) {
                continue;
            }
            sawValue = true;
            if (!std::isfinite(data::parseFloatData(value))) {
                allValuesNumeric = false;
                break;
            }
        }
        const bool explicitlyNumeric = settings.columnInfo.contains(column) &&
            settings.columnInfo.at(column).isText.has_value() &&
            !*settings.columnInfo.at(column).isText;
        columnIsNumeric[col] = allValuesNumeric && (sawValue || explicitlyNumeric);
    }

    ProgressBar progressBar(100);
    auto printProgress = [&progressBar](float progress) {
        progressBar.print(static_cast<int>(progress * 100.f));
    };

    bool hasUsablePosition = false;

    printProgress(0.f);

    for (int row = 1; row < nRows; row++) {
        ExoplanetItem p;

        p.id = row - 1;

        DataPoint coordinate1;
        DataPoint coordinate2;
        DataPoint coordinate3;

        if (settings.dataMapping.nameFromRowIndex) {
            if (settings.dataMapping.name.empty() || availableColumns.contains(settings.dataMapping.name)) {
                throw std::invalid_argument(
                    "Index-based object names require a generated column key not present in the CSV"
                );
            }
            p.name = std::to_string(row);
            p.dataColumns[settings.dataMapping.name] = p.name;
        }
        else if (settings.dataMapping.name.empty()) {
            throw std::invalid_argument(
                "No object-name column was selected; select a CSV column or use row index"
            );
        }

        for (int col = 0; col < columns.size(); col++) {
            const std::string& column = columns[col];
            const std::string& data = csvContent[row][col];

            // Extract mapped values independently, since mappings may share a source column.
            if (!settings.dataMapping.nameFromRowIndex &&
                column == settings.dataMapping.name)
            {
                p.name = data;
            }
            if (column == settings.dataMapping.hostName && !column.empty()) {
                p.hostName = isNan(data) ? "" : data;
            }
            if (column == settings.dataMapping.ringSize && !column.empty()) {
                p.sizeValue = data::parseFloatData(data);
            }
            if (isEquatorial) {
                if (column == mapping.positionRa) {
                    coordinate1.value = data::parseFloatData(data);
                }
                if (column == mapping.positionDec) {
                    coordinate2.value = data::parseFloatData(data);
                }
                if (column == mapping.positionDistance) {
                    coordinate3.value = data::parseFloatData(data);
                }
            }
            else {
                if (column == mapping.positionX) {
                    coordinate1.value = data::parseFloatData(data);
                }
                if (column == mapping.positionY) {
                    coordinate2.value = data::parseFloatData(data);
                }
                if (column == mapping.positionZ) {
                    coordinate3.value = data::parseFloatData(data);
                }
            }
            if (column == settings.dataMapping.referenceName && !column.empty()) {
                p.referenceName = data;
            }
            if (column == settings.dataMapping.referenceLink && !column.empty()) {
                p.referenceUrl= data;
            }

            const float parsedNumeric = data::parseFloatData(data);
            if (columnIsNumeric[col]) {
                p.dataColumns[column] = std::isfinite(parsedNumeric) ?
                    parsedNumeric : std::numeric_limits<float>::quiet_NaN();
            }
            else {
                p.dataColumns[column] = isNan(data) ? "" : data;
            }
        }

        // Compute galactic position of item
        const bool hasPos = std::isfinite(coordinate1.value) &&
            std::isfinite(coordinate2.value) && std::isfinite(coordinate3.value);
        if (hasPos) {
            if (isEquatorial) {
                const double distanceParsecs = convertDistance(
                    coordinate3.value,
                    mapping.positionUnit,
                    DistanceUnit::Parsec
                );
                p.position = icrsToGalacticCartesian(
                    coordinate1.value,
                    coordinate2.value,
                    distanceParsecs
                );
                double ra = std::fmod(coordinate1.value, 360.0);
                if (ra < 0.0) {
                    ra += 360.0;
                }
                p.skyCoordinates = glm::dvec3(ra, coordinate2.value, distanceParsecs);
            }
            else {
                p.position = glm::dvec3(
                    convertDistance(coordinate1.value, mapping.positionUnit, DistanceUnit::Parsec),
                    convertDistance(coordinate2.value, mapping.positionUnit, DistanceUnit::Parsec),
                    convertDistance(coordinate3.value, mapping.positionUnit, DistanceUnit::Parsec)
                );
                const double distanceParsecs = glm::length(*p.position);
                if (distanceParsecs > 0.0 && std::isfinite(distanceParsecs)) {
                    glm::dvec3 icrs = galacticCartesianToIcrs(
                        p.position->x,
                        p.position->y,
                        p.position->z
                    );
                    icrs.x = std::fmod(icrs.x, 360.0);
                    if (icrs.x < 0.0) {
                        icrs.x += 360.0;
                    }
                    p.skyCoordinates = icrs;
                }
            }
            hasUsablePosition = true;
        }

        planets.push_back(p);

        printProgress(static_cast<float>(row) / static_cast<float>(nRows));
    }
    planets.shrink_to_fit();
    printProgress(1.f);

    if (planets.empty()) {
        return planets;
    }

    if (!hasUsablePosition) {
        throw std::runtime_error(std::format(
            "CSV '{}' has no rows with valid numeric coordinates for the selected position type",
            csvFilePath
        ));
    }

    if (!settings.dataMapping.hostName.empty()) {
        std::map<std::string, std::vector<size_t>> hostIdToPlanetsMap;
        for (int i = 0; i < planets.size(); i++) {
            if (!planets[i].hostName.empty()) {
                hostIdToPlanetsMap[makeIdentifier(planets[i].hostName)].push_back(i);
            }
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
                          const std::vector<ColumnKey>& columns,
                          const std::map<ColumnKey, ComputedColumn>& computedColumns,
                          bool includeComputedColumns,
                          bool includeCartesianPosition, bool useParsec)
{
    // TODO: verify csv ending

    std::fstream f(targetPath, std::ios::out);

    if (!f.is_open()) {
        LERROR(std::format("Failed writing data to file: '{}'", targetPath));
        return;
    }

    if (includeCartesianPosition) {
        f << "x,y,z,";
    }

    const bool includeAllDataColumns = columns.empty();
    std::vector<ColumnKey> columnsToSave = columns;
    if (!includeComputedColumns) {
        std::erase_if(columnsToSave, [&computedColumns](const ColumnKey& key) {
            return computedColumns.contains(key);
        });
    }

    if (includeAllDataColumns) {
        columnsToSave.reserve(allItems.front().dataColumns.size());
        for (const auto& [key, _] : allItems.front().dataColumns) {
            columnsToSave.push_back(key);
        }
    }

    if (includeComputedColumns) {
        for (const auto& [key, _] : computedColumns) {
            if (std::find(columnsToSave.begin(), columnsToSave.end(), key) ==
                columnsToSave.end())
            {
                columnsToSave.push_back(key);
            }
        }
    }

    f << join(columnsToSave, ",") << std::endl;

    for (const size_t& i : indices) {
        const ExoplanetItem& item = allItems[i];

        if (includeCartesianPosition && item.position.has_value()) {
            double posFactor = useParsec ? 1.0 : distanceconstants::Parsec;
            f << posFactor * (*item.position).x << ",";
            f << posFactor * (*item.position).y << ",";
            f << posFactor * (*item.position).z << ",";
        }
        else if (includeCartesianPosition) {
            f << ",,,"; // no position exists
        }

        for (size_t columnIndex = 0; columnIndex < columnsToSave.size(); columnIndex++) {
            if (columnIndex > 0) {
                f << ",";
            }

            const ColumnKey& key = columnsToSave[columnIndex];
            if (includeComputedColumns && computedColumns.contains(key)) {
                const float value = computedColumns.at(key).values.at(i);
                if (!std::isnan(value)) {
                    f << to_string(value);
                }
            }
            else {
                const std::variant<std::string, float>& value = item.dataColumns.at(key);
                if (std::holds_alternative<float>(value)) {
                    const float floatValue = std::get<float>(value);
                    if (!std::isnan(floatValue)) {
                        f << to_string(floatValue);
                    }
                }
                else {
                    f << std::get<std::string>(value);
                }
            }
        }
        f << std::endl;
    }

    // TODO: handle indices empty
}

} // namespace openspace::exoplanets
