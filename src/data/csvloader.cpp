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

#include <openspace/data/csvloader.h>

#include <openspace/data/datamapping.h>
#include <openspace/util/progressbar.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/exception.h>
#include <ghoul/misc/stringhelper.h>
#include <algorithm>
#include <cmath>
#include <cctype>
#include <fstream>
#include <functional>
#include <sstream>
#include <string_view>

namespace {
    constexpr std::string_view _loggerCat = "DataLoader: CSV";
} // namespace

namespace openspace::dataloader::csv {

Dataset loadCsvFile(std::filesystem::path filePath, std::optional<DataMapping> specs) {
    ghoul_assert(std::filesystem::exists(filePath), "File must exist");

    auto readFloatData = [](const std::string& str) -> float {
        float result = 0.f;
#ifdef WIN32
        auto [p, ec] = std::from_chars(str.data(), str.data() + str.size(), result);
        if (ec == std::errc() && std::isfinite(result)) {
            return result;
        }
        return std::numeric_limits<float>::quiet_NaN();
#else // ^^^^ WIN32 // !WIN32 vvvv
        // clang is missing float support for std::from_chars
        try {
            result = std::stof(str, nullptr);
            if (std::isfinite(result)) {
                return result;
            }
        }
        catch (const std::invalid_argument&) {}
        return NAN;
#endif // WIN32
    };

    LDEBUG("Parsing CSV file");

    std::vector<std::vector<std::string>> rows = ghoul::loadCSVFile(filePath, true);
    if (rows.size() < 2) {
        LWARNING(std::format(
            "Error loading data file '{}'. No data items read", filePath
        ));
        return Dataset();
    }

    Dataset res;
    res.entries.reserve(rows.size() - 1);

    // First row is the column names
    const std::vector<std::string>& columns = rows.front();

    int xColumn = -1;
    int yColumn = -1;
    int zColumn = -1;
    int nameColumn = -1;
    int textureColumn = -1;

    int nDataColumns = 0;
    const bool hasExcludeColumns = specs.has_value() && specs->hasExcludeColumns();
    std::vector<size_t> skipColumns;
    if (hasExcludeColumns) {
        skipColumns.reserve((*specs).excludeColumns.size());
    }

    for (size_t i = 0; i < columns.size(); i++) {
        const std::string& col = columns[i];

        if (isPositionColumn(col, specs)) {
            if (isColumnX(col, specs)) {
                xColumn = static_cast<int>(i);
            }
            if (isColumnY(col, specs)) {
                yColumn = static_cast<int>(i);
            }
            if (isColumnZ(col, specs)) {
                zColumn = static_cast<int>(i);
            }
        }
        else if (isNameColumn(col, specs)) {
            nameColumn = static_cast<int>(i);
        }
        else if (hasExcludeColumns && specs->isExcludeColumn(col)) {
            skipColumns.push_back(i);
            continue;
        }
        else {
            // Note that the texture column is also a regular column. Just save the index
            if (isTextureColumn(col, specs)) {
                res.textureDataIndex = nDataColumns;
                textureColumn = static_cast<int>(i);
            }

            res.variables.push_back({
                .index = nDataColumns,
                .name = col
            });
            nDataColumns++;
        }
    }

    // Some errors / warnings
    if (specs.has_value()) {
        bool hasAllProvided = specs->checkIfAllProvidedColumnsExist(columns);
        if (!hasAllProvided) {
            LERROR(std::format(
                "Error loading data file {}. Not all columns provided in data mapping "
                "exists in dataset", filePath
            ));
        }
    }

    bool hasProvidedTextureFile = specs.has_value() && specs->textureMap.has_value();
    bool hasTextureIndex = (res.textureDataIndex >= 0);

    if (hasProvidedTextureFile && !hasTextureIndex && !specs->textureColumn.has_value()) {
        throw ghoul::RuntimeError(std::format(
            "Error loading data file {}. No texture column was specified in the data "
            "mapping", filePath
        ));
    }
    if (!hasProvidedTextureFile && hasTextureIndex) {
        throw ghoul::RuntimeError(std::format(
            "Error loading data file {}. Missing texture map file location in data "
            "mapping", filePath
        ));
    }

    if (xColumn < 0 || yColumn < 0 || zColumn < 0) {
        // One or more position columns weren't read
        LERROR(std::format(
            "Error loading data file '{}'. Missing X, Y or Z position column", filePath
        ));
    }

    LINFO(std::format("Loading {} rows with {} columns", rows.size(), columns.size()));
    ProgressBar progress = ProgressBar(static_cast<int>(rows.size()));

    std::set<int> uniqueTextureIndicesInData;

    // Skip first row (column names)
    for (size_t rowIdx = 1; rowIdx < rows.size(); ++rowIdx) {
        const std::vector<std::string>& row = rows[rowIdx];

        Dataset::Entry entry;
        entry.data.reserve(nDataColumns);

        for (size_t i = 0; i < row.size(); i++) {
            // Check if column should be exluded. Note that list of indices is sorted
            // so we can do a binary search
            if (hasExcludeColumns &&
                std::binary_search(skipColumns.begin(), skipColumns.end(), i))
            {
                continue;
            }

            const std::string& strValue = row[i];

            // For now, all values are converted to float
            const float value = readFloatData(strValue);

            if (i == xColumn) {
                entry.position.x = value;
            }
            else if (i == yColumn) {
                entry.position.y = value;
            }
            else if (i == zColumn) {
                entry.position.z = value;
            }
            else if (i == nameColumn) {
                // Note that were we use the original stirng value, rather than the
                // converted one
                entry.comment = strValue;
            }
            else {
                entry.data.push_back(value);
            }

            if (i == textureColumn) {
                uniqueTextureIndicesInData.emplace(static_cast<int>(value));
            }
        }

        const glm::vec3 positive = glm::abs(entry.position);
        const float max = glm::compMax(positive);
        if (max > res.maxPositionComponent) {
            res.maxPositionComponent = max;
        }

        res.entries.push_back(entry);

        progress.print(static_cast<int>(rowIdx + 1));
    }

    // Load the textures. Skip textures that are not included in the dataset
    if (hasProvidedTextureFile) {
        const std::filesystem::path path = *specs->textureMap;
        if (!std::filesystem::is_regular_file(path)) {
            throw ghoul::RuntimeError(std::format(
                "Failed to open texture map file {}", path
            ));
        }
        res.textures = loadTextureMapFile(path, uniqueTextureIndicesInData);
    }

    return res;
}

std::vector<Dataset::Texture> loadTextureMapFile(std::filesystem::path path,
                                          const std::set<int>& texturesInData)
{
    ghoul_assert(std::filesystem::exists(path), "File must exist");

    std::ifstream file(path);
    if (!file.good()) {
        throw ghoul::RuntimeError(std::format(
            "Failed to open texture map file {}", path
        ));
    }

    int currentLineNumber = 0;

    std::vector<Dataset::Texture> res;

    std::string line;
    while (ghoul::getline(file, line)) {
        ghoul::trimWhitespace(line);
        currentLineNumber++;

        if (line.empty() || line.starts_with("#")) {
            continue;
        }

        std::vector<std::string> tokens = ghoul::tokenizeString(line, ' ');
        int nNonEmptyTokens = static_cast<int>(std::count_if(
            tokens.begin(),
            tokens.end(),
            [](const std::string& t) { return !t.empty(); }
        ));

        if (nNonEmptyTokens > 2) {
            throw ghoul::RuntimeError(std::format(
                "Error loading texture map file {}: Line {} has too many parameters. "
                "Expected 2: an integer index followed by a filename, where the file "
                "name may not include whitespaces",
                path, currentLineNumber
            ));
        }

        std::stringstream str(line);

        // Each line is following the template:
        // <idx> <file name>
        Dataset::Texture texture;
        str >> texture.index >> texture.file;

        for (const Dataset::Texture& t : res) {
            if (t.index == texture.index) {
                throw ghoul::RuntimeError(std::format(
                    "Error loading texture map file {}: Texture index '{}' defined twice",
                    path, texture.index
                ));
            }
        }

        if (texturesInData.contains(texture.index)) {
            res.push_back(texture);
        }
    }

    return res;
}

} // namespace openspace::dataloader::csv
