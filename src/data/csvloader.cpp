/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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
#include <ghoul/fmt.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/exception.h>
#include <algorithm>
#include <cmath>
#include <cctype>
#include <fstream>
#include <functional>
#include <string_view>

namespace {
    constexpr std::string_view _loggerCat = "DataLoader: CSV";
} // namespace

namespace openspace::dataloader::csv {

Dataset loadCsvFile(std::filesystem::path filePath, std::optional<DataMapping> specs) {
    ghoul_assert(std::filesystem::exists(filePath), "File must exist");

    auto readFloatData = [](const std::string& str) -> float {
        float result;
#ifdef WIN32
        auto [p, ec] = std::from_chars(str.data(), str.data() + str.size(), result);
        if (ec == std::errc() && std::isfinite(result)) {
            return result;
        }
        return std::numeric_limits<float>::quiet_NaN();
#else
        // clang is missing float support for std::from_chars
        try {
            result = std::stof(str.c_str(), nullptr);
            if (std::isfinite(result)) {
                return result;
            }
        }
        catch (std::invalid_argument const& e) {}
        return NAN;
#endif
    };

    LDEBUG("Parsing CSV file");

    std::vector<std::vector<std::string>> rows = ghoul::loadCSVFile(
        filePath.string(),
        true
    );

    if (rows.size() < 2) {
        LWARNING(fmt::format(
            "Error loading data file {}. No data items read", filePath
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

    int nDataColumns = 0;
    bool hasExcludeColumns = specs.has_value() && (*specs).hasExcludeColumns();
    std::vector<size_t> skipColumns;
    if (hasExcludeColumns) {
        skipColumns.reserve((*specs).excludeColumns.size());
    }

    for (size_t i = 0; i < columns.size(); ++i) {
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
        else if (hasExcludeColumns && (*specs).isExcludeColumn(col)) {
            skipColumns.push_back(i);
            continue;
        }
        else {
            res.variables.push_back({
                .index = nDataColumns,
                .name = col
            });
            nDataColumns++;
        }
    }

    if (xColumn < 0 || yColumn < 0 || zColumn < 0) {
        // One or more position columns weren't read
        LERROR(fmt::format(
            "Error loading data file {}. Missing X, Y or Z position column", filePath
        ));
    }

    LINFO(fmt::format(
        "Loading {} rows with {} columns", rows.size(), columns.size()
    ));
    ProgressBar progress(rows.size());

    // Skip first row (column names)
    for (size_t rowIdx = 1; rowIdx < rows.size(); ++rowIdx) {
        const std::vector<std::string>& row = rows[rowIdx];

        Dataset::Entry entry;
        entry.data.reserve(nDataColumns);

        for (size_t i = 0; i < row.size(); ++i) {
            // Check if column should be exluded. Note that list of indices is sorted
            // so we can do a binary search
            if (hasExcludeColumns &&
                std::binary_search(skipColumns.begin(), skipColumns.end(), i))
            {
                continue;
            }

            const std::string& strValue = row[i];

            // For now, all values are converted to float
            float value = readFloatData(strValue);

            if (i == xColumn) {
                entry.position.x = value;
            }
            else if (i == yColumn) {
                entry.position.y = value;
            }
            else if (i == zColumn) {
                entry.position.z = value;
            }
            else {
                entry.data.push_back(value);
            }

            // @TODO: comment mapping
        }

        glm::vec3 positive = glm::abs(entry.position);
        float max = glm::compMax(positive);
        if (max > res.maxPositionComponent) {
            res.maxPositionComponent = max;
        }

        res.entries.push_back(entry);

        progress.print(rowIdx + 1);
    }

    return res;
}

} // namespace openspace::dataloader::csv
