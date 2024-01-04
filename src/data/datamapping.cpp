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

#include <openspace/data/datamapping.h>

#include <openspace/documentation/documentation.h>
#include <ghoul/misc/crc32.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/stringhelper.h>
#include <string_view>

namespace {
    constexpr std::string_view DefaultX = "x";
    constexpr std::string_view DefaultY = "y";
    constexpr std::string_view DefaultZ = "z";

    enum class PositionColumn {
        X, Y, Z
    };

    bool checkColumnInternal(PositionColumn columnCase, const std::string& c,
                         const std::optional<openspace::dataloader::DataMapping>& mapping,
                             const std::string_view defaultValue)
    {
        std::string testColumn = c;
        std::string column = std::string(defaultValue);
        if (mapping.has_value()) {
            switch (columnCase) {
                case PositionColumn::X:
                    column = (*mapping).xColumnName.value_or(column);
                    break;
                case PositionColumn::Y:
                    column = (*mapping).yColumnName.value_or(column);
                    break;
                case PositionColumn::Z:
                    column = (*mapping).zColumnName.value_or(column);
                    break;
            }
        }

        // Per default, allow both lower case and upper case versions of column names
        if (!mapping.has_value() || !(*mapping).isCaseSensitive) {
            column = ghoul::toLowerCase(column);
            testColumn = ghoul::toLowerCase(testColumn);
        }

        return testColumn == column;
    }

    // This is a data mapping structure that can be used when creating point cloud
    // datasets, e.g. from a CSV or Speck file.
    //
    // It allows specifying things like column names, whether the reading of those
    // column names should be case sensitive, data value that represents missing
    // values in the dataset, and more. See details for each field / class member.
    //
    // Note that things related to reading the point position will not be handled for
    // SPECK files, as for those we always expect the first three values per row to
    // specify the XYZ position
    struct [[codegen::Dictionary(DataMapping)]] Parameters {
        // Specifies the column name for the x coordinate
        std::optional<std::string> x;

        // Specifies the column name for the y coordinate
        std::optional<std::string> y;

        // Specifies the column name for the z coordinate
        std::optional<std::string> z;

        // Specifies whether to do case sensitive checks when reading column names.
        // Default is not to, so that 'X' and 'x' are both valid column names for the
        // x position column, for example
        std::optional<bool> caseSensitive;

        // Specifies a value that, when read from the file, should be interpreted as 'no
        // value', i.e. a missing data value. Note that the same value is used across all
        // data columns
        std::optional<float> missingDataValue;

        // A list of column names, of columns that will not be loaded into the dataset.
        // Note that not all data formats support this. E.g. SPECK files do not
        std::optional<std::vector<std::string>> excludeColumns;
    };
#include "datamapping_codegen.cpp"
}

namespace openspace::dataloader {

documentation::Documentation DataMapping::Documentation() {
    return codegen::doc<Parameters>("dataloader_datamapping");
}

DataMapping DataMapping::createFromDictionary(const ghoul::Dictionary& dictionary) {
    const Parameters p = codegen::bake<Parameters>(dictionary);

    DataMapping result;

    result.xColumnName = p.x;
    result.yColumnName = p.y;
    result.zColumnName = p.z;

    result.missingDataValue = p.missingDataValue;

    result.isCaseSensitive = p.caseSensitive.value_or(result.isCaseSensitive);
    result.excludeColumns = p.excludeColumns.value_or(result.excludeColumns);

    return result;
}

bool DataMapping::hasExcludeColumns() const {
    return !excludeColumns.empty();
}

bool DataMapping::isExcludeColumn(std::string_view column) const {
    auto found = std::find(excludeColumns.begin(), excludeColumns.end(), column);
    return (found != excludeColumns.end());
}

std::string generateHashString(const DataMapping& dm) {
    std::string a;
    for (std::string_view c : dm.excludeColumns) {
        a += c;
    }
    unsigned int excludeColumnsHash = ghoul::hashCRC32(a);

    return fmt::format(
        "DM|x{}|y{}|z{}|m{}|{}|{}",
        dm.xColumnName.value_or(""),
        dm.yColumnName.value_or(""),
        dm.zColumnName.value_or(""),
        dm.missingDataValue.has_value() ? ghoul::to_string(*dm.missingDataValue) : "",
        dm.isCaseSensitive ? "1" : "0",
        excludeColumnsHash
    );
}

bool isPositionColumn(const std::string& c, const std::optional<DataMapping>& mapping) {
    return isColumnX(c, mapping) || isColumnY(c, mapping) || isColumnZ(c, mapping);
}

bool isColumnX(const std::string& c, const std::optional<DataMapping>& mapping) {
    return checkColumnInternal(PositionColumn::X, c, mapping, DefaultX);
}

bool isColumnY(const std::string& c, const std::optional<DataMapping>& mapping) {
    return checkColumnInternal(PositionColumn::Y, c, mapping, DefaultY);
}

bool isColumnZ(const std::string& c, const std::optional<DataMapping>& mapping) {
    return checkColumnInternal(PositionColumn::Z, c, mapping, DefaultZ);
}

} // namespace openspace::dataloader
