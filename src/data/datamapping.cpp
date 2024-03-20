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

#include <openspace/data/datamapping.h>

#include <openspace/documentation/documentation.h>
#include <ghoul/misc/crc32.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/stringhelper.h>
#include <string_view>

namespace {
    constexpr std::string_view _loggerCat = "RenderablePolygonCloud";

    constexpr std::string_view DefaultX = "x";
    constexpr std::string_view DefaultY = "y";
    constexpr std::string_view DefaultZ = "z";

    enum class PositionColumn {
        X, Y, Z
    };

    bool checkPosColumnInternal(PositionColumn columnCase, const std::string& c,
                         const std::optional<openspace::dataloader::DataMapping>& mapping,
                             const std::string_view defaultValue)
    {
        std::string testColumn = c;
        std::string column = std::string(defaultValue);
        if (mapping.has_value()) {
            switch (columnCase) {
                case PositionColumn::X:
                    column = mapping->xColumnName.value_or(column);
                    break;
                case PositionColumn::Y:
                    column = mapping->yColumnName.value_or(column);
                    break;
                case PositionColumn::Z:
                    column = mapping->zColumnName.value_or(column);
                    break;
            }
        }

        // Per default, allow both lower case and upper case versions of column names
        if (!mapping.has_value() || !mapping->isCaseSensitive) {
            column = ghoul::toLowerCase(column);
            testColumn = ghoul::toLowerCase(testColumn);
        }

        return testColumn == column;
    }

    bool isSameStringColumn(const std::string& left, const std::string& right,
                            bool isCaseSensitive)
    {
        std::string l = isCaseSensitive ? ghoul::toLowerCase(left) : left;
        std::string r = isCaseSensitive ? ghoul::toLowerCase(right) : right;
        return (l == r);
    }

    bool containsColumn(const std::string& c, const std::vector<std::string>& columns,
                        bool isCaseSensitive)
    {
        auto it = std::find_if(
            columns.begin(),
            columns.end(),
            [&c, &isCaseSensitive](const std::string& col) {
                return isSameStringColumn(c, col, isCaseSensitive);
            }
        );
        return it != columns.end();
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

        // Specifies the column name for the optional name column. Not valid for SPECK
        // files, where the name is given by the comment at the end of each line
        std::optional<std::string> name;

        // Specifies a column name for a column that has the data for which texture to
        // use for each point (given as an integer index). If included, a texture map
        // file need to be included as well
        std::optional<std::string> textureColumn;

        // A file where each line contains an integer index and an image file name.
        // Not valid for SPECK files, which includes this information as part of its
        // data format. This map will be used to map the data in the TextureColumn to
        // an image file to use for rendering the points. Note that only the files with
        // indices that are used in the dataset will actually be loaded
        std::optional<std::filesystem::path> textureMapFile;

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
} // namespace

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
    result.nameColumn = p.name;
    result.textureColumn = p.textureColumn;
    result.textureMap = p.textureMapFile;

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

bool DataMapping::checkIfAllProvidedColumnsExist(
                                            const std::vector<std::string>& columns) const
{
    auto checkColumnIsOk = [this, &columns](std::optional<std::string> col,
                                            std::string_view key)
    {
        if (col.has_value() && !containsColumn(*col, columns, isCaseSensitive)) {
            LWARNING(fmt::format("Could not find provided {} column: '{}'", key, *col));
            return false;
        }
        return true;
    };

    bool hasAll = true;
    hasAll &= checkColumnIsOk(xColumnName, "X");
    hasAll &= checkColumnIsOk(yColumnName, "Y");
    hasAll &= checkColumnIsOk(zColumnName, "Z");
    hasAll &= checkColumnIsOk(nameColumn, "Name");
    hasAll &= checkColumnIsOk(textureColumn, "Texture");
    return hasAll;
}

std::string generateHashString(const DataMapping& dm) {
    std::string a;
    for (const std::string_view c : dm.excludeColumns) {
        a += c;
    }
    unsigned int excludeColumnsHash = ghoul::hashCRC32(a);

    return fmt::format(
        "DM|{}|{}|{}|{}|{}|{}|{}|{}",
        dm.xColumnName.value_or(""),
        dm.yColumnName.value_or(""),
        dm.zColumnName.value_or(""),
        dm.nameColumn.value_or(""),
        dm.textureColumn.value_or(""),
        dm.missingDataValue.has_value() ? ghoul::to_string(*dm.missingDataValue) : "",
        dm.isCaseSensitive ? 1 : 0,
        excludeColumnsHash
    );
}

bool isPositionColumn(const std::string& c, const std::optional<DataMapping>& mapping) {
    return isColumnX(c, mapping) || isColumnY(c, mapping) || isColumnZ(c, mapping);
}

bool isColumnX(const std::string& c, const std::optional<DataMapping>& mapping) {
    return checkPosColumnInternal(PositionColumn::X, c, mapping, DefaultX);
}

bool isColumnY(const std::string& c, const std::optional<DataMapping>& mapping) {
    return checkPosColumnInternal(PositionColumn::Y, c, mapping, DefaultY);
}

bool isColumnZ(const std::string& c, const std::optional<DataMapping>& mapping) {
    return checkPosColumnInternal(PositionColumn::Z, c, mapping, DefaultZ);
}

bool isNameColumn(const std::string& c, const std::optional<DataMapping>& mapping) {
    if (!mapping.has_value() || !mapping->nameColumn.has_value()) {
        return false;
    }
    return isSameStringColumn(c, *mapping->nameColumn, mapping->isCaseSensitive);
}

bool isTextureColumn(const std::string& c, const std::optional<DataMapping>& mapping) {
    if (!mapping.has_value() || !mapping->textureColumn.has_value()) {
        return false;
    }
    return isSameStringColumn(c, *mapping->textureColumn, mapping->isCaseSensitive);
}

} // namespace openspace::dataloader
