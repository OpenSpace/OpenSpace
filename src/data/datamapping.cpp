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
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/misc.h>
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
            ghoul::toLowerCase(column);
            ghoul::toLowerCase(testColumn);
        }

        return testColumn == column;
    }

    // @TODO (2023-11-21, emmbr) Line breaks are not reflected in generated documentation

    // This is a data mapping structure that can be used when creating point cloud
    // datasets, e.g. from a CSV or Speck file.
    //
    // It allows specifying things like column names, whether the reading of those
    // column names hsould be case sensitive, data value that represents missing
    // values in the dataset, and more. See details for each field / class member.
    struct [[codegen::Dictionary(DataMapping)]] Parameters {
        // Specifies the column name for the x coordinate
        std::optional<std::string> x;

        // Specifies the column name for the y coordinate
        std::optional<std::string> y;

        // Specifies the column name for the z coordinate
        std::optional<std::string> z;

        // Specifies whether to do case sensitive checks when reading column names.
        // Deafault is not to, so that 'X' nad 'x' are both valid column names for the
        // x position column, for example
        std::optional<bool> caseSensitive;

        // Specifies a value that, when read from the file, should be interpreted as 'no
        // value', i.e. a missing data value. Note that the same value is used across all
        // data columns
        std::optional<float> missingDataValue;
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
    if (p.x.has_value()) {
        result.xColumnName = *p.x;
    }
    if (p.y.has_value()) {
        result.yColumnName = *p.y;
    }
    if (p.z.has_value()) {
        result.zColumnName = *p.z;
    }

    if (p.missingDataValue.has_value()) {
        result.missingDataValue = *p.missingDataValue;
    }

    if (p.caseSensitive.has_value()) {
        result.isCaseSensitive = *p.caseSensitive;
    }

    return result;
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
