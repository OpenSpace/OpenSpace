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

#ifndef __OPENSPACE_CORE___DATAMAPPING___H__
#define __OPENSPACE_CORE___DATAMAPPING___H__

#include <optional>
#include <string>
#include <vector>

namespace openspace::documentation { struct Documentation; }
namespace ghoul { class Dictionary; }

namespace openspace::dataloader {

struct DataMapping {
    static DataMapping createFromDictionary(const ghoul::Dictionary& dictionary);
    static documentation::Documentation Documentation();

    bool hasExcludeColumns() const;
    bool isExcludeColumn(std::string_view column) const;

    bool checkIfAllProvidedColumnsExist(const std::vector<std::string>& columns) const;

    std::optional<std::string> xColumnName;
    std::optional<std::string> yColumnName;
    std::optional<std::string> zColumnName;
    std::optional<std::string> nameColumn;
    std::optional<std::string> textureColumn;

    std::optional<std::filesystem::path> textureMap;

    std::optional<float> missingDataValue;

    bool isCaseSensitive = false;

    std::vector<std::string> excludeColumns;

    // OBS! When new parameters are added they should be included in the generateHash
    // function
};

/**
 * Generate a string based on the data mapping, that can be used to uniquely
 * identify the dataset.
 */
std::string generateHashString(const DataMapping& dm);

bool isPositionColumn(const std::string& c, const std::optional<DataMapping>& mapping);

bool isColumnX(const std::string& c, const std::optional<DataMapping>& mapping);

bool isColumnY(const std::string& c, const std::optional<DataMapping>& mapping);

bool isColumnZ(const std::string& c, const std::optional<DataMapping>& mapping);

bool isNameColumn(const std::string& c, const std::optional<DataMapping>& mapping);

bool isTextureColumn(const std::string& c, const std::optional<DataMapping>& mapping);

} // namespace openspace::dataloader

#endif // __OPENSPACE_CORE___DATAMAPPING___H__
