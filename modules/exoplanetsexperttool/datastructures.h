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

#ifndef __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___DATASTRUCTURES___H__
#define __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___DATASTRUCTURES___H__

#include <ghoul/glm.h>
#include <limits>
#include <map>
#include <optional>
#include <string>
#include <unordered_map>
#include <variant>

namespace openspace::exoplanets {

using ColumnKey = std::string;

// Represent a data point with upper and lower uncertainty values
struct DataPoint {
    float value = std::numeric_limits<float>::quiet_NaN();
    float errorUpper = 0.f;
    float errorLower = 0.f;

    // TODO:move all these to a cpp file
    bool hasValue() const {
        return !std::isnan(value);
    };

    float errorRange() const {
        return errorUpper - errorLower;
    };

    float relativeErrorUpper() const {
        if (!hasValue()) {
            return std::numeric_limits<float>::quiet_NaN();
        }
        return 100.f * errorUpper / value;
    };

    float relativeErrorLower() const {
        if (!hasValue()) {
            return std::numeric_limits<float>::quiet_NaN();
        }
        return 100.f * errorLower / value;
    };

    float relativeErrorRange() const {
        if (!hasValue()) {
            return std::numeric_limits<float>::quiet_NaN();
        }
        return relativeErrorUpper() - relativeErrorLower();
    };
};

// TODO: Automatically determine which columns have uncertainty and should be
// represented with a datapoint.

struct ExoplanetItem {
    int id; // Id used for UI (same as row number in data file)

    std::optional<glm::dvec3> position = std::nullopt; // in Parsec

    std::string name;
    std::string hostName;

    // Data reference
    std::string referenceName;
    std::string referenceUrl;

    // A map between column name and a string/float value
    std::map<ColumnKey, std::variant<std::string, float>> dataColumns;

    float sizeValue = 0.f;

    // The planet's internal index within its system, from the inside out
    int indexInSystem = -1;
};

struct DataSettings {
    std::filesystem::path dataFile;

    // Column names for certain columns that we need for the tool to work
    struct DataMapping {
        ColumnKey positionRa;
        ColumnKey positionDec;
        ColumnKey positionDistance;

        ColumnKey name = "";
        ColumnKey hostName = "";
        ColumnKey ringSize = "";
        ColumnKey referenceName = "";
        ColumnKey referenceLink = "";
    } dataMapping;

    struct CmapInfo {
        ColumnKey column;
        float min = 0.f; // TODO: optional (set from range if excluded?)
        float max = 100.f; // TODO: optional
    };
    std::optional<CmapInfo> defaultColormapping;

    struct ColumnInfo {
        std::string name;
        std::string format = "";
        std::string description = "";

        // Sometimes, a seemingly numeric column should really be a text-based one.
        // This allows us to control that
        std::optional<bool> isText;
    };
    std::unordered_map<ColumnKey, ColumnInfo> columnInfo;

    struct QuickFilter {
        std::string name;

        struct Filter {
            ColumnKey column;
            std::string query;
        };
        std::vector<Filter> filters;

        std::string description = "";
    };

    struct QuickFilterGroup {
        std::string title = "";

        enum class Type {
            And = 0,
            Or
        } type;

        bool showOnSameLine = false;

        std::vector<QuickFilter> quickFilters;
    };

    std::vector<QuickFilterGroup> quickFilterGroups;


    ColumnKey nameColumn() const {
        return dataMapping.name.empty() ? "name" : dataMapping.name;
    };

    // Returns the column name, if there is one. Otherwise just the key.
    const char* columnName(const ColumnKey& key) const {
        return columnInfo.contains(key) ? columnInfo.at(key).name.c_str() : key.c_str();
    }

    bool hasName(const ColumnKey& key) const {
        return columnInfo.contains(key);
    }

    const std::string& description(const ColumnKey& key) const {
        return columnInfo.at(key).description;
    }

    bool hasDescription(const ColumnKey& key) const {
        return columnInfo.contains(key) && !columnInfo.at(key).description.empty();
    }
};

} // namespace openspace::exoplanets

#endif // __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___DATASTRUCTURES___H__
