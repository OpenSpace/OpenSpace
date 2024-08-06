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

#ifndef __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___DATASTRUCTURES___H__
#define __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___DATASTRUCTURES___H__

#include <ghoul/glm.h>
#include <limits>
#include <map>
#include <optional>
#include <string>
#include <variant>

namespace openspace::exoplanets {

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

// TODO: Automatically determine which columns have uncertainty and sould be represented with a datapoint.

struct ExoplanetItem {
    int id; // Id used for UI (same as row number in data file)

    std::optional<glm::dvec3> position = std::nullopt; // in Parsec

    std::string name;
    std::string hostName;

    // Data reference
    std::string referenceName;
    std::string referenceUrl;

    // A map between column name and a string/float value
    std::map<std::string, std::variant<std::string, float>> dataColumns;

    float sizeValue = 0.f;
    DataPoint ra;
    DataPoint dec;
    DataPoint distance;

    // The planet's internal index within its system, from the inside out
    int indexInSystem = -1;
};

struct DataSettings {
    std::string dataFile;

    // Column names for certain columns that we need for the tool to work
    struct {
        std::string positionRa;
        std::string positionDec;
        std::string positionDistance;

        std::string name;
        std::string hostName; // TODO: optional
        std::string ringSize; // TODO: optional
        std::string referenceLink; // TODO: optional
    } dataMapping;

    struct ColumnInfo {
        std::string name;
        std::optional<std::string> format;
        std::optional<std::string> description;

        // Sometimes, a seemingly numeric column should really be a text-based one.
        // This allows us to control that
        std::optional<bool> isText;
    };
    std::map<std::string, ColumnInfo> columnInfo; // Mapped by "column key"
};

} // namespace openspace::exoplanets

#endif // __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___DATASTRUCTURES___H__
