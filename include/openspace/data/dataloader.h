/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef __OPENSPACE_CORE___DATALOADER___H__
#define __OPENSPACE_CORE___DATALOADER___H__

#include <openspace/data/datamapping.h>
#include <ghoul/glm.h>
#include <ghoul/misc/boolean.h>
#include <ghoul/misc/csvreader.h>
#include <filesystem>
#include <optional>
#include <string>
#include <vector>

namespace openspace::dataloader {

/**
 * A dataset representing objects with positions and various other data columns.
 * Based on the SPECK format originally used for the digital universe datasets.
 * Mostly used for point-data.
 *
 * The read data files may also have associated texture values to be used for the
 * points.
 */
struct Dataset {
    struct Variable {
        int index = -1;
        std::string name;
    };
    std::vector<Variable> variables;

    struct Texture {
        int index = -1;
        std::string file;
    };
    std::vector<Texture> textures;

    int textureDataIndex = -1;
    int orientationDataIndex = -1;

    struct Entry {
        glm::vec3 position = glm::vec3(0.f);
        std::vector<float> data;
        std::optional<std::string> comment;
    };
    std::vector<Entry> entries;

    /// This variable can be used to get an understanding of the world scale size of
    /// the dataset
    float maxPositionComponent = 0.f;

    bool isEmpty() const;

    int index(std::string_view variableName) const;
    bool normalizeVariable(std::string_view variableName);
    glm::vec2 findValueRange(int variableIndex) const;
    glm::vec2 findValueRange(std::string_view variableName) const;
};

struct Labelset {
    int textColorIndex = -1;

    struct Entry {
        glm::vec3 position = glm::vec3(0.f);
        std::string identifier;
        std::string text;
        bool isEnabled = true;
    };
    std::vector<Entry> entries;
};

struct ColorMap {
    std::optional<glm::vec4> belowRangeColor;
    std::optional<glm::vec4> aboveRangeColor;
    std::optional<glm::vec4> nanColor;
    std::vector<glm::vec4> entries;
};

namespace data {

    Dataset loadFile(std::filesystem::path path,
        std::optional<DataMapping> specs = std::nullopt);

    std::optional<Dataset> loadCachedFile(const std::filesystem::path& path);
    void saveCachedFile(const Dataset& dataset, const std::filesystem::path& path);

    Dataset loadFileWithCache(std::filesystem::path path,
        std::optional<DataMapping> specs = std::nullopt);

} // namespace data

namespace label {

    Labelset loadFile(std::filesystem::path path,
        std::optional<DataMapping> specs = std::nullopt);

    std::optional<Labelset> loadCachedFile(const std::filesystem::path& path);
    void saveCachedFile(const Labelset& labelset, const std::filesystem::path& path);

    Labelset loadFileWithCache(std::filesystem::path path);

    Labelset loadFromDataset(const dataloader::Dataset& dataset);
} // namespace label

namespace color {

    ColorMap loadFile(std::filesystem::path path,
        std::optional<DataMapping> specs = std::nullopt);

    std::optional<ColorMap> loadCachedFile(const std::filesystem::path& path);
    void saveCachedFile(const ColorMap& colorMap, const std::filesystem::path& path);

    ColorMap loadFileWithCache(std::filesystem::path path);

} // namespace color

} // namespace openspace::dataloader

#endif // __OPENSPACE_CORE___DATALOADER___H__
