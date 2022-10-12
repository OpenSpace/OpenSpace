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

#ifndef __OPENSPACE_MODULE_SPACE___SPECKLOADER___H__
#define __OPENSPACE_MODULE_SPACE___SPECKLOADER___H__

#include <ghoul/glm.h>
#include <ghoul/misc/boolean.h>
#include <filesystem>
#include <optional>
#include <string>
#include <vector>

namespace openspace::speck {

BooleanType(SkipAllZeroLines);

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

    int index(std::string_view variableName) const;
    bool normalizeVariable(std::string_view variableName);
};

struct Labelset {
    int textColorIndex = -1;

    struct Entry {
        glm::vec3 position = glm::vec3(0.f);
        std::string identifier;
        std::string text;
    };
    std::vector<Entry> entries;
};

struct ColorMap {
    std::vector<glm::vec4> entries;
};

namespace data {

    Dataset loadFile(std::filesystem::path path,
        SkipAllZeroLines skipAllZeroLines = SkipAllZeroLines::Yes);

    std::optional<Dataset> loadCachedFile(std::filesystem::path path);
    void saveCachedFile(const Dataset& dataset, std::filesystem::path path);

    Dataset loadFileWithCache(std::filesystem::path speckPath,
        SkipAllZeroLines skipAllZeroLines = SkipAllZeroLines::Yes);

} // namespace data

namespace label {

    Labelset loadFile(std::filesystem::path path,
        SkipAllZeroLines skipAllZeroLines = SkipAllZeroLines::Yes);

    std::optional<Labelset> loadCachedFile(std::filesystem::path path);
    void saveCachedFile(const Labelset& labelset, std::filesystem::path path);

    Labelset loadFileWithCache(std::filesystem::path speckPath,
        SkipAllZeroLines skipAllZeroLines = SkipAllZeroLines::Yes);

} // namespace label

namespace color {

    ColorMap loadFile(std::filesystem::path path,
        SkipAllZeroLines skipAllZeroLines = SkipAllZeroLines::Yes);

    std::optional<ColorMap> loadCachedFile(std::filesystem::path path);
    void saveCachedFile(const ColorMap& colorMap, std::filesystem::path path);

    ColorMap loadFileWithCache(std::filesystem::path path,
        SkipAllZeroLines skipAllZeroLines = SkipAllZeroLines::Yes);

} // namespace color


} // namespace openspace::speck

#endif // __OPENSPACE_MODULE_SPACE___SPECKLOADER___H__
