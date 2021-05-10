/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

// @TODO Add loading for label files (from RenderableBillboardsCloud)

struct Dataset {
    struct Variable {
        int index;
        std::string name;
    };
    std::vector<Variable> variables;

    struct Texture {
        int index;
        std::string file;
    };
    std::vector<Texture> textures;

    int textureDataIndex = -1;
    int orientationDataIndex = -1;

    struct Entry {
        glm::vec3 position;
        std::vector<float> data;
        std::optional<std::string> comment;
    };
    std::vector<Entry> entries;
};

// In-out methods

Dataset loadSpeckFile(std::filesystem::path path,
    SkipAllZeroLines skipAllZeroLines = SkipAllZeroLines::Yes);

Dataset loadCachedFile(std::filesystem::path path);
void saveCachedFile(const Dataset& dataset, std::filesystem::path path);

// Misc
int indexForVariable(const Dataset& dataset, std::string_view variableName);
bool normalizeVariable(Dataset& dataset, std::string_view variableName);

} // namespace openspace::speck

#endif // __OPENSPACE_MODULE_SPACE___SPECKLOADER___H__
