/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#ifndef __OPENSPACE_MODULE_SOLARBROWSING___SPACECRAFTIMAGERYMANAGER___H__
#define __OPENSPACE_MODULE_SOLARBROWSING___SPACECRAFTIMAGERYMANAGER___H__

#include <modules/solarbrowsing/util/structs.h>
#include <openspace/util/timeline.h>
#include <ghoul/filesystem/filesystem.h>
#include <memory>
#include <unordered_map>
#include <optional>
#include <valarray>
#include <vector>

namespace ghoul {
  namespace opengl { class Texture; }
  namespace filesystem { class File; }
} // namespace ghoul

namespace openspace {

class TransferFunction;

using ImageMetadataMap = std::unordered_map<std::string, Timeline<ImageMetadata>>;

// @TODO anden88 2025-12-05: Move these functions to a utility file instead. Unlees there
// is a need for a manager (currently not since it has no member variables or keep tracks of
// anything particular)
class SpacecraftImageryManager {
public:
    void loadTransferFunctions(const std::filesystem::path& dir,
        std::unordered_map<std::string, std::shared_ptr<TransferFunction>>& _tfMap);
    void loadImageMetadata(const std::filesystem::path& rootDir,
        ImageMetadataMap& _imageMetadataMap);

private:
    std::optional<ImageMetadata> parseJ2kMetadata(const std::filesystem::path& filePath);
    //ImageMetadata parseJsonMetadata(const ghoul::filesystem::File& file);

    std::string ISO8601(std::string& datetime);

    bool loadMetadataFromDisk(const std::filesystem::path& rootDir,
       ImageMetadataMap& _imageMetadataMap);

    void saveMetadataToDisk(const std::filesystem::path& rootPath,
        const ImageMetadataMap& _imageMetadataMap);
};

} //namespace openspace

#endif // __OPENSPACE_MODULE_SOLARBROWSING___SPACECRAFTIMAGERYMANAGER___H__
