/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <ghoul/designpattern/singleton.h>
#include <memory>
#include <vector>
#include <valarray>
#include <unordered_map>
#include <unordered_set>
#include <modules/solarbrowsing/util/timedependentstatesequence.h>

#define IMG_PRECISION unsigned char

namespace ghoul {
  namespace opengl { class Texture; }
  namespace filesystem { class File; }
}

namespace openspace {

class TransferFunction;

// Assume everythign in arcsec to minimize metadata
struct ImageMetadata {
    std::string filename;
    std::string preprocessedFilename;
    std::string spacecraftType;
    int fullResolution;
    float scale;
    glm::vec2 centerPixel;
    bool isCoronaGraph;
};

struct VideoMetadata {
    std::string path;
    int startTime;
    int endTime;
    bool isCoronaGraph;
    float scale;
    int resolution;
    glm::vec2 centerPixel;
    int frameUpdateInterval;
};

class SpacecraftImageryManager : public ghoul::Singleton<SpacecraftImageryManager> {
    friend class ghoul::Singleton<SpacecraftImageryManager>;

public:
    SpacecraftImageryManager();
    void ConvertTileJ2kImages(const std::string& path, const unsigned int tileWidth,
                              const unsigned int tileHeight);
    void loadVideoMetadata(const std::string& path, std::unordered_map<std::string, VideoMetadata>& _videoMetadata);
    void loadTransferFunctions(
          const std::string& path,
          std::unordered_map<std::string, std::shared_ptr<TransferFunction>>& _tfMap,
          const std::unordered_set<std::string>& _filter);
    void loadTransferFunctions(
          const std::string& path,
          std::unordered_map<std::string, std::shared_ptr<TransferFunction>>& _tfMap);
    std::vector<ImageMetadata> loadImageMetadata(const std::string& path);
    void loadImageMetadata(
      const std::string& path,
      std::unordered_map<std::string, TimedependentStateSequence<ImageMetadata>>& _imageMetadataMap,
      const std::unordered_set<std::string>& _filter);
private:
    ImageMetadata parseMetadata(const ghoul::filesystem::File& file);
    void fetchServerImages(std::string type);
    void fillImageryInfo(std::string buffer, std::string type);
    void createLUT();
};

} //namespace openspace

#endif // __OPENSPACE_MODULE_SOLARBROWSING___SPACECRAFTIMAGERYMANAGER___H__
