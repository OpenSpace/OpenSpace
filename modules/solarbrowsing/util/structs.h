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

#ifndef __OPENSPACE_MODULE_SOLARBROWSING___STRUCTS___H__
#define __OPENSPACE_MODULE_SOLARBROWSING___STRUCTS___H__

#include <openspace/util/timeline.h>
#include <ghoul/glm.h>
#include <filesystem>
#include <memory>
#include <string>

//namespace openspace::solarbrowsing {
namespace openspace {

// Assume everything in arcsec to minimize metadata
struct ImageMetadata {
    std::filesystem::path filePath;
    int fullResolution = 0;
    float scale = 0;;
    glm::vec2 centerPixel;
    bool isCoronaGraph = false;
};

using InstrumentName = std::string;
using ImageMetadataMap = std::unordered_map<InstrumentName, Timeline<ImageMetadata>>;
using ImagePrecision = unsigned char;

namespace solarbrowsing {

struct DecodedImageData {
    std::vector<uint8_t> buffer;
    ImageMetadata metadata;
    unsigned int imageSize = 0;
};

using DecodeCompleteCallback = std::function<void(DecodedImageData&&)>;

struct DecodeRequest {
    ImageMetadata metadata;
    int downsamplingLevel = 0;
    // Synchronous callback assumed, can lead to race conditions if async
    DecodeCompleteCallback callback;
};

} // namespace openspace::solarbrowsing

} // namespace openspace

#endif // __OPENSPACE_MODULE_SOLARBROWSING___STRUCTS___H__
