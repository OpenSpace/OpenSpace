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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING__BASICTYPES___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING__BASICTYPES___H__

#include <ghoul/glm.h>
#include <array>
#include <memory>
#include <optional>
#include <vector>

namespace ghoul::opengl { class Texture; }

namespace openspace::globebrowsing {

struct AABB3 {
    glm::vec3 min = glm::vec3(std::numeric_limits<float>::max());
    glm::vec3 max = glm::vec3(-std::numeric_limits<float>::max());
};



struct PixelRegion {
    glm::ivec2 start = glm::ivec2(0);
    glm::ivec2 numPixels = glm::ivec2(0);
};



struct IODescription {
    struct ReadData {
        int overview;
        PixelRegion region;
        PixelRegion fullRegion;
    } read;

    struct WriteData {
        PixelRegion region;
        size_t bytesPerLine;
        size_t totalNumBytes;
    } write;
};



enum Quad {
    NORTH_WEST = 0,
    NORTH_EAST,
    SOUTH_WEST,
    SOUTH_EAST
};



struct TileDepthTransform {
    float scale = 1.f;
    float offset = 0.f;
};



struct TileMetaData {
    // 4 => the number of rasters, which has a maximum of 4 for RGBA images, we don't
    // currently support images with arbitrary number of color channels and I don't know
    // if GDAL does either.  The std::vector here causes a dynamic memory allocation every
    // time we return a Tile (which contains a TileMetaData as a member variable

    std::array<float, 4> maxValues;
    std::array<float, 4> minValues;
    std::array<bool, 4> hasMissingData;
    uint8_t nValues = 0;
};



/**
 * Defines a status and may have a Texture and TileMetaData.
 */
class Tile {
public:
    /**
     * Describe if this Tile is good for usage (OK) or otherwise the reason why it is not.
     */
    enum class Status {
        /// E.g when texture data is not currently in memory.
        /// `texture` and `tileMetaData` are both `nullptr`
        Unavailable,

        /// Can be set by `TileProvider`s if the requested `TileIndex` is undefined for
        /// that particular provider.
        /// `texture` and `metaData` are both `nullptr`
        OutOfRange,

        /// An IO Error happend
        /// `texture` and `metaData` are both `nullptr`
        IOError,

        /// The Texture is uploaded to the GPU and good for usage.
        /// `texture` is defined. `metaData` may be defined.
        OK
    };

    ghoul::opengl::Texture* texture = nullptr;
    std::optional<TileMetaData> metaData = std::nullopt;
    Status status = Status::Unavailable;
};



struct TileUvTransform {
    glm::vec2 uvOffset = glm::vec2(0.f);
    glm::vec2 uvScale = glm::vec2(0.f);
};



struct ChunkTile {
    Tile tile;
    TileUvTransform uvTransform;
    TileDepthTransform depthTransform;
};



// The ChunkTilePile either contains 1 or 3 ChunkTile, depending on if layer-blending is
// enabled. If it is enabled, we need the two adjacent levels, if it is not enabled, only
// the current layer is needed
using ChunkTilePile = std::array<std::optional<ChunkTile>, 3>;

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING__BASICTYPES___H__
