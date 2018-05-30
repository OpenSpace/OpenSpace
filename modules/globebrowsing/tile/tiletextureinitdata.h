/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___TILE_TEXTURE_INIT_DATA___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___TILE_TEXTURE_INIT_DATA___H__

#include <ghoul/glm.h>
#include <ghoul/misc/boolean.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/texture.h>
#include <string>

namespace openspace::globebrowsing {

/**
 * All information needed to create a texture used for a Tile.
 */
class TileTextureInitData {
public:
    using HashKey = unsigned long long;
    BooleanType(ShouldAllocateDataOnCPU);
    BooleanType(PadTiles);
    using Format = ghoul::opengl::Texture::Format;

    TileTextureInitData(size_t width, size_t height, GLenum glType, Format textureFormat,
        PadTiles padTiles,
        ShouldAllocateDataOnCPU shouldAllocateDataOnCPU = ShouldAllocateDataOnCPU::No);

    TileTextureInitData(const TileTextureInitData& original);

    ~TileTextureInitData() = default;

    glm::ivec3 dimensions() const;
    glm::ivec2 tilePixelStartOffset() const;
    glm::ivec2 tilePixelSizeDifference() const;
    size_t nRasters() const;
    size_t bytesPerDatum() const;
    size_t bytesPerPixel() const;
    size_t bytesPerLine() const;
    size_t totalNumBytes() const;
    GLenum glType() const;
    Format ghoulTextureFormat() const;
    GLenum glTextureFormat() const;
    bool shouldAllocateDataOnCPU() const;
    HashKey hashKey() const;

    const static glm::ivec2 TilePixelStartOffset;
    const static glm::ivec2 TilePixelSizeDifference;

private:

    void calculateHashKey();
    unsigned int getUniqueIdFromTextureFormat(Format textureFormat) const;

    HashKey _hashKey = HashKey(0);
    glm::ivec3 _dimensions;
    glm::ivec2 _tilePixelStartOffset;
    glm::ivec2 _tilePixelSizeDifference;
    GLenum _glType;
    Format _ghoulTextureFormat;
    GLenum _glTextureFormat;
    size_t _nRasters;
    size_t _bytesPerDatum;
    size_t _bytesPerPixel;
    size_t _bytesPerLine;
    size_t _totalNumBytes;
    bool _shouldAllocateDataOnCPU;
    bool _padTiles;
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___TILE_TEXTURE_INIT_DATA___H__
