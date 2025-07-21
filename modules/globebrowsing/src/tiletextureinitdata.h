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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___TILE_TEXTURE_INIT_DATA___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___TILE_TEXTURE_INIT_DATA___H__

#include <modules/globebrowsing/src/layergroupid.h>
#include <ghoul/glm.h>
#include <ghoul/misc/boolean.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/texture.h>

namespace openspace::globebrowsing {

/**
 * All information needed to create a texture used for a Tile.
 */
class TileTextureInitData {
public:
    using HashKey = uint64_t;
    BooleanType(ShouldAllocateDataOnCPU);

    TileTextureInitData(size_t width, size_t height, GLenum type,
        ghoul::opengl::Texture::Format textureFormat,
        ShouldAllocateDataOnCPU allocCpu = ShouldAllocateDataOnCPU::No);

    TileTextureInitData(const TileTextureInitData& original) = default;
    TileTextureInitData(TileTextureInitData&& original) = default;

    TileTextureInitData& operator=(const TileTextureInitData& rhs);
    TileTextureInitData& operator=(TileTextureInitData&& rhs) noexcept;

    ~TileTextureInitData() = default;

    const glm::ivec3 dimensions;
    const GLenum glType;
    const ghoul::opengl::Texture::Format ghoulTextureFormat;
    const size_t nRasters;
    const size_t bytesPerDatum;
    const size_t bytesPerPixel;
    const size_t bytesPerLine;
    const size_t totalNumBytes;
    const bool shouldAllocateDataOnCPU;
    const HashKey hashKey;
};

TileTextureInitData tileTextureInitData(layers::Group::ID id,
    size_t preferredTileSize = 0);

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___TILE_TEXTURE_INIT_DATA___H__
