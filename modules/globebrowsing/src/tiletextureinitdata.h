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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___TILE_TEXTURE_INIT_DATA___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___TILE_TEXTURE_INIT_DATA___H__

#include <modules/globebrowsing/src/layergroupid.h>
#include <openspace/glm.h>
#include <openspace/misc/boolean.h>
#include <openspace/opengl/gl.h>
#include <openspace/opengl/texture.h>
#include <cstdint>

namespace openspace {

/**
 * All information needed to create a texture used for a Tile.
 */
class TileTextureInitData {
public:
    using HashKey = uint64_t;
    BooleanType(ShouldAllocateDataOnCPU);

    TileTextureInitData(size_t width, size_t height, GLenum type,
        opengl::Texture::Format textureFormat,
        ShouldAllocateDataOnCPU allocCpu = ShouldAllocateDataOnCPU::No);

    glm::ivec3 dimensions;
    GLenum glType;
    opengl::Texture::Format textureFormat;
    size_t nRasters;
    size_t bytesPerDatum;
    size_t bytesPerPixel;
    size_t bytesPerLine;
    size_t totalNumBytes;
    bool shouldAllocateDataOnCPU;
    HashKey hashKey;
};

TileTextureInitData tileTextureInitData(layers::Group::ID id,
    size_t preferredTileSize = 0);

} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___TILE_TEXTURE_INIT_DATA___H__
