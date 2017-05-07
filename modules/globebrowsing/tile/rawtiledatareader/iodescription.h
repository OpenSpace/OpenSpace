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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___IO_DESCRIPTION___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___IO_DESCRIPTION___H__

#include <modules/globebrowsing/tile/textureformat.h>
#include <modules/globebrowsing/tile/tile.h>
#include <modules/globebrowsing/tile/tiledepthtransform.h>
#include <modules/globebrowsing/tile/tiledatalayout.h>
#include <modules/globebrowsing/tile/pixelregion.h>
#include <modules/globebrowsing/tile/rawtile.h>
#include <modules/globebrowsing/tile/rawtiledatareader/tiledatatype.h>

#include <ghoul/glm.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/texture.h>

#include <string>

namespace openspace {
namespace globebrowsing {

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
    
    IODescription cut(PixelRegion::Side side, int pos);
};

struct TileWriteDataDescription {
	PixelRegion region;
	size_t bytesPerLine;
	size_t totalNumBytes;
    GLuint glType;
    TextureFormat textureFormat;
};











    /**
     * All information needed to create a texture.
     */
    class TileTextureInitData
    {
    public:
        using HashKey = unsigned long long;

        TileTextureInitData(size_t width, size_t height, GLuint glType,
            ghoul::opengl::Texture::Format textureFormat)
            : _glType(glType)
            , _ghoulTextureFormat (textureFormat)
        {
            _dimensionsWithoutPadding = glm::ivec3(width, height, 1);
            _dimensionsWithPadding = glm::ivec3(
                width + tilePixelSizeDifference.x, height + tilePixelSizeDifference.y, 1);
            size_t nRasters = tiledatatype::numberOfRasters(_ghoulTextureFormat);
            size_t nBytesPerDatum = tiledatatype::numberOfBytes(glType);
            size_t nBytesPerPixel = nRasters * nBytesPerDatum;
            _bytesPerLine = nBytesPerPixel * _dimensionsWithPadding.x;
            _totalNumBytes = _bytesPerLine * _dimensionsWithPadding.y;
            _glTextureFormat = tiledatatype::glTextureFormat(_glType,
                _ghoulTextureFormat);
            calculateHashKey();
        };

        TileTextureInitData(const TileTextureInitData& original)
            : TileTextureInitData(
                original.dimensionsWithoutPadding().x,
                original.dimensionsWithoutPadding().y,
                original.glType(),
                original.ghoulTextureFormat())
        { }

        ~TileTextureInitData() = default;

        glm::ivec3 dimensionsWithPadding() const { return _dimensionsWithPadding; };
        glm::ivec3 dimensionsWithoutPadding() const { return _dimensionsWithoutPadding; };
        size_t bytesPerLine() const { return _bytesPerLine; };
        size_t totalNumBytes() const { return _totalNumBytes; };
        GLuint glType() const { return _glType; };
        ghoul::opengl::Texture::Format ghoulTextureFormat() const {
            return _ghoulTextureFormat;
        };
        GLint glTextureFormat() const {
            return _glTextureFormat;
        };
        HashKey hashKey() const { return _hashKey; };

        const static glm::ivec2 tilePixelStartOffset;
        const static glm::ivec2 tilePixelSizeDifference;

    private:
        void calculateHashKey() {
            ghoul_assert(_dimensionsWithoutPadding.x > 0, "Incorrect dimension");
            ghoul_assert(_dimensionsWithoutPadding.y > 0, "Incorrect dimension");
            ghoul_assert(_dimensionsWithoutPadding.x <= 1024, "Incorrect dimension");
            ghoul_assert(_dimensionsWithoutPadding.y <= 1024, "Incorrect dimension");
            ghoul_assert(_dimensionsWithoutPadding.z == 1, "Incorrect dimension");
            unsigned int format = getUniqueIdFromTextureFormat(_ghoulTextureFormat);
            ghoul_assert(format < 256, "Incorrect format");

            _hashKey = 0LL;

            _hashKey |= _dimensionsWithoutPadding.x;
            _hashKey |= _dimensionsWithoutPadding.y << 10;
            _hashKey |= _glType << (10 + 16);
            _hashKey |= format << (10 + 16 + 4);
        };

        unsigned int getUniqueIdFromTextureFormat(
            ghoul::opengl::Texture::Format textureFormat)
        {
            using Format = ghoul::opengl::Texture::Format;
            switch (textureFormat) {
                case Format::Red:
                    return 0;
                case Format::RG:
                    return 1;
                case Format::RGB:
                    return 2;
                case Format::BGR:
                    return 3;
                case Format::RGBA:
                    return 4;
                case Format::BGRA:
                    return 5;
                case Format::DepthComponent:
                    return 6;
                default:
                    ghoul_assert(false, "Unknown texture format");
            }
        }

        HashKey _hashKey;
        glm::ivec3 _dimensionsWithPadding;
        glm::ivec3 _dimensionsWithoutPadding;
        GLuint _glType;
        ghoul::opengl::Texture::Format _ghoulTextureFormat;
        GLint _glTextureFormat;
        size_t _bytesPerLine;
        size_t _totalNumBytes;
    };



} // namespace globebrowsing
} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___IO_DESCRIPTION___H__
