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
        TileTextureInitData(size_t width, size_t height, GLuint glType,
            ghoul::opengl::Texture::Format textureFormat)
            : _dimensions(width, height, 1)
            , _glType(glType)
            , _ghoulTextureFormat (textureFormat)
        {
            size_t nRasters = tiledatatype::numberOfRasters(_ghoulTextureFormat);
            size_t nBytesPerDatum = tiledatatype::numberOfBytes(glType);
            size_t nBytesPerPixel = nRasters * nBytesPerDatum;
            _bytesPerLine = nBytesPerPixel * _dimensions.x;
            _totalNumBytes = _bytesPerLine * _dimensions.y;
            _glTextureFormat = tiledatatype::glTextureFormat(_glType,
                _ghoulTextureFormat);
        };
        ~TileTextureInitData() = default;

        glm::ivec3 dimensions() { return _dimensions; };
        size_t bytesPerLine() { return _bytesPerLine; };
        size_t totalNumBytes() { return _totalNumBytes; };
        GLuint glType() { return _glType; };
        ghoul::opengl::Texture::Format ghoulTextureFormat() {
            return _ghoulTextureFormat;
        };
        GLint glTextureFormat() {
            return _glTextureFormat;
        };

    private:
        glm::ivec3 _dimensions;
        GLuint _glType;
        ghoul::opengl::Texture::Format _ghoulTextureFormat;
        GLint _glTextureFormat;
        size_t _bytesPerLine;
        size_t _totalNumBytes;
    };






} // namespace globebrowsing
} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___IO_DESCRIPTION___H__
