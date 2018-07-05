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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___TILE_DATA_TYPE___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___TILE_DATA_TYPE___H__

#include <modules/globebrowsing/tile/textureformat.h>

#include <ghoul/opengl/ghoul_gl.h>

#ifdef GLOBEBROWSING_USE_GDAL
#include <gdal.h>
#endif // GLOBEBROWSING_USE_GDAL

namespace openspace::globebrowsing { class Tile; }

namespace openspace::globebrowsing::tiledatatype {

#ifdef GLOBEBROWSING_USE_GDAL
GLenum getOpenGLDataType(GDALDataType gdalType);

GDALDataType getGdalDataType(GLenum glType);

TextureFormat getTextureFormat(int rasterCount, GDALDataType gdalType);

TextureFormat getTextureFormatOptimized(int rasterCount, GDALDataType gdalType);

size_t getMaximumValue(GDALDataType gdalType);

size_t numberOfBytes(GDALDataType gdalType);

float interpretFloat(GDALDataType gdalType, const char* src);
#endif // GLOBEBROWSING_USE_GDAL

GLenum glTextureFormat(GLenum glType, ghoul::opengl::Texture::Format format);

size_t numberOfRasters(ghoul::opengl::Texture::Format format);

size_t numberOfBytes(GLenum glType);

size_t getMaximumValue(GLenum glType);

float interpretFloat(GLenum glType, const char* src);

} // namespace openspace::globebrowsing::tiledatatype

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___TILE_DATA_TYPE___H__
