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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___TILEDATALAYOUT___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___TILEDATALAYOUT___H__

#include <modules/globebrowsing/tile/textureformat.h>

#include <ghoul/glm.h>
#include <ghoul/opengl/ghoul_gl.h>

class GDALDataset;

namespace openspace {
namespace globebrowsing {

struct TileDataLayout {
    GLuint glType;

    size_t bytesPerDatum;
    size_t numRasters;
    /// Number of rasters available in the GDAL dataset.
    /// Does not necessarily have to be equal to numRasters.
    /// In case an extra alpha channel needs to be added that
    /// does not exist in the GDAL dataset for example
    size_t numRastersAvailable;
    size_t bytesPerPixel;

    TextureFormat textureFormat;
};

} // namespace globebrowsing
} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___TILEDATALAYOUT___H__
