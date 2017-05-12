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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___TILE_DATAREADER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___TILE_DATAREADER___H__

#include <modules/globebrowsing/tile/textureformat.h>
#include <modules/globebrowsing/tile/tile.h>
#include <modules/globebrowsing/tile/tiledepthtransform.h>
#include <modules/globebrowsing/tile/tiledatalayout.h>
#include <modules/globebrowsing/tile/pixelregion.h>

#include <ghoul/glm.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/texture.h>

#include <gdal.h>
#include <string>

namespace openspace {
namespace globebrowsing {

struct RawTile;
class GeodeticPatch;

/**
 * Interface for reading <code>RawTile</code>s given a <code>TileIndex</code>
 */
class TileDataReader {
public:
    struct Configuration {
        bool doPreProcessing;
        int minimumTilePixelSize;
        GLuint dataType = 0; // default = no datatype reinterpretation
    };

    virtual TileDataReader(const Configuration& config);

    std::shared_ptr<RawTile> defaultTileData();
    
    virtual std::shared_ptr<RawTile> readTileData(TileIndex tileIndex) = 0;
    virtual int maxChunkLevel() = 0;
    virtual TileDepthTransform getDepthTransform() = 0;
    virtual const TileDataLayout& getDataLayout() = 0;
    virtual void reset() = 0;
    virtual float noDataValueAsFloat() = 0;
    virtual size_t rasterXSize() = 0;
    virtual size_t rasterYSize() = 0;

    const static glm::ivec2 tilePixelStartOffset;
    const static glm::ivec2 tilePixelSizeDifference;
    const static PixelRegion padding; // same as the two above
  
    const static glm::ivec2 tilePixelStartOffset;
    const static glm::ivec2 tilePixelSizeDifference;
    const static PixelRegion padding; // same as the two above

    static bool logReadErrors;
    
protected:
    Configuration _config;

    virtual std::array<double, 6> padfTransform getGeoTransform();
    PixelRegion::PixelCoordinate geodeticToPixel(const Geodetic2& geo) const;
    Geodetic2 pixelToGeodetic(const PixelRegion::PixelCoordinate& p) const;
};

} // namespace globebrowsing
} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___TILE_DATAREADER___H__
