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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___SIMPLE_RAW_TILE_DATA_READER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___SIMPLE_RAW_TILE_DATA_READER___H__

#include <modules/globebrowsing/tile/textureformat.h>
#include <modules/globebrowsing/tile/tile.h>
#include <modules/globebrowsing/tile/tiledepthtransform.h>
#include <modules/globebrowsing/tile/tiledatalayout.h>
#include <modules/globebrowsing/tile/pixelregion.h>
#include <modules/globebrowsing/tile/rawtile.h>

#include <modules/globebrowsing/tile/rawtiledatareader/rawtiledatareader.h>

#include <ghoul/glm.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/texture.h>

#include <string>

namespace openspace {
namespace globebrowsing {

class GeodeticPatch;

class SimpleRawTileDataReader : public RawTileDataReader {
public:

    SimpleRawTileDataReader(const std::string& filePath, const Configuration& config);

    // Public virtual function overloading
    virtual void reset() override;
    virtual int maxChunkLevel() override;
    virtual float noDataValueAsFloat() const override;
    virtual int rasterXSize() const override;
    virtual int rasterYSize() const override;
    virtual float depthOffset() const override;
    virtual float depthScale() const override;

protected:

    virtual IODescription getIODescription(const TileIndex& tileIndex) const override;

private:
    // Private virtual function overloading
    virtual void initialize() override;
    virtual char* readImageData(
        IODescription& io, RawTile::ReadError& worstError) const override;
    virtual RawTile::ReadError rasterRead(
        int rasterBand, const IODescription& io, char* dst) const override;

    // Member variables
    std::string _datasetFilePath;
    std::unique_ptr<ghoul::opengl::Texture> _dataTexture;
};

} // namespace globebrowsing
} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___SIMPLE_RAW_TILE_DATA_READER___H__
