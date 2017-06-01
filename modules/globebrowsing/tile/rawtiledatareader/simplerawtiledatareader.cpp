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

#include <modules/globebrowsing/tile/rawtiledatareader/simplerawtiledatareader.h>

#include <modules/globebrowsing/geometry/geodetic2.h>
#include <modules/globebrowsing/geometry/geodeticpatch.h>
#include <modules/globebrowsing/geometry/angle.h>
#include <modules/globebrowsing/tile/rawtiledatareader/tiledatatype.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h> // abspath
#include <ghoul/filesystem/file.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/dictionary.h>

#include <ghoul/io/texture/texturereader.h>

namespace {
    const std::string _loggerCat = "SimpleRawTileDataReader";
}

namespace openspace {
namespace globebrowsing {

SimpleRawTileDataReader::SimpleRawTileDataReader(const std::string& filePath,
        const TileTextureInitData& initData,
        RawTileDataReader::PerformPreprocessing preprocess)
    : RawTileDataReader(initData, preprocess)
{
    _datasetFilePath = filePath;
    ensureInitialized();
}

void SimpleRawTileDataReader::reset() {
    initialize();
}

int SimpleRawTileDataReader::maxChunkLevel() {
    return 2;
}

float SimpleRawTileDataReader::noDataValueAsFloat() const {
    return 0.0f;
}

int SimpleRawTileDataReader::rasterXSize() const {
    return _dataTexture->dimensions().x;
}

int SimpleRawTileDataReader::rasterYSize() const {
    return _dataTexture->dimensions().y;
}

float SimpleRawTileDataReader::depthOffset() const {
    return 0.0f;
}

float SimpleRawTileDataReader::depthScale() const {
    return 1.0f;
}

IODescription SimpleRawTileDataReader::getIODescription(const TileIndex& tileIndex) const {
    IODescription io;
    io.read.overview = 0;
    io.read.region = highestResPixelRegion(tileIndex);
    io.read.fullRegion = PixelRegion({0, 0}, {rasterXSize(), rasterYSize()});
    io.write.region = PixelRegion({0, 0}, io.read.region.numPixels);
    io.write.bytesPerLine = _dataTexture->bytesPerPixel() * io.write.region.numPixels.x;
    io.write.totalNumBytes = io.write.bytesPerLine * io.write.region.numPixels.y;
    
    return io;
}

void SimpleRawTileDataReader::initialize() {
    _dataTexture = ghoul::io::TextureReader::ref().loadTexture(_datasetFilePath);
    if (_dataTexture == nullptr) {
        throw ghoul::RuntimeError(
            "Unable to read dataset: " + _datasetFilePath +
            ".\nCompiling OpenSpace with GDAL will allow for better support for different"
            "formats."
        );
    }
    float exponentX = log2(_dataTexture->dimensions().x);
    float exponentY = log2(_dataTexture->dimensions().y);
    if ( (exponentX - static_cast<int>(exponentX)) > 0.0001 ||
       (exponentY - static_cast<int>(exponentY)) > 0.0001 ) {
        throw ghoul::RuntimeError(
            "Unable to read dataset: " + _datasetFilePath +
            ".\nCurrently only supporting power of 2 textures."
        );
    }

    _depthTransform = {depthScale(), depthOffset()};
}

void SimpleRawTileDataReader::readImageData(
    IODescription& io, RawTile::ReadError& worstError, char* dataDestination) const {
    
    // Modify to match OpenGL texture layout:
    IODescription modifiedIO = io;
    modifiedIO.read.region.start.y = modifiedIO.read.fullRegion.numPixels.y - modifiedIO.read.region.numPixels.y - modifiedIO.read.region.start.y;
  
    RawTile::ReadError err = repeatedRasterRead(0, modifiedIO, dataDestination);

    // None = 0, Debug = 1, Warning = 2, Failure = 3, Fatal = 4
    worstError = std::max(worstError, err);
}

RawTile::ReadError SimpleRawTileDataReader::rasterRead(
    int rasterBand, const IODescription& io, char* dataDestination) const
{
    ghoul_assert(static_cast<unsigned int>(io.read.fullRegion.numPixels.x) == _dataTexture->dimensions().x,
        "IODescription does not match data texture.");
    ghoul_assert(static_cast<unsigned int>(io.read.fullRegion.numPixels.y) == _dataTexture->dimensions().y,
        "IODescription does not match data texture.");
    ghoul_assert(io.read.region.numPixels.x == io.write.region.numPixels.x,
        "IODescription does not match data texture.");
    ghoul_assert(io.read.region.numPixels.y == io.write.region.numPixels.y,
        "IODescription does not match data texture.");

    char* pixelWriteRow = dataDestination;
    try {
        // For each row
        for (int y = 0; y < io.read.region.numPixels.y; y++) {
            int bytesPerLineDataTexture =
                _dataTexture->bytesPerPixel() * _dataTexture->dimensions().x;
            const char* textureRow = (static_cast<const char*>(_dataTexture->pixelData())
                + io.read.region.start.x * _dataTexture->bytesPerPixel())
                + io.read.region.start.y * bytesPerLineDataTexture
                + y * bytesPerLineDataTexture;
            memcpy(pixelWriteRow, textureRow, io.write.bytesPerLine);
            pixelWriteRow += io.write.bytesPerLine;
        }
    } catch (const std::exception&) {
        return RawTile::ReadError::Failure;
    }
    return RawTile::ReadError::None;
}


} // namespace globebrowsing
} // namespace openspace
