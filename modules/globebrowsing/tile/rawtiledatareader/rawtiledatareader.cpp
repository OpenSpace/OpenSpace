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

#include <modules/globebrowsing/tile/rawtiledatareader/rawtiledatareader.h>

#include <modules/globebrowsing/tile/rawtiledatareader/tiledatatype.h>

#include <modules/globebrowsing/tile/tile.h>
#include <modules/globebrowsing/tile/tiletextureinitdata.h>
#include <modules/globebrowsing/tile/tileprovider/tileprovider.h>
#include <modules/globebrowsing/tile/tiledepthtransform.h>
#include <modules/globebrowsing/tile/pixelregion.h>
#include <modules/globebrowsing/tile/rawtile.h>
#include <modules/globebrowsing/tile/tilemetadata.h>

#include <modules/globebrowsing/geometry/geodetic2.h>
#include <modules/globebrowsing/geometry/geodeticpatch.h>
#include <modules/globebrowsing/geometry/angle.h>

#include <openspace/engine/configurationmanager.h>

#include <float.h>
#include <sstream>
#include <algorithm>
#include <memory>
#include <set>
#include <queue>
#include <iostream>
#include <unordered_map>
#include <limits>

#define _USE_MATH_DEFINES
#include <math.h>

namespace openspace::globebrowsing {

const PixelRegion RawTileDataReader::padding = PixelRegion(
    TileTextureInitData::tilePixelStartOffset,
    TileTextureInitData::tilePixelSizeDifference
);
    
RawTileDataReader::RawTileDataReader(const TileTextureInitData& initData,
        PerformPreprocessing preprocess)
    : _initData(initData)
    , _preprocess(preprocess)
    , _hasBeenInitialized(false)
{}

std::shared_ptr<RawTile> RawTileDataReader::defaultTileData() const {
    return std::make_shared<RawTile>(RawTile::createDefault(_initData));
}

std::shared_ptr<RawTile> RawTileDataReader::readTileData(TileIndex tileIndex,
    char* dataDestination, char* pboMappedDataDestination) const
{
    IODescription io = getIODescription(tileIndex);
    RawTile::ReadError worstError = RawTile::ReadError::None;

    // Build the RawTile from the data we querred
    std::shared_ptr<RawTile> rawTile = std::make_shared<RawTile>();
    
    if (dataDestination && !pboMappedDataDestination) {
        // Write only to cpu data destination
        memset(dataDestination, 255, _initData.totalNumBytes());
        readImageData(io, worstError, dataDestination);
    }
    else if (!dataDestination && pboMappedDataDestination) {
        // Write only to pbo mapped data destination
        memset(pboMappedDataDestination, 255, _initData.totalNumBytes());
        readImageData(io, worstError, pboMappedDataDestination);
    }
    else if (dataDestination && pboMappedDataDestination) {
        // Write to both data destinations
        memset(dataDestination, 255, _initData.totalNumBytes());
        readImageData(io, worstError, dataDestination);
        size_t numBytes = _initData.totalNumBytes();
        memcpy(pboMappedDataDestination, dataDestination, numBytes);
    }
    else {
        ghoul_assert(false, "Need to specify a data destination");
    }

    rawTile->imageData = dataDestination;
    rawTile->error = worstError;
    rawTile->tileIndex = tileIndex;

    rawTile->textureInitData = std::make_shared<TileTextureInitData>(_initData);

    if (_preprocess == PerformPreprocessing::Yes) {
        rawTile->tileMetaData = getTileMetaData(rawTile, io.write.region);
        rawTile->error = std::max(rawTile->error, postProcessErrorCheck(rawTile));
    }

    return rawTile;
}

void RawTileDataReader::readImageData(
    IODescription& io, RawTile::ReadError& worstError, char* imageDataDest) const {
  
    io = adjustIODescription(io);

    // Only read the minimum number of rasters
    int nRastersToRead = std::min(dataSourceNumRasters(),
        static_cast<int>(_initData.nRasters()));

    switch (_initData.ghoulTextureFormat()) {
        case ghoul::opengl::Texture::Format::Red: {
            char* dataDestination = imageDataDest;
            RawTile::ReadError err = repeatedRasterRead(1, io, dataDestination);
            worstError = std::max(worstError, err);
            break;
        }
        case ghoul::opengl::Texture::Format::RG:
        case ghoul::opengl::Texture::Format::RGB:
        case ghoul::opengl::Texture::Format::RGBA: {
            if (nRastersToRead == 1) { // Grayscale
                for (int i = 0; i < 3; i++) {
                    // The final destination pointer is offsetted by one datum byte size
                    // for every raster (or data channel, i.e. R in RGB)
                    char* dataDestination = imageDataDest + (i * _initData.bytesPerDatum());

                    RawTile::ReadError err = repeatedRasterRead(1, io, dataDestination);

                    // CE_None = 0, CE_Debug = 1, CE_Warning = 2, CE_Failure = 3, CE_Fatal = 4
                    worstError = std::max(worstError, err);
                }
            }
            else if (nRastersToRead == 2) { // Grayscale + alpha
                for (int i = 0; i < 3; i++) {
                    // The final destination pointer is offsetted by one datum byte size
                    // for every raster (or data channel, i.e. R in RGB)
                    char* dataDestination = imageDataDest + (i * _initData.bytesPerDatum());

                    RawTile::ReadError err = repeatedRasterRead(1, io, dataDestination);

                    // CE_None = 0, CE_Debug = 1, CE_Warning = 2, CE_Failure = 3, CE_Fatal = 4
                    worstError = std::max(worstError, err);
                }
                // Last read is the alpha channel
                char* dataDestination = imageDataDest + (3 * _initData.bytesPerDatum());
                RawTile::ReadError err = repeatedRasterRead(2, io, dataDestination);

                // CE_None = 0, CE_Debug = 1, CE_Warning = 2, CE_Failure = 3, CE_Fatal = 4
                worstError = std::max(worstError, err);
            }
            else { // Three or more rasters
                for (int i = 0; i < nRastersToRead; i++) {
                    // The final destination pointer is offsetted by one datum byte size
                    // for every raster (or data channel, i.e. R in RGB)
                    char* dataDestination = imageDataDest + (i * _initData.bytesPerDatum());

                    RawTile::ReadError err = repeatedRasterRead(i + 1, io, dataDestination);

                    // CE_None = 0, CE_Debug = 1, CE_Warning = 2, CE_Failure = 3, CE_Fatal = 4
                    worstError = std::max(worstError, err);
                }
            }
            break;
        }
        case ghoul::opengl::Texture::Format::BGR:
        case ghoul::opengl::Texture::Format::BGRA: {
            if (nRastersToRead == 1) { // Grayscale
                for (int i = 0; i < 3; i++) {
                    // The final destination pointer is offsetted by one datum byte size
                    // for every raster (or data channel, i.e. R in RGB)
                    char* dataDestination = imageDataDest + (i * _initData.bytesPerDatum());

                    RawTile::ReadError err = repeatedRasterRead(1, io, dataDestination);

                    // CE_None = 0, CE_Debug = 1, CE_Warning = 2, CE_Failure = 3, CE_Fatal = 4
                    worstError = std::max(worstError, err);
                }
            }
            else if (nRastersToRead == 2) { // Grayscale + alpha
                for (int i = 0; i < 3; i++) {
                    // The final destination pointer is offsetted by one datum byte size
                    // for every raster (or data channel, i.e. R in RGB)
                    char* dataDestination = imageDataDest + (i * _initData.bytesPerDatum());

                    RawTile::ReadError err = repeatedRasterRead(1, io, dataDestination);

                    // CE_None = 0, CE_Debug = 1, CE_Warning = 2, CE_Failure = 3, CE_Fatal = 4
                    worstError = std::max(worstError, err);
                }
                // Last read is the alpha channel
                char* dataDestination = imageDataDest + (3 * _initData.bytesPerDatum());
                RawTile::ReadError err = repeatedRasterRead(2, io, dataDestination);

                // CE_None = 0, CE_Debug = 1, CE_Warning = 2, CE_Failure = 3, CE_Fatal = 4
                worstError = std::max(worstError, err);
            }
            else { // Three or more rasters
                for (int i = 0; i < 3 && i < nRastersToRead; i++) {
                    // The final destination pointer is offsetted by one datum byte size
                    // for every raster (or data channel, i.e. R in RGB)
                    char* dataDestination = imageDataDest + (i * _initData.bytesPerDatum());

                    RawTile::ReadError err = repeatedRasterRead(3 - i, io, dataDestination);

                    // CE_None = 0, CE_Debug = 1, CE_Warning = 2, CE_Failure = 3, CE_Fatal = 4
                    worstError = std::max(worstError, err);
                }
            }
            if (nRastersToRead > 3) { // Alpha channel exists
                // Last read is the alpha channel
                char* dataDestination = imageDataDest + (3 * _initData.bytesPerDatum());
                RawTile::ReadError err = repeatedRasterRead(4, io, dataDestination);

                // CE_None = 0, CE_Debug = 1, CE_Warning = 2, CE_Failure = 3, CE_Fatal = 4
                worstError = std::max(worstError, err);
            }
            break;
        }
        default: {
            ghoul_assert(false, "Texture format not supported for tiles");
            break;
        }
    }
}

IODescription RawTileDataReader::adjustIODescription(const IODescription& io) const {
    return io;
}

IODescription RawTileDataReader::getIODescription(const TileIndex& tileIndex) const {
    IODescription io;
    io.read.region = highestResPixelRegion(tileIndex);
    
    // write region starts in origin
    io.write.region.start = PixelRegion::PixelCoordinate(0, 0);
    io.write.region.numPixels = PixelRegion::PixelCoordinate(
        _initData.dimensionsWithoutPadding().x, _initData.dimensionsWithoutPadding().y);
    
    io.read.overview = 0;
    io.read.fullRegion = fullPixelRegion();
    // For correct sampling in dataset, we need to pad the texture tile
    
    PixelRegion scaledPadding = padding;
    double scale =
        io.read.region.numPixels.x / static_cast<double>(io.write.region.numPixels.x);
    scaledPadding.numPixels *= scale;
    scaledPadding.start *= scale;

    io.read.region.pad(scaledPadding);
    io.write.region.pad(padding);
    io.write.region.start = PixelRegion::PixelCoordinate(0, 0);
    
    io.write.bytesPerLine = _initData.bytesPerLine();
    io.write.totalNumBytes = _initData.totalNumBytes();

    ghoul_assert(io.write.region.numPixels.x == io.write.region.numPixels.y,
        "Write region must be square");
    ghoul_assert(io.write.region.numPixels.x == _initData.dimensionsWithPadding().x,
        "Write region must match tile it writes to.");

    return io;
}

TileDepthTransform RawTileDataReader::getDepthTransform() const {
    return _depthTransform;
}

const TileTextureInitData& RawTileDataReader::tileTextureInitData() const {
    return _initData;
}

const PixelRegion::PixelRange RawTileDataReader::fullPixelSize() const {
    return glm::uvec2(geodeticToPixel(Geodetic2(90, 180)));
}

PixelRegion RawTileDataReader::fullPixelRegion() const {
    PixelRegion fullRegion;
    fullRegion.start.x = 0;
    fullRegion.start.y = 0;
    fullRegion.numPixels.x = rasterXSize();
    fullRegion.numPixels.y = rasterYSize();
    return fullRegion;
}

std::array<double, 6> RawTileDataReader::getGeoTransform() const {
    std::array<double, 6> padfTransform;
    
    GeodeticPatch globalCoverage(Geodetic2(0,0), Geodetic2(M_PI / 2.0, M_PI));
    padfTransform[1] = Angle<double>::fromRadians(
        globalCoverage.size().lon).asDegrees() / rasterXSize();
    padfTransform[5] = -Angle<double>::fromRadians(
        globalCoverage.size().lat).asDegrees() / rasterYSize();
    padfTransform[0] = Angle<double>::fromRadians(
        globalCoverage.getCorner(Quad::NORTH_WEST).lon).asDegrees();
    padfTransform[3] = Angle<double>::fromRadians(
        globalCoverage.getCorner(Quad::NORTH_WEST).lat).asDegrees();
    padfTransform[2] = 0;
    padfTransform[4] = 0;
    return padfTransform;
}

PixelRegion::PixelCoordinate RawTileDataReader::geodeticToPixel(
    const Geodetic2& geo) const {
    std::array<double, 6> padfTransform = getGeoTransform();
        
    double Y = Angle<double>::fromRadians(geo.lat).asDegrees();
    double X = Angle<double>::fromRadians(geo.lon).asDegrees();

    // convert from pixel and line to geodetic coordinates
    // Xp = padfTransform[0] + P*padfTransform[1] + L*padfTransform[2];
    // Yp = padfTransform[3] + P*padfTransform[4] + L*padfTransform[5];

    // <=>
    double* a = &(padfTransform[0]);
    double* b = &(padfTransform[3]);

    // Xp = a[0] + P*a[1] + L*a[2];
    // Yp = b[0] + P*b[1] + L*b[2];

    // <=>
    double divisor = (a[2] * b[1] - a[1] * b[2]);
    ghoul_assert(divisor != 0.0, "Division by zero!");
    //ghoul_assert(a[2] != 0.0, "a2 must not be zero!");
    double P = (a[0] * b[2] - a[2] * b[0] + a[2] * Y - b[2] * X) / divisor;
    double L = (-a[0] * b[1] + a[1] * b[0] - a[1] * Y + b[1] * X) / divisor;
    // ref: https://www.wolframalpha.com/input/?i=X+%3D+a0+%2B+a1P+%2B+a2L,+Y+%3D+b0+%2B+b1P+%2B+b2L,+solve+for+P+and+L

    double Xp = a[0] + P*a[1] + L*a[2];
    double Yp = b[0] + P*b[1] + L*b[2];

    ghoul_assert(std::abs(X - Xp) < 1e-10, "inverse should yield X as before");
    ghoul_assert(std::abs(Y - Yp) < 1e-10, "inverse should yield Y as before");

    return PixelRegion::PixelCoordinate(glm::round(P), glm::round(L));
}

Geodetic2 RawTileDataReader::pixelToGeodetic(
    const PixelRegion::PixelCoordinate& p) const {
    std::array<double, 6> padfTransform = getGeoTransform();
    Geodetic2 geodetic;
    // Should be using radians and not degrees?
    geodetic.lon = padfTransform[0] + p.x * padfTransform[1] + p.y * padfTransform[2];
    geodetic.lat = padfTransform[3] + p.x * padfTransform[4] + p.y * padfTransform[5];
    return geodetic;
}

PixelRegion RawTileDataReader::highestResPixelRegion(const GeodeticPatch& geodeticPatch) const {
    Geodetic2 nwCorner = geodeticPatch.getCorner(Quad::NORTH_WEST);
    Geodetic2 swCorner = geodeticPatch.getCorner(Quad::SOUTH_EAST);
    PixelRegion::PixelCoordinate pixelStart = geodeticToPixel(nwCorner);
    PixelRegion::PixelCoordinate pixelEnd = geodeticToPixel(swCorner);
    PixelRegion region(pixelStart, pixelEnd - pixelStart);
    return region;
}

RawTile::ReadError RawTileDataReader::repeatedRasterRead(
        int rasterBand, const IODescription& fullIO, char* dataDestination,
        int depth) const
{
    RawTile::ReadError worstError = RawTile::ReadError::None;
    
    // NOTE: 
    // Ascii graphics illustrates the implementation details of this method, for one  
    // specific case. Even though the illustrated case is specific, readers can 
    // hopefully find it useful to get the general idea.

    // Make a copy of the full IO desription as we will have to modify it
    IODescription io = fullIO;

    // Example: 
    // We have an io description that defines a WRITE and a READ region.
    // In this case the READ region extends outside of the defined io.read.fullRegion,
    // meaning we will have to perform wrapping

    // io.write.region             io.read.region
    //    |                         |
    //    V                         V
    // +-------+                +-------+ 
    // |       |                |       |--------+ 
    // |       |                |       |        |
    // |       |                |       |        |
    // +-------+                +-------+        |
    //                            |              | <-- io.read.fullRegion  
    //                            |              |
    //                            +--------------+

    if (!io.read.region.isInside(io.read.fullRegion)) {
        //  Loop through each side: left, top, right, bottom
        for (int i = 0; i < 4; ++i) {

            // Example: 
            // We are currently considering the left side of the pixel region
            PixelRegion::Side side = static_cast<PixelRegion::Side>(i);
            IODescription cutoff = io.cut(side, io.read.fullRegion.edge(side));

            // Example: 
            // We cut off the left part that was outside the io.read.fullRegion, and we
            // now have an additional io description for the cut off region. 
            // Note that the cut-method used above takes care of the corresponding 
            // WRITE region for us.

            // cutoff.write.region    cutoff.read.region
            //  |  io.write.region     |  io.read.region
            //  |   |                  |   |
            //  V   V                  V   V
            // +-+-----+               +-+-----+ 
            // | |     |               | |     |--------+
            // | |     |               | |     |        |
            // | |     |               | |     |        |
            // +-+-----+               +-+-----+        |
            //                           |              | <-- io.read.fullRegion  
            //                           |              |
            //                           +--------------+

            if (cutoff.read.region.area() > 0) {
                // Wrap by repeating
                PixelRegion::Side oppositeSide = static_cast<PixelRegion::Side>((i + 2) % 4);

                cutoff.read.region.align(
                    oppositeSide, io.read.fullRegion.edge(oppositeSide));

                // Example:
                // The cut off region is wrapped to the opposite side of the region,
                // i.e. "repeated". Note that we don't want WRITE region to change, 
                // we're only wrapping the READ region.

                // cutoff.write.region   io.read.region cutoff.read.region
                //  |  io.write.region        |          |
                //  |   |                     V          V
                //  V   V                  +-----+      +-+
                // +-+-----+               |     |------| |
                // | |     |               |     |      | | 
                // | |     |               |     |      | |
                // | |     |               +-----+      +-+
                // +-+-----+               |              | <-- io.read.fullRegion  
                //                         |              |
                //                         +--------------+

                // Example:
                // The cutoff region has been repeated along one of its sides, but 
                // as we can see in this example, it still has a top part outside the
                // defined gdal region. This is handled through recursion.
                RawTile::ReadError err = repeatedRasterRead(
                    rasterBand, cutoff, dataDestination, depth + 1);

                worstError = std::max(worstError, err);
            }
        }
    }
        
    RawTile::ReadError err = rasterRead(rasterBand, io, dataDestination);

    // The return error from a repeated rasterRead is ONLY based on the main region,
    // which in the usual case will cover the main area of the patch anyway
    return err;
}

std::shared_ptr<TileMetaData> RawTileDataReader::getTileMetaData(
    std::shared_ptr<RawTile> rawTile, const PixelRegion& region) const
{
    size_t bytesPerLine = _initData.bytesPerPixel() * region.numPixels.x;

    TileMetaData* preprocessData = new TileMetaData();
    preprocessData->maxValues.resize(_initData.nRasters());
    preprocessData->minValues.resize(_initData.nRasters());
    preprocessData->hasMissingData.resize(_initData.nRasters());
        
    std::vector<float> noDataValues;
    noDataValues.resize(_initData.nRasters());

    for (size_t raster = 0; raster < _initData.nRasters(); ++raster) {
        preprocessData->maxValues[raster] = -FLT_MAX;
        preprocessData->minValues[raster] = FLT_MAX;
        preprocessData->hasMissingData[raster] = false;
        noDataValues[raster] = noDataValueAsFloat();
    }

    bool allIsMissing = true;

    for (int y = 0; y < region.numPixels.y; ++y) {
        size_t yi = (region.numPixels.y - 1 - y) * bytesPerLine;
        size_t i = 0;
        for (int x = 0; x < region.numPixels.x; ++x) {
            for (size_t raster = 0; raster < _initData.nRasters(); ++raster) {
                float noDataValue = noDataValueAsFloat();
                float val = tiledatatype::interpretFloat(
                    _initData.glType(),
                    &(rawTile->imageData[yi + i])
                );
                if (val != noDataValue &&
                    val == val)
                {
                    preprocessData->maxValues[raster] = std::max(
                        val,
                        preprocessData->maxValues[raster]
                    );
                    preprocessData->minValues[raster] = std::min(
                        val,
                        preprocessData->minValues[raster]
                    );
                    allIsMissing = false;
                }
                else {
                    preprocessData->hasMissingData[raster] = true;
                    float& floatToRewrite = reinterpret_cast<float&>(rawTile->imageData[yi + i]);
                    floatToRewrite = -FLT_MAX;
                }
                i += _initData.bytesPerDatum();
            }
        }
    }
  
    if (allIsMissing) {
        rawTile->error = RawTile::ReadError::Failure;
    }

    return std::shared_ptr<TileMetaData>(preprocessData);
}

float RawTileDataReader::depthOffset() const {
    return 0.0f;
}

float RawTileDataReader::depthScale() const {
    return 1.0f;
}

TileDepthTransform RawTileDataReader::calculateTileDepthTransform() {
    bool isFloat =
        (_initData.glType() == GL_HALF_FLOAT || _initData.glType() == GL_FLOAT || _initData.glType() == GL_DOUBLE);
    double maximumValue =
        isFloat ? 1.0 : tiledatatype::getMaximumValue(_initData.glType());

    TileDepthTransform transform;
    transform.depthOffset = depthOffset();
    transform.depthScale = depthScale() * maximumValue;
    return transform;
}

RawTile::ReadError RawTileDataReader::postProcessErrorCheck(
    std::shared_ptr<const RawTile> rawTile) const
{
    float missingDataValue = noDataValueAsFloat();

    bool hasMissingData = false;
    
    for (size_t c = 0; c < _initData.nRasters(); c++) {
        hasMissingData |= rawTile->tileMetaData->maxValues[c] == missingDataValue;
    }
    
    bool onHighLevel = rawTile->tileIndex.level > 6;
    if (hasMissingData && onHighLevel) {
        return RawTile::ReadError::Fatal;
    }
    return RawTile::ReadError::None;
}

} // namespace openspace::globebrowsing
