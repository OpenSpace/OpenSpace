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

#include <modules/globebrowsing/tile/rawtiledatareader/rawtiledatareader.h>

#include <modules/globebrowsing/geometry/angle.h>
#include <modules/globebrowsing/geometry/geodetic2.h>
#include <modules/globebrowsing/geometry/geodeticpatch.h>
#include <modules/globebrowsing/tile/rawtiledatareader/iodescription.h>
#include <modules/globebrowsing/tile/rawtiledatareader/tiledatatype.h>
#include <modules/globebrowsing/tile/tilemetadata.h>

namespace openspace::globebrowsing {

RawTileDataReader::RawTileDataReader(const TileTextureInitData& initData,
                                     PerformPreprocessing preprocess)
    : _initData(initData)
    , _preprocess(preprocess)
{}

std::shared_ptr<RawTile> RawTileDataReader::defaultTileData() const {
    return std::make_shared<RawTile>(RawTile::createDefault(_initData));
}

std::shared_ptr<RawTile> RawTileDataReader::readTileData(TileIndex tileIndex,
                                                         char* dataDestination,
                                                     char* pboMappedDataDestination) const
{
    IODescription io = ioDescription(tileIndex);
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
    rawTile->tileIndex = std::move(tileIndex);

    rawTile->textureInitData = std::make_shared<TileTextureInitData>(_initData);

    if (_preprocess) {
        rawTile->tileMetaData = getTileMetaData(rawTile, io.write.region);
        rawTile->error = std::max(rawTile->error, postProcessErrorCheck(rawTile));
    }

    return rawTile;
}

void RawTileDataReader::readImageData(IODescription& io, RawTile::ReadError& worstError,
                                      char* imageDataDest) const
{
    io = adjustIODescription(io);

    // Only read the minimum number of rasters
    int nRastersToRead = std::min(
        dataSourceNumRasters(),
        static_cast<int>(_initData.nRasters())
    );

    switch (_initData.ghoulTextureFormat()) {
        case ghoul::opengl::Texture::Format::Red: {
            char* dest = imageDataDest;
            const RawTile::ReadError err = repeatedRasterRead(1, io, dest);
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
                    char* dest = imageDataDest + (i * _initData.bytesPerDatum());
                    const RawTile::ReadError err = repeatedRasterRead(1, io, dest);
                    worstError = std::max(worstError, err);
                }
            }
            else if (nRastersToRead == 2) { // Grayscale + alpha
                for (int i = 0; i < 3; i++) {
                    // The final destination pointer is offsetted by one datum byte size
                    // for every raster (or data channel, i.e. R in RGB)
                    char* dest = imageDataDest + (i * _initData.bytesPerDatum());
                    const RawTile::ReadError err = repeatedRasterRead(1, io, dest);
                    worstError = std::max(worstError, err);
                }
                // Last read is the alpha channel
                char* dest = imageDataDest + (3 * _initData.bytesPerDatum());
                const RawTile::ReadError err = repeatedRasterRead(2, io, dest);
                worstError = std::max(worstError, err);
            }
            else { // Three or more rasters
                for (int i = 0; i < nRastersToRead; i++) {
                    // The final destination pointer is offsetted by one datum byte size
                    // for every raster (or data channel, i.e. R in RGB)
                    char* dest = imageDataDest + (i * _initData.bytesPerDatum());
                    const RawTile::ReadError err = repeatedRasterRead(i + 1, io, dest);
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
                    char* dest = imageDataDest + (i * _initData.bytesPerDatum());
                    const RawTile::ReadError err = repeatedRasterRead(1, io, dest);
                    worstError = std::max(worstError, err);
                }
            }
            else if (nRastersToRead == 2) { // Grayscale + alpha
                for (int i = 0; i < 3; i++) {
                    // The final destination pointer is offsetted by one datum byte size
                    // for every raster (or data channel, i.e. R in RGB)
                    char* dest = imageDataDest + (i * _initData.bytesPerDatum());
                    const RawTile::ReadError err = repeatedRasterRead(1, io, dest);
                    worstError = std::max(worstError, err);
                }
                // Last read is the alpha channel
                char* dest = imageDataDest + (3 * _initData.bytesPerDatum());
                const RawTile::ReadError err = repeatedRasterRead(2, io, dest);
                worstError = std::max(worstError, err);
            }
            else { // Three or more rasters
                for (int i = 0; i < 3 && i < nRastersToRead; i++) {
                    // The final destination pointer is offsetted by one datum byte size
                    // for every raster (or data channel, i.e. R in RGB)
                    char* dest = imageDataDest + (i * _initData.bytesPerDatum());
                    const RawTile::ReadError err = repeatedRasterRead(3 - i, io, dest);
                    worstError = std::max(worstError, err);
                }
            }
            if (nRastersToRead > 3) { // Alpha channel exists
                // Last read is the alpha channel
                char* dest = imageDataDest + (3 * _initData.bytesPerDatum());
                const RawTile::ReadError err = repeatedRasterRead(4, io, dest);
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

IODescription RawTileDataReader::ioDescription(const TileIndex& tileIndex) const {
    IODescription io;
    io.read.region = highestResPixelRegion(tileIndex);

    // write region starts in origin
    io.write.region.start = PixelRegion::PixelCoordinate(0, 0);
    io.write.region.numPixels = PixelRegion::PixelCoordinate(
        _initData.dimensions().x, _initData.dimensions().y);

    io.read.overview = 0;
    io.read.fullRegion = fullPixelRegion();
    // For correct sampling in dataset, we need to pad the texture tile

    const PixelRegion padding = PixelRegion(
        _initData.tilePixelStartOffset(),
        _initData.tilePixelSizeDifference()
    );

    PixelRegion scaledPadding = padding;
    const double scale = static_cast<double>(io.read.region.numPixels.x) /
                         static_cast<double>(io.write.region.numPixels.x);
    scaledPadding.numPixels *= scale;
    scaledPadding.start *= scale;

    io.read.region.pad(scaledPadding);
    //io.write.region.pad(padding);
    //io.write.region.start = PixelRegion::PixelCoordinate(0, 0);

    io.write.bytesPerLine = _initData.bytesPerLine();
    io.write.totalNumBytes = _initData.totalNumBytes();

    ghoul_assert(
        io.write.region.numPixels.x == io.write.region.numPixels.y,
        "Write region must be square"
    );
    ghoul_assert(
        io.write.region.numPixels.x == _initData.dimensions().x,
        "Write region must match tile it writes to."
    );

    return io;
}

const TileDepthTransform& RawTileDataReader::depthTransform() const {
    return _depthTransform;
}

const TileTextureInitData& RawTileDataReader::tileTextureInitData() const {
    return _initData;
}

const PixelRegion::PixelRange RawTileDataReader::fullPixelSize() const {
    return glm::uvec2(geodeticToPixel(Geodetic2(90, 180)));
}

PixelRegion RawTileDataReader::fullPixelRegion() const {
    return { { 0, 0 }, { rasterXSize(), rasterYSize() } };
}

std::array<double, 6> RawTileDataReader::geoTransform() const {
    GeodeticPatch cov(
        Geodetic2(0,0),
        Geodetic2(glm::half_pi<double>(), glm::pi<double>())
    );
    return {
        Angle<double>::fromRadians(cov.corner(Quad::NORTH_WEST).lon).asDegrees(),
        Angle<double>::fromRadians(cov.size().lon).asDegrees() / rasterXSize(),
        0.0,
        Angle<double>::fromRadians(cov.corner(Quad::NORTH_WEST).lat).asDegrees(),
        0.0,
        -Angle<double>::fromRadians(cov.size().lat).asDegrees() / rasterYSize()
    };
}

PixelRegion::PixelCoordinate RawTileDataReader::geodeticToPixel(
                                                               const Geodetic2& geo) const
{
    const std::array<double, 6>& t = geoTransform();

    const double Y = Angle<double>::fromRadians(geo.lat).asDegrees();
    const double X = Angle<double>::fromRadians(geo.lon).asDegrees();

    const double divisor = t[2] * t[4] - t[1] * t[5];
    ghoul_assert(divisor != 0.0, "Division by zero!");

    const double P = (t[0] * t[5] - t[2] * t[3] + t[2] * Y - t[5] * X) / divisor;
    const double L = (-t[0] * t[4] + t[1] * t[3] - t[1] * Y + t[4] * X) / divisor;
    // ref: https://www.wolframalpha.com/input/?i=X+%3D+a0+%2B+a1P+%2B+a2L,
    //      +Y+%3D+b0+%2B+b1P+%2B+b2L,+solve+for+P+and+L

    [[ maybe_unused ]] const double Xp = t[0] + P * t[1] + L * t[2];
    [[ maybe_unused ]] const double Yp = t[3] + P * t[4] + L * t[5];
    ghoul_assert(std::abs(X - Xp) < 1e-10, "inverse should yield X as before");
    ghoul_assert(std::abs(Y - Yp) < 1e-10, "inverse should yield Y as before");

    return PixelRegion::PixelCoordinate(glm::round(P), glm::round(L));
}

Geodetic2 RawTileDataReader::pixelToGeodetic(const PixelRegion::PixelCoordinate& p) const
{
    std::array<double, 6> padfTransform = geoTransform();
    return {
        padfTransform[0] + p.x * padfTransform[1] + p.y * padfTransform[2],
        padfTransform[3] + p.x * padfTransform[4] + p.y * padfTransform[5]
    };
}

PixelRegion RawTileDataReader::highestResPixelRegion(
                                                 const GeodeticPatch& geodeticPatch) const
{
    const Geodetic2 nwCorner = geodeticPatch.corner(Quad::NORTH_WEST);
    const Geodetic2 swCorner = geodeticPatch.corner(Quad::SOUTH_EAST);
    const PixelRegion::PixelCoordinate pixelStart = geodeticToPixel(nwCorner);
    const PixelRegion::PixelCoordinate pixelEnd = geodeticToPixel(swCorner);
    PixelRegion region(pixelStart, pixelEnd - pixelStart);
    return region;
}

RawTile::ReadError RawTileDataReader::repeatedRasterRead(int rasterBand,
                                                         const IODescription& fullIO,
                                                         char* dataDestination,
                                                         int depth) const
{

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

    RawTile::ReadError worstError = RawTile::ReadError::None;
    if (!io.read.region.isInside(io.read.fullRegion)) {
        //  Loop through each side: left, top, right, bottom
        for (int i = 0; i < 4; ++i) {
            // Example:
            // We are currently considering the left side of the pixel region
            const PixelRegion::Side side = static_cast<PixelRegion::Side>(i);
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
                PixelRegion::Side oppositeSide = static_cast<PixelRegion::Side>(
                    (i + 2) % 4
                );

                cutoff.read.region.align(
                    oppositeSide,
                    io.read.fullRegion.edge(oppositeSide)
                );

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
                const RawTile::ReadError err = repeatedRasterRead(
                    rasterBand,
                    cutoff,
                    dataDestination,
                    depth + 1
                );

                worstError = std::max(worstError, err);
            }
        }
    }

    const RawTile::ReadError err = rasterRead(rasterBand, io, dataDestination);

    // The return error from a repeated rasterRead is ONLY based on the main region,
    // which in the usual case will cover the main area of the patch anyway
    return err;
}

std::shared_ptr<TileMetaData> RawTileDataReader::getTileMetaData(
                                                         std::shared_ptr<RawTile> rawTile,
                                                          const PixelRegion& region) const
{
    const size_t bytesPerLine = _initData.bytesPerPixel() * region.numPixels.x;

    std::shared_ptr<TileMetaData> preprocessData = std::make_shared<TileMetaData>();
    preprocessData->maxValues.resize(_initData.nRasters());
    preprocessData->minValues.resize(_initData.nRasters());
    preprocessData->hasMissingData.resize(_initData.nRasters());

    std::vector<float> noDataValues(_initData.nRasters());
    for (size_t raster = 0; raster < _initData.nRasters(); ++raster) {
        preprocessData->maxValues[raster] = -FLT_MAX;
        preprocessData->minValues[raster] = FLT_MAX;
        preprocessData->hasMissingData[raster] = false;
        noDataValues[raster] = noDataValueAsFloat();
    }

    bool allIsMissing = true;
    for (int y = 0; y < region.numPixels.y; ++y) {
        const size_t yi = (region.numPixels.y - 1 - y) * bytesPerLine;
        size_t i = 0;
        for (int x = 0; x < region.numPixels.x; ++x) {
            for (size_t raster = 0; raster < _initData.nRasters(); ++raster) {
                const float noDataValue = noDataValueAsFloat();
                const float val = tiledatatype::interpretFloat(
                    _initData.glType(),
                    &(rawTile->imageData[yi + i])
                );
                if (val != noDataValue && val == val) {
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
                    float& floatToRewrite = reinterpret_cast<float&>(
                        rawTile->imageData[yi + i]
                    );
                    floatToRewrite = -std::numeric_limits<float>::max();
                }
                i += _initData.bytesPerDatum();
            }
        }
    }

    if (allIsMissing) {
        rawTile->error = RawTile::ReadError::Failure;
    }

    return preprocessData;
}

float RawTileDataReader::depthOffset() const {
    return 0.f;
}

float RawTileDataReader::depthScale() const {
    return 1.f;
}

TileDepthTransform RawTileDataReader::calculateTileDepthTransform() {
    const bool isFloat =
        (_initData.glType() == GL_HALF_FLOAT ||
         _initData.glType() == GL_FLOAT ||
         _initData.glType() == GL_DOUBLE);

    const double maximumValue = isFloat ?
        1.f :
        tiledatatype::getMaximumValue(_initData.glType());

    return {
        static_cast<float>(depthScale() * maximumValue),
        depthOffset()
    };
}

RawTile::ReadError RawTileDataReader::postProcessErrorCheck(
                                             std::shared_ptr<const RawTile> rawTile) const
{
    const float missingDataValue = noDataValueAsFloat();

    bool hasMissingData = false;
    for (size_t c = 0; c < _initData.nRasters(); c++) {
        hasMissingData |= rawTile->tileMetaData->maxValues[c] == missingDataValue;
    }

    const bool onHighLevel = rawTile->tileIndex.level > 6;
    if (hasMissingData && onHighLevel) {
        return RawTile::ReadError::Fatal;
    }
    return RawTile::ReadError::None;
}

} // namespace openspace::globebrowsing
