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
#ifdef GLOBEBROWSING_USE_GDAL

#include <modules/globebrowsing/tile/rawtiledatareader/gdalrawtiledatareader.h>

#include <modules/globebrowsing/geometry/geodetic2.h>
#include <modules/globebrowsing/geometry/geodeticpatch.h>
#include <modules/globebrowsing/geometry/angle.h>
#include <modules/globebrowsing/tile/rawtiledatareader/tiledatatype.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h> // abspath
#include <ghoul/filesystem/file.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/dictionary.h>

#include <ogr_featurestyle.h>
#include <ogr_spatialref.h>
#include <cpl_virtualmem.h>

#include <gdal_priv.h>

#include <algorithm>

namespace {
    const std::string _loggerCat = "GdalRawTileDataReader";
}

namespace openspace {
namespace globebrowsing {

std::ostream& operator<<(std::ostream& os, const PixelRegion& pr) {
    return os << pr.start.x << ", " << pr.start.y << " with size " << pr.numPixels.x <<
        ", " << pr.numPixels.y;
}

GdalRawTileDataReader::GdalRawTileDataReader(const std::string& filePath,
        const TileTextureInitData& initData,
        const std::string& baseDirectory,
        RawTileDataReader::PerformPreprocessing preprocess)
    : RawTileDataReader(initData, preprocess)
    , _dataset(nullptr)
{
    _initDirectory = baseDirectory.empty() ? CPLGetCurrentDir() : baseDirectory;
    _datasetFilePath = filePath;

    { // Aquire lock
        std::lock_guard<std::mutex> lockGuard(_datasetLock);
        initialize();
    }
}

GdalRawTileDataReader::~GdalRawTileDataReader() {
    std::lock_guard<std::mutex> lockGuard(_datasetLock);
    if (_dataset != nullptr) {
        GDALClose(_dataset);
        _dataset = nullptr;
    }
}

void GdalRawTileDataReader::reset() {
    std::lock_guard<std::mutex> lockGuard(_datasetLock);
    _cached._maxLevel = -1;
    if (_dataset != nullptr) {
        GDALClose(_dataset);
        _dataset = nullptr;
    }
    initialize();
}

int GdalRawTileDataReader::maxChunkLevel() const {
    return _cached._maxLevel;
}

float GdalRawTileDataReader::noDataValueAsFloat() const {
    return _gdalDatasetMetaDataCached.noDataValue;
}

int GdalRawTileDataReader::rasterXSize() const {
    return _gdalDatasetMetaDataCached.rasterXSize;
}

int GdalRawTileDataReader::rasterYSize() const {
    return _gdalDatasetMetaDataCached.rasterYSize;
}

float GdalRawTileDataReader::depthOffset() const {
    return _gdalDatasetMetaDataCached.offset;
}

float GdalRawTileDataReader::depthScale() const {
    return _gdalDatasetMetaDataCached.scale;
}

std::array<double, 6> GdalRawTileDataReader::getGeoTransform() const {
    return _gdalDatasetMetaDataCached.padfTransform;
}

IODescription GdalRawTileDataReader::getIODescription(const TileIndex& tileIndex) const {
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

    ghoul_assert(io.write.region.numPixels.x == io.write.region.numPixels.y, "");
    ghoul_assert(io.write.region.numPixels.x == _initData.dimensionsWithPadding().x, "");

    return io;
}

void GdalRawTileDataReader::initialize() {
    _dataset = openGdalDataset(_datasetFilePath);

    // Assume all raster bands have the same data type
    _gdalDatasetMetaDataCached.rasterCount = _dataset->GetRasterCount();
    _gdalDatasetMetaDataCached.scale = _dataset->GetRasterBand(1)->GetScale();
    _gdalDatasetMetaDataCached.offset = _dataset->GetRasterBand(1)->GetOffset();
    _gdalDatasetMetaDataCached.rasterXSize = _dataset->GetRasterXSize();
    _gdalDatasetMetaDataCached.rasterYSize = _dataset->GetRasterYSize();
    _gdalDatasetMetaDataCached.noDataValue = _dataset->GetRasterBand(1)->GetNoDataValue();
    _gdalDatasetMetaDataCached.dataType = tiledatatype::getGdalDataType(_initData.glType());
    
    CPLErr err = _dataset->GetGeoTransform(&_gdalDatasetMetaDataCached.padfTransform[0]);
    if (err == CE_Failure) {
        _gdalDatasetMetaDataCached.padfTransform = RawTileDataReader::getGeoTransform();
    }

    _depthTransform = calculateTileDepthTransform();
    _cached._tileLevelDifference =
        calculateTileLevelDifference(_initData.dimensionsWithoutPadding().x);

    int numOverviews = _dataset->GetRasterBand(1)->GetOverviewCount();
    _cached._maxLevel = -_cached._tileLevelDifference;
    if (numOverviews > 0) {
        _cached._maxLevel += numOverviews - 1;
    }
}

void GdalRawTileDataReader::readImageData(
    IODescription& io, RawTile::ReadError& worstError, char* imageDataDest) const {
  
    // Only read the minimum number of rasters
    int nRastersToRead = std::min(_gdalDatasetMetaDataCached.rasterCount,
        static_cast<int>(_initData.nRasters()));

    switch (_initData.ghoulTextureFormat()) {
        case ghoul::opengl::Texture::Format::Red:
        case ghoul::opengl::Texture::Format::RG:
        case ghoul::opengl::Texture::Format::RGB:
        case ghoul::opengl::Texture::Format::RGBA: {
            // Read the data (each rasterband is a separate channel)
            for (int i = 0; i < nRastersToRead; i++) {
                // The final destination pointer is offsetted by one datum byte size
                // for every raster (or data channel, i.e. R in RGB)
                char* dataDestination = imageDataDest + (i * _initData.bytesPerDatum());

                RawTile::ReadError err = repeatedRasterRead(i + 1, io, dataDestination);

                // CE_None = 0, CE_Debug = 1, CE_Warning = 2, CE_Failure = 3, CE_Fatal = 4
                worstError = std::max(worstError, err);
            }
            break;
        }
        case ghoul::opengl::Texture::Format::BGR: {
            // Read the data (each rasterband is a separate channel)
            for (int i = 0; i < 3 && i < nRastersToRead; i++) {
                // The final destination pointer is offsetted by one datum byte size
                // for every raster (or data channel, i.e. R in RGB)
                char* dataDestination = imageDataDest + (i * _initData.bytesPerDatum());

                RawTile::ReadError err = repeatedRasterRead(3 - i, io, dataDestination);

                // CE_None = 0, CE_Debug = 1, CE_Warning = 2, CE_Failure = 3, CE_Fatal = 4
                worstError = std::max(worstError, err);
            }
            break;
        }
        case ghoul::opengl::Texture::Format::BGRA: {
            for (int i = 0; i < 3 && i < nRastersToRead; i++) {
                // The final destination pointer is offsetted by one datum byte size
                // for every raster (or data channel, i.e. R in RGB)
                char* dataDestination = imageDataDest + (i * _initData.bytesPerDatum());

                RawTile::ReadError err = repeatedRasterRead(3 - i, io, dataDestination);

                // CE_None = 0, CE_Debug = 1, CE_Warning = 2, CE_Failure = 3, CE_Fatal = 4
                worstError = std::max(worstError, err);
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

RawTile::ReadError GdalRawTileDataReader::rasterRead(
    int rasterBand, const IODescription& io, char* dataDestination) const
{
    ghoul_assert(io.read.region.isInside(io.read.fullRegion), "write region of bounds!");
    ghoul_assert(
        io.write.region.start.x >= 0 && io.write.region.start.y >= 0,
        "Invalid write region"
    );

    PixelRegion::PixelCoordinate end = io.write.region.end();
    size_t largestIndex =
        (end.y - 1) * io.write.bytesPerLine + (end.x - 1) * _initData.bytesPerPixel();
    ghoul_assert(largestIndex <= io.write.totalNumBytes, "Invalid write region");

    char* dataDest = dataDestination;

    // GDAL reads pixels top to bottom, but we want our pixels bottom to top.
    // Therefore, we increment the destination pointer to the last line on in the 
    // buffer, and the we specify in the rasterIO call that we want negative line 
    // spacing. Doing this compensates the flipped Y axis
    dataDest += (io.write.totalNumBytes - io.write.bytesPerLine);

    // handle requested write region. Note -= since flipped y axis
    dataDest -= io.write.region.start.y * io.write.bytesPerLine;
    dataDest += io.write.region.start.x * _initData.bytesPerPixel();
  
    GDALRasterBand* gdalRasterBand = _dataset->GetRasterBand(rasterBand);
    CPLErr readError = CE_Failure;
    readError = gdalRasterBand->RasterIO(
        GF_Read,
        io.read.region.start.x,         // Begin read x
        io.read.region.start.y,         // Begin read y
        io.read.region.numPixels.x,     // width to read x
        io.read.region.numPixels.y,     // width to read y
        dataDest,                       // Where to put data
        io.write.region.numPixels.x,    // width to write x in destination
        io.write.region.numPixels.y,    // width to write y in destination
        _gdalDatasetMetaDataCached.dataType,                      // Type
        _initData.bytesPerPixel(),      // Pixel spacing
        -io.write.bytesPerLine          // Line spacing
    );
  
    // Convert error to RawTile::ReadError
    RawTile::ReadError error;
    switch (readError) {
        case CE_None: error = RawTile::ReadError::None; break;
        case CE_Debug: error = RawTile::ReadError::Debug; break;
        case CE_Warning: error = RawTile::ReadError::Warning; break;
        case CE_Failure: error = RawTile::ReadError::Failure; break;
        case CE_Fatal: error = RawTile::ReadError::Fatal; break;
        default: error = RawTile::ReadError::Failure; break;
    }
    return error;
}

GDALDataset* GdalRawTileDataReader::openGdalDataset(const std::string& filePath) {
    GDALDataset* dataset = static_cast<GDALDataset*>(
        GDALOpen(filePath.c_str(), GA_ReadOnly));
    if (!dataset) {
        using namespace ghoul::filesystem;
        std::string correctedPath = FileSystem::ref().pathByAppendingComponent(
            _initDirectory, filePath
        );

        dataset = static_cast<GDALDataset*>(GDALOpen(correctedPath.c_str(), GA_ReadOnly));
        if (!dataset) {
            throw ghoul::RuntimeError("Failed to load dataset:\n" + filePath);
        }
    }
    return dataset;
}

int GdalRawTileDataReader::calculateTileLevelDifference(int minimumPixelSize) const {
    GDALRasterBand* firstBand = _dataset->GetRasterBand(1);
    GDALRasterBand* maxOverview;
    int numOverviews = firstBand->GetOverviewCount();
    int sizeLevel0;
    if (numOverviews <= 0) { // No overviews. Use first band.
        maxOverview = firstBand;
    }
    else { // Pick the highest overview.
        maxOverview = firstBand->GetOverview(numOverviews - 1);
    }
    sizeLevel0 = maxOverview->GetXSize();
    double diff = log2(minimumPixelSize) - log2(sizeLevel0);
    return diff;
}

} // namespace globebrowsing
} // namespace openspace

#endif // GLOBEBROWSING_USE_GDAL
