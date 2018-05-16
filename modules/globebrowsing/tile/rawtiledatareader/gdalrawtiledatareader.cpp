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

#ifdef GLOBEBROWSING_USE_GDAL

#include <modules/globebrowsing/tile/rawtiledatareader/gdalrawtiledatareader.h>

#include <modules/globebrowsing/tile/rawtiledatareader/tiledatatype.h>
#include <ghoul/misc/exception.h>

#ifdef _MSC_VER
#pragma warning (push)
 // CPL throws warning about missing DLL interface
#pragma warning (disable : 4251)
#endif // _MSC_VER

#include <ogr_featurestyle.h>
#include <ogr_spatialref.h>
#include <cpl_virtualmem.h>

#include <gdal_priv.h>

#ifdef _MSC_VER
#pragma warning (pop)
#endif // _MSC_VER

#include <algorithm>

namespace openspace::globebrowsing {

std::ostream& operator<<(std::ostream& os, const PixelRegion& pr) {
    return os << pr.start.x << ", " << pr.start.y << " with size " << pr.numPixels.x <<
        ", " << pr.numPixels.y;
}

GdalRawTileDataReader::GdalRawTileDataReader(const std::string& filePath,
                                             const TileTextureInitData& initData,
                                       RawTileDataReader::PerformPreprocessing preprocess)
    : RawTileDataReader(initData, preprocess)
{
    _datasetFilePath = filePath;

    {
        std::lock_guard<std::mutex> lockGuard(_datasetLock);
        initialize();
    }
}

GdalRawTileDataReader::~GdalRawTileDataReader() {
    std::lock_guard<std::mutex> lockGuard(_datasetLock);
    if (_dataset) {
        GDALClose(_dataset);
        _dataset = nullptr;
    }
}

void GdalRawTileDataReader::reset() {
    std::lock_guard<std::mutex> lockGuard(_datasetLock);
    _cached._maxLevel = -1;
    if (_dataset) {
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

int GdalRawTileDataReader::dataSourceNumRasters() const {
    return _gdalDatasetMetaDataCached.rasterCount;
}

float GdalRawTileDataReader::depthOffset() const {
    return _gdalDatasetMetaDataCached.offset;
}

float GdalRawTileDataReader::depthScale() const {
    return _gdalDatasetMetaDataCached.scale;
}

std::array<double, 6> GdalRawTileDataReader::geoTransform() const {
    return _gdalDatasetMetaDataCached.padfTransform;
}

void GdalRawTileDataReader::initialize() {
    if (_datasetFilePath.empty()) {
        throw ghoul::RuntimeError("File path must not be empty");
    }
    _dataset = openGdalDataset(_datasetFilePath);
    if (!_dataset) {
        throw ghoul::RuntimeError("Failed to load dataset: " + _datasetFilePath);
    }

    // Assume all raster bands have the same data type
    _gdalDatasetMetaDataCached.rasterCount = _dataset->GetRasterCount();
    _gdalDatasetMetaDataCached.scale = static_cast<float>(
        _dataset->GetRasterBand(1)->GetScale()
    );
    _gdalDatasetMetaDataCached.offset = static_cast<float>(
        _dataset->GetRasterBand(1)->GetOffset()
    );
    _gdalDatasetMetaDataCached.rasterXSize = _dataset->GetRasterXSize();
    _gdalDatasetMetaDataCached.rasterYSize = _dataset->GetRasterYSize();
    _gdalDatasetMetaDataCached.noDataValue = static_cast<float>(
        _dataset->GetRasterBand(1)->GetNoDataValue()
    );
    _gdalDatasetMetaDataCached.dataType = tiledatatype::getGdalDataType(
        _initData.glType()
    );

    CPLErr err = _dataset->GetGeoTransform(&_gdalDatasetMetaDataCached.padfTransform[0]);
    if (err == CE_Failure) {
        _gdalDatasetMetaDataCached.padfTransform = RawTileDataReader::geoTransform();
    }

    _depthTransform = calculateTileDepthTransform();
    _cached._tileLevelDifference = calculateTileLevelDifference(
        _initData.dimensions().x
    );

    const int numOverviews = _dataset->GetRasterBand(1)->GetOverviewCount();
    _cached._maxLevel = static_cast<int>(-_cached._tileLevelDifference);
    if (numOverviews > 0) {
        _cached._maxLevel += numOverviews - 1;
    }
    _cached._maxLevel = std::max(_cached._maxLevel, 2);
}

RawTile::ReadError GdalRawTileDataReader::rasterRead(int rasterBand,
                                                     const IODescription& io,
                                                     char* dataDestination) const
{
    ghoul_assert(io.read.region.isInside(io.read.fullRegion), "write region of bounds!");
    ghoul_assert(
        io.write.region.start.x >= 0 && io.write.region.start.y >= 0,
        "Invalid write region"
    );

    const PixelRegion::PixelCoordinate end = io.write.region.end();
    [[maybe_unused]] const size_t largestIndex =
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
        _gdalDatasetMetaDataCached.dataType,         // Type
        static_cast<int>(_initData.bytesPerPixel()), // Pixel spacing
        -static_cast<int>(io.write.bytesPerLine)     // Line spacing
    );

    // Convert error to RawTile::ReadError
    switch (readError) {
        case CE_None:    return RawTile::ReadError::None;
        case CE_Debug:   return RawTile::ReadError::Debug;
        case CE_Warning: return RawTile::ReadError::Warning;
        case CE_Failure: return RawTile::ReadError::Failure;
        case CE_Fatal:   return RawTile::ReadError::Fatal;
        default:         return RawTile::ReadError::Failure;
    }
}

GDALDataset* GdalRawTileDataReader::openGdalDataset(const std::string& filePath) {
    return static_cast<GDALDataset*>(GDALOpen(filePath.c_str(), GA_ReadOnly));
}

int GdalRawTileDataReader::calculateTileLevelDifference(int minimumPixelSize) const {
    GDALRasterBand* firstBand = _dataset->GetRasterBand(1);
    GDALRasterBand* maxOverview;
    int numOverviews = firstBand->GetOverviewCount();
    if (numOverviews <= 0) { // No overviews. Use first band.
        maxOverview = firstBand;
    }
    else { // Pick the highest overview.
        maxOverview = firstBand->GetOverview(numOverviews - 1);
    }
    const int sizeLevel0 = maxOverview->GetXSize();
    const double diff = log2(minimumPixelSize) - log2(sizeLevel0);
    return static_cast<int>(diff);
}

} // namespace openspace::globebrowsing

#endif // GLOBEBROWSING_USE_GDAL
