/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___GDAL_RAW_TILE_DATA_READER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___GDAL_RAW_TILE_DATA_READER___H__

#include <modules/globebrowsing/src/basictypes.h>
#include <modules/globebrowsing/src/rawtile.h>
#include <modules/globebrowsing/src/tiletextureinitdata.h>
#include <modules/globebrowsing/src/tilecacheproperties.h>
#include <ghoul/misc/boolean.h>
#include <string>
#include <mutex>
#include <gdal.h>

class GDALDataset;
class GDALRasterBand;

namespace openspace::globebrowsing {

class GeodeticPatch;

class RawTileDataReader {
public:
    BooleanType(PerformPreprocessing);

    /**
     * Opens a GDALDataset in readonly mode and calculates meta data required for
     * reading tile using a TileIndex.
     *
     * \param filePath the path to a specific file GDAL can read
     * \param initData information about the textures that will be creatd by this reader
     * \param cacheProperties contains settings about whether the reader should
     *        utilize cache
     * \param preprocess whether the loaded data should be calculate meta data about the
     *        dataset
     */
    RawTileDataReader(std::string filePath, TileTextureInitData initData,
        TileCacheProperties cacheProperties,
        PerformPreprocessing preprocess = PerformPreprocessing::No);
    ~RawTileDataReader();

    void reset();
    int maxChunkLevel() const;
    float noDataValueAsFloat() const;

    RawTile readTileData(TileIndex tileIndex) const;
    const TileDepthTransform& depthTransform() const;
    glm::ivec2 fullPixelSize() const;

private:
    std::optional<std::string> mrfCache();

    void initialize();

    RawTile::ReadError rasterRead(int rasterBand, const IODescription& io,
        char* dataDestination) const;

    void readImageData(IODescription& io, RawTile::ReadError& worstError,
        char* imageDataDest) const;

    IODescription ioDescription(const TileIndex& tileIndex) const;

    TileMetaData tileMetaData(RawTile& rawTile, const PixelRegion& region) const;

    const std::string _datasetFilePath;
    GDALDataset* _dataset = nullptr;

    // Dataset parameters
    int _rasterCount;
    int _rasterXSize;
    int _rasterYSize;
    float _noDataValue;
    std::array<double, 6> _padfTransform;
    GDALDataType _dataType;
    int _maxChunkLevel = -1;

    const TileTextureInitData _initData;
    const TileCacheProperties _cacheProperties;
    const PerformPreprocessing _preprocess;
    TileDepthTransform _depthTransform = { .scale = 0.f, .offset = 0.f };

    mutable std::mutex _datasetLock;
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___GDAL_RAW_TILE_DATA_READER___H__
