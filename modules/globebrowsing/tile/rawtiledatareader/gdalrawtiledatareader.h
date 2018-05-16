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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___GDAL_RAW_TILE_DATA_READER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___GDAL_RAW_TILE_DATA_READER___H__

#ifdef GLOBEBROWSING_USE_GDAL

#include <modules/globebrowsing/tile/rawtiledatareader/rawtiledatareader.h>

#include <modules/globebrowsing/tile/rawtiledatareader/iodescription.h>
#include <string>
#include <mutex>
#include <gdal.h>

class GDALDataset;
class GDALRasterBand;

namespace openspace::globebrowsing {

class GeodeticPatch;

class GdalRawTileDataReader : public RawTileDataReader {
public:
    /**
    * Opens a GDALDataset in readonly mode and calculates meta data required for
    * reading tile using a TileIndex.
    *
    * \param filePath, a path to a specific file GDAL can read
    * \param config, Configuration used for initialization
    * \param baseDirectory, the base directory to use in future loading operations
    */
    GdalRawTileDataReader(const std::string& filePath,
        const TileTextureInitData& initData,
        RawTileDataReader::PerformPreprocessing preprocess =
            RawTileDataReader::PerformPreprocessing::No);

    virtual ~GdalRawTileDataReader() override;

    // Public virtual function overloading
    virtual void reset() override;
    virtual int maxChunkLevel() const override;
    virtual float noDataValueAsFloat() const override;
    virtual int rasterXSize() const override;
    virtual int rasterYSize() const override;
    virtual int dataSourceNumRasters() const override;
    virtual float depthOffset() const override;
    virtual float depthScale() const override;

protected:

    /**
     * Returns the geo transform from raster space to projection coordinates as defined
     * by GDAL.
     * If the transform is not available, the function returns a transform to map
     * the pixel coordinates to cover the whole geodetic lat long space.
     */
    virtual std::array<double, 6> geoTransform() const override;

private:
    // Private virtual function overloading
    virtual void initialize() override;
    virtual RawTile::ReadError rasterRead(int rasterBand, const IODescription& io,
                                          char* dst) const override;

    // GDAL Helper methods
    GDALDataset* openGdalDataset(const std::string& filePath);

    /**
     * Use as a helper function when determining the maximum tile level. This function
     * returns the negated number of overviews requred to downscale the highest overview
     * dataset so that it fits within minimumPixelSize pixels in the x-dimension.
     */
    int calculateTileLevelDifference(int minimumPixelSize) const;

    std::string _datasetFilePath;

    GDALDataset* _dataset = nullptr;

    struct GdalDatasetMetaDataCached {
        int rasterCount;
        float scale;
        float offset;
        int rasterXSize;
        int rasterYSize;
        float noDataValue;
        std::array<double, 6> padfTransform;

        GDALDataType dataType;

    } _gdalDatasetMetaDataCached;

    mutable std::mutex _datasetLock;
};

} // namespace openspace::globebrowsing

#endif // GLOBEBROWSING_USE_GDAL

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___GDAL_RAW_TILE_DATA_READER___H__
