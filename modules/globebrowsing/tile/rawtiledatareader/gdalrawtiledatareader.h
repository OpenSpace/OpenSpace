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

#include <modules/globebrowsing/tile/pixelregion.h>
#include <modules/globebrowsing/tile/rawtile.h>
#include <modules/globebrowsing/tile/tiledepthtransform.h>
#include <modules/globebrowsing/tile/tiletextureinitdata.h>
#include <ghoul/misc/boolean.h>

#include <modules/globebrowsing/tile/rawtiledatareader/iodescription.h>
#include <string>
#include <mutex>
#include <gdal.h>

class GDALDataset;
class GDALRasterBand;

namespace openspace::globebrowsing {

class GeodeticPatch;

class GdalRawTileDataReader {
public:
    BooleanType(PerformPreprocessing);


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
        PerformPreprocessing preprocess = PerformPreprocessing::No);

    ~GdalRawTileDataReader();

    // Public virtual function overloading
    void reset();
    int maxChunkLevel() const;
    float noDataValueAsFloat() const;
    int rasterXSize() const;
    int rasterYSize() const;
    int dataSourceNumRasters() const;
    float depthOffset() const;
    float depthScale() const;
    PixelRegion fullPixelRegion() const;

    RawTile readTileData(TileIndex tileIndex) const;
    const TileDepthTransform& depthTransform() const;
    const TileTextureInitData& tileTextureInitData() const;
    const PixelRegion::PixelRange fullPixelSize() const;

protected:
    /**
     * Returns the geo transform from raster space to projection coordinates as defined
     * by GDAL.
     * If the transform is not available, the function returns a transform to map
     * the pixel coordinates to cover the whole geodetic lat long space.
     */
    std::array<double, 6> geoTransform() const;
    std::array<double, 6> calculategeoTransform() const;
    

private:
    // Private virtual function overloading
    void initialize();
    RawTile::ReadError rasterRead(int rasterBand, const IODescription& io,
        char* dst) const;

    void readImageData(IODescription& io, RawTile::ReadError& worstError,
        char* imageDataDest) const;

    IODescription adjustIODescription(const IODescription& io) const;
    IODescription ioDescription(const TileIndex& tileIndex) const;

    /**
 * Get the pixel corresponding to a specific position on the globe defined by the
 * Geodetic2 coordinate \p geo. If the dataset has overviews the function returns the
 * pixel at the lowest overview (highest resolution).
 *
 * \param geo The position on the globe to convert to pixel space.
 * \return a pixel coordinate in the dataset.
 */
    PixelRegion::PixelCoordinate geodeticToPixel(const Geodetic2& geo) const;

    /**
     * Get the geodetic coordinate corresponding to the given pixel in the dataset. If
     * The dataset has overviews it is the lowest overview that is used. That is the
     * one with highest resolution.
     */
    Geodetic2 pixelToGeodetic(const PixelRegion::PixelCoordinate& p) const;

    /**
     * Get a pixel region corresponding to the given GeodeticPatch. If the dataset has
     * overviews the function returns the pixel region at the lowest overview (highest
     * resolution).
     *
     * \param \p geodeticPatch is a patch covering an area in geodetic coordinates
     * \return A PixelRegion covering the given geodetic patch at highest resolution.
     */
    PixelRegion highestResPixelRegion(const GeodeticPatch& geodeticPatch) const;

    /**
     * A recursive function that is able to perform wrapping in case the read region of
     * the given IODescription is outside of the given write region.
     */
    RawTile::ReadError repeatedRasterRead(int rasterBand, const IODescription& fullIO,
        char* dataDestination, int depth = 0) const;

    TileMetaData getTileMetaData(RawTile& rawTile, const PixelRegion& region) const;
    TileDepthTransform calculateTileDepthTransform();
    RawTile::ReadError postProcessErrorCheck(const RawTile& rawTile) const;



    GDALDataset* _dataset = nullptr;
    std::string _datasetFilePath;

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

    struct {
        int _maxLevel = -1;
        double _tileLevelDifference;
    } _cached;
    const TileTextureInitData _initData;
    PerformPreprocessing _preprocess;
    TileDepthTransform _depthTransform = { 0.f, 0.f };

    mutable std::mutex _datasetLock;
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___GDAL_RAW_TILE_DATA_READER___H__
