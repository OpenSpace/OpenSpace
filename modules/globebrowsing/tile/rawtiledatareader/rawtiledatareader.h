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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___RAW_TILE_DATA_READER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___RAW_TILE_DATA_READER___H__

#include <modules/globebrowsing/tile/pixelregion.h>
#include <modules/globebrowsing/tile/rawtile.h>
#include <modules/globebrowsing/tile/tiledepthtransform.h>
#include <modules/globebrowsing/tile/tiletextureinitdata.h>
#include <ghoul/misc/boolean.h>

namespace openspace::globebrowsing {

class GeodeticPatch;
struct IODescription;

class RawTileDataReader {
public:
    BooleanType(PerformPreprocessing);

    RawTileDataReader(const TileTextureInitData& initData,
        PerformPreprocessing preprocess = PerformPreprocessing::No);
    virtual ~RawTileDataReader() = default;

    /**
     * Reads data from the current dataset and initializes a <code>RawTile</code>
     * which gets returned.
     */
    std::shared_ptr<RawTile> readTileData(TileIndex tileIndex,
        char* dataDestination, char* pboMappedDataDestination) const;
    const TileDepthTransform& depthTransform() const;
    const TileTextureInitData& tileTextureInitData() const;
    const PixelRegion::PixelRange fullPixelSize() const;

    /**
     * \return The maximum chunk level available in the dataset. Should be a value
     * between 2 and 31.
     */
    virtual int maxChunkLevel() const = 0;

    /**
     * Reset the dataset to its initial state. This is the place to clear any cache used.
     */
    virtual void reset() = 0;
    virtual float noDataValueAsFloat() const = 0;
    virtual int rasterXSize() const = 0;
    virtual int rasterYSize() const = 0;
    virtual int dataSourceNumRasters() const = 0;
    virtual float depthOffset() const;
    virtual float depthScale() const;
    PixelRegion fullPixelRegion() const;

    /**
     * Returns a single channeled empty RawTile of size 16 * 16 pixels.
     */
    std::shared_ptr<RawTile> defaultTileData() const;

protected:
    /**
     * This function should set the variables <code>_cached</code>,
     * <code>_dataLayout</code> and <code>_depthTransform</code>.
     */
    virtual void initialize() = 0;

    /**
     * The function returns a transform to map the pixel coordinates to cover the whole
     * geodetic lat long space.
     */
    virtual std::array<double, 6> geoTransform() const;

    /**
     * Read image data as described by the given IODescription.
     *
     * \param io describes how to read the data.
     * \param worstError should be set to the error code returned when reading the data.
     */
    void readImageData(
        IODescription& io, RawTile::ReadError& worstError, char* imageDataDest) const;

    /**
     * The default does not affect the IODescription but this function can be used for
     * example to flip the y axis.
     */
    virtual IODescription adjustIODescription(const IODescription& io) const;

    virtual RawTile::ReadError rasterRead(
        int rasterBand, const IODescription& io, char* dst) const = 0;

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

    std::shared_ptr<TileMetaData> getTileMetaData(std::shared_ptr<RawTile> rawTile,
        const PixelRegion& region) const;
    TileDepthTransform calculateTileDepthTransform();
    RawTile::ReadError postProcessErrorCheck(
        std::shared_ptr<const RawTile> rawTile) const;

    struct Cached {
        int _maxLevel = -1;
        double _tileLevelDifference;
    } _cached;
    const TileTextureInitData _initData;
    PerformPreprocessing _preprocess;
    TileDepthTransform _depthTransform = { 0.f, 0.f };
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___RAW_TILE_DATA_READER___H__
