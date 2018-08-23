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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___DEFAULT_TILE_PROVIDER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___DEFAULT_TILE_PROVIDER___H__

#include <modules/globebrowsing/tile/tileprovider/tileprovider.h>

#include <modules/globebrowsing/tile/tiletextureinitdata.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/intproperty.h>

namespace openspace { class PixelBuffer; }

namespace openspace::globebrowsing {
    class AsyncTileDataProvider;
    struct RawTile;

    namespace cache { class MemoryAwareTileCache; }
} // namespace openspace::globebrowsing

namespace openspace::globebrowsing::tileprovider {

/**
 * Provides tiles loaded by <code>AsyncTileDataProvider</code> and
 * caches them in memory using LRU caching
 */
class DefaultTileProvider : public TileProvider {
public:
    DefaultTileProvider(const ghoul::Dictionary& dictionary);
    DefaultTileProvider(std::shared_ptr<AsyncTileDataProvider> reader);

    virtual ~DefaultTileProvider();

    /**
     * \return A Tile with status OK iff it exists in in-memory cache. If not, it may
     *         enqueue some IO operations on a separate thread.
     */
    virtual Tile tile(const TileIndex& tileIndex) override;

    virtual Tile::Status tileStatus(const TileIndex& tileIndex) override;
    virtual TileDepthTransform depthTransform() override;
    virtual void update() override;
    virtual void reset() override;
    virtual int maxLevel() override;
    virtual float noDataValueAsFloat() override;

private:
    /**
     * Collects all asynchronously downloaded <code>RawTile</code>
     * and uses <code>createTile</code> to create <code>Tile</code>s,
     * which are put in the LRU cache - potentially pushing out outdated
     * Tiles.
     */
    void initTexturesFromLoadedData();

    void initAsyncTileDataReader(TileTextureInitData initData);

    std::shared_ptr<AsyncTileDataProvider> _asyncTextureDataProvider;

    cache::MemoryAwareTileCache* _tileCache = nullptr;

    properties::StringProperty _filePath;
    properties::IntProperty _tilePixelSize;
    layergroupid::GroupID _layerGroupID = layergroupid::GroupID::Unknown;
    int _preCacheLevel = 0;
    bool _performPreProcessing = false;
    bool _padTiles = true;
};

} // namespace openspace::globebrowsing::tileprovider

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___DEFAULT_TILE_PROVIDER___H__
