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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___CACHING_TILE_PROVIDER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___CACHING_TILE_PROVIDER___H__

#include <modules/globebrowsing/tile/tileprovider/tileprovider.h>

namespace openspace {
namespace globebrowsing {

class AsyncTileDataProvider;
class RawTile;
    
namespace tileprovider {

/**
* Provides tiles loaded by <code>AsyncTileDataProvider</code> and 
* caches them in memory using LRU caching
*/
class CachingTileProvider : public TileProvider {
public:
    CachingTileProvider(const ghoul::Dictionary& dictionary);

    CachingTileProvider(
        std::shared_ptr<AsyncTileDataProvider> tileReader, 
        std::shared_ptr<TileCache> tileCache,
        int framesUntilFlushRequestQueue);

    virtual ~CachingTileProvider();
        
    /**
    * \returns a Tile with status OK iff it exists in in-memory 
    * cache. If not, it may enqueue some IO operations on a 
    * separate thread.
    */
    virtual Tile getTile(const TileIndex& tileIndex);

    virtual Tile getDefaultTile();
    virtual Tile::Status getTileStatus(const TileIndex& tileIndex);
    virtual TileDepthTransform depthTransform();
    virtual void update();
    virtual void reset();
    virtual int maxLevel();
    virtual float noDataValueAsFloat();

private:
    /**
    * Collects all asynchronously downloaded <code>RawTile</code>
    * and uses <code>createTile</code> to create <code>Tile</code>s, 
    * which are put in the LRU cache - potentially pushing out outdated
    * Tiles.
    */
    void initTexturesFromLoadedData();

    /**
    * \returns A tile with <code>Tile::Status::OK</code> if no errors
    * occured, a tile with <code>Tile::Status::IOError</code> otherwise
    */
    Tile createTile(std::shared_ptr<RawTile> res);

    /**
    * Deletes all enqueued, but not yet started async downloads of textures.
    * Note that this does not cancel any currently ongoing async downloads.
    */
    void clearRequestQueue();

    std::shared_ptr<AsyncTileDataProvider> _asyncTextureDataProvider;
    std::shared_ptr<TileCache> _tileCache;

    int _framesSinceLastRequestFlush;
    int _framesUntilRequestFlush;

    Tile _defaultTile;
};

} // namespace tileprovider
} // namespace globebrowsing
} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___CACHING_TILE_PROVIDER___H__
