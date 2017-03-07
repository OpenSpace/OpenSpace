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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___MEMORY_AWARE_TILE_CACHE___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___MEMORY_AWARE_TILE_CACHE___H__

#include <modules/globebrowsing/tile/tile.h>
#include <modules/globebrowsing/tile/tileindex.h>
#include <modules/globebrowsing/cache/memoryawarelrucache.h>

#include <memory>

namespace openspace {
namespace globebrowsing {
namespace cache {

/**
 * Enumerable type used in the LRU cache. 128 bytes are able to account for tile index
 * and an unique identifier for the tile provider.
 */
//typedef unsigned __int128 uint128_t;
//using ProviderTileHashKey = uint128_t;

struct ProviderTileKey {
    TileIndex tileIndex;
    int providerID;

    bool operator==(const ProviderTileKey& r ) const
    {
       return (providerID == r.providerID) &&
            (tileIndex == r.tileIndex);
    }
};

struct ProviderTileHasher {
    /**
    Creates a hash which can be used as key in hash maps.
    First set the bits to be unique for all tiles.
    +-------+------------+-------+------------+
    | USAGE | BIT RANGE  | #BITS | MAX VALUE  |
    +-------+------------+-------+------------+
    | level |   0 -  5   |   5   |         31 |
    |     x |   5 - 35   |  30   | 1073741824 |
    |     y |  35 - 64   |  29   |  536870912 |
    +-------+------------+-------+------------+

    */
    unsigned long long operator()(const ProviderTileKey& t) const {
        unsigned long long key;
        key |= static_cast<unsigned long long>(t.tileIndex.level);
        key |= static_cast<unsigned long long>(t.tileIndex.x << 5);
        key |= static_cast<unsigned long long>(t.tileIndex.y << 35);
        // Now the key is unique for all tiles, however not for all tile providers.
        // Add to the key depending on the tile provider to avoid some hash collisions.
        // (All hash collisions can not be avoided due to the limit in 64 bit for the
        // hash key)
        // Idea make some offset in the place of the bits for the x value. Lesser chance
        // of having different x-value than having different tile provider ids.
        key += static_cast<unsigned long long>(t.providerID << 25);
        return key;
    }
};


/**
 * Singleton class used to cache tiles for all <code>CachingTileProvider</code>s.
 */
class MemoryAwareTileCache
{
public:
    static void create(size_t cacheSize);
    static void destroy();

    void clear();
    bool exist(ProviderTileKey key);
    Tile get(ProviderTileKey key);
    void put(ProviderTileKey key, Tile tile);
    
    /**
     * Cleans the cache if the amount of allocated data is more than the maximum cache
     * size.
     * \param extraMemorySize is the amount of extra memory the cache needs to consider
     * when cleaning the cache. This memory size is simply added to the current cache
     * size when checking if the cache size is too big.
     */
    void clean(size_t extraMemorySize = 0);

    static MemoryAwareTileCache& ref();
private:
    /**
     * \param cacheSize is the cache size given in bytes.
     */
    MemoryAwareTileCache(size_t cacheSize);
    ~MemoryAwareTileCache() = default;
    
    static MemoryAwareTileCache* _singleton;
    std::shared_ptr<MemoryAwareLRUCache<ProviderTileKey, Tile, ProviderTileHasher> > _tileCache;
};

} // namespace cache
} // namespace globebrowsing
} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___MEMORY_AWARE_TILE_CACHE___H__
