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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___MEMORY_TILE_CACHE___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___MEMORY_TILE_CACHE___H__

#include <modules/globebrowsing/tile/tile.h>
#include <modules/globebrowsing/cache/lrumemorycache.h>

#include <memory>

namespace openspace {
namespace globebrowsing {
namespace cache {

/*
struct ProviderTileHashKey
{
    /// Each <code>TileProvider</code> has its own unique identifier
    unsigned int tileProviderId;
    TileIndex::TileHashKey tileKey;
};
*/

typedef unsigned __int128 uint128_t;
using ProviderTileHashKey = uint128_t;

class MemoryTileCache
{
public:
    static void create(size_t cacheSize);
    static void destroy();

    void clear();
    bool exist(ProviderTileHashKey key);
    Tile get(ProviderTileHashKey key);
    void put(ProviderTileHashKey key, Tile tile);

    static MemoryTileCache& ref();
private:
    /**
     * \param cacheSize is the cache size given in bytes.
     */
    MemoryTileCache(size_t cacheSize);
    ~MemoryTileCache() = default;
    
    static MemoryTileCache* _singleton;
    std::shared_ptr<LRUMemoryCache<ProviderTileHashKey, Tile> > _tileCache;
};

} // namespace cache
} // namespace globebrowsing
} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___MEMORY_TILE_CACHE___H__
