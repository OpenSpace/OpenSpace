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

#include <modules/globebrowsing/cache/memorytilecache.h>

#include <ghoul/ghoul.h>
#include <ghoul/logging/consolelog.h>

namespace openspace {
namespace globebrowsing {
namespace cache {

MemoryTileCache* MemoryTileCache::_singleton = nullptr;

void MemoryTileCache::create(size_t cacheSize) {
    _singleton = new MemoryTileCache(cacheSize);
}

void MemoryTileCache::destroy() {
    delete _singleton;
}

MemoryTileCache& MemoryTileCache::ref() {
    ghoul_assert(_singleton, "MemoryTileCache not created");
    return *_singleton;
}

void MemoryTileCache::clear() {
    _tileCache->clear();
}

bool MemoryTileCache::exist(ProviderTileHashKey key) {
    return _tileCache->exist(key);
}

Tile MemoryTileCache::get(ProviderTileHashKey key) {
    return _tileCache->get(key);
}

void MemoryTileCache::put(ProviderTileHashKey key, Tile tile) {
    _tileCache->put(key, tile);
}

MemoryTileCache::MemoryTileCache(size_t cacheSize)
{
	_tileCache = std::make_shared<LRUMemoryCache<ProviderTileHashKey, Tile> >(static_cast<size_t>(cacheSize));
}

} // namespace cache
} // namespace globebrowsing
} // namespace openspace

