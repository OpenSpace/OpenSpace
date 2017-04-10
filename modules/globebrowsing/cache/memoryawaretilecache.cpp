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

#include <modules/globebrowsing/cache/memoryawaretilecache.h>

#include <ghoul/ghoul.h>
#include <ghoul/logging/consolelog.h>

namespace openspace {
namespace globebrowsing {
namespace cache {

MemoryAwareTileCache* MemoryAwareTileCache::_singleton = nullptr;
std::mutex MemoryAwareTileCache::_mutexLock;

void MemoryAwareTileCache::create(size_t cacheSize) {
    std::lock_guard<std::mutex> guard(_mutexLock);
    _singleton = new MemoryAwareTileCache(cacheSize);
}

void MemoryAwareTileCache::destroy() {
    std::lock_guard<std::mutex> guard(_mutexLock);
    delete _singleton;
}

MemoryAwareTileCache& MemoryAwareTileCache::ref() {
    std::lock_guard<std::mutex> guard(_mutexLock);
    ghoul_assert(_singleton, "MemoryAwareTileCache not created");
    return *_singleton;
}

void MemoryAwareTileCache::clear() {
    std::lock_guard<std::mutex> guard(_mutexLock);
    _tileCache.clear();
}

bool MemoryAwareTileCache::exist(ProviderTileKey key) const {
    std::lock_guard<std::mutex> guard(_mutexLock);
    return _tileCache.exist(key);
}

Tile MemoryAwareTileCache::get(ProviderTileKey key) {
    std::lock_guard<std::mutex> guard(_mutexLock);
    return _tileCache.get(key);
}

void MemoryAwareTileCache::put(ProviderTileKey key, Tile tile) {
    std::lock_guard<std::mutex> guard(_mutexLock);
    _tileCache.put(key, tile);
}

void MemoryAwareTileCache::setMaximumSize(size_t maximumSize) {
    std::lock_guard<std::mutex> guard(_mutexLock);
  _tileCache.setMaximumSize(maximumSize);
}

MemoryAwareTileCache::MemoryAwareTileCache(size_t cacheSize)
    : _tileCache(cacheSize) {}

} // namespace cache
} // namespace globebrowsing
} // namespace openspace

