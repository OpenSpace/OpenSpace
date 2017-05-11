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

#include <modules/globebrowsing/tile/rawtiledatareader/iodescription.h>
#include <ghoul/ghoul.h>
#include <ghoul/logging/consolelog.h>

namespace openspace {
namespace globebrowsing {
namespace cache {

MemoryAwareTileCache* MemoryAwareTileCache::_singleton = nullptr;
std::mutex MemoryAwareTileCache::_mutexLock;

void MemoryAwareTileCache::create() {
    std::lock_guard<std::mutex> guard(_mutexLock);
    _singleton = new MemoryAwareTileCache();
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
    for (TextureContainerMap::iterator it = _textureContainerMap.begin(); it != _textureContainerMap.end(); it++) {
        it->second.second->clear();
    }
}

bool MemoryAwareTileCache::exist(ProviderTileKey key) const {
    std::lock_guard<std::mutex> guard(_mutexLock);
    for (TextureContainerMap::const_iterator it = _textureContainerMap.cbegin(); it != _textureContainerMap.cend(); it++) {
        if(it->second.second->exist(key)) {
            return true;
        }
    }
    return false;
}

Tile MemoryAwareTileCache::get(ProviderTileKey key) {
    std::lock_guard<std::mutex> guard(_mutexLock);
    for (TextureContainerMap::iterator it = _textureContainerMap.begin(); it != _textureContainerMap.end(); it++) {
        if(it->second.second->exist(key)) {
            return it->second.second->get(key);
        }
    }
    ghoul_assert(false, "");
}
/*
void MemoryAwareTileCache::put(ProviderTileKey key, Tile tile) {
    std::lock_guard<std::mutex> guard(_mutexLock);
    _tileCache.put(key, tile);
}
*/
void MemoryAwareTileCache::createTileAndPut(ProviderTileKey key,
    std::shared_ptr<RawTile> rawTile)
{
    using ghoul::opengl::Texture;
    
    if (rawTile->error != RawTile::ReadError::None) {
        return;
    }
    else {
        std::shared_ptr<Texture> texture;
        // if this texture type does not exist among the texture containers
        // it needs to be created
        const TileTextureInitData& initData = *rawTile->textureInitData;
        TileTextureInitData::HashKey initDataKey = initData.hashKey();
        if (_textureContainerMap.find(initDataKey) == _textureContainerMap.end()) {
            // For now create 50 textures of this type
            _textureContainerMap.emplace(initDataKey,
                TextureContainerTileCache(
                    std::make_unique<TextureContainer>(initData, 500),
                    std::make_unique<TileCache>(std::numeric_limits<std::size_t>::max())
                    )
                );
        }
        // Now we know that the texture container exists,
        // check if there are any unused textures
        texture = _textureContainerMap[initDataKey].first->getTextureIfFree();
        // Second option. No more textures available. Pop from the LRU cache
        if (!texture) {
            Tile oldTile = _textureContainerMap[initDataKey].second->popLRU();
            // Use the old tile's texture
            texture = oldTile.texture();
        }
        
        // Re-upload texture, either using PBO or by using RAM data
        if (rawTile->pbo != 0) {
            texture->reUploadTextureFromPBO(rawTile->pbo);
            if (initData.shouldAllocateDataOnCPU()) {
                texture->setPixelData(rawTile->imageData,
                    Texture::TakeOwnership::Yes);
            }
        }
        else {
            ghoul_assert(texture->dataOwnership(),
                "Texture must have ownership of old data to avoid leaks");
            texture->setPixelData(rawTile->imageData, Texture::TakeOwnership::Yes);
            size_t expectedDataSize = texture->expectedPixelDataSize();
            size_t numBytes = rawTile->textureInitData->totalNumBytes();
            ghoul_assert(expectedDataSize == numBytes, "Pixel data size is incorrect");
            texture->reUploadTexture();
        }
        Tile tile(texture, rawTile->tileMetaData, Tile::Status::OK);
        _textureContainerMap[initDataKey].second->put(key, tile);
    }
    return;
}

/*
void MemoryAwareTileCache::setMaximumSize(size_t maximumSize) {
    std::lock_guard<std::mutex> guard(_mutexLock);
  _tileCache.setMaximumSize(maximumSize);
}
*/

MemoryAwareTileCache::MemoryAwareTileCache()
{ }

} // namespace cache
} // namespace globebrowsing
} // namespace openspace

