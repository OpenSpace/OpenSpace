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

void MemoryAwareTileCache::createTileAndPut(ProviderTileKey key, std::shared_ptr<RawTile> rawTile) {
	using Texture = ghoul::opengl::Texture;
	std::lock_guard<std::mutex> guard(_mutexLock);
    
    std::shared_ptr<Texture> texture;
    // First option, there exists unused textures
    if (_freeTexture < _textureContainer.size()) {
         texture = _textureContainer[_freeTexture];
        _freeTexture++;
    }
    // Second option. No more textures available. Pop from the LRU cache
    else {
        Tile oldTile = _tileCache.popLRU();
        // Use the old tiles texture
        texture = oldTile.texture();
    }

    // Re-upload texture, either using PBO or by using RAM data
    if (rawTile->pbo != 0) {
        texture->reUploadTextureFromPBO(rawTile->pbo);
    }
    else {
        texture->setPixelData(rawTile->imageData, Texture::TakeOwnership::Yes);
        size_t expectedDataSize = texture->expectedPixelDataSize();
        ghoul_assert(expectedDataSize == rawTile->nBytesImageData,
            "Pixel data size is incorrect");
        texture->reUploadTexture();
    }

    // Create and put tile in cache
    Tile tile(texture, rawTile->tileMetaData, Tile::Status::OK);
    _tileCache.put(key, tile);
}

void MemoryAwareTileCache::setMaximumSize(size_t maximumSize) {
    std::lock_guard<std::mutex> guard(_mutexLock);
  _tileCache.setMaximumSize(maximumSize);
}

MemoryAwareTileCache::MemoryAwareTileCache(size_t cacheSize)
    : _tileCache(cacheSize)
    , _freeTexture(0) // Set the first texture to be "free"
{
    TileTextureInitData initData(512 + 4, 512 + 4, GL_UNSIGNED_BYTE,
        ghoul::opengl::Texture::Format::RGBA);
    for (int i = 0; i < 100; ++i)
    {
        _textureContainer.push_back(std::make_shared<ghoul::opengl::Texture>(
            initData.dimensions(),
            initData.ghoulTextureFormat(),
            initData.glTextureFormat(),
            initData.glType(),
            ghoul::opengl::Texture::FilterMode::Linear,
            ghoul::opengl::Texture::WrappingMode::ClampToEdge
        ));
        _textureContainer.back()->uploadTexture();
    }
}

} // namespace cache
} // namespace globebrowsing
} // namespace openspace

