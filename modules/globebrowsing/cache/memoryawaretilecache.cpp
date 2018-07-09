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

#include <modules/globebrowsing/cache/memoryawaretilecache.h>

#include <modules/globebrowsing/rendering/layer/layermanager.h>
#include <modules/globebrowsing/tile/tile.h>
#include <modules/globebrowsing/tile/rawtile.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/systemcapabilities/generalcapabilitiescomponent.h>
#include <numeric>

namespace {
    constexpr const char* _loggerCat = "MemoryAwareTileCache";

    constexpr openspace::properties::Property::PropertyInfo CpuAllocatedDataInfo = {
        "CpuAllocatedTileData",
        "CPU allocated tile data (MB)",
        "This value denotes the amount of RAM memory (in MB) that this tile cache is "
        "utilizing."
    };

    constexpr openspace::properties::Property::PropertyInfo GpuAllocatedDataInfo = {
        "GpuAllocatedTileData",
        "GPU allocated tile data (MB)",
        "This value denotes the amount of GPU memory (in MB) that this tile cache is "
        "utilizing."
    };

    constexpr openspace::properties::Property::PropertyInfo TileCacheSizeInfo = {
        "TileCacheSize",
        "Tile cache size",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo ApplyTileCacheInfo = {
        "ApplyTileCacheSize",
        "Apply tile cache size",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo ClearTileCacheInfo = {
        "ClearTileCache",
        "Clear tile cache",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo UsePboInfo = {
        "UsePbo",
        "Use PBO",
        "If this value is enabled, pixel buffer objects are used to upload the texture "
        "data asynchronously. If this value is disabled, the upload is synchronously."
    };

} // namespace

namespace openspace::globebrowsing::cache {

MemoryAwareTileCache::MemoryAwareTileCache()
    : PropertyOwner({ "TileCache" })
    , _numTextureBytesAllocatedOnCPU(0)
    , _cpuAllocatedTileData(CpuAllocatedDataInfo, 1024, 128, 16384, 1)
    , _gpuAllocatedTileData(GpuAllocatedDataInfo, 1024, 128, 16384, 1)
    , _tileCacheSize(TileCacheSizeInfo, 1024, 128, 16384, 1)
    , _applyTileCacheSize(ApplyTileCacheInfo)
    , _clearTileCache(ClearTileCacheInfo)
    , _usePbo(UsePboInfo, false)
{
    createDefaultTextureContainers();

    _clearTileCache.onChange([&]() { clear(); });
    addProperty(_clearTileCache);

    _applyTileCacheSize.onChange([&](){
        setSizeEstimated(_tileCacheSize * 1024 * 1024);
    });
    addProperty(_applyTileCacheSize);

    _cpuAllocatedTileData.setMaxValue(
        static_cast<int>(CpuCap.installedMainMemory() * 0.95)
    );
    _cpuAllocatedTileData.setReadOnly(true);
    addProperty(_cpuAllocatedTileData);

    _gpuAllocatedTileData.setMaxValue(
        static_cast<int>(CpuCap.installedMainMemory() * 0.95)
    );
    _gpuAllocatedTileData.setReadOnly(true);
    addProperty(_gpuAllocatedTileData);

    _tileCacheSize.setMaxValue(
        static_cast<int>(CpuCap.installedMainMemory() * 0.95)
    );
    addProperty(_tileCacheSize);

    addProperty(_usePbo);

    setSizeEstimated(_tileCacheSize * 1024 * 1024);
}

void MemoryAwareTileCache::clear() {
    LINFO("Clearing tile cache");
    _numTextureBytesAllocatedOnCPU = 0;
    using K = TileTextureInitData::HashKey;
    using V = TextureContainerTileCache;
    for (std::pair<const K, V>& p : _textureContainerMap) {
        p.second.first->reset();
        p.second.second->clear();
    }
    LINFO("Tile cache cleared");
}

void MemoryAwareTileCache::createDefaultTextureContainers() {
    for (int id = 0; id < layergroupid::NUM_LAYER_GROUPS; id++) {
        TileTextureInitData initData = LayerManager::getTileTextureInitData(
            layergroupid::GroupID(id),
            LayerManager::PadTiles::Yes
        );
        assureTextureContainerExists(initData);
    }
}

void MemoryAwareTileCache::assureTextureContainerExists(
                                                      const TileTextureInitData& initData)
{
    TileTextureInitData::HashKey initDataKey = initData.hashKey();
    if (_textureContainerMap.find(initDataKey) == _textureContainerMap.end()) {
        // For now create 500 textures of this type
        _textureContainerMap.emplace(initDataKey,
            TextureContainerTileCache(
                std::make_unique<TextureContainer>(initData, 500),
                std::make_unique<TileCache>(std::numeric_limits<std::size_t>::max())
            )
        );
    }
}

void MemoryAwareTileCache::setSizeEstimated(size_t estimatedSize) {
    LDEBUG("Resetting tile cache size");
    ghoul_assert(!_textureContainerMap.empty(), "Texture containers must exist.");

    const size_t sumTextureTypeSize = std::accumulate(
        _textureContainerMap.cbegin(),
        _textureContainerMap.cend(),
        size_t(0),
        [](size_t s, const std::pair<const TileTextureInitData::HashKey,
                                     TextureContainerTileCache>& p)
        {
            return s + p.second.first->tileTextureInitData().totalNumBytes();
        }
    );

    if (sumTextureTypeSize > 0) {
        const size_t numTexturesPerType = estimatedSize / sumTextureTypeSize;
        resetTextureContainerSize(numTexturesPerType);
    }
    else {
        resetTextureContainerSize(0);
    }
    LINFO("Tile cache size was reset");
}

void MemoryAwareTileCache::resetTextureContainerSize(size_t numTexturesPerTextureType) {
    _numTextureBytesAllocatedOnCPU = 0;
    for (std::pair<const TileTextureInitData::HashKey,
        TextureContainerTileCache>& p : _textureContainerMap)
    {
        p.second.first->reset(numTexturesPerTextureType);
        p.second.second->clear();
    }
}

bool MemoryAwareTileCache::exist(const ProviderTileKey& key) const {
    const TextureContainerMap::const_iterator result = std::find_if(
        _textureContainerMap.cbegin(),
        _textureContainerMap.cend(),
        [&](const std::pair<const TileTextureInitData::HashKey,
                            TextureContainerTileCache>& p)
        {
            return p.second.second->exist(key);
        }
    );
    return result != _textureContainerMap.cend();
}

Tile MemoryAwareTileCache::get(const ProviderTileKey& key) {
    const TextureContainerMap::const_iterator it = std::find_if(
        _textureContainerMap.cbegin(),
        _textureContainerMap.cend(),
        [&](const std::pair<const TileTextureInitData::HashKey,
                            TextureContainerTileCache>& p)
        {
            return p.second.second->exist(key);
        }
    );
    if (it != _textureContainerMap.cend()) {
        return it->second.second->get(key);
    }
    else {
        return Tile::TileUnavailable;
    }
}

ghoul::opengl::Texture* MemoryAwareTileCache::texture(
                                                      const TileTextureInitData& initData)
{
    // if this texture type does not exist among the texture containers
    // it needs to be created
    TileTextureInitData::HashKey initDataKey = initData.hashKey();
    assureTextureContainerExists(initData);
    // Now we know that the texture container exists,
    // check if there are any unused textures
    ghoul::opengl::Texture* texture =
        _textureContainerMap[initDataKey].first->getTextureIfFree();
    // Second option. No more textures available. Pop from the LRU cache
    if (!texture) {
        Tile oldTile = _textureContainerMap[initDataKey].second->popLRU().second;
        // Use the old tile's texture
        texture = oldTile.texture();
    }
    return texture;
}

void MemoryAwareTileCache::createTileAndPut(ProviderTileKey key, RawTile& rawTile) {
    using ghoul::opengl::Texture;

    if (rawTile.error != RawTile::ReadError::None) {
        return;
    }
    else {
        const TileTextureInitData& initData = *rawTile.textureInitData;
        Texture* tex = texture(initData);

        // Re-upload texture, either using PBO or by using RAM data
        if (rawTile.pbo != 0) {
            tex->reUploadTextureFromPBO(rawTile.pbo);
            if (initData.shouldAllocateDataOnCPU()) {
                if (!tex->dataOwnership()) {
                    _numTextureBytesAllocatedOnCPU += initData.totalNumBytes();
                }
                tex->setPixelData(rawTile.imageData, Texture::TakeOwnership::Yes);
            }
        }
        else {
            size_t previousExpectedDataSize = tex->expectedPixelDataSize();
            ghoul_assert(
                tex->dataOwnership(),
                "Texture must have ownership of old data to avoid leaks"
            );
            tex->setPixelData(rawTile.imageData, Texture::TakeOwnership::Yes);
            [[ maybe_unused ]] size_t expectedDataSize = tex->expectedPixelDataSize();
            const size_t numBytes = rawTile.textureInitData->totalNumBytes();
            ghoul_assert(expectedDataSize == numBytes, "Pixel data size is incorrect");
            _numTextureBytesAllocatedOnCPU += numBytes - previousExpectedDataSize;
            tex->reUploadTexture();
        }
        tex->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
        Tile tile(tex, rawTile.tileMetaData, Tile::Status::OK);
        TileTextureInitData::HashKey initDataKey = initData.hashKey();
        _textureContainerMap[initDataKey].second->put(std::move(key), tile);
    }
}

void MemoryAwareTileCache::put(const ProviderTileKey& key,
                               const TileTextureInitData::HashKey& initDataKey,
                               Tile tile)
{
    _textureContainerMap[initDataKey].second->put(key, std::move(tile));
}

void MemoryAwareTileCache::update() {
    const size_t dataSizeCPU = cpuAllocatedDataSize();
    const size_t dataSizeGPU = gpuAllocatedDataSize();

    const size_t ByteToMegaByte = 1024 * 1024;

    _cpuAllocatedTileData = static_cast<int>(dataSizeCPU / ByteToMegaByte);
    _gpuAllocatedTileData = static_cast<int>(dataSizeGPU / ByteToMegaByte);
}

size_t MemoryAwareTileCache::gpuAllocatedDataSize() const {
    return std::accumulate(
        _textureContainerMap.cbegin(),
        _textureContainerMap.cend(),
        size_t(0),
        [](size_t s, const std::pair<const TileTextureInitData::HashKey,
        TextureContainerTileCache>& p)
        {
            const TextureContainer& textureContainer = *p.second.first;
            const size_t nBytes = textureContainer.tileTextureInitData().totalNumBytes();
            return s + nBytes * textureContainer.size();
        }
    );
}

size_t MemoryAwareTileCache::cpuAllocatedDataSize() const {
    const size_t dataSize = std::accumulate(
        _textureContainerMap.cbegin(),
        _textureContainerMap.cend(),
        size_t(0),
        [](size_t s, const std::pair<const TileTextureInitData::HashKey,
        TextureContainerTileCache>& p)
        {
            const TextureContainer& textureContainer = *p.second.first;
            const TileTextureInitData& initData = textureContainer.tileTextureInitData();
            if (initData.shouldAllocateDataOnCPU()) {
                size_t bytesPerTexture = initData.totalNumBytes();
                return s + bytesPerTexture * textureContainer.size();
            }
            return s;
        }
    );
    return dataSize + _numTextureBytesAllocatedOnCPU;
}

bool MemoryAwareTileCache::shouldUsePbo() const {
    return _usePbo;
}

} // namespace openspace::globebrowsing::cache
