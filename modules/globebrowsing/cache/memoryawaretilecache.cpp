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

#include <modules/globebrowsing/rendering/layer/layergroupid.h>
#include <modules/globebrowsing/rendering/layer/layermanager.h>

#include <ghoul/ghoul.h>
#include <ghoul/logging/consolelog.h>
#include <ghoul/misc/invariants.h>
#include <ghoul/systemcapabilities/generalcapabilitiescomponent.h>

#include <numeric>
#include <algorithm>

namespace {
    const char* _loggerCat = "MemoryAwareTileCache";

    static const openspace::properties::Property::PropertyInfo CpuAllocatedDataInfo = {
        "CpuAllocatedTileData",
        "CPU allocated tile data (MB)",
        "This value denotes the amount of RAM memory (in MB) that this tile cache is "
        "utilizing."
    };

    static const openspace::properties::Property::PropertyInfo GpuAllocatedDataInfo = {
        "GpuAllocatedTileData",
        "GPU allocated tile data (MB)",
        "This value denotes the amount of GPU memory (in MB) that this tile cache is "
        "utilizing."
    };

    static const openspace::properties::Property::PropertyInfo TileCacheSizeInfo = {
        "TileCacheSize",
        "Tile cache size",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo ApplyTileCacheInfo = {
        "ApplyTileCacheSize",
        "Apply tile cache size",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo ClearTileCacheInfo = {
        "ClearTileCache",
        "Clear tile cache",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo UsePboInfo = {
        "UsePbo",
        "Use PBO",
        "If this value is enabled, pixel buffer objects are used to upload the texture "
        "data asynchronously. If this value is disabled, the upload is synchronously."
    };

} // namespace

namespace openspace::globebrowsing::cache {

MemoryAwareTileCache::MemoryAwareTileCache()
    : PropertyOwner("TileCache")
    , _numTextureBytesAllocatedOnCPU(0)
    , _cpuAllocatedTileData(CpuAllocatedDataInfo, 1024, 128, 2048, 1)
    , _gpuAllocatedTileData(GpuAllocatedDataInfo, 1024, 128, 2048, 1)
    , _tileCacheSize(TileCacheSizeInfo, 1024, 128, 2048, 1)
    , _applyTileCacheSize(ApplyTileCacheInfo)
    , _clearTileCache(ClearTileCacheInfo)
    , _usePbo(UsePboInfo, false)
{
    createDefaultTextureContainers();

    // Properties
    _clearTileCache.onChange(
    [&]{
        clear();
    });
    _applyTileCacheSize.onChange(
    [&]{
        setSizeEstimated(_tileCacheSize * 1024 * 1024);
    });
    _cpuAllocatedTileData.setMaxValue(
        static_cast<int>(CpuCap.installedMainMemory() * 0.25)
    );
    _gpuAllocatedTileData.setMaxValue(
        static_cast<int>(CpuCap.installedMainMemory() * 0.25)
    );
    _tileCacheSize.setMaxValue(
        static_cast<int>(CpuCap.installedMainMemory() * 0.25)
    );
  
    setSizeEstimated(_tileCacheSize * 1024 * 1024);
  
    _cpuAllocatedTileData.setReadOnly(true);
    _gpuAllocatedTileData.setReadOnly(true);

    addProperty(_clearTileCache);
    addProperty(_applyTileCacheSize);
    addProperty(_cpuAllocatedTileData);
    addProperty(_gpuAllocatedTileData);
    addProperty(_tileCacheSize);
    addProperty(_usePbo);
}

MemoryAwareTileCache::~MemoryAwareTileCache()
{ }

void MemoryAwareTileCache::clear() {
    LINFO("Clearing tile cache");
    _numTextureBytesAllocatedOnCPU = 0;
    for (std::pair<const TileTextureInitData::HashKey,
        TextureContainerTileCache>& p : _textureContainerMap)
    {
        p.second.first->reset();
        p.second.second->clear();
    }
    LINFO("Tile cache cleared");
}

void MemoryAwareTileCache::createDefaultTextureContainers() {
    for (int id = 0; id < layergroupid::NUM_LAYER_GROUPS; id++) {
        TileTextureInitData initData =
            LayerManager::getTileTextureInitData(layergroupid::GroupID(id));
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
    LINFO("Resetting tile cache size");
    ghoul_assert(_textureContainerMap.size() > 0, "Texture containers must exist.");
  
    size_t sumTextureTypeSize = std::accumulate(
        _textureContainerMap.cbegin(),
        _textureContainerMap.cend(), 0,
        [](size_t s, const std::pair<const TileTextureInitData::HashKey,
        TextureContainerTileCache>& p)
        {
            return s + p.second.first->tileTextureInitData().totalNumBytes();
        }
    );
  
    size_t numTexturesPerType = estimatedSize / sumTextureTypeSize;
    resetTextureContainerSize(numTexturesPerType);
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

bool MemoryAwareTileCache::exist(ProviderTileKey key) const {
    TextureContainerMap::const_iterator result =
        std::find_if(_textureContainerMap.cbegin(), _textureContainerMap.cend(),
            [&](const std::pair<const TileTextureInitData::HashKey,
                    TextureContainerTileCache>& p){
                return p.second.second->exist(key);
            });
    return result != _textureContainerMap.cend();
}

Tile MemoryAwareTileCache::get(ProviderTileKey key) {
    TextureContainerMap::const_iterator it =
        std::find_if(_textureContainerMap.cbegin(), _textureContainerMap.cend(),
            [&](const std::pair<const TileTextureInitData::HashKey,
                    TextureContainerTileCache>& p){
                return p.second.second->exist(key);
            });
    if (it != _textureContainerMap.cend()) {
        return it->second.second->get(key);
    }
    else {
        return Tile::TileUnavailable;
    }
}

ghoul::opengl::Texture* MemoryAwareTileCache::getTexture(
    const TileTextureInitData& initData)
{
    ghoul::opengl::Texture* texture;
    // if this texture type does not exist among the texture containers
    // it needs to be created
    TileTextureInitData::HashKey initDataKey = initData.hashKey();
    assureTextureContainerExists(initData);
    // Now we know that the texture container exists,
    // check if there are any unused textures
    texture = _textureContainerMap[initDataKey].first->getTextureIfFree();
    // Second option. No more textures available. Pop from the LRU cache
    if (!texture) {
        Tile oldTile = _textureContainerMap[initDataKey].second->popLRU().second;
        // Use the old tile's texture
        texture = oldTile.texture();
    }
    return texture;
}

void MemoryAwareTileCache::createTileAndPut(ProviderTileKey key,
    std::shared_ptr<RawTile> rawTile)
{
    ghoul_precondition(rawTile, "RawTile can not be null");
    using ghoul::opengl::Texture;
    
    if (rawTile->error != RawTile::ReadError::None) {
        return;
    }
    else {
        const TileTextureInitData& initData = *rawTile->textureInitData;
        Texture* texture = getTexture(initData);
        
        // Re-upload texture, either using PBO or by using RAM data
        if (rawTile->pbo != 0) {
            texture->reUploadTextureFromPBO(rawTile->pbo);
            if (initData.shouldAllocateDataOnCPU()) {
                if (!texture->dataOwnership()) {
                    _numTextureBytesAllocatedOnCPU += initData.totalNumBytes();
                }
                texture->setPixelData(rawTile->imageData,
                    Texture::TakeOwnership::Yes);
            }
        }
        else {
            size_t previousExpectedDataSize = texture->expectedPixelDataSize();
            ghoul_assert(texture->dataOwnership(),
                "Texture must have ownership of old data to avoid leaks");
            texture->setPixelData(rawTile->imageData, Texture::TakeOwnership::Yes);
            size_t expectedDataSize = texture->expectedPixelDataSize();
            size_t numBytes = rawTile->textureInitData->totalNumBytes();
            ghoul_assert(expectedDataSize == numBytes, "Pixel data size is incorrect");
            _numTextureBytesAllocatedOnCPU += numBytes - previousExpectedDataSize;
            texture->reUploadTexture();
        }
        texture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
        Tile tile(texture, rawTile->tileMetaData, Tile::Status::OK);
        TileTextureInitData::HashKey initDataKey = initData.hashKey();
        _textureContainerMap[initDataKey].second->put(key, tile);
    }
    return;
}

void MemoryAwareTileCache::put(const ProviderTileKey& key,
        const TileTextureInitData::HashKey& initDataKey, Tile tile)
{
    _textureContainerMap[initDataKey].second->put(key, tile);
    return;
}

void MemoryAwareTileCache::update() {
    const size_t dataSizeCPU = getCPUAllocatedDataSize();
    const size_t dataSizeGPU = getGPUAllocatedDataSize();

    const size_t ByteToMegaByte = 1024 * 1024;

    _cpuAllocatedTileData.setValue(dataSizeCPU / ByteToMegaByte);
    _gpuAllocatedTileData.setValue(dataSizeGPU / ByteToMegaByte);
}

size_t MemoryAwareTileCache::getGPUAllocatedDataSize() const {
    return std::accumulate(
        _textureContainerMap.cbegin(),
        _textureContainerMap.cend(), 0,
        [](size_t s, const std::pair<const TileTextureInitData::HashKey,
        TextureContainerTileCache>& p)
        {
            const TextureContainer& textureContainer = *p.second.first;
            size_t bytesPerTexture =
                textureContainer.tileTextureInitData().totalNumBytes();
            return s + bytesPerTexture * textureContainer.size();
        }
    );
}

size_t MemoryAwareTileCache::getCPUAllocatedDataSize() const {
    size_t dataSize = std::accumulate(
        _textureContainerMap.cbegin(),
        _textureContainerMap.cend(), 0,
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
