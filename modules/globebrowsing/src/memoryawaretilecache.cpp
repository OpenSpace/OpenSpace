/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <modules/globebrowsing/src/memoryawaretilecache.h>

#include <modules/globebrowsing/src/basictypes.h>
#include <modules/globebrowsing/src/layermanager.h>
#include <modules/globebrowsing/src/rawtile.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/systemcapabilities/generalcapabilitiescomponent.h>
#include <ghoul/systemcapabilities/openglcapabilitiescomponent.h>
#include <numeric>

namespace {
    constexpr std::string_view _loggerCat = "MemoryAwareTileCache";

    constexpr openspace::properties::Property::PropertyInfo CpuAllocatedDataInfo = {
        "CpuAllocatedTileData",
        "CPU allocated tile data (MB)",
        "This value denotes the amount of RAM memory (in MB) that this tile cache is "
        "utilizing",
        openspace::properties::Property::Visibility::Developer
    };

    constexpr openspace::properties::Property::PropertyInfo GpuAllocatedDataInfo = {
        "GpuAllocatedTileData",
        "GPU allocated tile data (MB)",
        "This value denotes the amount of GPU memory (in MB) that this tile cache is "
        "utilizing",
        openspace::properties::Property::Visibility::Developer
    };

    constexpr openspace::properties::Property::PropertyInfo TileCacheSizeInfo = {
        "TileCacheSize",
        "Tile cache size",
        "", // @TODO Missing documentation
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ApplyTileCacheInfo = {
        "ApplyTileCacheSize",
        "Apply tile cache size",
        "", // @TODO Missing documentation
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ClearTileCacheInfo = {
        "ClearTileCache",
        "Clear tile cache",
        "", // @TODO Missing documentation
        openspace::properties::Property::Visibility::AdvancedUser
    };

    GLenum toGlTextureFormat(GLenum glType, ghoul::opengl::Texture::Format format) {
        switch (format) {
            case ghoul::opengl::Texture::Format::Red:
                switch (glType) {
                    case GL_BYTE:
                        return GL_R8;
                    case GL_UNSIGNED_BYTE:
                        return GL_R8;
                    case GL_INT:
                        return GL_R32I;
                    case GL_UNSIGNED_INT:
                        return GL_R32UI;
                    case GL_FLOAT:
                        return GL_R32F;
                    case GL_HALF_FLOAT:
                        return GL_R16F;
                    default:
                        ghoul_assert(false, "glType data type unknown");
                        throw ghoul::MissingCaseException();
                }
            case ghoul::opengl::Texture::Format::RG:
                switch (glType) {
                    case GL_BYTE:
                        return GL_RG8;
                    case GL_UNSIGNED_BYTE:
                        return GL_RG8;
                    case GL_INT:
                        return GL_RG32I;
                    case GL_UNSIGNED_INT:
                        return GL_RG32UI;
                    case GL_FLOAT:
                        return GL_RG32F;
                    case GL_HALF_FLOAT:
                        return GL_RG16F;
                    default:
                        ghoul_assert(false, "glType data type unknown");
                        throw ghoul::MissingCaseException();
                }
            case ghoul::opengl::Texture::Format::RGB:
                switch (glType) {
                    case GL_BYTE:
                        return GL_RGB8;
                    case GL_UNSIGNED_BYTE:
                        return GL_RGB8;
                    case GL_INT:
                        return GL_RGB32I;
                    case GL_UNSIGNED_INT:
                        return GL_RGB32UI;
                    case GL_FLOAT:
                        return GL_RGB32F;
                    case GL_HALF_FLOAT:
                        return GL_RGB16F;
                    default:
                        ghoul_assert(false, "glType data type unknown");
                        throw ghoul::MissingCaseException();
                }
            case ghoul::opengl::Texture::Format::RGBA:
                switch (glType) {
                    case GL_BYTE:
                        return GL_RGBA8;
                    case GL_UNSIGNED_BYTE:
                        return GL_RGBA8;
                    case GL_INT:
                        return GL_RGBA32I;
                    case GL_UNSIGNED_INT:
                        return GL_RGBA32UI;
                    case GL_FLOAT:
                        return GL_RGBA32F;
                    case GL_HALF_FLOAT:
                        return GL_RGBA16F;
                    default:
                        ghoul_assert(false, "glType data type unknown");
                        throw ghoul::MissingCaseException();
                }
            case ghoul::opengl::Texture::Format::BGR:
                switch (glType) {
                    case GL_BYTE:
                        return GL_RGB8;
                    case GL_UNSIGNED_BYTE:
                        return GL_RGB8;
                    case GL_INT:
                        return GL_RGB32I;
                    case GL_UNSIGNED_INT:
                        return GL_RGB32UI;
                    case GL_FLOAT:
                        return GL_RGB32F;
                    case GL_HALF_FLOAT:
                        return GL_RGB16F;
                    default:
                        ghoul_assert(false, "glType data type unknown");
                        throw ghoul::MissingCaseException();
                }
            case ghoul::opengl::Texture::Format::BGRA:
                switch (glType) {
                    case GL_BYTE:
                        return GL_RGBA8;
                    case GL_UNSIGNED_BYTE:
                        return GL_RGBA8;
                    case GL_INT:
                        return GL_RGBA32I;
                    case GL_UNSIGNED_INT:
                        return GL_RGBA32UI;
                    case GL_FLOAT:
                        return GL_RGBA32F;
                    case GL_HALF_FLOAT:
                        return GL_RGBA16F;
                    default:
                        ghoul_assert(false, "glType data type unknown");
                        throw ghoul::MissingCaseException();
                }
            default:
                ghoul_assert(false, "Unknown format for OpenGL texture");
                throw ghoul::MissingCaseException();
        }
    }


} // namespace

namespace openspace::globebrowsing::cache {

//
// TextureContainer
//
MemoryAwareTileCache::TextureContainer::TextureContainer(TileTextureInitData initData,
                                                         size_t numTextures)
    : _initData(std::move(initData))
    , _numTextures(numTextures)
{
    ZoneScoped;

    _textures.reserve(_numTextures);
    reset();
}

void MemoryAwareTileCache::TextureContainer::reset() {
    ZoneScoped;

    _textures.clear();
    _freeTexture = 0;

    using namespace ghoul::systemcapabilities;

    const ghoul::opengl::Texture::FilterMode mode =
        OpenGLCap.gpuVendor() == OpenGLCapabilitiesComponent::Vendor::AmdATI ?
        ghoul::opengl::Texture::FilterMode::Linear :
        ghoul::opengl::Texture::FilterMode::AnisotropicMipMap;

    for (size_t i = 0; i < _numTextures; i++) {
        ZoneScopedN("Texture");

        using namespace ghoul::opengl;

        std::unique_ptr<Texture> tex = std::make_unique<Texture>(
            _initData.dimensions,
            GL_TEXTURE_2D,
            _initData.ghoulTextureFormat,
            toGlTextureFormat(_initData.glType, _initData.ghoulTextureFormat),
            _initData.glType,
            mode,
            Texture::WrappingMode::ClampToEdge,
            Texture::AllocateData(_initData.shouldAllocateDataOnCPU)
        );

        tex->setDataOwnership(Texture::TakeOwnership::Yes);
        tex->setFilter(mode);
        tex->uploadTexture();

        _textures.push_back(std::move(tex));
    }
}

void MemoryAwareTileCache::TextureContainer::reset(size_t numTextures) {
    ZoneScoped;

    _numTextures = numTextures;
    reset();
}

ghoul::opengl::Texture* MemoryAwareTileCache::TextureContainer::getTextureIfFree() {
    if (_freeTexture < _textures.size()) {
        ghoul::opengl::Texture* texture = _textures[_freeTexture].get();
        _freeTexture++;
        return texture;
    }
    else {
        return nullptr;
    }
}

const TileTextureInitData&
MemoryAwareTileCache::TextureContainer::tileTextureInitData() const
{
    return _initData;
}

size_t MemoryAwareTileCache::TextureContainer::size() const {
    return _textures.size();
}

//
// MemoryAwareTileCache
//

MemoryAwareTileCache::MemoryAwareTileCache(int tileCacheSize)
    : PropertyOwner({ "TileCache", "Tile Cache" })
    , _numTextureBytesAllocatedOnCPU(0)
    , _cpuAllocatedTileData(CpuAllocatedDataInfo, tileCacheSize, 128, 16384, 1)
    , _gpuAllocatedTileData(GpuAllocatedDataInfo, tileCacheSize, 128, 16384, 1)
    , _tileCacheSize(TileCacheSizeInfo, tileCacheSize, 128, 16384, 1)
    , _applyTileCacheSize(ApplyTileCacheInfo)
    , _clearTileCache(ClearTileCacheInfo)
{
    ZoneScoped;

    createDefaultTextureContainers();

    _clearTileCache.onChange([this]() { clear(); });
    addProperty(_clearTileCache);

    _applyTileCacheSize.onChange([this](){
        setSizeEstimated(uint64_t(_tileCacheSize) * 1024ul * 1024ul);
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

    setSizeEstimated(uint64_t(_tileCacheSize) * 1024ul * 1024ul);
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
    ZoneScoped;

    for (const layers::Group& gi : layers::Groups) {
        const TileTextureInitData initData = tileTextureInitData(gi.id);
        assureTextureContainerExists(initData);
    }
}

void MemoryAwareTileCache::assureTextureContainerExists(
                                                      const TileTextureInitData& initData)
{
    ZoneScoped;

    const TileTextureInitData::HashKey initDataKey = initData.hashKey;
    if (_textureContainerMap.find(initDataKey) == _textureContainerMap.end()) {
        // For now create 500 textures of this type
        _textureContainerMap.emplace(initDataKey,
            TextureContainerTileCache(
                std::make_unique<TextureContainer>(initData, 500),
                std::make_unique<TileCache>(std::numeric_limits<size_t>::max())
            )
        );
    }
}

void MemoryAwareTileCache::setSizeEstimated(size_t estimatedSize) {
    ZoneScoped;
    ghoul_assert(!_textureContainerMap.empty(), "Texture containers must exist");

    LDEBUG("Resetting tile cache size");

    const size_t sumTextureTypeSize = std::accumulate(
        _textureContainerMap.cbegin(),
        _textureContainerMap.cend(),
        size_t(0),
        [](size_t s, const std::pair<const TileTextureInitData::HashKey,
                                     TextureContainerTileCache>& p)
        {
            return s + p.second.first->tileTextureInitData().totalNumBytes;
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
    ZoneScoped;

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
        [&key](const std::pair<const TileTextureInitData::HashKey,
                               TextureContainerTileCache>& p)
        {
            return p.second.second->exist(key);
        }
    );
    return result != _textureContainerMap.cend();
}

Tile MemoryAwareTileCache::get(const ProviderTileKey& key) {
    ZoneScoped;

    const TextureContainerMap::const_iterator it = std::find_if(
        _textureContainerMap.cbegin(),
        _textureContainerMap.cend(),
        [&key](const std::pair<const TileTextureInitData::HashKey,
                               TextureContainerTileCache>& p)
        {
            return p.second.second->exist(key);
        }
    );
    if (it != _textureContainerMap.cend()) {
        return it->second.second->get(key);
    }
    else {
        return Tile();
    }
}

ghoul::opengl::Texture* MemoryAwareTileCache::texture(const TileTextureInitData& initData)
{
    // if this texture type does not exist among the texture containers
    // it needs to be created
    const TileTextureInitData::HashKey initDataKey = initData.hashKey;
    assureTextureContainerExists(initData);
    // Now we know that the texture container exists,
    // check if there are any unused textures
    ghoul::opengl::Texture* texture =
        _textureContainerMap[initDataKey].first->getTextureIfFree();
    // Second option. No more textures available. Pop from the LRU cache
    if (!texture) {
        const Tile oldTile = _textureContainerMap[initDataKey].second->popLRU().second;
        // Use the old tile's texture
        texture = oldTile.texture;
    }
    return texture;
}

void MemoryAwareTileCache::createTileAndPut(ProviderTileKey key, RawTile rawTile) {
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
            if (initData.shouldAllocateDataOnCPU) {
                if (!tex->dataOwnership()) {
                    _numTextureBytesAllocatedOnCPU += initData.totalNumBytes;
                }
                tex->setPixelData(
                    rawTile.imageData.release(),
                    Texture::TakeOwnership::Yes
                );
                rawTile.imageData = nullptr;
            }
        }
        else {
            const size_t previousExpectedDataSize = tex->expectedPixelDataSize();
            ghoul_assert(
                tex->dataOwnership(),
                "Texture must have ownership of old data to avoid leaks"
            );
            tex->setPixelData(rawTile.imageData.release(), Texture::TakeOwnership::Yes);
            rawTile.imageData = nullptr;
            [[ maybe_unused ]] const size_t expectedSize = tex->expectedPixelDataSize();
            const size_t numBytes = rawTile.textureInitData->totalNumBytes;
            ghoul_assert(expectedSize == numBytes, "Pixel data size is incorrect");
            _numTextureBytesAllocatedOnCPU += numBytes - previousExpectedDataSize;
            tex->reUploadTexture();
        }
        // Hi there, I know someone will be tempted to change this to a Linear filtering
        // mode at some point. This will introduce rendering artifacts when looking at the
        // globe at oblique angles (see #2752)
        using namespace ghoul::systemcapabilities;
        const ghoul::opengl::Texture::FilterMode mode =
            OpenGLCap.gpuVendor() == OpenGLCapabilitiesComponent::Vendor::AmdATI ?
            ghoul::opengl::Texture::FilterMode::Linear :
            ghoul::opengl::Texture::FilterMode::AnisotropicMipMap;

        tex->setFilter(mode);
        Tile tile{ tex, std::move(rawTile.tileMetaData), Tile::Status::OK };
        const TileTextureInitData::HashKey initDataKey = initData.hashKey;
        _textureContainerMap[initDataKey].second->put(std::move(key), std::move(tile));
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
            const size_t nBytes = textureContainer.tileTextureInitData().totalNumBytes;
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
            if (initData.shouldAllocateDataOnCPU) {
                const size_t bytesPerTexture = initData.totalNumBytes;
                return s + bytesPerTexture * textureContainer.size();
            }
            return s;
        }
    );
    return dataSize + _numTextureBytesAllocatedOnCPU;
}

} // namespace openspace::globebrowsing::cache
