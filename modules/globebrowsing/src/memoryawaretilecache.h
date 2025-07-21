/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <modules/globebrowsing/src/lrucache.h>
#include <modules/globebrowsing/src/tileindex.h>
#include <modules/globebrowsing/src/tiletextureinitdata.h>
#include <openspace/properties/misc/triggerproperty.h>
#include <openspace/properties/propertyowner.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <memory>
#include <unordered_map>
#include <vector>

namespace openspace::globebrowsing {
    struct RawTile;
    class Tile;
} // namespace openspace::globebrowsing

namespace openspace::globebrowsing::cache {

struct ProviderTileKey {
    TileIndex tileIndex;
    uint16_t providerID;

    bool operator==(const ProviderTileKey& r) const {
        return (providerID == r.providerID) && (tileIndex == r.tileIndex);
    }
};

struct ProviderTileHasher {
    /**
     * Creates a hash which can be used as key in hash maps.
     * First set the bits to be unique for all tiles.
     * +-------+------------+-------+------------+
     * | USAGE | BIT RANGE  |  BITS | MAX VALUE  |
     * +-------+------------+-------+------------+
     * | level |   0 -  5   |   5   |         31 |
     * |     x |   5 - 35   |  30   | 1073741824 |
     * |     y |  35 - 64   |  29   |  536870912 |
     * +-------+------------+-------+------------+
     *
     * Bits are then shifted depending on the tile provider used.
    */
    unsigned long long operator()(const ProviderTileKey& t) const {
        unsigned long long key = 0;
        key |= static_cast<unsigned long long>(t.tileIndex.level);
        key |= static_cast<unsigned long long>(t.tileIndex.x) << 5ULL;
        key |= static_cast<unsigned long long>(t.tileIndex.y) << 35ULL;
        // Now the key is unique for all tiles, however not for all tile providers.
        // Add to the key depending on the tile provider to avoid some hash collisions.
        // (All hash collisions can not be avoided due to the limit in 64 bit for the
        // hash key)
        // Idea: make some offset in the place of the bits for the x value. Lesser chance
        // of having different x-value than having different tile provider ids.
        key += static_cast<unsigned long long>(t.providerID) << 25ULL;
        return key;
    }
};

class MemoryAwareTileCache : public properties::PropertyOwner {
public:
    explicit MemoryAwareTileCache(int tileCacheSize = 1024);

    void clear();
    void setSizeEstimated(size_t estimatedSize);
    bool exist(const ProviderTileKey& key) const;
    Tile get(const ProviderTileKey& key);
    ghoul::opengl::Texture* texture(const TileTextureInitData& initData);
    void createTileAndPut(ProviderTileKey key, RawTile rawTile);
    void put(const ProviderTileKey& key,
        const TileTextureInitData::HashKey& initDataKey, Tile tile);
    void update();

    size_t gpuAllocatedDataSize() const;
    size_t cpuAllocatedDataSize() const;

private:
    /**
     * Owner of texture data used for tiles. Instead of dynamically allocating textures
     * one by one, they are created once and reused.
     */
    class TextureContainer {
    public:
        /**
         * \param initData is the description of the texture type
         * \param numTextures is the number of textures to allocate
         */
        TextureContainer(TileTextureInitData initData, size_t numTextures);

        ~TextureContainer() = default;

        void reset();
        void reset(size_t numTextures);

        /**
         * \return A pointer to a texture if there is one texture never used before. If
         *         there are no textures left, nullptr is returned. TextureContainer still
         *         owns the texture so no delete should be called on the raw pointer.
         */
        ghoul::opengl::Texture* getTextureIfFree();

        const TileTextureInitData& tileTextureInitData() const;

        /**
         * \return the number of textures in this TextureContainer
         */
        size_t size() const;

    private:
        std::vector<std::unique_ptr<ghoul::opengl::Texture>> _textures;

        const TileTextureInitData _initData;
        size_t _freeTexture = 0;
        size_t _numTextures;
    };


    void createDefaultTextureContainers();
    void assureTextureContainerExists(const TileTextureInitData& initData);
    void resetTextureContainerSize(size_t numTexturesPerTextureType);

    using TileCache = LRUCache<ProviderTileKey, Tile, ProviderTileHasher>;
    using TextureContainerTileCache = std::pair<
        std::unique_ptr<TextureContainer>,
        std::unique_ptr<TileCache>
    >;
    using TextureContainerMap = std::unordered_map<
        TileTextureInitData::HashKey,
        TextureContainerTileCache
    >;

    TextureContainerMap _textureContainerMap;
    size_t _numTextureBytesAllocatedOnCPU;

    // Properties
    properties::IntProperty _cpuAllocatedTileData;
    properties::IntProperty _gpuAllocatedTileData;
    properties::IntProperty _tileCacheSize;
    properties::TriggerProperty _applyTileCacheSize;
    properties::TriggerProperty _clearTileCache;
};

} // namespace openspace::globebrowsing::cache

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___MEMORY_AWARE_TILE_CACHE___H__
