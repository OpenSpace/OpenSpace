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

#include <modules/globebrowsing/tile/tileprovider/tileprovider.h>

#include <modules/globebrowsing/tile/chunktile.h>
#include <modules/globebrowsing/tile/tileselector.h>
#include <modules/globebrowsing/tile/tiletextureinitdata.h>
#include <modules/globebrowsing/tile/tileindex.h>
#include <openspace/util/factorymanager.h>
#include <ghoul/misc/templatefactory.h>
#include <ghoul/logging/logmanager.h>

namespace openspace::globebrowsing::tileprovider {

unsigned int TileProvider::_numTileProviders = 0;

std::unique_ptr<TileProvider> TileProvider::createFromDictionary(
                                                         layergroupid::TypeID layerTypeID,
                                                      const ghoul::Dictionary& dictionary)
{
    const char* type = layergroupid::LAYER_TYPE_NAMES[static_cast<int>(layerTypeID)];
    auto factory = FactoryManager::ref().factory<TileProvider>();
    std::unique_ptr<TileProvider> result = factory->create(type, dictionary);
    return result;
}

TileProvider::TileProvider(const ghoul::Dictionary&)
    : properties::PropertyOwner({ "tileProvider" })
{}

TileProvider::~TileProvider() {} // NOLINT

bool TileProvider::initialize() {
    ghoul_assert(!_isInitialized, "TileProvider can only be initialized once.");

    initializeDefaultTile();

    _uniqueIdentifier = _numTileProviders;
    _numTileProviders++;
    if (_numTileProviders == std::numeric_limits<unsigned int>::max()) {
        _numTileProviders--;
        return false;
    }

    _isInitialized = true;
    return true;
}

bool TileProvider::deinitialize() {
    return true;
}

float TileProvider::noDataValueAsFloat() {
    ghoul_assert(_isInitialized, "TileProvider was not initialized.");
    return std::numeric_limits<float>::min();
}

ChunkTile TileProvider::chunkTile(TileIndex tileIndex, int parents, int maxParents) {
    ghoul_assert(_isInitialized, "TileProvider was not initialized.");
    TileUvTransform uvTransform = {
        glm::vec2(0.f, 0.f),
        glm::vec2(1.f, 1.f)
    };

    // Step 1. Traverse 0 or more parents up the chunkTree as requested by the caller
    for (int i = 0; i < parents && tileIndex.level > 1; i++) {
        tileselector::ascendToParent(tileIndex, uvTransform);
    }
    maxParents -= parents;

    // Step 2. Traverse 0 or more parents up the chunkTree to make sure we're inside
    //         the range of defined data.
    int maximumLevel = maxLevel();
    while (tileIndex.level > maximumLevel) {
        tileselector::ascendToParent(tileIndex, uvTransform);
        maxParents--;
    }
    if (maxParents < 0) {
        return ChunkTile{ Tile::TileUnavailable, uvTransform, TileDepthTransform() };
    }

    // Step 3. Traverse 0 or more parents up the chunkTree until we find a chunk that
    //         has a loaded tile ready to use.
    while (tileIndex.level > 1) {
        Tile t = tile(tileIndex);
        if (t.status() != Tile::Status::OK) {
            if (--maxParents < 0) {
                return ChunkTile {
                    Tile::TileUnavailable,
                    uvTransform,
                    TileDepthTransform()
                };
            }
            tileselector::ascendToParent(tileIndex, uvTransform);
        }
        else {
            return ChunkTile { std::move(t), uvTransform, TileDepthTransform() };
        }
    }

    return ChunkTile{ Tile::TileUnavailable, uvTransform, TileDepthTransform() };
}

ChunkTilePile TileProvider::chunkTilePile(TileIndex tileIndex, int pileSize) {
    ghoul_assert(_isInitialized, "TileProvider was not initialized.");
    ghoul_assert(pileSize >= 0, "pileSize must be positive");

    ChunkTilePile chunkTilePile(pileSize);
    for (int i = 0; i < pileSize; ++i) {
        chunkTilePile[i] = chunkTile(tileIndex, i);
        if (chunkTilePile[i].tile.status() == Tile::Status::Unavailable) {
            if (i > 0) {
                chunkTilePile[i].tile = chunkTilePile[i-1].tile;
                chunkTilePile[i].uvTransform.uvOffset =
                    chunkTilePile[i-1].uvTransform.uvOffset;
                chunkTilePile[i].uvTransform.uvScale =
                    chunkTilePile[i-1].uvTransform.uvScale;
            }
            else {
                chunkTilePile[i].tile = defaultTile();
                chunkTilePile[i].uvTransform.uvOffset = { 0.f, 0.f };
                chunkTilePile[i].uvTransform.uvScale = { 1.f, 1.f };
            }
        }
    }
    return chunkTilePile;
}

void TileProvider::initializeDefaultTile() {
    ghoul_assert(!_defaultTile.texture(), "Default tile should not have been created");
    using namespace ghoul::opengl;

    // Create pixel data
    TileTextureInitData initData(
        8,
        8,
        GL_UNSIGNED_BYTE,
        Texture::Format::RGBA,
        TileTextureInitData::PadTiles::No,
        TileTextureInitData::ShouldAllocateDataOnCPU::Yes
    );
    const size_t numBytes = initData.totalNumBytes();
    char* pixels = new char[numBytes];
    memset(pixels, 0, numBytes * sizeof(char));

    // Create ghoul texture
    _defaultTileTexture = std::make_unique<Texture>(initData.dimensions());
    _defaultTileTexture->setDataOwnership(Texture::TakeOwnership::Yes);
    _defaultTileTexture->setPixelData(pixels);
    _defaultTileTexture->uploadTexture();
    _defaultTileTexture->setFilter(ghoul::opengl::Texture::FilterMode::LinearMipMap);

    // Create tile
    _defaultTile = Tile(_defaultTileTexture.get(), nullptr, Tile::Status::OK);
}

unsigned int TileProvider::uniqueIdentifier() const {
    ghoul_assert(_isInitialized, "TileProvider was not initialized.");

    return _uniqueIdentifier;
}

Tile TileProvider::defaultTile() const {
    return _defaultTile;
}

} // namespace openspace::globebrowsing::tileprovider
