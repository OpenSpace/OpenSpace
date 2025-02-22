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

#include <modules/globebrowsing/src/tileprovider/tileprovider.h>

#include <modules/globebrowsing/globebrowsingmodule.h>
#include <modules/globebrowsing/src/asynctiledataprovider.h>
#include <modules/globebrowsing/src/geodeticpatch.h>
#include <modules/globebrowsing/src/layermanager.h>
#include <modules/globebrowsing/src/memoryawaretilecache.h>
#include <modules/globebrowsing/src/rawtiledatareader.h>
#include <modules/globebrowsing/src/tileprovider/defaulttileprovider.h>
#include <modules/globebrowsing/src/tileprovider/imagesequencetileprovider.h>
#include <modules/globebrowsing/src/tileprovider/singleimagetileprovider.h>
#include <modules/globebrowsing/src/tileprovider/sizereferencetileprovider.h>
#include <modules/globebrowsing/src/tileprovider/temporaltileprovider.h>
#include <modules/globebrowsing/src/tileprovider/tileindextileprovider.h>
#include <modules/globebrowsing/src/tileprovider/tileproviderbyindex.h>
#include <modules/globebrowsing/src/tileprovider/tileproviderbylevel.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/memorymanager.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/timemanager.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/textureunit.h>
#include <filesystem>
#include <fstream>
#include "cpl_minixml.h"

namespace {
    std::unique_ptr<ghoul::opengl::Texture> DefaultTileTexture;
    openspace::globebrowsing::Tile DefaultTile = openspace::globebrowsing::Tile {
        nullptr,
        std::nullopt,
        openspace::globebrowsing::Tile::Status::Unavailable
    };
} // namespace

namespace openspace::globebrowsing {

unsigned int TileProvider::NumTileProviders = 0;

std::unique_ptr<TileProvider> TileProvider::createFromDictionary(
                                                      const ghoul::Dictionary& dictionary)
{
    ZoneScoped;

    layers::Layer::ID layerTypeID = layers::Layer::ID::DefaultTileProvider;
    if (dictionary.hasValue<std::string>("Type")) {
        const std::string type = dictionary.value<std::string>("Type");
        layerTypeID = ghoul::from_string<layers::Layer::ID>(type);
    }

    const std::string_view type =
        layers::Layers[static_cast<int>(layerTypeID)].identifier;

    ghoul::TemplateFactory<TileProvider>* factory =
        FactoryManager::ref().factory<TileProvider>();
    ghoul_assert(factory, "TileProvider factory not created");

    TileProvider* result = factory->create(type, dictionary);
    return std::unique_ptr<TileProvider>(result);
}

void TileProvider::initializeDefaultTile() {
    ZoneScoped;

    ghoul_assert(!DefaultTile.texture, "Default tile should not have been created");
    using namespace ghoul::opengl;

    // Create pixel data
    const TileTextureInitData initData(
        8,
        8,
        GL_UNSIGNED_BYTE,
        Texture::Format::RGBA,
        TileTextureInitData::ShouldAllocateDataOnCPU::Yes
    );
    char* pixels = new char[initData.totalNumBytes];
    memset(pixels, 0, initData.totalNumBytes * sizeof(char));

    // Create ghoul texture
    DefaultTileTexture = std::make_unique<Texture>(initData.dimensions, GL_TEXTURE_2D);
    DefaultTileTexture->setDataOwnership(Texture::TakeOwnership::Yes);
    DefaultTileTexture->setPixelData(pixels);
    DefaultTileTexture->uploadTexture();
    DefaultTileTexture->setFilter(ghoul::opengl::Texture::FilterMode::LinearMipMap);

    // Create tile
    DefaultTile = Tile{ DefaultTileTexture.get(), std::nullopt, Tile::Status::OK };
}

void TileProvider::deinitializeDefaultTile() {
    DefaultTileTexture = nullptr;
}

TileProvider::TileProvider()
    : properties::PropertyOwner({ "TileProvider", "Tile Provider" })
{}

void TileProvider::initialize() {
    ZoneScoped;

    ghoul_assert(!isInitialized, "TileProvider can only be initialized once");

    if (TileProvider::NumTileProviders >
        static_cast<unsigned int>(std::numeric_limits<uint16_t>::max()) - 1)
    {
        LERRORC(
            "TileProvider",
            "Number of tile providers exceeds 65535. Something will break soon"
        );
        TileProvider::NumTileProviders = 0;
    }
    uniqueIdentifier = static_cast<uint16_t>(TileProvider::NumTileProviders++);
    if (TileProvider::NumTileProviders == std::numeric_limits<unsigned int>::max()) {
        --TileProvider::NumTileProviders;
        return;
    }

    internalInitialize();
    isInitialized = true;
}

void TileProvider::deinitialize() {
    ZoneScoped;

    internalDeinitialize();
}

ChunkTile TileProvider::traverseTree(TileIndex tileIndex, int parents, int maxParents,
                  const std::function<void(TileIndex&, TileUvTransform&)>& ascendToParent,
                                                             TileUvTransform& uvTransform)
{
    // Step 1. Traverse 0 or more parents up the chunkTree as requested by the caller
    for (int i = 0; i < parents && tileIndex.level > 1; i++) {
        ascendToParent(tileIndex, uvTransform);
    }
    maxParents -= parents;

    // Step 2. Traverse 0 or more parents up the chunkTree to make sure we're inside
    //         the range of defined data.
    const int maximumLevel = maxLevel();
    while (tileIndex.level > maximumLevel) {
        ascendToParent(tileIndex, uvTransform);
        maxParents--;
    }
    if (maxParents < 0) {
        return ChunkTile{ Tile(), uvTransform, TileDepthTransform() };
    }

    // Step 3. Traverse 0 or more parents up the chunkTree until we find a chunk that
    //         has a loaded tile ready to use.
    const int minimumLevel = minLevel();
    while (tileIndex.level > minimumLevel) {
        Tile t = tile(tileIndex);
        if (t.status != Tile::Status::OK) {
            if (--maxParents < 0) {
                return ChunkTile{ Tile(), uvTransform, TileDepthTransform() };
            }
            ascendToParent(tileIndex, uvTransform);
        }
        else {
            return ChunkTile{ std::move(t), uvTransform, TileDepthTransform() };
        }
    }

    return ChunkTile{ Tile(), uvTransform, TileDepthTransform() };
}

void TileProvider::internalInitialize() {}
void TileProvider::internalDeinitialize() {}

ChunkTile TileProvider::chunkTile(TileIndex tileIndex, int parents, int maxParents) {
    ZoneScoped;

    ghoul_assert(isInitialized, "TileProvider was not initialized");

    constexpr auto ascendToParent = [](TileIndex& ti, TileUvTransform& uv) {
        uv.uvOffset *= 0.5;
        uv.uvScale *= 0.5;

        uv.uvOffset += ti.positionRelativeParent();

        ti.x /= 2;
        ti.y /= 2;
        ti.level--;
    };

    TileUvTransform uvTransform = {
       .uvOffset = glm::vec2(0.f, 0.f),
       .uvScale = glm::vec2(1.f, 1.f)
    };

    return traverseTree(tileIndex, parents, maxParents, ascendToParent, uvTransform);
}

ChunkTilePile TileProvider::chunkTilePile(TileIndex tileIndex, int pileSize) {
    ZoneScoped;

    ghoul_assert(isInitialized, "TileProvider was not initialized");
    ghoul_assert(pileSize >= 0, "pileSize must be positive");

    ChunkTilePile chunkTilePile;
    std::fill(chunkTilePile.begin(), chunkTilePile.end(), std::nullopt);
    for (int i = 0; i < pileSize; i++) {
        chunkTilePile[i] = chunkTile(tileIndex, i);
        if (chunkTilePile[i]->tile.status == Tile::Status::Unavailable) {
            if (i == 0) {
                // First iteration
                chunkTilePile[i]->tile = DefaultTile;
                chunkTilePile[i]->uvTransform.uvOffset = glm::vec2(0.f, 0.f);
                chunkTilePile[i]->uvTransform.uvScale = glm::vec2(1.f, 1.f);
            }
            else {
                // We are iterating through the array one-by-one, so we are guaranteed
                // that for tile 'i', tile 'i-1' already was initializated
                chunkTilePile[i]->tile = chunkTilePile[i - 1]->tile;
                chunkTilePile[i]->uvTransform.uvOffset =
                    chunkTilePile[i - 1]->uvTransform.uvOffset;
                chunkTilePile[i]->uvTransform.uvScale =
                    chunkTilePile[i - 1]->uvTransform.uvScale;
            }
        }
    }
    return chunkTilePile;
}

} // namespace openspace::globebrowsing
