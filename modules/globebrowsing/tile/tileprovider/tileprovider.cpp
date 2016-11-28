/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2016                                                               *
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

#include <limits>

#include <modules/globebrowsing/tile/tileprovider/tileprovider.h>

#include <openspace/util/factorymanager.h>
#include <modules/globebrowsing/tile/chunktile.h>
#include <modules/globebrowsing/tile/tileselector.h>

#include <ghoul/logging/logmanager.h>

namespace {
    const std::string _loggerCat = "TileProvider";

    const std::string KeyType = "Type";
}

namespace openspace {
namespace globebrowsing {

TileProvider* TileProvider::createFromDictionary(const ghoul::Dictionary& dictionary) {
    
    std::string type = "LRUCaching";
    dictionary.getValue(KeyType, type);
    ghoul::TemplateFactory<TileProvider>* factory
        = FactoryManager::ref().factory<TileProvider>();
    TileProvider* result = factory->create(type, dictionary);

    if (result == nullptr) {
        LERROR("Failed creating TileProvider of type '" << type << "'");
        return nullptr;
    }

    return result;
}

TileProvider::TileProvider(const ghoul::Dictionary& dictionary) { };

float TileProvider::noDataValueAsFloat() {
    return std::numeric_limits<float>::min();
}

ChunkTile TileProvider::getChunkTile(TileIndex tileIndex, int parents){
    TileUvTransform uvTransform;
    uvTransform.uvOffset = glm::vec2(0, 0);
    uvTransform.uvScale = glm::vec2(1, 1);

    // Step 1. Traverse 0 or more parents up the chunkTree as requested by the caller
    for (int i = 0; i < parents && tileIndex.level > 1; i++) {
        TileSelector::ascendToParent(tileIndex, uvTransform);
    }

    // Step 2. Traverse 0 or more parents up the chunkTree to make sure we're inside 
    //         the range of defined data.
    int maximumLevel = maxLevel();
    while(tileIndex.level > maximumLevel){
        TileSelector::ascendToParent(tileIndex, uvTransform);
    }
    
    // Step 3. Traverse 0 or more parents up the chunkTree until we find a chunk that 
    //         has a loaded tile ready to use. 
    while (tileIndex.level > 1) {
        Tile tile = getTile(tileIndex);
        if (tile.status != Tile::Status::OK) {
            TileSelector::ascendToParent(tileIndex, uvTransform);
        }
        else return { tile, uvTransform };
    }
    
    return{ Tile::TileUnavailable, uvTransform };
}

ChunkTilePile TileProvider::getChunkTilePile(TileIndex tileIndex, int pileSize){
    ghoul_assert(pileSize >= 0, "pileSize must be positive");
    ChunkTilePile chunkTilePile;
    chunkTilePile.chunkTiles.resize(pileSize);
    for (size_t i = 0; i < pileSize; ++i){
        chunkTilePile.chunkTiles[i] = getChunkTile(tileIndex, i);
        if (chunkTilePile.chunkTiles[i].tile.status == Tile::Status::Unavailable) {
            if(i>0){
                chunkTilePile.chunkTiles[i].tile = chunkTilePile.chunkTiles[i-1].tile;
                chunkTilePile.chunkTiles[i].uvTransform.uvOffset = chunkTilePile.chunkTiles[i-1].uvTransform.uvOffset;
                chunkTilePile.chunkTiles[i].uvTransform.uvScale = chunkTilePile.chunkTiles[i-1].uvTransform.uvScale;
            }
            else{
                chunkTilePile.chunkTiles[i].tile = getDefaultTile();
                chunkTilePile.chunkTiles[i].uvTransform.uvOffset = { 0, 0 };
                chunkTilePile.chunkTiles[i].uvTransform.uvScale = { 1, 1 };
            }
        }
    }
    return std::move(chunkTilePile);
}

} // namespace globebrowsing
} // namespace openspace
