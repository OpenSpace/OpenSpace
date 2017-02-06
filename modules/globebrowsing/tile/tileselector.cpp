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

#include <modules/globebrowsing/tile/tileselector.h>

#include <modules/globebrowsing/rendering/layermanager.h>
#include <modules/globebrowsing/tile/tileprovider/tileprovider.h>

namespace openspace {
namespace globebrowsing {

ChunkTile TileSelector::getHighestResolutionTile(const LayerGroup& layerGroup,
                                                 TileIndex tileIndex)
{
    ChunkTile mostHighResolution;
    mostHighResolution.tile = Tile::TileUnavailable;
    mostHighResolution.uvTransform.uvScale.x = 0;

    for (const auto& layer : layerGroup.activeLayers()) {
        ChunkTile chunkTile = layer->tileProvider()->getChunkTile(tileIndex);
        bool tileIsOk = chunkTile.tile.status == Tile::Status::OK;
        bool tileHasMetaData = chunkTile.tile.metaData != nullptr;
        bool tileIsHigherResolution =
            chunkTile.uvTransform.uvScale.x > mostHighResolution.uvTransform.uvScale.x;
        if (tileIsOk && tileHasMetaData && tileIsHigherResolution) {
            mostHighResolution = chunkTile;
        }
    }

    return mostHighResolution;
}

std::vector<ChunkTile> TileSelector::getTilesSortedByHighestResolution(
                                                             const LayerGroup& layerGroup,
                                                               const TileIndex& tileIndex)
{
    std::vector<ChunkTile> tiles;
    for (const auto& layer : layerGroup.activeLayers()) {
        tiles.push_back(layer->tileProvider()->getChunkTile(tileIndex));
    }

    std::sort(
        tiles.begin(),
        tiles.end(),
        [](const ChunkTile& lhs, const ChunkTile& rhs) {
            return lhs.uvTransform.uvScale.x > rhs.uvTransform.uvScale.x;
        }
    );

    return tiles;
}

void TileSelector::ascendToParent(TileIndex& tileIndex, TileUvTransform& uv) {
    uv.uvOffset *= 0.5;
    uv.uvScale *= 0.5;

    uv.uvOffset += tileIndex.positionRelativeParent();

    --tileIndex;
}

} // namespace globebrowsing
} // namespace openspace
