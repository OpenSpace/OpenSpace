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

#include <modules/globebrowsing/tile/tileselector.h>
#include <modules/globebrowsing/tile/tileprovider/tileprovider.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>

#include <sstream>
#include <algorithm>

#include "gdal_priv.h"

namespace {
    const std::string _loggerCat = "TileSelector";
}

namespace openspace {
namespace globebrowsing {

    const TileSelector::CompareResolution TileSelector::HIGHEST_RES = TileSelector::CompareResolution();

    ChunkTile TileSelector::getHighestResolutionTile(const LayerGroup& layerGroup, TileIndex tileIndex) {
        ChunkTile mostHighResolution;
        mostHighResolution.tile = Tile::TileUnavailable;
        mostHighResolution.uvTransform.uvScale.x = 0;

        for (const auto& layer : layerGroup.activeLayers()) {
            ChunkTile chunkTile = layer->tileProvider()->getChunkTile(tileIndex);
            bool tileIsOk = chunkTile.tile.status == Tile::Status::OK;
            bool tileHasMetaData = chunkTile.tile.metaData != nullptr;
            bool tileIsHigherResolution = chunkTile.uvTransform.uvScale.x > mostHighResolution.uvTransform.uvScale.x;
            if (tileIsOk && tileHasMetaData && tileIsHigherResolution) {
                mostHighResolution = chunkTile;
            }
        }

        return mostHighResolution;
    }

    bool TileSelector::CompareResolution::operator()(const ChunkTile& a, const ChunkTile& b) {
        // large uv scale means smaller resolution
        return a.uvTransform.uvScale.x > b.uvTransform.uvScale.x;
    }

    std::vector<ChunkTile> TileSelector::getTilesSortedByHighestResolution(const LayerGroup& layerGroup, const TileIndex& tileIndex) {
        std::vector<ChunkTile> tiles;
        for (const auto& layer : layerGroup.activeLayers()) {
            tiles.push_back(layer->tileProvider()->getChunkTile(tileIndex));
        }

        std::sort(tiles.begin(), tiles.end(), TileSelector::HIGHEST_RES);

        return tiles;
    }

    void TileSelector::ascendToParent(TileIndex& tileIndex, TileUvTransform& uv) {
        uv.uvOffset *= 0.5;
        uv.uvScale *= 0.5;

        if (tileIndex.isEastChild()) {
            uv.uvOffset.x += 0.5;
        }

        // In OpenGL, positive y direction is up
        if (tileIndex.isNorthChild()) {
            uv.uvOffset.y += 0.5;
        }

        --tileIndex;
    }

} // namespace globebrowsing
} // namespace openspace
