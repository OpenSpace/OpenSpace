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

#include <modules/globebrowsing/tile/tileselector.h>

#include <modules/globebrowsing/rendering/layer/layer.h>
#include <modules/globebrowsing/rendering/layer/layergroup.h>
#include <modules/globebrowsing/tile/tileindex.h>
#include <modules/globebrowsing/tile/tileprovider/tileprovider.h>

namespace openspace::globebrowsing::tileselector {

ChunkTile getHighestResolutionTile(const LayerGroup& layerGroup,
                                   const TileIndex& tileIndex)
{
    TileUvTransform uvTrans;
    uvTrans.uvScale.x = 0;
    ChunkTile mostHighResolution{ Tile::TileUnavailable, uvTrans, TileDepthTransform() };
    mostHighResolution.tile = Tile::TileUnavailable;

    for (const std::shared_ptr<Layer>& layer : layerGroup.activeLayers()) {
        const ChunkTile chunkTile = layer->tileProvider()->chunkTile(tileIndex);
        const bool tileIsOk = chunkTile.tile.status() == Tile::Status::OK;
        const bool tileHasMetaData = chunkTile.tile.metaData() != nullptr;
        const bool tileIsHigherResolution =
            chunkTile.uvTransform.uvScale.x > mostHighResolution.uvTransform.uvScale.x;

        if (tileIsOk && tileHasMetaData && tileIsHigherResolution) {
            mostHighResolution = chunkTile;
        }
    }

    return mostHighResolution;
}

std::vector<ChunkTile> getTilesSortedByHighestResolution(const LayerGroup& layerGroup,
                                                         const TileIndex& tileIndex)
{
    std::vector<ChunkTile> tiles;
    for (const std::shared_ptr<Layer>& layer : layerGroup.activeLayers()) {
        tiles.push_back(layer->tileProvider()->chunkTile(tileIndex));
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

std::vector<std::pair<ChunkTile, const LayerRenderSettings*>>
getTilesAndSettingsSortedByHighestResolution(const LayerGroup& layerGroup,
                                             const TileIndex& tileIndex)
{
    std::vector<std::pair<ChunkTile, const LayerRenderSettings*>> tilesAndSettings;
    for (const std::shared_ptr<Layer>& layer : layerGroup.activeLayers()) {
        tilesAndSettings.emplace_back(
            layer->tileProvider()->chunkTile(tileIndex),
            &layer->renderSettings()
        );
    }
    std::sort(
        tilesAndSettings.begin(),
        tilesAndSettings.end(),
        [](const std::pair<ChunkTile, const LayerRenderSettings*>& lhs,
           const std::pair<ChunkTile, const LayerRenderSettings*>& rhs)
        {
            return lhs.first.uvTransform.uvScale.x > rhs.first.uvTransform.uvScale.x;
        }
    );
    return tilesAndSettings;
}

std::vector<std::pair<ChunkTile, const LayerRenderSettings*>>
getTilesAndSettingsUnsorted(const LayerGroup& layerGroup, const TileIndex& tileIndex)
{
    std::vector<std::pair<ChunkTile, const LayerRenderSettings*> > tilesAndSettings;
    for (const std::shared_ptr<Layer>& layer : layerGroup.activeLayers()) {
        if (layer->tileProvider()) {
            tilesAndSettings.emplace_back(
                layer->tileProvider()->chunkTile(tileIndex),
                &layer->renderSettings()
            );
        }
    }
    std::reverse(tilesAndSettings.begin(), tilesAndSettings.end());
    return tilesAndSettings;
}

void ascendToParent(TileIndex& tileIndex, TileUvTransform& uv) {
    uv.uvOffset *= 0.5;
    uv.uvScale *= 0.5;

    uv.uvOffset += tileIndex.positionRelativeParent();

    --tileIndex;
}

} // namespace openspace::globebrowsing::tileselector
