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

#include <modules/globebrowsing/chunk/chunklevelevaluator/availabletiledataevaluator.h>

#include <modules/globebrowsing/chunk/chunk.h>
#include <modules/globebrowsing/globes/chunkedlodglobe.h>
#include <modules/globebrowsing/globes/renderableglobe.h>
#include <modules/globebrowsing/rendering/layer/layergroup.h>
#include <modules/globebrowsing/rendering/layer/layermanager.h>
#include <modules/globebrowsing/tile/tileprovider/tileprovider.h>

namespace openspace::globebrowsing::chunklevelevaluator {
    
int AvailableTileData::getDesiredLevel(const Chunk& chunk, const RenderData& data) const {
    auto layerManager = chunk.owner().chunkedLodGlobe()->layerManager();
    // auto layers = layerManager->layerGroup(LayerManager::HeightLayers).activeLayers();
    int currLevel = chunk.tileIndex().level;
        
    for (size_t i = 0; i < layergroupid::NUM_LAYER_GROUPS; ++i) {
        for (const auto& layer : layerManager->layerGroup(i).activeLayers()) {
            Tile::Status status = layer->getTileStatus(chunk.tileIndex());
            if (status == Tile::Status::OK) {
                return UnknownDesiredLevel;
            }
        }
    }

    return currLevel - 1;
}

} // namespace openspace::globebrowsing::chunklevelevaluator
