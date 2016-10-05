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

#include <sstream>
#include <algorithm>

#include "gdal_priv.h"



namespace {
    const std::string _loggerCat = "TileSelector";
}


namespace openspace {

    const TileSelector::CompareResolution TileSelector::HIGHEST_RES = TileSelector::CompareResolution();

    TileAndTransform TileSelector::getHighestResolutionTile(TileProvider* tileProvider, TileIndex tileIndex, int parents) {
        TileUvTransform uvTransform;
        uvTransform.uvOffset = glm::vec2(0, 0);
        uvTransform.uvScale = glm::vec2(1, 1);

        // Step 1. Traverse 0 or more parents up the chunkTree as requested by the caller
        for (int i = 0; i < parents && tileIndex.level > 1; i++) {
            ascendToParent(tileIndex, uvTransform);
        }

        // Step 2. Traverse 0 or more parents up the chunkTree to make sure we're inside 
        //         the range of defined data.
        int maximumLevel = tileProvider->maxLevel();
        while(tileIndex.level > maximumLevel){
            ascendToParent(tileIndex, uvTransform);
        }
        
        // Step 3. Traverse 0 or more parents up the chunkTree until we find a chunk that 
        //         has a loaded tile ready to use. 
        while (tileIndex.level > 1) {
            Tile tile = tileProvider->getTile(tileIndex);
            if (tile.status != Tile::Status::OK) {
                ascendToParent(tileIndex, uvTransform);
            }
            else return { tile, uvTransform };
        }
        
        return{ Tile::TileUnavailable, uvTransform };
    }

    TileAndTransform TileSelector::getHighestResolutionTile(const TileProviderGroup& tileProviderGroup, TileIndex tileIndex) {
        TileAndTransform mostHighResolution;
        mostHighResolution.tile = Tile::TileUnavailable;
        mostHighResolution.uvTransform.uvScale.x = 0;

        auto activeProviders = tileProviderGroup.getActiveTileProviders();
        for (size_t i = 0; i < activeProviders.size(); i++) {
            TileAndTransform tileAndTransform = getHighestResolutionTile(activeProviders[i].get(), tileIndex);
            bool tileIsOk = tileAndTransform.tile.status == Tile::Status::OK;
            bool tileHasPreprocessData = tileAndTransform.tile.preprocessData != nullptr;
            bool tileIsHigherResolution = tileAndTransform.uvTransform.uvScale.x > mostHighResolution.uvTransform.uvScale.x;
            if (tileIsOk && tileHasPreprocessData && tileIsHigherResolution) {
                mostHighResolution = tileAndTransform;
            }
        }

        return mostHighResolution;
    }

    bool TileSelector::CompareResolution::operator()(const TileAndTransform& a, const TileAndTransform& b) {
        // large uv scale means smaller resolution
        return a.uvTransform.uvScale.x > b.uvTransform.uvScale.x;
    }

    std::vector<TileAndTransform> TileSelector::getTilesSortedByHighestResolution(const TileProviderGroup& tileProviderGroup, const TileIndex& tileIndex) {
        auto activeProviders = tileProviderGroup.getActiveTileProviders();
        std::vector<TileAndTransform> tiles;
        for (auto provider : activeProviders){
            tiles.push_back(getHighestResolutionTile(provider.get(), tileIndex));
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

}  // namespace openspace
