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



#include <ghoul/logging/logmanager.h>

#include <sstream>




namespace {
    const std::string _loggerCat = "TileSelector";
}


namespace openspace {

    TileAndTransform TileSelector::getHighestResolutionTile(TileProvider* tileProvider, ChunkIndex chunkIndex, int parents) {
        TileUvTransform uvTransform;
        uvTransform.uvOffset = glm::vec2(0, 0);
        uvTransform.uvScale = glm::vec2(1, 1);

        // Step 1. Traverse 0 or more parents up the chunkTree as requested by the caller
        for (int i = 0; i < parents && chunkIndex.level > 1; i++) {
            ascendToParent(chunkIndex, uvTransform);
        }

        // Step 2. Traverse 0 or more parents up the chunkTree to make sure we're inside 
        //         the range of defined data.
        int maximumLevel = tileProvider->getAsyncTileReader()->getTextureDataProvider()->getMaximumLevel();
        while(chunkIndex.level > maximumLevel){
            ascendToParent(chunkIndex, uvTransform);
        }
        
        // Step 3. Traverse 0 or more parents up the chunkTree until we find a chunk that 
        //         has a loaded tile ready to use. 
        while (chunkIndex.level > 1) {
            Tile tile = tileProvider->getTile(chunkIndex);
            if (tile.status != Tile::Status::OK) {
                ascendToParent(chunkIndex, uvTransform);
            }
            else return { tile, uvTransform };
        }
        return {Tile::TileUnavailable, uvTransform};
    }

    void TileSelector::ascendToParent(ChunkIndex& chunkIndex, TileUvTransform& uv) {
        uv.uvOffset *= 0.5;
        uv.uvScale *= 0.5;

        if (chunkIndex.isEastChild()) {
            uv.uvOffset.x += 0.5;
        }

        // In OpenGL, positive y direction is up
        if (chunkIndex.isNorthChild()) {
            uv.uvOffset.y += 0.5;
        }

        --chunkIndex;
    }

}  // namespace openspace
