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

#include <modules/globebrowsing/other/patchcoverageprovider.h>

#include <ghoul/logging/logmanager.h>

#include <glm/glm.hpp>

namespace {
    const std::string _loggerCat = "PatchCoverageProvider";
}

namespace openspace {

    PatchCoverageProvider::PatchCoverageProvider(
        Geodetic2 sizeLevel0,
        Geodetic2 offsetLevel0,
        int depth)
        : _sizeLevel0(sizeLevel0)
        , _offsetLevel0(offsetLevel0)
        , _depth(depth)
    {
    }

    PatchCoverageProvider::~PatchCoverageProvider(){

    }

    ChunkIndex PatchCoverageProvider::getChunkIndex(const GeodeticPatch& patch) {
        // Calculate the level of the index depending on the size of the incoming patch.
        // The level is as big as possible (as far down as possible) but it can't be
        // too big since at maximum four tiles should be used to cover a patch
        int level = log2(static_cast<int>(glm::max(
            _sizeLevel0.lat / (patch.size().lat),
            _sizeLevel0.lon / (patch.size().lon))));
        
        // If the depth is not big enough, the level must be clamped.
        level = glm::min(level, _depth);
        
        // Calculate the index in x y where the tile should be positioned
        int nIndices = pow(2, level);
        Geodetic2 tileSize = _sizeLevel0 / nIndices;
        Geodetic2 nw = patch.northWestCorner();
        glm::ivec2 chunkIndexXY =
            glm::floor((nw.toLonLatVec2() - _offsetLevel0.toLonLatVec2()) / tileSize.toLonLatVec2());

        // Flip y since indices increase from top to bottom
        chunkIndexXY.y = nIndices - 1 - chunkIndexXY.y;

        // Create the tileindex
        ChunkIndex chunkIndex = { chunkIndexXY.x, chunkIndexXY.y, level };
        return chunkIndex;
    }

    glm::mat3 PatchCoverageProvider::getUvTransformationPatchToTile(
        GeodeticPatch patch,
        const ChunkIndex& chunkIndex)
    {
        GeodeticPatch otherPatch(chunkIndex);
        return getUvTransformationPatchToTile(patch, otherPatch);
    }



    glm::mat3 PatchCoverageProvider::getUvTransformationPatchToTile(
        GeodeticPatch patch,
        GeodeticPatch tile)
    {
        Vec2 posDiff =
            patch.southWestCorner().toLonLatVec2() -
            tile.southWestCorner().toLonLatVec2();

        glm::mat3 invTileScale = glm::mat3(
        {	1 / (tile.size().lon),	        0,								0,
            0,								1 / (tile.size().lat),          0,
            0,								0,								1 });

        glm::mat3 globalTranslation = glm::mat3(
        {	1,			0,			0,
            0,			1,			0,
            posDiff.x,	posDiff.y,	1 });

        glm::mat3 patchScale = glm::mat3(
        {	(patch.halfSize().lon * 2),	0,								0,
            0,							(patch.halfSize().lat * 2),		0,
            0,							0,								1 });

        return invTileScale * globalTranslation * patchScale;
    }



    PatchCoverage PatchCoverageProvider::getCoverage(
        GeodeticPatch patch,
        std::shared_ptr<TileProvider> tileProvider)
    {
        static const int numTilesInCoverageX = 2;
        static const int numTilesInCoverageY = 2;
        PatchCoverage patchCoverageToReturn;

        Geodetic2 startPoint = patch.northWestCorner();
        int level = (int) patch.minimumTileLevel();
        ChunkIndex startIndex = ChunkIndex(startPoint, level);

        for (int y = 0; y < numTilesInCoverageY; y++)
        {
            for (int x = 0; x < numTilesInCoverageX; x++)
            {
                int linearIdx = x + y * numTilesInCoverageX;

                // May return negative indices
                ChunkIndex chunkIndex = getChunkIndex(patch);
                // Offset chunkIndex
                chunkIndex.x += x;
                chunkIndex.y += y;


                // If the tile index is negative or too big it needs to wrap around
                int nIndices = pow(2, chunkIndex.level);
                ChunkIndex positiveChunkIndex = ChunkIndex(chunkIndex);
                positiveChunkIndex.x += (chunkIndex.x < 0) ? nIndices : ((chunkIndex.x > nIndices - 1) ? -nIndices : 0);
                positiveChunkIndex.y += (chunkIndex.y < 0) ? nIndices : ((chunkIndex.y > nIndices - 1) ? -nIndices : 0);



                int numLevelsToLoop = chunkIndex.level;
                // Start at the highest level and go down if the texture don't exist
                for (int j = numLevelsToLoop; j >= 0; j--)
                {
                    // Try if the texture exists
                    std::shared_ptr<Texture> tile = tileProvider->getOrStartFetchingTile(positiveChunkIndex);
                    if (tile == nullptr)
                    { // If it doesn't exist, go down a level
                        chunkIndex.x /= 2;
                        chunkIndex.y /= 2;
                        chunkIndex.level -= 1;
                    }
                    else
                    { // A texture was found, put it in the data structure to return
                        patchCoverageToReturn.textureTransformPairs[linearIdx].first =
                            tile;
                        patchCoverageToReturn.textureTransformPairs[linearIdx].second =
                            getUvTransformationPatchToTile(patch, chunkIndex);
                    }
                }
                // If the texture still doesn't exist put a temporary texture
                if (patchCoverageToReturn.textureTransformPairs[linearIdx].first == nullptr)
                {
                    patchCoverageToReturn.textureTransformPairs[linearIdx].first =
                        tileProvider->getDefaultTexture();
                    patchCoverageToReturn.textureTransformPairs[linearIdx].second =
                        getUvTransformationPatchToTile(patch, chunkIndex);
                }
            }
        }
        
        return patchCoverageToReturn;
    }

}  // namespace openspace
