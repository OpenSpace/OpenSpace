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

#include <modules/globebrowsing/rendering/gpu/gpuchunktilepile.h>


namespace openspace {
namespace globebrowsing {

void GPUChunkTilePile::setValue(ProgramObject* programObject,
                                const ChunkTilePile& chunkTilePile)
{
    ghoul_assert(
        gpuChunkTiles.size() == chunkTilePile.size(),
        "GPU and CPU ChunkTilePile must have same size!"
    );
    for (size_t i = 0; i < gpuChunkTiles.size(); ++i) {
        gpuChunkTiles[i].setValue(programObject, chunkTilePile[i]);
    }
}

void GPUChunkTilePile::bind(ProgramObject* programObject, const std::string& nameBase, 
                            int pileSize)
{
    gpuChunkTiles.resize(pileSize);
    for (size_t i = 0; i < gpuChunkTiles.size(); ++i) {
        std::string nameExtension = "chunkTile" + std::to_string(i) + ".";
        gpuChunkTiles[i].bind(programObject, nameBase + nameExtension);
    }
}

void GPUChunkTilePile::deactivate() {
    for (auto& gpuChunkTile : gpuChunkTiles) {
        gpuChunkTile.deactivate();
    }
}

}  // namespace globebrowsing
}  // namespace openspace
