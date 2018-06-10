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

#include <modules/globebrowsing/rendering/gpu/gpuchunktilepile.h>

#include <openspace/util/gpudata.h>

namespace openspace::globebrowsing {

void GPUChunkTilePile::setValue(ghoul::opengl::ProgramObject* programObject,
                                const ChunkTilePile& chunkTilePile)
{
    ghoul_assert(
        _gpuChunkTiles.size() == chunkTilePile.size(),
        "GPU and CPU ChunkTilePile must have same size!"
    );
    for (size_t i = 0; i < _gpuChunkTiles.size(); ++i) {
        _gpuChunkTiles[i].setValue(programObject, chunkTilePile[i]);
    }
}

void GPUChunkTilePile::bind(ghoul::opengl::ProgramObject* programObject,
                            const std::string& nameBase, int pileSize)
{
    _gpuChunkTiles.resize(pileSize);
    for (size_t i = 0; i < _gpuChunkTiles.size(); ++i) {
        std::string nameExtension = "chunkTile" + std::to_string(i) + ".";
        _gpuChunkTiles[i].bind(programObject, nameBase + nameExtension);
    }
}

void GPUChunkTilePile::deactivate() {
    for (GPUChunkTile& t : _gpuChunkTiles) {
        t.deactivate();
    }
}

}  // namespace openspace::globebrowsing
