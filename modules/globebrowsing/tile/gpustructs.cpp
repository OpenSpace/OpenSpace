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

#include <modules/globebrowsing/tile/gpustructs.h>

#include <modules/globebrowsing/tile/chunktile.h>
#include <modules/globebrowsing/tile/tileselector.h>
#include <modules/globebrowsing/tile/tileselector.h>
#include <modules/globebrowsing/tile/tileprovider/tileprovider.h>

#include <modules/globebrowsing/layered_rendering/layeredtextures.h>

#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/assert.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/opengl/programobject.h>

#include <string>
#include <memory>

namespace {
    const std::string _loggerCat = "GPUStructs";
}

namespace openspace {
namespace globebrowsing {

    // TileUvTransform

    void GPUTileUvTransform::setValue(ProgramObject* programObject, const TileUvTransform& tileUvTransform){
        gpuUvOffset.setValue(programObject, tileUvTransform.uvOffset);
        gpuUvScale.setValue(programObject, tileUvTransform.uvScale);
    }

    void GPUTileUvTransform::updateUniformLocations(ProgramObject* programObject, const std::string& nameBase){
        gpuUvOffset.updateUniformLocations(programObject, nameBase + "uvOffset");
        gpuUvScale.updateUniformLocations(programObject, nameBase + "uvScale");
    }


    // TileDepthTransform

    void GPUTileDepthTransform::setValue(ProgramObject* programObject, const TileDepthTransform& depthTransform){
        gpuDepthOffset.setValue(programObject, depthTransform.depthOffset);
        gpuDepthScale.setValue(programObject, depthTransform.depthScale);
    }

    void GPUTileDepthTransform::updateUniformLocations(ProgramObject* programObject, const std::string& nameBase){
        gpuDepthOffset.updateUniformLocations(programObject, nameBase + "depthOffset");
        gpuDepthScale.updateUniformLocations(programObject, nameBase + "depthScale");
    }


    // ChunkTile

    void GPUChunkTile::setValue(ProgramObject* programObject, const ChunkTile& chunkTile){
        gpuTexture.setValue(programObject, chunkTile.tile.texture);
        gpuTileUvTransform.setValue(programObject, chunkTile.uvTransform);
    }

    void GPUChunkTile::updateUniformLocations(ProgramObject* programObject, const std::string& nameBase){
        gpuTexture.updateUniformLocations(programObject, nameBase + "textureSampler");
        gpuTileUvTransform.updateUniformLocations(programObject, nameBase + "uvTransform.");
    }


    // ChunkTilePile
    
    void GPUChunkTilePile::setValue(ProgramObject* programObject, const ChunkTilePile& chunkTilePile){
        for (size_t i = 0; i < ChunkTilePile::SIZE; ++i){
            gpuChunkTilePile[i].setValue(programObject, chunkTilePile.chunkTiles[i]);
        }
    }

    void GPUChunkTilePile::updateUniformLocations(ProgramObject* programObject, const std::string& nameBase){
        for (size_t i = 0; i < ChunkTilePile::SIZE; ++i){
            std::string nameExtension = "chunkTile" + std::to_string(i) + ".";
            gpuChunkTilePile[i].updateUniformLocations(programObject, nameBase + nameExtension);
        }
    }



}  // namespace globebrowsing
}  // namespace openspace
