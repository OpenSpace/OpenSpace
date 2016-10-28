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

#include <modules/globebrowsing/tile/tileIndex.h>
#include <modules/globebrowsing/tile/chunktile.h>
#include <modules/globebrowsing/tile/tileselector.h>
#include <modules/globebrowsing/tile/tileprovider/tileprovider.h>

#include <modules/globebrowsing/rendering/gpulayermanager.h>
#include <modules/globebrowsing/rendering/layermanager.h>

#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/assert.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/opengl/programobject.h>

#include <string>
#include <memory>

namespace {
    const std::string _loggerCat = "GPULayerManager";
}

namespace openspace {
namespace globebrowsing {


// TileUvTransform

void GPUTileUvTransform::setValue(ProgramObject* programObject, 
                                  const TileUvTransform& tileUvTransform){
    gpuUvOffset.setValue(programObject, tileUvTransform.uvOffset);
    gpuUvScale.setValue(programObject, tileUvTransform.uvScale);
}

void GPUTileUvTransform::bind(ProgramObject* programObject, const std::string& nameBase) {
    gpuUvOffset.bind(programObject, nameBase + "uvOffset");
    gpuUvScale.bind(programObject, nameBase + "uvScale");
}


// TileDepthTransform

void GPUTileDepthTransform::setValue(ProgramObject* programObject, 
                                     const TileDepthTransform& depthTransform){
    gpuDepthOffset.setValue(programObject, depthTransform.depthOffset);
    gpuDepthScale.setValue(programObject, depthTransform.depthScale);
}

void GPUTileDepthTransform::bind(ProgramObject* programObject, const std::string& nameBase){
    gpuDepthOffset.bind(programObject, nameBase + "depthOffset");
    gpuDepthScale.bind(programObject, nameBase + "depthScale");
}


// ChunkTile

void GPUChunkTile::setValue(ProgramObject* programObject, const ChunkTile& chunkTile){
    gpuTexture.setValue(programObject, chunkTile.tile.texture);
    gpuTileUvTransform.setValue(programObject, chunkTile.uvTransform);
}

void GPUChunkTile::bind(ProgramObject* programObject, const std::string& nameBase){
    gpuTexture.bind(programObject, nameBase + "textureSampler");
    gpuTileUvTransform.bind(programObject, nameBase + "uvTransform.");
}

void GPUChunkTile::deactivate(){
    gpuTexture.deactivate();
}


// ChunkTilePile

void GPUChunkTilePile::setValue(ProgramObject* programObject, const ChunkTilePile& chunkTilePile){
    ghoul_assert(gpuChunkTiles.size() == chunkTilePile.chunkTiles.size(), "GPU and CPU ChunkTilePile must have same size!");
    for (size_t i = 0; i < gpuChunkTiles.size(); ++i){
        gpuChunkTiles[i].setValue(programObject, chunkTilePile.chunkTiles[i]);
    }
}

void GPUChunkTilePile::bind(ProgramObject* programObject, const std::string& nameBase, 
                            int pileSize){
    gpuChunkTiles.resize(pileSize);
    for (size_t i = 0; i < gpuChunkTiles.size(); ++i){
        std::string nameExtension = "chunkTile" + std::to_string(i) + ".";
        gpuChunkTiles[i].bind(programObject, nameBase + nameExtension);
    }
}

void GPUChunkTilePile::deactivate(){
    for (auto& gpuChunkTile : gpuChunkTiles){
        gpuChunkTile.deactivate();
    }
}


void GPULayerRenderSettings::setValue(ProgramObject* programObject, 
                                      const LayerRenderSettings& layerSettings){
    gpuOpacity.setValue(programObject, layerSettings.opacity.value());
    gpuGamma.setValue(programObject, layerSettings.gamma.value());
    gpuMultiplier.setValue(programObject, layerSettings.multiplier.value());
}

void GPULayerRenderSettings::bind(ProgramObject* programObject, const std::string& nameBase){
    gpuOpacity.bind(programObject, nameBase + "opacity");
    gpuGamma.bind(programObject, nameBase + "gamma");
    gpuMultiplier.bind(programObject, nameBase + "multiplier");
}



// Layer

void GPULayer::setValue(ProgramObject* programObject, const Layer& layer, 
                        const TileIndex& tileIndex, int pileSize){
    ChunkTilePile chunkTilePile = layer.getChunkTilePile(tileIndex, pileSize);
    gpuChunkTilePile.setValue(programObject, chunkTilePile);
    gpuRenderSettings.setValue(programObject, layer.renderSettings());
}

void GPULayer::bind(ProgramObject* programObject, const Layer& layer, 
                    const std::string& nameBase,  int pileSize){
    gpuChunkTilePile.bind(programObject, nameBase + "pile.", pileSize);
    gpuRenderSettings.bind(programObject, nameBase + "settings.");
}

void GPULayer::deactivate(){
    gpuChunkTilePile.deactivate();
}


// Override behavior for HeightLayer

void GPUHeightLayer::setValue(ProgramObject* programObject, const Layer& layer, 
                              const TileIndex& tileIndex, int pileSize){
    GPULayer::setValue(programObject, layer, tileIndex, pileSize);
    gpuDepthTransform.setValue(programObject, layer.tileProvider()->depthTransform());
}

void GPUHeightLayer::bind(ProgramObject* programObject, const Layer& layer, const std::string& nameBase, int pileSize){
    GPULayer::bind(programObject, layer, nameBase, pileSize);
    gpuDepthTransform.bind(programObject, nameBase + "depthTransform.");
}



// LayerGroup

void GPULayerGroup::setValue(ProgramObject* programObject, const LayerGroup& layerGroup, 
                             const TileIndex& tileIndex){
    auto& activeLayers = layerGroup.activeLayers();
    ghoul_assert(activeLayers.size() == gpuActiveLayers.size(), "GPU and CPU active layers must have same size!");
    for (int i = 0; i < activeLayers.size(); ++i){
        gpuActiveLayers[i]->setValue(programObject, *activeLayers[i], tileIndex, layerGroup.pileSize());
    }
}

void GPULayerGroup::bind(ProgramObject* programObject, const LayerGroup& layerGroup, 
                         const std::string& nameBase, int category){
    auto activeLayers = layerGroup.activeLayers();
    gpuActiveLayers.resize(activeLayers.size());
    int pileSize = layerGroup.pileSize();
    for (size_t i = 0; i < gpuActiveLayers.size(); ++i){
        // should maybe a proper GPULayer factory
        gpuActiveLayers[i] = category == LayerManager::HeightLayers ?
            std::make_unique<GPUHeightLayer>() : 
            std::make_unique<GPULayer>();
        std::string nameExtension = "[" + std::to_string(i) + "].";
        gpuActiveLayers[i]->bind(programObject, *activeLayers[i], nameBase + nameExtension, pileSize);
    }
}

void GPULayerGroup::deactivate(){
    for (size_t i = 0; i < gpuActiveLayers.size(); ++i){
        gpuActiveLayers[i]->deactivate();
    }
}


// LayerManager
    
void GPULayerManager::setValue(ProgramObject* programObject, const LayerManager& layerManager, 
                               const TileIndex& tileIndex){
    auto layerGroups = layerManager.layerGroups();
    for (size_t i = 0; i < layerGroups.size(); ++i){
        gpuLayerGroups[i]->setValue(programObject, *layerGroups[i], tileIndex);
    }
}

void GPULayerManager::bind(ProgramObject* programObject, const LayerManager& layerManager){
    auto layerGroups = layerManager.layerGroups();
    if(gpuLayerGroups.size() != layerGroups.size()){
        gpuLayerGroups.resize(layerGroups.size());
        for(auto& gpuLayerGroup : gpuLayerGroups){
            gpuLayerGroup = std::make_unique<GPULayerGroup>();
        }
    }
    
    for (size_t i = 0; i < layerGroups.size(); ++i){
        std::string nameBase = LayerManager::LAYER_GROUP_NAMES[i];
        gpuLayerGroups[i]->bind(programObject, *layerGroups[i], nameBase, i);
    }
}

void GPULayerManager::deactivate(){
    for (size_t i = 0; i < gpuLayerGroups.size(); ++i){
        gpuLayerGroups[i]->deactivate();
    }
}

}  // namespace globebrowsing
}  // namespace openspace
