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

#ifndef __GPU_LAYER_MANAGER_H__
#define __GPU_LAYER_MANAGER_H__

#include <modules/globebrowsing/tile/tileindex.h>
#include <modules/globebrowsing/tile/chunktile.h>
#include <modules/globebrowsing/rendering/layermanager.h>

#include <openspace/util/gpudata.h>

#include <glm/glm.hpp>

#include <vector>

namespace ghoul{
namespace opengl{
    class ProgramObject;
}//namespace opengl
}//namespace ghoul

namespace openspace {
namespace globebrowsing {

using namespace ghoul::opengl;

class GPUTileUvTransform {
public:
	void setValue(ProgramObject* programObject, const TileUvTransform& uvTransform);
	void updateUniformLocations(
        ProgramObject* programObject,
        const std::string& nameBase);
	
private:
	GPUData<glm::vec2> gpuUvOffset;
	GPUData<glm::vec2> gpuUvScale;
};

class GPUTileDepthTransform {
public:
    void setValue(ProgramObject* programObject, const TileDepthTransform& depthTransform);
    void updateUniformLocations(
        ProgramObject* programObject,
        const std::string& nameBase);
    
private:
    GPUData<float> gpuDepthOffset;
    GPUData<float> gpuDepthScale;
};

class GPUChunkTile {
public:
	void setValue(ProgramObject* programObject, const ChunkTile& chunkTile);
	void updateUniformLocations(
        ProgramObject* programObject,
        const std::string& nameBase);
    void deactivate();

private:
	GPUTexture gpuTexture;
	GPUTileUvTransform gpuTileUvTransform;
};

class GPUChunkTilePile{
public:    
    void setValue(ProgramObject* programObject, const ChunkTilePile& chunkTilePile);
    void updateUniformLocations(
        ProgramObject* programObject,
        const std::string& nameBase,
        int pileSize);
    void deactivate();

private:
    std::vector<GPUChunkTile> gpuChunkTiles;
};

class LayerRenderSettings;
class GPULayerRenderSettings{
public:
    void setValue(ProgramObject* programObject, const LayerRenderSettings& layerSettings);
    void updateUniformLocations(
        ProgramObject* programObject,
        const std::string& nameBase);

private:
    GPUData<float> gpuOpacity;
    GPUData<float> gpuGamma;
    GPUData<float> gpuMultiplier;
};

class Layer;
class GPULayer {
public:
    virtual void setValue(
        ProgramObject* programObject,
        const Layer& layer,
        const TileIndex& tileIndex,
        int pileSize);
    virtual void updateUniformLocations(
        ProgramObject* programObject,
        const Layer& layer,
        const std::string& nameBase,
        int pileSize);
    virtual void deactivate();
private:
    GPUChunkTilePile gpuChunkTilePile;
    GPULayerRenderSettings gpuRenderSettings;
};

class GPUHeightLayer : public GPULayer{
public:
    virtual void setValue(
        ProgramObject* programObject,
        const Layer& layer,
        const TileIndex& tileIndex,
        int pileSize);
    virtual void updateUniformLocations(
        ProgramObject* programObject,
        const Layer& layer,
        const std::string& nameBase,
        int pileSize);
private:
    GPUTileDepthTransform gpuDepthTransform;
};

class LayerGroup;
class GPULayerGroup{
public:
    virtual void setValue(
        ProgramObject* programObject,
        const LayerGroup& layerGroup,
        const TileIndex& tileIndex);
    virtual void updateUniformLocations(
        ProgramObject* programObject,
        const LayerGroup& layerGroup,
        const std::string& nameBase,
        int category);
    virtual void deactivate();
private:
    std::vector<std::unique_ptr<GPULayer>> gpuActiveLayers;
};

class LayerManager;
class GPULayerManager{
public:    
    virtual void setValue(
        ProgramObject* programObject,
        const LayerManager& layerManager,
        const TileIndex& tileIndex);
    virtual void updateUniformLocations(
        ProgramObject* programObject,
        const LayerManager& layerManager);
    virtual void deactivate();
private:
    std::vector<std::unique_ptr<GPULayerGroup>> gpuLayerGroups;
};

} // namespace globebrowsing
} // namespace openspace
#endif  // __GPU_LAYER_MANAGER_H__