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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___GPU_LAYER_MANAGER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___GPU_LAYER_MANAGER___H__

#include <openspace/util/gpudata.h>

#include <string>

namespace ghoul { namespace opengl {
class ProgramObject;
}}

namespace openspace {
namespace globebrowsing {

struct ChunkTile;
struct ChunkTilePile;
class Layer;
class LayerRenderSettings;
struct TileDepthTransform;
struct TileIndex;
struct TileUvTransform;

/**
 * Manages a GPU representation of a <code>TileUvTransform</code>
 */
class GPUTileUvTransform {
public:
    /**
     * Sets the value of <code>TileUvTransform</code> to its corresponding
     * GPU struct. OBS! Users must ensure bind has been 
     * called before setting using this method.
     */
    void setValue(ProgramObject* programObject, const TileUvTransform& uvTransform);

    /** 
     * Binds GLSL variables with identifiers starting with 
     * nameBase within the provided shader program with this object. 
     * After this method has been called, users may invoke setValue.
     */
    void bind(ProgramObject* programObject, const std::string& nameBase);

private:
    GPUData<glm::vec2> gpuUvOffset;
    GPUData<glm::vec2> gpuUvScale;
};


/**
 * Manages a GPU representation of a <code>TileDepthTransform</code>
 */
class GPUTileDepthTransform {
public:
    /**
     * Sets the value of <code>TileDepthTransform</code> to its corresponding
     * GPU struct. OBS! Users must ensure bind has been 
     * called before setting using this method.
     */
    void setValue(ProgramObject* programObject, const TileDepthTransform& depthTransform);

    /** 
     * Binds GLSL variables with identifiers starting with 
     * nameBase within the provided shader program with this object. 
     * After this method has been called, users may invoke setValue.
     */
    void bind(ProgramObject* programObject, const std::string& nameBase);
    
private:
    GPUData<float> gpuDepthOffset;
    GPUData<float> gpuDepthScale;
};

/**
 * Manages a GPU representation of a <code>ChunkTile</code>
 */
class GPUChunkTile {
public:

    /**
     * Sets the value of <code>ChunkTile</code> to its corresponding
     * GPU struct. OBS! Users must ensure bind has been 
     * called before setting using this method.
     */
    void setValue(ProgramObject* programObject, const ChunkTile& chunkTile);

    /** 
     * Binds GLSL variables with identifiers starting with 
     * nameBase within the provided shader program with this object. 
     * After this method has been called, users may invoke setValue.
     */
    void bind(ProgramObject* programObject, const std::string& nameBase);

    /**
    * Deactivates any <code>TextureUnit</code>s assigned by this object.
    * This method should be called after the OpenGL draw call.
    */
    void deactivate();

private:
    GPUTexture gpuTexture;
    GPUTileUvTransform gpuTileUvTransform;
};

/**
 * Manages a GPU representation of a <code>ChunkTilePile</code>
 */
class GPUChunkTilePile{
public:

    /**
     * Sets the value of <code>ChunkTilePile</code> to its corresponding
     * GPU struct. OBS! Users must ensure bind has been 
     * called before setting using this method.
     */
    void setValue(ProgramObject* programObject, const ChunkTilePile& chunkTilePile);

    /** 
     * Binds this object with GLSL variables with identifiers starting 
     * with nameBase within the provided shader program.
     * After this method has been called, users may invoke setValue.
     */
    void bind(ProgramObject* programObject, const std::string& nameBase, 
              int pileSize);
    /**
    * Deactivates any <code>TextureUnit</code>s assigned by this object.
    * This method should be called after the OpenGL draw call.
    */
    void deactivate();

private:
    std::vector<GPUChunkTile> gpuChunkTiles;
};


class LayerRenderSettings;

/**
 * Manages a GPU representation of a <code>LayerRenderSettings</code>
 */
class GPULayerRenderSettings{
public:

    /**
     * Sets the value of <code>LayerRenderSettings</code> to its corresponding
     * GPU struct. OBS! Users must ensure bind has been 
     * called before setting using this method.
     */
    void setValue(ProgramObject* programObject, const LayerRenderSettings& layerSettings);

    /** 
     * Binds this object with GLSL variables with identifiers starting 
     * with nameBase within the provided shader program.
     * After this method has been called, users may invoke setValue.
     */
    void bind(ProgramObject* programObject, const std::string& nameBase);

private:
    GPUData<float> gpuOpacity;
    GPUData<float> gpuGamma;
    GPUData<float> gpuMultiplier;
};


class Layer;

/**
 * Manages a GPU representation of a <code>Layer</code>
 */
class GPULayer {
public:

    /**
     * Sets the value of <code>Layer</code> to its corresponding
     * GPU struct. OBS! Users must ensure bind has been 
     * called before setting using this method.
     */
    virtual void setValue(ProgramObject* programObject, const Layer& layer,
                          const TileIndex& tileIndex, int pileSize);

    /** 
     * Binds this object with GLSL variables with identifiers starting 
     * with nameBase within the provided shader program.
     * After this method has been called, users may invoke setValue.
     */
    virtual void bind(ProgramObject* programObject, const Layer& layer,
                      const std::string& nameBase, int pileSize);

    /**
    * Deactivates any <code>TextureUnit</code>s assigned by this object.
    * This method should be called after the OpenGL draw call.
    */
    virtual void deactivate();

private:
    GPUChunkTilePile gpuChunkTilePile;
    GPULayerRenderSettings gpuRenderSettings;
};


/**
 * Manages a GPU representation of a <code>Layer</code> representing
 * a height map.
 */
class GPUHeightLayer : public GPULayer {
public:

    /**
     * Sets the value of <code>Layer</code> to its corresponding
     * GPU struct. OBS! Users must ensure bind has been 
     * called before setting using this method.
     */
    virtual void setValue(ProgramObject* programObject, const Layer& layer,
                          const TileIndex& tileIndex, int pileSize);

    /** 
     * Binds this object with GLSL variables with identifiers starting 
     * with nameBase within the provided shader program.
     * After this method has been called, users may invoke setValue.
     */
    virtual void bind(ProgramObject* programObject, const Layer& layer,
                      const std::string& nameBase, int pileSize);

private:
    GPUTileDepthTransform gpuDepthTransform;
};

class LayerGroup;

/**
 * Manages a GPU representation of a <code>LayerGroup</code>
 */
class GPULayerGroup{
public:

    /**
     * Sets the value of <code>LayerGroup</code> to its corresponding
     * GPU struct. OBS! Users must ensure bind has been 
     * called before setting using this method.
     */
    virtual void setValue(ProgramObject* programObject, const LayerGroup& layerGroup,
                          const TileIndex& tileIndex);

    /** 
     * Binds this object with GLSL variables with identifiers starting 
     * with nameBase within the provided shader program.
     * After this method has been called, users may invoke setValue.
     */
    virtual void bind(ProgramObject* programObject, const LayerGroup& layerGroup,
                      const std::string& nameBase, int category);

    /**
    * Deactivates any <code>TextureUnit</code>s assigned by this object.
    * This method should be called after the OpenGL draw call.
    */
    virtual void deactivate();

private:
    std::vector<std::unique_ptr<GPULayer>> gpuActiveLayers;
};

class LayerManager;

/**
 * Manages a GPU representation of a <code>LayerGroup</code>
 */
class GPULayerManager{
public:    

    /**
     * Sets the value of <code>LayerGroup</code> to its corresponding
     * GPU struct. OBS! Users must ensure bind has been 
     * called before setting using this method.
     */
    virtual void setValue(ProgramObject* programObject, const LayerManager& layerManager,
                          const TileIndex& tileIndex);

    /** 
     * Binds this object with GLSL variables with identifiers starting 
     * with nameBase within the provided shader program.
     * After this method has been called, users may invoke setValue.
     */
    virtual void bind(ProgramObject* programObject, const LayerManager& layerManager);

    /**
    * Deactivates any <code>TextureUnit</code>s assigned by this object.
    * This method should be called after the OpenGL draw call.
    */
    virtual void deactivate();

private:
    std::vector<std::unique_ptr<GPULayerGroup>> gpuLayerGroups;
};

} // namespace globebrowsing
} // namespace openspace

#endif  // __OPENSPACE_MODULE_GLOBEBROWSING___GPU_LAYER_MANAGER___H__
