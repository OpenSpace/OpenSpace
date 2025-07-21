/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___GPULAYERGROUP___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___GPULAYERGROUP___H__

#include <ghoul/opengl/textureunit.h>
#include <ghoul/opengl/uniformcache.h>
#include <memory>
#include <string>
#include <vector>

namespace ghoul::opengl { class ProgramObject; }

namespace openspace::globebrowsing {

struct ChunkTile;
class Layer;
class GPULayer;
struct TileDepthTransform;
struct TileUvTransform;

struct LayerGroup;
struct TileIndex;

/**
 * Manages a GPU representation of a `LayerGroup`.
 */
class GPULayerGroup {
public:
    /**
     * Sets the value of `LayerGroup` to its corresponding GPU struct. OBS! Users must
     * ensure bind has been called before setting using this method.
     */
    void setValue(ghoul::opengl::ProgramObject& programObject,
        const LayerGroup& layerGroup, const TileIndex& tileIndex);

    /**
     * Binds this object with GLSL variables with identifiers starting with nameBase
     * within the provided shader program. After this method has been called, users may
     * invoke setValue.
     */
    void bind(ghoul::opengl::ProgramObject& programObject, const LayerGroup& layerGroup);

    /**
    * Deactivates any `TextureUnit`s assigned by this object. This method should be called
    * after the OpenGL draw call.
    */
    void deactivate();

private:
    struct GPULayer {
        struct GPUChunkTile {
            ghoul::opengl::TextureUnit texUnit;
            UniformCache(texture, uvOffset, uvScale) uniformCache;
        };
        std::vector<GPUChunkTile> gpuChunkTiles;

        UniformCache(opacity, gamma, multiplier, offset, valueBlending, chromaKeyColor,
            chromaKeyTolerance, color, depthOffset, depthScale) uniformCache;

        bool isHeightLayer = false;
    };

    std::vector<GPULayer> _gpuActiveLayers;
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___GPULAYERGROUP___H__
