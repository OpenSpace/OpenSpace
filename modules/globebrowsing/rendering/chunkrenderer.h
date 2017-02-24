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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___CHUNK_RENDERER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___CHUNK_RENDERER___H__

#include <memory>

namespace ghoul { namespace opengl {
    class ProgramObject;
} }

namespace openspace {

struct RenderData;

namespace globebrowsing {

class Chunk;
class Grid;
class GPULayerManager;
class LayerManager;
class LayerShaderManager;

class ChunkRenderer {
public:
    ChunkRenderer(std::shared_ptr<Grid> grid,
        std::shared_ptr<LayerManager> layerManager);

    /**
     * Chooses to render a chunk either locally or globally depending on the chunklevel
     * of the <code>Chunk</code>.
    */
    void renderChunk(const Chunk& chunk, const RenderData& data);
    void update();

private:
    /**
     * Chunks can be rendered either globally or locally. Global rendering is performed
     * in the model space of the globe. With global rendering, the vertex positions
     * of a chunk are calculated in the vertex shader by transforming the geodetic
     * coordinates of the chunk to model space coordinates. We can only achieve floating
     * point precision by doing this which means that the camera too close to a global
     * tile will lead to jagging. We only render global chunks for lower chunk levels.
     */
    void renderChunkGlobally(const Chunk& chunk, const RenderData& data);

    /**
     * Local rendering of chunks are done using linear interpolation in camera space.
     * All four corner points of the chunk are calculated in double precision on the
     * CPU and transformed to camera space with double precision matrix transforms.
     * These positions can then be cast to floats and uploaded to the vertex shader.
     * The vertex shader rendering performs linear interpolation between the four
     * corner points to get the resulting chunk. This means that there will be an error
     * due to the curvature of the globe. The smaller the patch is (with higher chunk
     * levels) the better the approximation becomes. This is why we only render local
     * chunks for higher chunk levels.
     */
    void renderChunkLocally(const Chunk& chunk, const RenderData& data);

    ghoul::opengl::ProgramObject* getActivatedProgramWithTileData(
        std::shared_ptr<LayerShaderManager> layeredShaderManager,
        std::shared_ptr<GPULayerManager> gpuLayerManager,
        const Chunk& chunk);

    // shared pointer to a grid which can be the same for all rendered chunks.
    std::shared_ptr<Grid> _grid;
    std::shared_ptr<LayerManager> _layerManager;

    // Two different shader programs. One for global and one for local rendering.
    std::shared_ptr<LayerShaderManager> _globalLayerShaderManager;
    std::shared_ptr<LayerShaderManager> _localLayerShaderManager;

    // Layered texture uniforms are chached in the uniform ID handles.
    std::shared_ptr<GPULayerManager> _globalGpuLayerManager;
    std::shared_ptr<GPULayerManager> _localGpuLayerManager;
};

} // namespace globebrowsing
} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___CHUNK_RENDERER___H__
