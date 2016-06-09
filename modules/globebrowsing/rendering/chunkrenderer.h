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

#ifndef __CHUNK_RENDERER_H__
#define __CHUNK_RENDERER_H__

#include <memory>
#include <glm/glm.hpp>

// open space includes
#include <openspace/rendering/renderable.h>

#include <modules/globebrowsing/geodetics/geodetic2.h>
#include <modules/globebrowsing/geodetics/ellipsoid.h>

#include <modules/globebrowsing/meshes/grid.h>

#include <modules/globebrowsing/other/tileprovidermanager.h>
#include <modules/globebrowsing/other/layeredtextureshaderprovider.h>

#include <modules/globebrowsing/globes/chunknode.h>

namespace ghoul {
namespace opengl {
    class ProgramObject;
}
}

namespace openspace {
    
    class ChunkRenderer {
    public:
        ChunkRenderer(std::shared_ptr<Grid> grid,
            std::shared_ptr<TileProviderManager> tileProviderManager);

        void renderChunk(const Chunk& chunk, const RenderData& data);
        void update();

    private:

        void renderChunkGlobally(const Chunk& chunk, const RenderData& data);
        void renderChunkLocally(const Chunk& chunk, const RenderData& data);

        void setDepthTransformUniforms(
            ProgramObject* programObject, 
            const std::string& indexedTileKey,
            const TileDepthTransform& tileDepthTransform);

        void activateTileAndSetTileUniforms(
            ProgramObject* programObject,
            ghoul::opengl::TextureUnit& texUnit, 
            const std::string indexedTileKey,
            const TileAndTransform& tileAndTransform);

        ProgramObject* getActivatedProgramWithTileData(
            LayeredTextureShaderProvider* layeredTextureShaderProvider, 
            const Chunk& chunk);

        //////////////////////////////////////////////////////////////////////////////////
        //                              Member variables                                //
        //////////////////////////////////////////////////////////////////////////////////

        std::shared_ptr<Grid> _grid;
        std::shared_ptr<TileProviderManager> _tileProviderManager;

        std::unique_ptr<LayeredTextureShaderProvider> _globalRenderingShaderProvider;
        std::unique_ptr<LayeredTextureShaderProvider> _localRenderingShaderProvider;

    };

}  // namespace openspace

#endif  // __CHUNK_RENDERER_H__