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

#include <modules/globebrowsing/geometry/geodetic2.h>
#include <modules/globebrowsing/geometry/ellipsoid.h>

#include <modules/globebrowsing/meshes/grid.h>

#include <modules/globebrowsing/tile/tileprovidermanager.h>
#include <modules/globebrowsing/tile/layeredtextureshaderprovider.h>
#include <modules/globebrowsing/tile/tileselector.h>

#include <modules/globebrowsing/chunk/chunknode.h>

#include <ghoul/opengl/textureunit.h>


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
            std::shared_ptr<LayeredTextureShaderUniformIdHandler> uniformIdHandler,
            LayeredTextures::TextureCategory textureCategory,
            LayeredTextureShaderUniformIdHandler::BlendLayerSuffix blendLayerSuffix,
            size_t layerIndex,
            const TileDepthTransform& tileDepthTransform);

        void activateTileAndSetTileUniforms(
            std::shared_ptr<LayeredTextureShaderUniformIdHandler> uniformIdHandler,
            LayeredTextures::TextureCategory textureCategory,
            LayeredTextureShaderUniformIdHandler::BlendLayerSuffix blendLayerSuffix,
            size_t layerIndex,
            ghoul::opengl::TextureUnit& texUnit,
            const TileAndTransform& tileAndTransform);

        ProgramObject* getActivatedProgramWithTileData(
            LayeredTextureShaderProvider* layeredTextureShaderProvider,
            std::shared_ptr<LayeredTextureShaderUniformIdHandler> programUniformHandler,
            const Chunk& chunk);

        //////////////////////////////////////////////////////////////////////////////////
        //                              Member variables                                //
        //////////////////////////////////////////////////////////////////////////////////

        std::shared_ptr<Grid> _grid;
        std::shared_ptr<TileProviderManager> _tileProviderManager;

        std::shared_ptr<LayeredTextureShaderProvider> _globalRenderingShaderProvider;
        std::shared_ptr<LayeredTextureShaderProvider> _localRenderingShaderProvider;

        std::shared_ptr<LayeredTextureShaderUniformIdHandler> _globalProgramUniformHandler;
        std::shared_ptr<LayeredTextureShaderUniformIdHandler> _localProgramUniformHandler;

    };

}  // namespace openspace

#endif  // __CHUNK_RENDERER_H__