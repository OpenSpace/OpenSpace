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

#ifndef __CHUNK_LOD_GLOBE__
#define __CHUNK_LOD_GLOBE__

#include <memory>


#include <ghoul/logging/logmanager.h>


// open space includes
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/renderable.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/util/updatestructures.h>


#include <modules/globebrowsing/geodetics/ellipsoid.h>
#include <modules/globebrowsing/globes/chunknode.h>
#include <modules/globebrowsing/rendering/chunkrenderer.h>
#include <modules/globebrowsing/other/tileprovider.h>

namespace ghoul {
    namespace opengl {
        class ProgramObject;
    }
}

namespace openspace {

    class ChunkedLodGlobe : public Renderable {
    public:
        ChunkedLodGlobe(
            const Ellipsoid& ellipsoid,
            size_t segmentsPerPatch,
            std::shared_ptr<TileProviderManager> tileProviderManager);
        virtual ~ChunkedLodGlobe();

        ChunkRenderer& getPatchRenderer() const;

        bool initialize() override;
        bool deinitialize() override;
        bool isReady() const override;

        void render(const RenderData& data) override;
        void update(const UpdateData& data) override;

        void setStateMatrix(const glm::dmat3& stateMatrix);

        bool testIfCullable(const Chunk& chunk, const RenderData& renderData);

        double minDistToCamera;

        //Scalar globeRadius;
        const Ellipsoid& ellipsoid() const;
        const glm::dmat3& stateMatrix();

        const int minSplitDepth;
        const int maxSplitDepth;


        std::shared_ptr<TileProviderManager> getTileProviderManager() const;


        Camera* getSavedCamera() const { return _savedCamera; }
        void setSaveCamera(Camera* c) { 
            if (_savedCamera != nullptr) delete _savedCamera;
            _savedCamera = c; 
        }
        

        bool doHorizonCulling = true;
        bool doFrustumCulling = true;
        bool mergeInvisible;
        float lodScaleFactor;
        bool initChunkVisible;
        bool renderSmallChunksFirst = true;
        float chunkHeight;

        // Layered rendering
        bool blendHeightMap;
        bool blendColorMap;
        bool blendNightTexture;
        bool blendWaterMask;
        bool blendOverlay;
        bool atmosphereEnabled;

    private:

        void renderChunkTree(ChunkNode* node, const RenderData& data) const;

        // Covers all negative longitudes
        std::unique_ptr<ChunkNode> _leftRoot;

        // Covers all positive longitudes
        std::unique_ptr<ChunkNode> _rightRoot;

        // the patch used for actual rendering
        std::unique_ptr<ChunkRenderer> _patchRenderer;

        static const GeodeticPatch LEFT_HEMISPHERE;
        static const GeodeticPatch RIGHT_HEMISPHERE;

        static const ChunkIndex LEFT_HEMISPHERE_INDEX;
        static const ChunkIndex RIGHT_HEMISPHERE_INDEX;

        std::vector<ChunkCuller*> _chunkCullers;

        const Ellipsoid& _ellipsoid;
        glm::dmat3 _stateMatrix;

        Camera* _savedCamera;
        
        std::shared_ptr<TileProviderManager> _tileProviderManager;
    };

}  // namespace openspace

#endif  // __CHUNK_LOD_GLOBE__