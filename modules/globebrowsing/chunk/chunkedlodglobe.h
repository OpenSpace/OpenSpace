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

#include <modules/globebrowsing/geometry/ellipsoid.h>

#include <modules/globebrowsing/chunk/chunknode.h>
#include <modules/globebrowsing/chunk/chunkrenderer.h>

#include <modules/globebrowsing/tile/tileprovider.h>
#include <modules/globebrowsing/other/statscollector.h>


namespace ghoul {
    namespace opengl {
        class ProgramObject;
    }
}

namespace openspace {
    class ChunkLevelEvaluator;
}

namespace openspace {

    class ChunkedLodGlobe : public Renderable {
    public:
        ChunkedLodGlobe(
            const Ellipsoid& ellipsoid,
            size_t segmentsPerPatch,
            std::shared_ptr<TileProviderManager> tileProviderManager);
        virtual ~ChunkedLodGlobe();

        bool initialize() override;
        bool deinitialize() override;
        bool isReady() const override;

        void render(const RenderData& data) override;
        void update(const UpdateData& data) override;

        const ChunkNode& findChunkNode(const Geodetic2 location) const;
        ChunkNode& findChunkNode(const Geodetic2 location);

        void setStateMatrix(const glm::dmat3& stateMatrix);

        bool testIfCullable(const Chunk& chunk, const RenderData& renderData) const;
        int getDesiredLevel(const Chunk& chunk, const RenderData& renderData) const;

        double minDistToCamera;

        //Scalar globeRadius;
        const Ellipsoid& ellipsoid() const;
        const glm::dmat3& stateMatrix();

        const int minSplitDepth;
        const int maxSplitDepth;


        std::shared_ptr<TileProviderManager> getTileProviderManager() const;


        const std::shared_ptr<const Camera> getSavedCamera() const { return _savedCamera; }
        void setSaveCamera(std::shared_ptr<Camera> c) { 
            _savedCamera = c; 
        }

        
        float lodScaleFactor;

        bool atmosphereEnabled;

        struct DebugOptions {
            bool showChunkEdges = false;
            bool showChunkBounds = false;
            bool showChunkAABB = false;

            bool doHorizonCulling = true;
            bool doFrustumCulling = true;

            bool limitLevelByAvailableHeightData = true;
            bool levelByProjAreaElseDistance = true;
        } debugOptions;

        StatsCollector stats;

    private:

        void debugRenderChunk(const Chunk& chunk, const glm::dmat4& data) const;

        static const GeodeticPatch COVERAGE;

        // Covers all negative longitudes
        std::unique_ptr<ChunkNode> _leftRoot;

        // Covers all positive longitudes
        std::unique_ptr<ChunkNode> _rightRoot;

        // the patch used for actual rendering
        std::unique_ptr<ChunkRenderer> _renderer;

        static const ChunkIndex LEFT_HEMISPHERE_INDEX;
        static const ChunkIndex RIGHT_HEMISPHERE_INDEX;

        std::vector<std::unique_ptr<ChunkCuller>> _chunkCullers;

        std::unique_ptr<ChunkLevelEvaluator> _chunkEvaluatorByAvailableTiles;
        std::unique_ptr<ChunkLevelEvaluator> _chunkEvaluatorByProjectedArea;
        std::unique_ptr<ChunkLevelEvaluator> _chunkEvaluatorByDistance;

        const Ellipsoid& _ellipsoid;
        glm::dmat3 _stateMatrix;

        std::shared_ptr<Camera> _savedCamera;
        
        std::shared_ptr<TileProviderManager> _tileProviderManager;
    };

}  // namespace openspace

#endif  // __CHUNK_LOD_GLOBE__