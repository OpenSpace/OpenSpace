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

#ifndef __CHUNKED_LOD_GLOBE__
#define __CHUNKED_LOD_GLOBE__

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
#include <modules/globebrowsing/tile/tileprovider/tileprovider.h>
#include <modules/globebrowsing/other/statscollector.h>

namespace openspace {
namespace globebrowsing {

    class ChunkLevelEvaluator;
    class RenderableGlobe;

    class ChunkedLodGlobe : public Renderable {
    public:
        ChunkedLodGlobe(
            const RenderableGlobe& owner,
            size_t segmentsPerPatch,
            std::shared_ptr<LayerManager> layerManager);
        virtual ~ChunkedLodGlobe();

        bool initialize() override;
        bool deinitialize() override;
        bool isReady() const override;

        void render(const RenderData& data) override;
        void update(const UpdateData& data) override;

        const ChunkNode& findChunkNode(const Geodetic2 location) const;
        ChunkNode& findChunkNode(const Geodetic2 location);

        bool testIfCullable(const Chunk& chunk, const RenderData& renderData) const;
        int getDesiredLevel(const Chunk& chunk, const RenderData& renderData) const;

        const int minSplitDepth;
        const int maxSplitDepth;

        std::shared_ptr<LayerManager> layerManager() const;

        StatsCollector stats;
    
    private:
        void debugRenderChunk(const Chunk& chunk, const glm::dmat4& data) const;

        static const GeodeticPatch COVERAGE;
        static const TileIndex LEFT_HEMISPHERE_INDEX;
        static const TileIndex RIGHT_HEMISPHERE_INDEX;

        // Covers all negative longitudes
        std::unique_ptr<ChunkNode> _leftRoot;

        // Covers all positive longitudes
        std::unique_ptr<ChunkNode> _rightRoot;

        // the patch used for actual rendering
        std::unique_ptr<ChunkRenderer> _renderer;

        std::vector<std::unique_ptr<ChunkCuller>> _chunkCullers;

        std::unique_ptr<ChunkLevelEvaluator> _chunkEvaluatorByAvailableTiles;
        std::unique_ptr<ChunkLevelEvaluator> _chunkEvaluatorByProjectedArea;
        std::unique_ptr<ChunkLevelEvaluator> _chunkEvaluatorByDistance;

        std::shared_ptr<LayerManager> _layerManager;

        const RenderableGlobe& _owner;
    };

} // namespace globebrowsing
} // namespace openspace

#endif  // __CHUNKED_LOD_GLOBE__
