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

#include <modules/globebrowsing/chunk/chunkedlodglobe.h>

#include <modules/globebrowsing/meshes/skirtedgrid.h>
#include <modules/globebrowsing/chunk/culling.h>
#include <modules/globebrowsing/chunk/chunklevelevaluator.h>

#include <modules/debugging/rendering/debugrenderer.h>


// open space includes
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/spicemanager.h>
#include <openspace/scene/scenegraphnode.h>

// ghoul includes
#include <ghoul/misc/assert.h>

#define _USE_MATH_DEFINES
#include <math.h>

namespace {
    const std::string _loggerCat = "ChunkLodGlobe";
}

namespace openspace {

    const GeodeticPatch ChunkedLodGlobe::LEFT_HEMISPHERE = GeodeticPatch(0, -M_PI/2, M_PI/2, M_PI/2);
    const GeodeticPatch ChunkedLodGlobe::RIGHT_HEMISPHERE = GeodeticPatch(0, M_PI/2, M_PI/2, M_PI/2);

    const ChunkIndex ChunkedLodGlobe::LEFT_HEMISPHERE_INDEX = ChunkIndex(0, 0, 1);
    const ChunkIndex ChunkedLodGlobe::RIGHT_HEMISPHERE_INDEX = ChunkIndex(1, 0, 1);


    ChunkedLodGlobe::ChunkedLodGlobe(
        const Ellipsoid& ellipsoid,
        size_t segmentsPerPatch,
        std::shared_ptr<TileProviderManager> tileProviderManager)
        : _ellipsoid(ellipsoid)
        , _leftRoot(new ChunkNode(Chunk(this, LEFT_HEMISPHERE_INDEX)))
        , _rightRoot(new ChunkNode(Chunk(this, RIGHT_HEMISPHERE_INDEX)))
        , minSplitDepth(2)
        , maxSplitDepth(22)
        , _savedCamera(nullptr)
        , _tileProviderManager(tileProviderManager)
    {

        auto geometry = std::make_shared<SkirtedGrid>(
            (unsigned int) segmentsPerPatch,
            (unsigned int) segmentsPerPatch,
            TriangleSoup::Positions::No,
            TriangleSoup::TextureCoordinates::Yes,
            TriangleSoup::Normals::No);

        _chunkCullers.push_back(std::make_unique<HorizonCuller>());
        _chunkCullers.push_back(std::make_unique<FrustumCuller>(AABB3(vec3(-1, -1, 0), vec3(1, 1, 1e35))));

        
        
        _chunkEvaluatorByAvailableTiles = std::make_unique<EvaluateChunkLevelByAvailableTileData>();
        _chunkEvaluatorByProjectedArea = std::make_unique<EvaluateChunkLevelByProjectedArea>();
        _chunkEvaluatorByDistance = std::make_unique<EvaluateChunkLevelByDistance>();

        _renderer = std::make_unique<ChunkRenderer>(geometry, tileProviderManager);
    }

    ChunkedLodGlobe::~ChunkedLodGlobe() {

    }

    bool ChunkedLodGlobe::initialize() {
        return isReady();
    }

    bool ChunkedLodGlobe::deinitialize() {
        return true;
    }

    bool ChunkedLodGlobe::isReady() const {
        bool ready = true;
        return ready;
    }

    std::shared_ptr<TileProviderManager> ChunkedLodGlobe::getTileProviderManager() const {
        return _tileProviderManager;
    }

    bool ChunkedLodGlobe::testIfCullable(const Chunk& chunk, const RenderData& renderData) const {
        if (debugOptions.doHorizonCulling && _chunkCullers[0]->isCullable(chunk, renderData)) {
            return true;
        }
        if (debugOptions.doFrustumCulling && _chunkCullers[1]->isCullable(chunk, renderData)) {
            return true;
        }
        return false;
    }

    int ChunkedLodGlobe::getDesiredLevel(const Chunk& chunk, const RenderData& renderData) const {
        int desiredLevel = 0;
        if (debugOptions.levelByProjAreaElseDistance) {
            desiredLevel = _chunkEvaluatorByProjectedArea->getDesiredLevel(chunk, renderData);
        }
        else {
            desiredLevel = _chunkEvaluatorByDistance->getDesiredLevel(chunk, renderData);
        }

        if (debugOptions.limitLevelByAvailableHeightData) {
            int desiredLevelByAvailableData = _chunkEvaluatorByAvailableTiles->getDesiredLevel(chunk, renderData);
            if (desiredLevelByAvailableData != ChunkLevelEvaluator::UNKNOWN_DESIRED_LEVEL) {
                desiredLevel = min(desiredLevel, desiredLevelByAvailableData);
            }
        }

        desiredLevel = glm::clamp(desiredLevel, minSplitDepth, maxSplitDepth);
        return desiredLevel;
    }

    
    void ChunkedLodGlobe::render(const RenderData& data){
        minDistToCamera = INFINITY;

        _leftRoot->updateChunkTree(data);
        _rightRoot->updateChunkTree(data);

        // Calculate the MVP matrix
        dmat4 modelTransform = translate(dmat4(1), data.position.dvec3());
        dmat4 viewTransform = dmat4(data.camera.combinedViewMatrix());
        dmat4 vp = dmat4(data.camera.projectionMatrix()) * viewTransform;
        dmat4 mvp = vp * modelTransform;

        // Render function
        std::function<void(const ChunkNode&)> renderJob = [this, &data, &mvp](const ChunkNode& chunkNode) {
            const Chunk& chunk = chunkNode.getChunk();
            if (chunkNode.isLeaf() && chunk.isVisible()) {
                _renderer->renderChunk(chunkNode.getChunk(), data);
                debugRenderChunk(chunk, mvp);
            }
        };
        
        _leftRoot->reverseBreadthFirst(renderJob);
        _rightRoot->reverseBreadthFirst(renderJob);


        if (_savedCamera != nullptr) {
            DebugRenderer::ref()->renderCameraFrustum(data, *_savedCamera);
        }
        

        //LDEBUG("min distnace to camera: " << minDistToCamera);

        Vec3 cameraPos = data.camera.position().dvec3();
        //LDEBUG("cam pos  x: " << cameraPos.x << "  y: " << cameraPos.y << "  z: " << cameraPos.z);

        //LDEBUG("ChunkNode count: " << ChunkNode::chunkNodeCount);
        //LDEBUG("RenderedPatches count: " << ChunkNode::renderedChunks);
        //LDEBUG(ChunkNode::renderedChunks << " / " << ChunkNode::chunkNodeCount << " chunks rendered");
    }


    void ChunkedLodGlobe::debugRenderChunk(const Chunk& chunk, const glm::dmat4& mvp) const {
        if (debugOptions.showChunkBounds || debugOptions.showChunkAABB) {
            const std::vector<glm::dvec4> modelSpaceCorners = chunk.getBoundingPolyhedronCorners();
            std::vector<glm::vec4> clippingSpaceCorners(8);
            AABB3 screenSpaceBounds;
            for (size_t i = 0; i < 8; i++) {
                const vec4& clippingSpaceCorner = mvp * modelSpaceCorners[i];
                clippingSpaceCorners[i] = clippingSpaceCorner;

                vec3 screenSpaceCorner = (1.0f / clippingSpaceCorner.w) * clippingSpaceCorner.xyz();
                screenSpaceBounds.expand(screenSpaceCorner);
            }

            unsigned int colorBits = 1 + chunk.index().level % 6;
            vec4 color = vec4(colorBits & 1, colorBits & 2, colorBits & 4, 0.3);

            if (debugOptions.showChunkBounds) {
                DebugRenderer::ref()->renderNiceBox(clippingSpaceCorners, color);
            }

            if (debugOptions.showChunkAABB) {
                auto& screenSpacePoints = DebugRenderer::ref()->verticesFor(screenSpaceBounds);
                DebugRenderer::ref()->renderNiceBox(screenSpacePoints, color);
            }
        }
    }

    void ChunkedLodGlobe::update(const UpdateData& data) {
        _renderer->update();
    }

    void ChunkedLodGlobe::setStateMatrix(const glm::dmat3& stateMatrix)
    {
        _stateMatrix = stateMatrix;
    }

    const glm::dmat3& ChunkedLodGlobe::stateMatrix()
    {
        return _stateMatrix;
    }

    const Ellipsoid& ChunkedLodGlobe::ellipsoid() const
    {
        return _ellipsoid;
    }

}  // namespace openspace
