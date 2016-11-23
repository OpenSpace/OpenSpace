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


#include <modules/globebrowsing/globes/chunkedlodglobe.h>

#include <modules/globebrowsing/tile/tileprovider/tileprovider.h>
#include <modules/globebrowsing/globes/renderableglobe.h>
#include <modules/globebrowsing/meshes/skirtedgrid.h>
#include <modules/globebrowsing/chunk/culling.h>
#include <modules/globebrowsing/chunk/chunklevelevaluator.h>

#include <modules/debugging/rendering/debugrenderer.h>

// open space includes
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/spicemanager.h>
#include <openspace/scene/scenegraphnode.h>

#include <openspace/util/time.h>

// ghoul includes
#include <ghoul/misc/assert.h>

#include <ghoul/logging/logmanager.h>

// open space includes
#include <openspace/rendering/renderengine.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/util/updatestructures.h>

#include <modules/globebrowsing/chunk/chunknode.h>
#include <modules/globebrowsing/rendering/chunkrenderer.h>



#define _USE_MATH_DEFINES
#include <math.h>

#include <ctime>
#include <chrono>

namespace {
    const std::string _loggerCat = "ChunkedLodGlobe";
}

namespace openspace {
namespace globebrowsing {

    const TileIndex ChunkedLodGlobe::LEFT_HEMISPHERE_INDEX =
        TileIndex(0, 0, 1);
    const TileIndex ChunkedLodGlobe::RIGHT_HEMISPHERE_INDEX =
        TileIndex(1, 0, 1);
    const GeodeticPatch ChunkedLodGlobe::COVERAGE =
        GeodeticPatch(0, 0, 90, 180);

    ChunkedLodGlobe::ChunkedLodGlobe(
        const RenderableGlobe& owner,
        size_t segmentsPerPatch,
        std::shared_ptr<LayerManager> layerManager)
        : _owner(owner)
        , _leftRoot(std::make_unique<ChunkNode>(
            Chunk(owner, LEFT_HEMISPHERE_INDEX)))
        , _rightRoot(std::make_unique<ChunkNode>(
            Chunk(owner, RIGHT_HEMISPHERE_INDEX)))
        , minSplitDepth(2)
        , maxSplitDepth(22)
        , _layerManager(layerManager)
        , stats(StatsCollector(absPath("test_stats"), 1,
            StatsCollector::Enabled::No)) {
        auto geometry = std::make_shared<SkirtedGrid>(
            (unsigned int) segmentsPerPatch,
            (unsigned int) segmentsPerPatch,
            TriangleSoup::Positions::No,
            TriangleSoup::TextureCoordinates::Yes,
            TriangleSoup::Normals::No);

        _chunkCullers.push_back(std::make_unique<HorizonCuller>());
        _chunkCullers.push_back(std::make_unique<FrustumCuller>(
            AABB3(vec3(-1, -1, 0), vec3(1, 1, 1e35))));

        _chunkEvaluatorByAvailableTiles =
            std::make_unique<EvaluateChunkLevelByAvailableTileData>();
        _chunkEvaluatorByProjectedArea =
            std::make_unique<EvaluateChunkLevelByProjectedArea>();
        _chunkEvaluatorByDistance =
            std::make_unique<EvaluateChunkLevelByDistance>();

        _renderer = std::make_unique<ChunkRenderer>(geometry, layerManager);
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

    std::shared_ptr<LayerManager> ChunkedLodGlobe::layerManager() const {
        return _layerManager;
    }

    bool ChunkedLodGlobe::testIfCullable(
        const Chunk& chunk, const RenderData& renderData) const {
        if (_owner.debugProperties().performHorizonCulling &&
            _chunkCullers[0]->isCullable(chunk, renderData)) {
            return true;
        }
        if (_owner.debugProperties().performFrustumCulling &&
            _chunkCullers[1]->isCullable(chunk, renderData)) {
            return true;
        }
        return false;
    }

    const ChunkNode& ChunkedLodGlobe::findChunkNode(const Geodetic2 p) const {
        ghoul_assert(COVERAGE.contains(p),
            "Point must be in lat [-90, 90] and lon [-180, 180]");
        return
            p.lon < COVERAGE.center().lon ?
            _leftRoot->find(p) :
            _rightRoot->find(p);
    }

    int ChunkedLodGlobe::getDesiredLevel(
        const Chunk& chunk, const RenderData& renderData) const {
        int desiredLevel = 0;
        if (_owner.debugProperties().levelByProjectedAreaElseDistance) {
            desiredLevel = _chunkEvaluatorByProjectedArea->getDesiredLevel(
                chunk, renderData);
        }
        else {
            desiredLevel = _chunkEvaluatorByDistance->getDesiredLevel(
                chunk, renderData);
        }

        int desiredLevelByAvailableData =
            _chunkEvaluatorByAvailableTiles->getDesiredLevel(chunk, renderData);
        if (desiredLevelByAvailableData !=
            ChunkLevelEvaluator::UNKNOWN_DESIRED_LEVEL) {
            desiredLevel = min(desiredLevel, desiredLevelByAvailableData);
        }

        desiredLevel = glm::clamp(desiredLevel, minSplitDepth, maxSplitDepth);
        return desiredLevel;
    }
    
    float ChunkedLodGlobe::getHeight(glm::dvec3 position) const {
        float height = 0;
        
        // Get the uv coordinates to sample from
        Geodetic2 geodeticPosition = _owner.ellipsoid().cartesianToGeodetic2(position);
        int chunkLevel = findChunkNode(geodeticPosition).getChunk().tileIndex().level;
        
        TileIndex tileIndex = TileIndex(geodeticPosition, chunkLevel);
        GeodeticPatch patch = GeodeticPatch(tileIndex);

        Geodetic2 geoDiffPatch =
            patch.getCorner(Quad::NORTH_EAST) -
            patch.getCorner(Quad::SOUTH_WEST);
        Geodetic2 geoDiffPoint = geodeticPosition - patch.getCorner(Quad::SOUTH_WEST);
        glm::vec2 patchUV = glm::vec2(
            geoDiffPoint.lon / geoDiffPatch.lon, geoDiffPoint.lat / geoDiffPatch.lat);

        // Get the tile providers for the height maps
        const auto& heightMapLayers = _layerManager->layerGroup(LayerManager::HeightLayers).activeLayers();
        
        for (const auto& layer : heightMapLayers) {
            TileProvider* tileProvider = layer->tileProvider();
            // Transform the uv coordinates to the current tile texture
            ChunkTile chunkTile = TileSelector::getHighestResolutionTile(tileProvider, tileIndex);
            const auto& tile = chunkTile.tile;
            const auto& uvTransform = chunkTile.uvTransform;
            const auto& depthTransform = tileProvider->depthTransform();
            if (tile.status != Tile::Status::OK) {
                return 0;
            }

            glm::vec2 transformedUv = Tile::TileUvToTextureSamplePosition(
                uvTransform,
                patchUV,
                glm::uvec2(tile.texture->dimensions()));

            // Sample and do linear interpolation
            // (could possibly be moved as a function in ghoul texture)
            // Suggestion: a function in ghoul::opengl::Texture that takes uv coordinates
            // in range [0,1] and uses the set interpolation method and clamping.

            glm::uvec3 dimensions = tile.texture->dimensions();
            
            glm::vec2 samplePos = transformedUv * glm::vec2(dimensions);
            glm::uvec2 samplePos00 = samplePos;
            samplePos00 = glm::clamp(
                samplePos00, glm::uvec2(0, 0), glm::uvec2(dimensions) - glm::uvec2(1));
            glm::vec2 samplePosFract = samplePos - glm::vec2(samplePos00);

            glm::uvec2 samplePos10 = glm::min(
                samplePos00 + glm::uvec2(1, 0), glm::uvec2(dimensions) - glm::uvec2(1));
            glm::uvec2 samplePos01 = glm::min(
                samplePos00 + glm::uvec2(0, 1), glm::uvec2(dimensions) - glm::uvec2(1));
            glm::uvec2 samplePos11 = glm::min(
                samplePos00 + glm::uvec2(1, 1), glm::uvec2(dimensions) - glm::uvec2(1));

            float sample00 = tile.texture->texelAsFloat(samplePos00).x;
            float sample10 = tile.texture->texelAsFloat(samplePos10).x;
            float sample01 = tile.texture->texelAsFloat(samplePos01).x;
            float sample11 = tile.texture->texelAsFloat(samplePos11).x;

            // In case the texture has NaN or no data values don't use this height map.
            bool anySampleIsNaN =
                isnan(sample00) ||
                isnan(sample01) ||
                isnan(sample10) ||
                isnan(sample11);

            bool anySampleIsNoData =
                sample00 == tileProvider->noDataValueAsFloat() ||
                sample01 == tileProvider->noDataValueAsFloat() ||
                sample10 == tileProvider->noDataValueAsFloat() ||
                sample11 == tileProvider->noDataValueAsFloat();
        
            if (anySampleIsNaN || anySampleIsNoData) {
                continue;
            }

            float sample0 = sample00 * (1.0 - samplePosFract.x) + sample10 * samplePosFract.x;
            float sample1 = sample01 * (1.0 - samplePosFract.x) + sample11 * samplePosFract.x;

            float sample = sample0 * (1.0 - samplePosFract.y) + sample1 * samplePosFract.y;

            // Perform depth transform to get the value in meters
            height = depthTransform.depthOffset + depthTransform.depthScale * sample;
        }
        // Return the result
        return height;
    }

    void ChunkedLodGlobe::render(const RenderData& data) {
        
        stats.startNewRecord();
        
        auto duration = std::chrono::system_clock::now().time_since_epoch();
        auto millis = std::chrono::duration_cast<std::chrono::milliseconds>(
            duration).count();
        stats.i["time"] = millis;

        _leftRoot->updateChunkTree(data);
        _rightRoot->updateChunkTree(data);

        // Calculate the MVP matrix
        dmat4 viewTransform = dmat4(data.camera.combinedViewMatrix());
        dmat4 vp = dmat4(data.camera.projectionMatrix()) * viewTransform;
        dmat4 mvp = vp * _owner.modelTransform();

        // Render function
        auto renderJob = [this, &data, &mvp](const ChunkNode& chunkNode) {
            stats.i["chunks nodes"]++;
            const Chunk& chunk = chunkNode.getChunk();
            if (chunkNode.isLeaf()){
                stats.i["leafs chunk nodes"]++;
                if (chunk.isVisible()) {
                    stats.i["rendered chunks"]++;
                    double t0 = Time::now().j2000Seconds();
                    _renderer->renderChunk(chunkNode.getChunk(), data);
                    debugRenderChunk(chunk, mvp);
                }
            }
        };

        _leftRoot->breadthFirst(renderJob);
        _rightRoot->breadthFirst(renderJob);
        
        //_leftRoot->reverseBreadthFirst(renderJob);
        //_rightRoot->reverseBreadthFirst(renderJob);

        auto duration2 = std::chrono::system_clock::now().time_since_epoch();
        auto millis2 = std::chrono::duration_cast<std::chrono::milliseconds>(duration2).count();
        stats.i["chunk globe render time"] = millis2 - millis;
    }

    void ChunkedLodGlobe::debugRenderChunk(
        const Chunk& chunk, const glm::dmat4& mvp) const {
        if (_owner.debugProperties().showChunkBounds ||
            _owner.debugProperties().showChunkAABB) {
            const std::vector<glm::dvec4> modelSpaceCorners =
                chunk.getBoundingPolyhedronCorners();
            std::vector<glm::vec4> clippingSpaceCorners(8);
            AABB3 screenSpaceBounds;
            
            for (size_t i = 0; i < 8; i++) {
                const vec4& clippingSpaceCorner = mvp * modelSpaceCorners[i];
                clippingSpaceCorners[i] = clippingSpaceCorner;

                vec3 screenSpaceCorner =
                    (1.0f / clippingSpaceCorner.w) * clippingSpaceCorner;
                screenSpaceBounds.expand(screenSpaceCorner);
            }

            unsigned int colorBits = 1 + chunk.tileIndex().level % 6;
            vec4 color = vec4(colorBits & 1, colorBits & 2, colorBits & 4, 0.3);

            if (_owner.debugProperties().showChunkBounds) {
                DebugRenderer::ref().renderNiceBox(clippingSpaceCorners, color);
            }

            if (_owner.debugProperties().showChunkAABB) {
                auto& screenSpacePoints =
                    DebugRenderer::ref().verticesFor(screenSpaceBounds);
                DebugRenderer::ref().renderNiceBox(screenSpacePoints, color);
            }
        }
    }

    void ChunkedLodGlobe::update(const UpdateData& data) {
        _renderer->update();
    }
    
} // namespace globebrowsing
} // namespace openspace
