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

#include <modules/globebrowsing/globes/chunkedlodglobe.h>

#include <modules/globebrowsing/chunk/chunk.h>
#include <modules/globebrowsing/chunk/chunklevelevaluator/chunklevelevaluator.h>
#include <modules/globebrowsing/chunk/chunklevelevaluator/availabletiledataevaluator.h>
#include <modules/globebrowsing/chunk/chunklevelevaluator/distanceevaluator.h>
#include <modules/globebrowsing/chunk/chunklevelevaluator/projectedareaevaluator.h>
#include <modules/globebrowsing/chunk/chunknode.h>
#include <modules/globebrowsing/chunk/culling/chunkculler.h>
#include <modules/globebrowsing/chunk/culling/frustumculler.h>
#include <modules/globebrowsing/chunk/culling/horizonculler.h>
#include <modules/globebrowsing/globes/renderableglobe.h>
#include <modules/globebrowsing/meshes/skirtedgrid.h>
#include <modules/globebrowsing/tile/tileprovider/tileprovider.h>
#include <modules/globebrowsing/rendering/chunkrenderer.h>
#include <modules/globebrowsing/rendering/layer/layergroup.h>
#include <modules/globebrowsing/rendering/layer/layermanager.h>
#include <modules/debugging/rendering/debugrenderer.h>
#include <modules/globebrowsing/tile/tileindex.h>

#include <openspace/util/time.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/texture.h>

#include <math.h>

namespace openspace::globebrowsing {

const TileIndex ChunkedLodGlobe::LEFT_HEMISPHERE_INDEX = TileIndex(0, 0, 1);
const TileIndex ChunkedLodGlobe::RIGHT_HEMISPHERE_INDEX = TileIndex(1, 0, 1);
const GeodeticPatch ChunkedLodGlobe::COVERAGE = GeodeticPatch(0, 0, 90, 180);

ChunkedLodGlobe::ChunkedLodGlobe(const RenderableGlobe& owner, size_t segmentsPerPatch,
                                 std::shared_ptr<LayerManager> layerManager)
    : minSplitDepth(2)
    , maxSplitDepth(22)
    , stats(StatsCollector(absPath("test_stats"), 1, StatsCollector::Enabled::No))
    , _owner(owner)
    , _leftRoot(std::make_unique<ChunkNode>(Chunk(owner, LEFT_HEMISPHERE_INDEX)))
    , _rightRoot(std::make_unique<ChunkNode>(Chunk(owner, RIGHT_HEMISPHERE_INDEX)))
    , _layerManager(layerManager)
    , _shadersNeedRecompilation(true)
{
    auto geometry = std::make_shared<SkirtedGrid>(
        static_cast<unsigned int>(segmentsPerPatch),
        static_cast<unsigned int>(segmentsPerPatch),
        TriangleSoup::Positions::No,
        TriangleSoup::TextureCoordinates::Yes,
        TriangleSoup::Normals::No
    );

    _chunkCullers.push_back(std::make_unique<culling::HorizonCuller>());
    _chunkCullers.push_back(std::make_unique<culling::FrustumCuller>(
        AABB3(glm::vec3(-1, -1, 0), glm::vec3(1, 1, 1e35)))
    );

    _chunkEvaluatorByAvailableTiles = 
        std::make_unique<chunklevelevaluator::AvailableTileData>();
    _chunkEvaluatorByProjectedArea =
    std::make_unique<chunklevelevaluator::ProjectedArea>();
    _chunkEvaluatorByDistance =
    std::make_unique<chunklevelevaluator::Distance>();

    _renderer = std::make_unique<ChunkRenderer>(geometry, layerManager);
}

// The destructor is defined here to make it feasiable to use a unique_ptr
// with a forward declaration
ChunkedLodGlobe::~ChunkedLodGlobe() {}
    
bool ChunkedLodGlobe::initialize() {
    return true;
}

bool ChunkedLodGlobe::deinitialize() {
    return true;
}

bool ChunkedLodGlobe::isReady() const {
    return true;
}

std::shared_ptr<LayerManager> ChunkedLodGlobe::layerManager() const {
    return _layerManager;
}

bool ChunkedLodGlobe::testIfCullable(const Chunk& chunk,
                                     const RenderData& renderData) const
{
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

const ChunkNode& ChunkedLodGlobe::findChunkNode(const Geodetic2& p) const {
    ghoul_assert(COVERAGE.contains(p),
        "Point must be in lat [-90, 90] and lon [-180, 180]");

    return p.lon < COVERAGE.center().lon ? _leftRoot->find(p) : _rightRoot->find(p);
}

int ChunkedLodGlobe::getDesiredLevel(
    const Chunk& chunk, const RenderData& renderData) const {
    int desiredLevel = 0;
    if (_owner.debugProperties().levelByProjectedAreaElseDistance) {
        desiredLevel = _chunkEvaluatorByProjectedArea->getDesiredLevel(chunk, renderData);
    }
    else {
        desiredLevel = _chunkEvaluatorByDistance->getDesiredLevel(chunk, renderData);
    }

    int desiredLevelByAvailableData = _chunkEvaluatorByAvailableTiles->getDesiredLevel(
        chunk, renderData
    );
    if (desiredLevelByAvailableData != chunklevelevaluator::Evaluator::UnknownDesiredLevel &&
        _owner.debugProperties().limitLevelByAvailableData) {
        desiredLevel = glm::min(desiredLevel, desiredLevelByAvailableData);
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
        patch.getCorner(Quad::NORTH_EAST) - patch.getCorner(Quad::SOUTH_WEST);

    Geodetic2 geoDiffPoint = geodeticPosition - patch.getCorner(Quad::SOUTH_WEST);
    glm::vec2 patchUV = glm::vec2(
        geoDiffPoint.lon / geoDiffPatch.lon,
        geoDiffPoint.lat / geoDiffPatch.lat
    );

    // Get the tile providers for the height maps
    const std::vector<std::shared_ptr<Layer>>& heightMapLayers =
        _layerManager->layerGroup(layergroupid::GroupID::HeightLayers).activeLayers();
        
    for (const std::shared_ptr<Layer>& layer : heightMapLayers) {
        tileprovider::TileProvider* tileProvider = layer->tileProvider();
        if (!tileProvider) {
            continue;
        }
        // Transform the uv coordinates to the current tile texture
        ChunkTile chunkTile = tileProvider->getChunkTile(tileIndex);
        const Tile& tile = chunkTile.tile;
        const TileUvTransform& uvTransform = chunkTile.uvTransform;
        const TileDepthTransform& depthTransform = tileProvider->depthTransform();
        if (tile.status() != Tile::Status::OK) {
            return 0;
        }

        ghoul::opengl::Texture* tileTexture = tile.texture();
        if (!tileTexture) {
            return 0;
        }

        glm::vec2 transformedUv = Tile::TileUvToTextureSamplePosition(
            uvTransform,
            patchUV,
            glm::uvec2(tileTexture->dimensions())
        );

        // Sample and do linear interpolation
        // (could possibly be moved as a function in ghoul texture)
        // Suggestion: a function in ghoul::opengl::Texture that takes uv coordinates
        // in range [0,1] and uses the set interpolation method and clamping.

        glm::uvec3 dimensions = tileTexture->dimensions();
            
        glm::vec2 samplePos = transformedUv * glm::vec2(dimensions);
        glm::uvec2 samplePos00 = samplePos;
        samplePos00 = glm::clamp(
            samplePos00,
            glm::uvec2(0, 0),
            glm::uvec2(dimensions) - glm::uvec2(1)
        );
        glm::vec2 samplePosFract = samplePos - glm::vec2(samplePos00);

        glm::uvec2 samplePos10 = glm::min(
            samplePos00 + glm::uvec2(1, 0),
            glm::uvec2(dimensions) - glm::uvec2(1)
        );
        glm::uvec2 samplePos01 = glm::min(
            samplePos00 + glm::uvec2(0, 1),
            glm::uvec2(dimensions) - glm::uvec2(1)
        );
        glm::uvec2 samplePos11 = glm::min(
            samplePos00 + glm::uvec2(1, 1),
            glm::uvec2(dimensions) - glm::uvec2(1)
        );

        float sample00 = tileTexture->texelAsFloat(samplePos00).x;
        float sample10 = tileTexture->texelAsFloat(samplePos10).x;
        float sample01 = tileTexture->texelAsFloat(samplePos01).x;
        float sample11 = tileTexture->texelAsFloat(samplePos11).x;

        // In case the texture has NaN or no data values don't use this height map.
        bool anySampleIsNaN =
            std::isnan(sample00) ||
            std::isnan(sample01) ||
            std::isnan(sample10) ||
            std::isnan(sample11);

        bool anySampleIsNoData =
            sample00 == tileProvider->noDataValueAsFloat() ||
            sample01 == tileProvider->noDataValueAsFloat() ||
            sample10 == tileProvider->noDataValueAsFloat() ||
            sample11 == tileProvider->noDataValueAsFloat();
        
        if (anySampleIsNaN || anySampleIsNoData) {
            continue;
        }

        float sample0 = sample00 * (1.f - samplePosFract.x) + sample10 * samplePosFract.x;
        float sample1 = sample01 * (1.f - samplePosFract.x) + sample11 * samplePosFract.x;

        float sample = sample0 * (1.f - samplePosFract.y) + sample1 * samplePosFract.y;

        // Same as is used in the shader. This is not a perfect solution but
        // if the sample is actually a no-data-value (min_float) the interpolated
        // value might not be. Therefore we have a cut-off. Assuming no data value
        // is smaller than -100000
        if (sample > -100000)
        {
            // Perform depth transform to get the value in meters
            height = depthTransform.depthOffset + depthTransform.depthScale * sample;
            // Make sure that the height value follows the layer settings.
            // For example if the multiplier is set to a value bigger than one,
            // the sampled height should be modified as well.
            height = layer->renderSettings().performLayerSettings(height);
        }
    }
    // Return the result
    return height;
}

void ChunkedLodGlobe::notifyShaderRecompilation() {
    _shadersNeedRecompilation = true;
}

void ChunkedLodGlobe::render(const RenderData& data, RendererTasks&) {
    stats.startNewRecord();
    if (_shadersNeedRecompilation) {
        _renderer->recompileShaders(_owner);
        _shadersNeedRecompilation = false;
    }
        
    auto duration = std::chrono::system_clock::now().time_since_epoch();
    auto millis = std::chrono::duration_cast<std::chrono::milliseconds>(duration).count();
    stats.i["time"] = millis;

    _leftRoot->updateChunkTree(data);
    _rightRoot->updateChunkTree(data);

    // Calculate the MVP matrix
    glm::dmat4 viewTransform = glm::dmat4(data.camera.combinedViewMatrix());
    glm::dmat4 vp = glm::dmat4(data.camera.sgctInternal.projectionMatrix()) * viewTransform;
    glm::dmat4 mvp = vp * _owner.modelTransform();

    // Render function
    auto renderJob = [this, &data, &mvp](const ChunkNode& chunkNode) {
        stats.i["chunks nodes"]++;
        const Chunk& chunk = chunkNode.getChunk();
        if (chunkNode.isLeaf()) {
            stats.i["leafs chunk nodes"]++;
            if (chunk.isVisible()) {
                stats.i["rendered chunks"]++;
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

void ChunkedLodGlobe::debugRenderChunk(const Chunk& chunk, const glm::dmat4& mvp) const {
    if (_owner.debugProperties().showChunkBounds ||
        _owner.debugProperties().showChunkAABB)
    {
        const std::vector<glm::dvec4> modelSpaceCorners =
            chunk.getBoundingPolyhedronCorners();
        std::vector<glm::vec4> clippingSpaceCorners(8);
        AABB3 screenSpaceBounds;
            
        for (size_t i = 0; i < 8; ++i) {
            const glm::vec4& clippingSpaceCorner = mvp * modelSpaceCorners[i];
            clippingSpaceCorners[i] = clippingSpaceCorner;

            glm::vec3 screenSpaceCorner =
                (1.0f / clippingSpaceCorner.w) * clippingSpaceCorner;
            screenSpaceBounds.expand(screenSpaceCorner);
        }

        unsigned int colorBits = 1 + chunk.tileIndex().level % 6;
        glm::vec4 color = glm::vec4(colorBits & 1, colorBits & 2, colorBits & 4, 0.3);

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
    setBoundingSphere(static_cast<float>(
        _owner.ellipsoid().maximumRadius() * data.modelTransform.scale
    ));
    _renderer->update();
}
    
} // namespace openspace::globebrowsing
