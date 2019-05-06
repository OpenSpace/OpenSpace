/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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
#include <modules/globebrowsing/tile/tileindex.h>
#include <modules/globebrowsing/tile/tileprovider/tileprovider.h>
#include <modules/globebrowsing/rendering/chunkrenderer.h>
#include <modules/globebrowsing/rendering/layer/layer.h>
#include <modules/globebrowsing/rendering/layer/layergroup.h>
#include <modules/globebrowsing/rendering/layer/layermanager.h>
#include <modules/debugging/rendering/debugrenderer.h>
#include <openspace/util/time.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/texture.h>
//#include <math.h>

namespace {
    const openspace::globebrowsing::GeodeticPatch Coverage =
        openspace::globebrowsing::GeodeticPatch(0, 0, 90, 180);

    const openspace::globebrowsing::TileIndex LeftHemisphereIndex =
        openspace::globebrowsing::TileIndex(0, 0, 1);

    const openspace::globebrowsing::TileIndex RightHemisphereIndex =
        openspace::globebrowsing::TileIndex(1, 0, 1);
} // namespace

namespace openspace::globebrowsing {

ChunkedLodGlobe::ChunkedLodGlobe(const RenderableGlobe& owner, size_t segmentsPerPatch,
                                 std::shared_ptr<LayerManager> layerManager,
                                 Ellipsoid& ellipsoid)
    : Renderable({ { "Identifier", owner.identifier() }, { "Name", owner.guiName() } })
#ifdef DEBUG_GLOBEBROWSING_STATSRECORD
    , stats(StatsCollector(absPath("test_stats"), 1, StatsCollector::Enabled::No))
#endif // DEBUG_GLOBEBROWSING_STATSRECORD
    , _owner(owner)
    , _leftRoot(std::make_unique<ChunkNode>(Chunk(owner, LeftHemisphereIndex)))
    , _rightRoot(std::make_unique<ChunkNode>(Chunk(owner, RightHemisphereIndex)))
    , _layerManager(layerManager)
{
    std::shared_ptr<SkirtedGrid> geometry = std::make_shared<SkirtedGrid>(
        static_cast<unsigned int>(segmentsPerPatch),
        static_cast<unsigned int>(segmentsPerPatch),
        TriangleSoup::Positions::No,
        TriangleSoup::TextureCoordinates::Yes,
        TriangleSoup::Normals::No
    );

    _chunkCullers.push_back(std::make_unique<culling::HorizonCuller>());
    _chunkCullers.push_back(std::make_unique<culling::FrustumCuller>(
        AABB3(
            glm::vec3(-1, -1, 0),
            glm::vec3(1, 1, 1e35)
        )
    ));

    _chunkEvaluatorByAvailableTiles =
        std::make_unique<chunklevelevaluator::AvailableTileData>();
    _chunkEvaluatorByProjectedArea =
        std::make_unique<chunklevelevaluator::ProjectedArea>();
    _chunkEvaluatorByDistance = std::make_unique<chunklevelevaluator::Distance>();

    _renderer = std::make_unique<ChunkRenderer>(geometry, layerManager, ellipsoid);
}

ChunkedLodGlobe::~ChunkedLodGlobe() {} // NOLINT

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

const ChunkNode& ChunkedLodGlobe::findChunkNode(const Geodetic2& location) const {
    ghoul_assert(
        Coverage.contains(location),
        "Point must be in lat [-90, 90] and lon [-180, 180]"
    );

    return location.lon < Coverage.center().lon ?
        _leftRoot->find(location) :
        _rightRoot->find(location);
}

int ChunkedLodGlobe::desiredLevel(const Chunk& chunk,
                                     const RenderData& renderData) const
{
    int desiredLevel = 0;
    if (_owner.debugProperties().levelByProjectedAreaElseDistance) {
        desiredLevel = _chunkEvaluatorByProjectedArea->desiredLevel(chunk, renderData);
    }
    else {
        desiredLevel = _chunkEvaluatorByDistance->desiredLevel(chunk, renderData);
    }

    int levelByAvailableData = _chunkEvaluatorByAvailableTiles->desiredLevel(
        chunk,
        renderData
    );
    if (levelByAvailableData != chunklevelevaluator::Evaluator::UnknownDesiredLevel &&
        _owner.debugProperties().limitLevelByAvailableData)
    {
        desiredLevel = glm::min(desiredLevel, levelByAvailableData);
    }

    desiredLevel = glm::clamp(desiredLevel, MinSplitDepth, MaxSplitDepth);
    return desiredLevel;
}

float ChunkedLodGlobe::getHeight(const glm::dvec3& position) const {
    float height = 0;

    // Get the uv coordinates to sample from
    const Geodetic2 geodeticPosition = _owner.ellipsoid().cartesianToGeodetic2(position);
    const int chunkLevel = findChunkNode(geodeticPosition).chunk().tileIndex().level;

    const TileIndex tileIndex = TileIndex(geodeticPosition, chunkLevel);
    const GeodeticPatch patch = GeodeticPatch(tileIndex);

    const Geodetic2 geoDiffPatch = patch.corner(Quad::NORTH_EAST) -
                                   patch.corner(Quad::SOUTH_WEST);

    const Geodetic2 geoDiffPoint = geodeticPosition - patch.corner(Quad::SOUTH_WEST);
    const glm::vec2 patchUV = glm::vec2(
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
        const ChunkTile chunkTile = tileProvider->chunkTile(tileIndex);
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

        glm::vec2 transformedUv = layer->TileUvToTextureSamplePosition(
            uvTransform,
            patchUV,
            glm::uvec2(tileTexture->dimensions())
        );

        // Sample and do linear interpolation
        // (could possibly be moved as a function in ghoul texture)
        // Suggestion: a function in ghoul::opengl::Texture that takes uv coordinates
        // in range [0,1] and uses the set interpolation method and clamping.

        const glm::uvec3 dimensions = tileTexture->dimensions();

        const glm::vec2 samplePos = transformedUv * glm::vec2(dimensions);
        glm::uvec2 samplePos00 = samplePos;
        samplePos00 = glm::clamp(
            samplePos00,
            glm::uvec2(0, 0),
            glm::uvec2(dimensions) - glm::uvec2(1)
        );
        const glm::vec2 samplePosFract = samplePos - glm::vec2(samplePos00);

        const glm::uvec2 samplePos10 = glm::min(
            samplePos00 + glm::uvec2(1, 0),
            glm::uvec2(dimensions) - glm::uvec2(1)
        );
        const glm::uvec2 samplePos01 = glm::min(
            samplePos00 + glm::uvec2(0, 1),
            glm::uvec2(dimensions) - glm::uvec2(1)
        );
        const glm::uvec2 samplePos11 = glm::min(
            samplePos00 + glm::uvec2(1, 1),
            glm::uvec2(dimensions) - glm::uvec2(1)
        );

        const float sample00 = tileTexture->texelAsFloat(samplePos00).x;
        const float sample10 = tileTexture->texelAsFloat(samplePos10).x;
        const float sample01 = tileTexture->texelAsFloat(samplePos01).x;
        const float sample11 = tileTexture->texelAsFloat(samplePos11).x;

        // In case the texture has NaN or no data values don't use this height map.
        const bool anySampleIsNaN =
            std::isnan(sample00) ||
            std::isnan(sample01) ||
            std::isnan(sample10) ||
            std::isnan(sample11);

        const bool anySampleIsNoData =
            sample00 == tileProvider->noDataValueAsFloat() ||
            sample01 == tileProvider->noDataValueAsFloat() ||
            sample10 == tileProvider->noDataValueAsFloat() ||
            sample11 == tileProvider->noDataValueAsFloat();

        if (anySampleIsNaN || anySampleIsNoData) {
            continue;
        }

        const float sample0 = sample00 * (1.f - samplePosFract.x) +
                              sample10 * samplePosFract.x;
        const float sample1 = sample01 * (1.f - samplePosFract.x) +
                              sample11 * samplePosFract.x;

        const float sample = sample0 * (1.f - samplePosFract.y) +
                             sample1 * samplePosFract.y;

        // Same as is used in the shader. This is not a perfect solution but
        // if the sample is actually a no-data-value (min_float) the interpolated
        // value might not be. Therefore we have a cut-off. Assuming no data value
        // is smaller than -100000
        if (sample > -100000) {
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

void ChunkedLodGlobe::recompileShaders() {
    _renderer->recompileShaders(_owner);
    _shadersNeedRecompilation = false;
}

void ChunkedLodGlobe::render(const RenderData& data, RendererTasks&) {
#ifdef DEBUG_GLOBEBROWSING_STATSRECORD
    stats.startNewRecord();
#endif // DEBUG_GLOBEBROWSING_STATSRECORD
    if (_shadersNeedRecompilation) {
        _renderer->recompileShaders(_owner);
        _shadersNeedRecompilation = false;
    }

#ifdef DEBUG_GLOBEBROWSING_STATSRECORD
    auto duration = std::chrono::system_clock::now().time_since_epoch();
    auto millis = std::chrono::duration_cast<std::chrono::milliseconds>(duration).count();
    stats.i["time"] = millis;
#endif // DEBUG_GLOBEBROWSING_STATSRECORD

    _leftRoot->updateChunkTree(data);
    _rightRoot->updateChunkTree(data);

    // Calculate the MVP matrix
    const glm::dmat4 viewTransform = glm::dmat4(data.camera.combinedViewMatrix());
    const glm::dmat4 vp = glm::dmat4(data.camera.sgctInternal.projectionMatrix()) *
                    viewTransform;
    const glm::dmat4 mvp = vp * _owner.modelTransform();

    // Render function
    auto renderJob = [this, &data, &mvp](const ChunkNode& chunkNode) {
#ifdef DEBUG_GLOBEBROWSING_STATSRECORD
        stats.i["chunks nodes"]++;
#endif // DEBUG_GLOBEBROWSING_STATSRECORD
        const Chunk& chunk = chunkNode.chunk();
        if (chunkNode.isLeaf()) {
#ifdef DEBUG_GLOBEBROWSING_STATSRECORD
            stats.i["leafs chunk nodes"]++;
#endif // DEBUG_GLOBEBROWSING_STATSRECORD

            if (chunk.isVisible()) {
#ifdef DEBUG_GLOBEBROWSING_STATSRECORD
                stats.i["rendered chunks"]++;
#endif // DEBUG_GLOBEBROWSING_STATSRECORD
                _renderer->renderChunk(chunkNode.chunk(), data);
                debugRenderChunk(chunk, mvp);
            }
        }
    };

    _leftRoot->breadthFirst(renderJob);
    _rightRoot->breadthFirst(renderJob);

    //_leftRoot->reverseBreadthFirst(renderJob);
    //_rightRoot->reverseBreadthFirst(renderJob);

#ifdef DEBUG_GLOBEBROWSING_STATSRECORD
    auto duration2 = std::chrono::system_clock::now().time_since_epoch();
    auto ms2 = std::chrono::duration_cast<std::chrono::milliseconds>(duration2).count();
    stats.i["chunk globe render time"] = ms2 - millis;
#endif // DEBUG_GLOBEBROWSING_STATSRECORD
}

void ChunkedLodGlobe::debugRenderChunk(const Chunk& chunk, const glm::dmat4& mvp) const {
    if (_owner.debugProperties().showChunkBounds ||
        _owner.debugProperties().showChunkAABB)
    {
        const std::vector<glm::dvec4>& modelSpaceCorners =
            chunk.boundingPolyhedronCorners();

        std::vector<glm::vec4> clippingSpaceCorners(8);
        AABB3 screenSpaceBounds;

        for (size_t i = 0; i < 8; ++i) {
            const glm::vec4& clippingSpaceCorner = mvp * modelSpaceCorners[i];
            clippingSpaceCorners[i] = clippingSpaceCorner;

            glm::vec3 screenSpaceCorner =
                glm::vec3((1.f / clippingSpaceCorner.w) * clippingSpaceCorner);
            screenSpaceBounds.expand(std::move(screenSpaceCorner));
        }

        const unsigned int colorBits = 1 + chunk.tileIndex().level % 6;
        const glm::vec4 color = glm::vec4(
            colorBits & 1,
            colorBits & 2,
            colorBits & 4,
            0.3f
        );

        if (_owner.debugProperties().showChunkBounds) {
            DebugRenderer::ref().renderNiceBox(clippingSpaceCorners, color);
        }

        if (_owner.debugProperties().showChunkAABB) {
            const std::vector<glm::vec4>& screenSpacePoints =
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
