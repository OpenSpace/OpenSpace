/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___RENDERABLEGLOBE___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___RENDERABLEGLOBE___H__

#include <openspace/rendering/renderable.h>

#include <modules/globebrowsing/src/ellipsoid.h>
#include <modules/globebrowsing/src/geodeticpatch.h>
#include <modules/globebrowsing/src/geojson/geojsonmanager.h>
#include <modules/globebrowsing/src/globelabelscomponent.h>
#include <modules/globebrowsing/src/gpulayergroup.h>
#include <modules/globebrowsing/src/layermanager.h>
#include <modules/globebrowsing/src/ringscomponent.h>
#include <modules/globebrowsing/src/shadowcomponent.h>
#include <modules/globebrowsing/src/skirtedgrid.h>
#include <modules/globebrowsing/src/tileindex.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <ghoul/misc/memorypool.h>
#include <ghoul/opengl/uniformcache.h>
#include <cstddef>
#include <memory>

namespace openspace::documentation { struct Documentation; }

namespace openspace::globebrowsing {

class GPULayerGroup;
class RenderableGlobe;
struct TileIndex;

struct BoundingHeights {
    float min;
    float max;
    bool available;
    bool tileOK;
};

namespace chunklevelevaluator { class Evaluator; }
namespace culling { class ChunkCuller; }

struct Chunk {
    enum class Status : uint8_t {
        DoNothing,
        WantMerge,
        WantSplit
    };

    Chunk(const TileIndex& ti);

    const TileIndex tileIndex;
    const GeodeticPatch surfacePatch;

    Status status;

    bool isVisible = true;
    bool colorTileOK = false;
    bool heightTileOK = false;

    std::array<glm::dvec4, 8> corners;
    std::array<Chunk*, 4> children = { { nullptr, nullptr, nullptr, nullptr } };
};

enum class ShadowCompType {
    GLOBAL_SHADOW,
    LOCAL_SHADOW
};

/**
 * A RenderableGlobe is a globe modeled as an ellipsoid using a chunked LOD algorithm for
 * rendering.
 */
class RenderableGlobe : public Renderable {
public:
    RenderableGlobe(const ghoul::Dictionary& dictionary);
    ~RenderableGlobe() override = default;

    void initializeGL() override;
    void deinitialize() override;
    void deinitializeGL() override;
    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void renderSecondary(const RenderData& data, RendererTasks&) override;
    void update(const UpdateData& data) override;

    SurfacePositionHandle calculateSurfacePositionHandle(
        const glm::dvec3& targetModelSpace) const override;

    bool renderedWithDesiredData() const override;

    const Ellipsoid& ellipsoid() const;
    const LayerManager& layerManager() const;
    LayerManager& layerManager();
    const GeoJsonManager& geoJsonManager() const;
    GeoJsonManager& geoJsonManager();

    const glm::dmat4& modelTransform() const;

    static documentation::Documentation Documentation();

private:
    static constexpr int MinSplitDepth = 2;
    static constexpr int MaxSplitDepth = 22;

    struct {
        properties::BoolProperty showChunkEdges;
        properties::BoolProperty levelByProjectedAreaElseDistance;
        properties::BoolProperty resetTileProviders;
        properties::BoolProperty performFrustumCulling;
        properties::IntProperty  modelSpaceRenderingCutoffLevel;
        properties::IntProperty  dynamicLodIterationCount;
    } _debugProperties;

    struct {
        properties::BoolProperty  performShading;
        properties::BoolProperty  useAccurateNormals;
        properties::BoolProperty  eclipseShadowsEnabled;
        properties::BoolProperty  eclipseHardShadows;
        properties::BoolProperty  shadowMapping;
        properties::BoolProperty  renderAtDistance;
        properties::FloatProperty zFightingPercentage;
        properties::IntProperty   nShadowSamples;
        properties::FloatProperty targetLodScaleFactor;
        properties::FloatProperty currentLodScaleFactor;
        properties::FloatProperty orenNayarRoughness;
        properties::FloatProperty ambientIntensity;
        properties::IntProperty   nActiveLayers;
    } _generalProperties;

    properties::PropertyOwner _debugPropertyOwner;

    properties::PropertyOwner _shadowMappingPropertyOwner;

    /**
     * Test if a specific chunk can safely be culled without affecting the rendered image.
     *
     * Goes through all available `ChunkCuller`s and check if any of them allows culling
     * of the `Chunk`s in question.
     */
    bool testIfCullable(const Chunk& chunk, const RenderData& renderData,
        const BoundingHeights& heights, const glm::dmat4& mvp) const;

    /**
     * Gets the desired level which can be used to determine if a chunk should split or
     * merge.
     *
     * Using `ChunkLevelEvaluator`s, the desired level can be higher or lower than the
     * current level of the `Chunks`s `TileIndex`. If the desired level is higher than
     * that of the `Chunk`, it wants to split. If it is lower, it wants to merge with its
     * siblings.
     */
    int desiredLevel(const Chunk& chunk, const RenderData& renderData,
        const BoundingHeights& heights) const;

    /**
     * Calculates the height from the surface of the reference ellipsoid to the height
     * mapped surface.
     *
     * The height can be negative if the height map contains negative values.
     *
     * \param `position` is the position of a point that gets geodetically projected on
     *        the reference ellipsoid. `position` must be in Cartesian model space
     * \return The height from the reference ellipsoid to the globe surface
     */
    float getHeight(const glm::dvec3& position) const;

    void renderChunks(const RenderData& data, RendererTasks& rendererTask,
        const ShadowComponent::ShadowMapData& shadowData = {}, bool renderGeomOnly = false
    );

    /**
     * Chunks can be rendered either globally or locally. Global rendering is performed
     * in the model space of the globe. With global rendering, the vertex positions of a
     * chunk are calculated in the vertex shader by transforming the geodetic coordinates
     * of the chunk to model space coordinates. We can only achieve floating point
     * precision by doing this which means that the camera too close to a global tile will
     * lead to jagging. We only render global chunks for lower chunk levels.
     */
    void renderChunkGlobally(const Chunk& chunk, const RenderData& data,
        const ShadowComponent::ShadowMapData& shadowData = {}, bool renderGeomOnly = false
    );

    /**
     * Local rendering of chunks are done using linear interpolation in camera space. All
     * four corner points of the chunk are calculated in double precision on the CPU and
     * transformed to camera space with double precision matrix transforms. These
     * positions can then be cast to floats and uploaded to the vertex shader. The vertex
     * shader rendering performs linear interpolation between the four corner points to
     * get the resulting chunk. This means that there will be an error due to the
     * curvature of the globe. The smaller the patch is (with higher chunk levels) the
     * better the approximation becomes. This is why we only render local chunks for
     * higher chunk levels.
     */
    void renderChunkLocally(const Chunk& chunk, const RenderData& data,
        const ShadowComponent::ShadowMapData& shadowData = {}, bool renderGeomOnly = false
    );

    void debugRenderChunk(const Chunk& chunk, const glm::dmat4& mvp,
        bool renderBounds) const;

    bool isCullableByFrustum(const Chunk& chunk, const RenderData& renderData,
        const glm::dmat4& mvp) const;
    bool isCullableByHorizon(const Chunk& chunk, const RenderData& renderData,
        const BoundingHeights& heights) const;

    int desiredLevelByDistance(const Chunk& chunk, const RenderData& data,
        const BoundingHeights& heights) const;
    int desiredLevelByProjectedArea(const Chunk& chunk, const RenderData& data,
        const BoundingHeights& heights) const;
    int desiredLevelByAvailableTileData(const Chunk& chunk) const;


    void calculateEclipseShadows(ghoul::opengl::ProgramObject& programObject,
        const RenderData& data, ShadowCompType stype);

    void setCommonUniforms(ghoul::opengl::ProgramObject& programObject,
        const Chunk& chunk, const RenderData& data);


    void recompileShaders();


    void splitChunkNode(Chunk& cn, int depth);
    void mergeChunkNode(Chunk& cn);
    bool updateChunkTree(Chunk& cn, const RenderData& data, const glm::dmat4& mvp);
    void updateChunk(Chunk& chunk, const RenderData& data, const glm::dmat4& mvp) const;
    void freeChunkNode(Chunk* n);

    Ellipsoid _ellipsoid;
    SkirtedGrid _grid;
    LayerManager _layerManager;

    GeoJsonManager _geoJsonManager;

    glm::dmat4 _cachedModelTransform = glm::dmat4(1.0);
    glm::dmat4 _cachedInverseModelTransform = glm::dmat4(1.0);

    ghoul::ReusableTypedMemoryPool<Chunk, 256> _chunkPool;

    std::vector<const Chunk*> _globalChunkBuffer;
    std::vector<const Chunk*> _localChunkBuffer;
    std::vector<const Chunk*> _traversalMemory;


    Chunk _leftRoot;  // Covers all negative longitudes
    Chunk _rightRoot; // Covers all positive longitudes

    // Two different shader programs. One for global and one for local rendering.
    struct {
        std::unique_ptr<ghoul::opengl::ProgramObject> program;
        bool updatedSinceLastCall = false;
        UniformCache(skirtLength, minLatLon, lonLatScalingFactor) uniformCache;

        std::array<GPULayerGroup, LayerManager::NumLayerGroups> gpuLayerGroups;
    } _globalRenderer;

    struct {
        std::unique_ptr<ghoul::opengl::ProgramObject> program;
        bool updatedSinceLastCall = false;
        UniformCache(skirtLength, p01, p11, p00, p10,
            patchNormalCameraSpace) uniformCache;

        std::array<GPULayerGroup, LayerManager::NumLayerGroups> gpuLayerGroups;
    } _localRenderer;

    bool _shadersNeedRecompilation = true;
    bool _lodScaleFactorDirty = true;
    bool _chunkCornersDirty = true;
    bool _nLayersIsDirty = true;
    bool _allChunksAvailable = true;
    bool _layerManagerDirty = true;
    size_t _iterationsOfAvailableData = 0;
    size_t _iterationsOfUnavailableData = 0;
    Layer* _lastChangedLayer = nullptr;

    // Components
    std::unique_ptr<RingsComponent> _ringsComponent;
    std::unique_ptr<ShadowComponent> _shadowComponent;

    // Labels
    GlobeLabelsComponent _globeLabelsComponent;
    ghoul::Dictionary _labelsDictionary;
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___RENDERABLEGLOBE___H__
