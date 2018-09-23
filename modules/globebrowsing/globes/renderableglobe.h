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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___RENDERABLEGLOBE___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___RENDERABLEGLOBE___H__

#include <openspace/rendering/renderable.h>

#include <modules/globebrowsing/geometry/ellipsoid.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <modules/globebrowsing/meshes/skirtedgrid.h>
#include <modules/globebrowsing/rendering/layer/layermanager.h>
#include <ghoul/opengl/uniformcache.h>

namespace openspace::globebrowsing {

    namespace chunklevelevaluator { class Evaluator; }

    namespace culling { class ChunkCuller; }

    class Chunk;
    struct ChunkNode;
    class ChunkRenderer;
    class Ellipsoid;
    struct Geodetic2;
    class LayerManager;
    class RenderableGlobe;
    class Chunk;
    class Ellipsoid;
    class Grid;
    class GPULayerManager;
    class LayerManager;
    class LayerShaderManager;
    class RenderableGlobe;
    class GPULayerGroup;

class PointGlobe;
class LayerManager;

/**
 * A RenderableGlobe is a globe modeled as an ellipsoid using a chunked LOD algorithm for
 * rendering.
 */
class RenderableGlobe : public Renderable {
public:


    RenderableGlobe(const ghoul::Dictionary& dictionary);
    ~RenderableGlobe() = default;

    void initializeGL() override;
    void deinitialize() override;
    void deinitializeGL() override;
    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    virtual SurfacePositionHandle calculateSurfacePositionHandle(
                                       const glm::dvec3& targetModelSpace) const override;

//private:
    // Properties
    struct {
        properties::BoolProperty saveOrThrowCamera;
        properties::BoolProperty showChunkEdges;
        properties::BoolProperty showChunkBounds;
        properties::BoolProperty showChunkAABB;
        properties::BoolProperty showHeightResolution;
        properties::BoolProperty showHeightIntensities;
        properties::BoolProperty performFrustumCulling;
        properties::BoolProperty performHorizonCulling;
        properties::BoolProperty levelByProjectedAreaElseDistance;
        properties::BoolProperty resetTileProviders;
        properties::BoolProperty limitLevelByAvailableData;
        properties::IntProperty modelSpaceRenderingCutoffLevel;
    } _debugProperties;

    struct {
        properties::BoolProperty performShading;
        properties::BoolProperty useAccurateNormals;
        properties::BoolProperty eclipseShadowsEnabled;
        properties::BoolProperty eclipseHardShadows;
        properties::FloatProperty lodScaleFactor;
        properties::FloatProperty cameraMinHeight;
        properties::FloatProperty orenNayarRoughness;
    } _generalProperties;

    properties::PropertyOwner _debugPropertyOwner;



    /**
     * Traverse the chunk tree and find the highest level chunk node.
     *
     * \param location is given in geodetic coordinates and must be in the range
     * latitude [-90, 90] and longitude [-180, 180]. In other words, it must be a
     * position defined on the globe in georeferenced coordinates.
     */
    //const ChunkNode& findChunkNode(const Geodetic2& location) const;

    /**
     * Test if a specific chunk can safely be culled without affecting the rendered
     * image.
     *
     * Goes through all available <code>ChunkCuller</code>s and check if any of them
     * allows culling of the <code>Chunk</code>s in question.
     */
    bool testIfCullable(const Chunk& chunk, const RenderData& renderData) const;

    /**
     * Gets the desired level which can be used to determine if a chunk should split
     * or merge.
     *
     * Using <code>ChunkLevelEvaluator</code>s, the desired level can be higher or
     * lower than the current level of the <code>Chunks</code>s
     * <code>TileIndex</code>. If the desired level is higher than that of the
     * <code>Chunk</code>, it wants to split. If it is lower, it wants to merge with
     * its siblings.
     */
    int desiredLevel(const Chunk& chunk, const RenderData& renderData) const;

    /**
     * Calculates the height from the surface of the reference ellipsoid to the
     * heigh mapped surface.
     *
     * The height can be negative if the height map contains negative values.
     *
     * \param <code>position</code> is the position of a point that gets geodetically
     * projected on the reference ellipsoid. <code>position</code> must be in
     * cartesian model space.
     * \returns the height from the reference ellipsoid to the globe surface.
     */
    float getHeight(const glm::dvec3& position) const;


    const LayerManager& layerManager() const;
    LayerManager& layerManager() ;
    const Ellipsoid& ellipsoid() const;
    const glm::dmat4& modelTransform() const;
    Camera* savedCamera() const;



private:
    constexpr static const int MinSplitDepth = 2;
    constexpr static const int MaxSplitDepth = 22;  // increase? (abock)

    // Shadow structure
    struct ShadowRenderingStruct {
        double xu;
        double xp;
        double rs;
        double rc;
        glm::dvec3 sourceCasterVec;
        glm::dvec3 casterPositionVec;
        bool isShadowing;
    };

    void renderChunks(const RenderData& data, RendererTasks& rendererTask);
    void debugRenderChunk(const Chunk& chunk, const glm::dmat4& mvp) const;

    bool isCullableByFrustum(const Chunk& chunk, const RenderData& renderData) const;
    bool isCullableByHorizon(const Chunk& chunk, const RenderData& renderData) const;

    int desiredLevelByDistance(const Chunk& chunk, const RenderData& data) const;
    int desiredLevelByProjectedArea(const Chunk& chunk, const RenderData& data) const;
    int desiredLevelByAvailableTileData(const Chunk& chunk, const RenderData& data) const;

    /**
     * Chunks can be rendered either globally or locally. Global rendering is performed
     * in the model space of the globe. With global rendering, the vertex positions
     * of a chunk are calculated in the vertex shader by transforming the geodetic
     * coordinates of the chunk to model space coordinates. We can only achieve floating
     * point precision by doing this which means that the camera too close to a global
     * tile will lead to jagging. We only render global chunks for lower chunk levels.
     */
    void renderChunkGlobally(const Chunk& chunk, const RenderData& data);

    /**
     * Local rendering of chunks are done using linear interpolation in camera space.
     * All four corner points of the chunk are calculated in double precision on the
     * CPU and transformed to camera space with double precision matrix transforms.
     * These positions can then be cast to floats and uploaded to the vertex shader.
     * The vertex shader rendering performs linear interpolation between the four
     * corner points to get the resulting chunk. This means that there will be an error
     * due to the curvature of the globe. The smaller the patch is (with higher chunk
     * levels) the better the approximation becomes. This is why we only render local
     * chunks for higher chunk levels.
     */
    void renderChunkLocally(const Chunk& chunk, const RenderData& data);

    void calculateEclipseShadows(const Chunk& chunk,
        ghoul::opengl::ProgramObject* programObject, const RenderData& data);

    void setCommonUniforms(ghoul::opengl::ProgramObject& programObject,
        const Chunk& chunk, const RenderData& data);


    /**
     * Directly recompile the shaders of the renderer.
     */
    void recompileShaders();





    SkirtedGrid _grid;

    Ellipsoid _ellipsoid;
    LayerManager _layerManager;
    std::unique_ptr<Camera> _savedCamera;

    glm::dmat4 _cachedModelTransform;
    glm::dmat4 _cachedInverseModelTransform;

    // Covers all negative longitudes
    std::unique_ptr<ChunkNode> _leftRoot;

    // Covers all positive longitudes
    std::unique_ptr<ChunkNode> _rightRoot;



    // Two different shader programs. One for global and one for local rendering.
    struct {
        std::unique_ptr<ghoul::opengl::ProgramObject> program;
        bool updatedSinceLastCall = false;
        UniformCache(skirtLength, minLatLon, lonLatScalingFactor,
            orenNayarRoughness) uniformCache;

        std::vector<std::unique_ptr<GPULayerGroup>> gpuLayerGroups;
    } _globalRenderer;

    struct {
        std::unique_ptr<ghoul::opengl::ProgramObject> program;
        bool updatedSinceLastCall = false;
        UniformCache(skirtLength, p01, p11, p00, p10, patchNormalModelSpace, 
            patchNormalCameraSpace, orenNayarRoughness) uniformCache;

        std::vector<std::unique_ptr<GPULayerGroup>> gpuLayerGroups;
    } _localRenderer;

    bool _shadersNeedRecompilation = true;
    bool _lodScaleFactorDirty = true;
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___RENDERABLEGLOBE___H__
