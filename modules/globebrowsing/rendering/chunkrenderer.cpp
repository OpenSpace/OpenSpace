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

#include <modules/globebrowsing/rendering/chunkrenderer.h>

#include <modules/globebrowsing/chunk/chunk.h>
#include <modules/globebrowsing/geometry/ellipsoid.h>
#include <modules/globebrowsing/globes/renderableglobe.h>
#include <modules/globebrowsing/meshes/grid.h>
#include <modules/globebrowsing/meshes/trianglesoup.h>
#include <modules/globebrowsing/rendering/layershadermanager.h>
#include <modules/globebrowsing/rendering/layer/layermanager.h>
#include <modules/globebrowsing/rendering/gpu/gpulayergroup.h>
#include <modules/globebrowsing/rendering/gpu/gpulayermanager.h>
#include <modules/globebrowsing/rendering/layer/layergroup.h>
#include <openspace/util/updatestructures.h>
#include <openspace/util/spicemanager.h>
#include <ghoul/opengl/programobject.h>

namespace {
    constexpr const double KM_TO_M = 1000.0;
} // namespace

namespace openspace::globebrowsing {

ChunkRenderer::ChunkRenderer(std::shared_ptr<Grid> grid,
                             std::shared_ptr<LayerManager> layerManager,
                             Ellipsoid& ellipsoid)
    : _grid(std::move(grid))
    , _layerManager(std::move(layerManager))
    , _ellipsoid(ellipsoid)
{
    _globalLayerShaderManager = std::make_shared<LayerShaderManager>(
        "GlobalChunkedLodPatch",
        "${MODULE_GLOBEBROWSING}/shaders/globalchunkedlodpatch_vs.glsl",
        "${MODULE_GLOBEBROWSING}/shaders/globalchunkedlodpatch_fs.glsl"
    );

    _localLayerShaderManager = std::make_shared<LayerShaderManager>(
        "LocalChunkedLodPatch",
        "${MODULE_GLOBEBROWSING}/shaders/localchunkedlodpatch_vs.glsl",
        "${MODULE_GLOBEBROWSING}/shaders/localchunkedlodpatch_fs.glsl"
    );

    _globalGpuLayerManager = std::make_shared<GPULayerManager>();
    _localGpuLayerManager = std::make_shared<GPULayerManager>();
}

ChunkRenderer::~ChunkRenderer() {} // NOLINT

void ChunkRenderer::renderChunk(const Chunk& chunk, const RenderData& data) {
    // A little arbitrary with 10 but it works
    if (chunk.tileIndex().level <
        chunk.owner().debugProperties().modelSpaceRenderingCutoffLevel)
    {
        renderChunkGlobally(chunk, data);
    }
    else {
        renderChunkLocally(chunk, data);
    }
}

void ChunkRenderer::update() {
    // unused atm. Could be used for caching or precalculating
}

void ChunkRenderer::recompileShaders(const RenderableGlobe& globe) {
    LayerShaderManager::LayerShaderPreprocessingData preprocessingData =
        LayerShaderManager::LayerShaderPreprocessingData::get(globe);
    _globalLayerShaderManager->recompileShaderProgram(preprocessingData);
    _localLayerShaderManager->recompileShaderProgram(preprocessingData);
}

ghoul::opengl::ProgramObject* ChunkRenderer::getActivatedProgramWithTileData(
                                                 LayerShaderManager& layeredShaderManager,
                                                         GPULayerManager& gpuLayerManager,
                                                                       const Chunk& chunk)
{
    const TileIndex& tileIndex = chunk.tileIndex();

    // Now the shader program can be accessed
    ghoul::opengl::ProgramObject* programObject = layeredShaderManager.programObject();

    if (layeredShaderManager.updatedSinceLastCall()) {
        gpuLayerManager.bind(programObject, *_layerManager);
    }

    // Activate the shader program
    programObject->activate();

    gpuLayerManager.setValue(programObject, *_layerManager, tileIndex);

    // The length of the skirts is proportional to its size
    programObject->setUniform(
        "skirtLength",
        static_cast<float>(
            glm::min(
                chunk.surfacePatch().halfSize().lat * 1000000,
                _ellipsoid.minimumRadius()
            )
        )
    );

    //programObject->setUniform("skirtLength",
    //    glm::min(static_cast<float>(chunk.surfacePatch().halfSize().lat * 1000000),
    //        8700.0f));

    programObject->setUniform("xSegments", _grid->xSegments());

    if (chunk.owner().debugProperties().showHeightResolution) {
        programObject->setUniform(
            "vertexResolution",
            glm::vec2(_grid->xSegments(), _grid->ySegments())
        );
    }

    return programObject;
}

void ChunkRenderer::calculateEclipseShadows(const Chunk& chunk,
                                            ghoul::opengl::ProgramObject* programObject,
                                            const RenderData& data)
{
    // Shadow calculations..
    if (chunk.owner().ellipsoid().hasEclipseShadows()) {
        std::vector<RenderableGlobe::ShadowRenderingStruct> shadowDataArray;
        std::vector<Ellipsoid::ShadowConfiguration> shadowConfArray =
            chunk.owner().ellipsoid().shadowConfigurationArray();
        shadowDataArray.reserve(shadowConfArray.size());
        double lt;
        for (const auto & shadowConf : shadowConfArray) {
            // TO REMEMBER: all distances and lengths in world coordinates are in
            // meters!!! We need to move this to view space...
            // Getting source and caster:
            glm::dvec3 sourcePos = SpiceManager::ref().targetPosition(
                shadowConf.source.first,
                "SUN",
                "GALACTIC",
                {},
                data.time.j2000Seconds(),
                lt
            );
            sourcePos *= KM_TO_M; // converting to meters
            glm::dvec3 casterPos = SpiceManager::ref().targetPosition(
                shadowConf.caster.first,
                "SUN",
                "GALACTIC",
                {},
                data.time.j2000Seconds(),
                lt
            );
            casterPos *= KM_TO_M; // converting to meters
            // psc caster_pos = PowerScaledCoordinate::CreatePowerScaledCoordinate(
            //     casterPos.x,
            //     casterPos.y,
            //     casterPos.z
            // );


            // First we determine if the caster is shadowing the current planet (all
            // calculations in World Coordinates):
            const glm::dvec3 planetCasterVec = casterPos - data.position.dvec3();
            const glm::dvec3 sourceCasterVec = casterPos - sourcePos;
            const double sc_length = glm::length(sourceCasterVec);
            const glm::dvec3 planetCaster_proj =
                (glm::dot(planetCasterVec, sourceCasterVec) / (sc_length*sc_length)) *
                sourceCasterVec;
            const double d_test = glm::length(planetCasterVec - planetCaster_proj);
            const double xp_test = shadowConf.caster.second * sc_length /
                                   (shadowConf.source.second + shadowConf.caster.second);
            const double rp_test = shadowConf.caster.second *
                                   (glm::length(planetCaster_proj) + xp_test) / xp_test;

            const glm::dvec3 sunPos = SpiceManager::ref().targetPosition(
                "SUN",
                "SUN",
                "GALACTIC",
                {},
                data.time.j2000Seconds(),
                lt
            );
            const double casterDistSun = glm::length(casterPos - sunPos);
            const double planetDistSun = glm::length(data.position.dvec3() - sunPos);

            RenderableGlobe::ShadowRenderingStruct shadowData;
            shadowData.isShadowing = false;

            // Eclipse shadows considers planets and moons as spheres
            if (((d_test - rp_test) < (chunk.owner().ellipsoid().radii().x * KM_TO_M)) &&
                (casterDistSun < planetDistSun))
            {
                // The current caster is shadowing the current planet
                shadowData.isShadowing = true;
                shadowData.rs = shadowConf.source.second;
                shadowData.rc = shadowConf.caster.second;
                shadowData.sourceCasterVec = glm::normalize(sourceCasterVec);
                shadowData.xp = xp_test;
                shadowData.xu = shadowData.rc * sc_length /
                                (shadowData.rs - shadowData.rc);
                shadowData.casterPositionVec = casterPos;
            }
            shadowDataArray.push_back(shadowData);
        }

        const std::string uniformVarName("shadowDataArray[");
        unsigned int counter = 0;
        for (const RenderableGlobe::ShadowRenderingStruct& sd : shadowDataArray) {
            constexpr const char* NameIsShadowing = "shadowDataArray[{}].isShadowing";
            constexpr const char* NameXp = "shadowDataArray[{}].xp";
            constexpr const char* NameXu = "shadowDataArray[{}].xu";
            constexpr const char* NameRc = "shadowDataArray[{}].rc";
            constexpr const char* NameSource = "shadowDataArray[{}].sourceCasterVec";
            constexpr const char* NamePos= "shadowDataArray[{}].casterPositionVec";

            programObject->setUniform(
                fmt::format(NameIsShadowing, counter), sd.isShadowing
            );
            if (sd.isShadowing) {
                programObject->setUniform(fmt::format(NameXp, counter), sd.xp);
                programObject->setUniform(fmt::format(NameXu, counter), sd.xu);
                programObject->setUniform(fmt::format(NameRc, counter), sd.rc);
                programObject->setUniform(
                    fmt::format(NameSource, counter), sd.sourceCasterVec
                );
                programObject->setUniform(
                    fmt::format(NamePos, counter), sd.casterPositionVec
                );
            }
            counter++;
        }

        programObject->setUniform(
            "inverseViewTransform",
            glm::inverse(data.camera.combinedViewMatrix())
        );
        programObject->setUniform("modelTransform", chunk.owner().modelTransform());
        programObject->setUniform(
            "hardShadows",
            chunk.owner().generalProperties().eclipseHardShadows
        );
        programObject->setUniform("calculateEclipseShadows", true);
    }
}

void ChunkRenderer::setCommonUniforms(ghoul::opengl::ProgramObject& programObject,
                                      const Chunk& chunk, const RenderData& data)
{
    const glm::dmat4 modelTransform = chunk.owner().modelTransform();
    const glm::dmat4 viewTransform = data.camera.combinedViewMatrix();
    const glm::dmat4 modelViewTransform = viewTransform * modelTransform;

    const bool nightLayersActive =
        !_layerManager->layerGroup(layergroupid::NightLayers).activeLayers().empty();
    const bool waterLayersActive =
        !_layerManager->layerGroup(layergroupid::WaterMasks).activeLayers().empty();

    if (nightLayersActive || waterLayersActive ||
        chunk.owner().generalProperties().atmosphereEnabled ||
        chunk.owner().generalProperties().performShading)
    {
        const glm::dvec3 directionToSunWorldSpace =
            length(data.modelTransform.translation) > 0.0 ?
            glm::normalize(-data.modelTransform.translation) :
            glm::dvec3(0.0);

        const glm::vec3 directionToSunCameraSpace = glm::vec3(viewTransform *
                                                 glm::dvec4(directionToSunWorldSpace, 0));
        programObject.setUniform(
            "lightDirectionCameraSpace",
            -glm::normalize(directionToSunCameraSpace)
        );
    }

    if (chunk.owner().generalProperties().performShading) {
        programObject.setUniform(
            "orenNayarRoughness",
            chunk.owner().generalProperties().orenNayarRoughness);
    }

    if (chunk.owner().generalProperties().useAccurateNormals &&
        !_layerManager->layerGroup(layergroupid::HeightLayers).activeLayers().empty())
    {
        const glm::dvec3 corner00 = chunk.owner().ellipsoid().cartesianSurfacePosition(
            chunk.surfacePatch().corner(Quad::SOUTH_WEST)
        );
        const glm::dvec3 corner10 = chunk.owner().ellipsoid().cartesianSurfacePosition(
            chunk.surfacePatch().corner(Quad::SOUTH_EAST)
        );
        const glm::dvec3 corner01 = chunk.owner().ellipsoid().cartesianSurfacePosition(
            chunk.surfacePatch().corner(Quad::NORTH_WEST)
        );
        const glm::dvec3 corner11 = chunk.owner().ellipsoid().cartesianSurfacePosition(
            chunk.surfacePatch().corner(Quad::NORTH_EAST)
        );

        // This is an assumption that the height tile has a resolution of 64 * 64
        // If it does not it will still produce "correct" normals. If the resolution is
        // higher the shadows will be softer, if it is lower, pixels will be visible.
        // Since default is 64 this will most likely work fine.

        const glm::mat3& modelViewTransformMat3 = glm::mat3(modelViewTransform);

        constexpr const float TileDelta = 1.f / 64.f;
        const glm::vec3 deltaTheta0 = modelViewTransformMat3 *
                                      (glm::vec3(corner10 - corner00) * TileDelta);
        const glm::vec3 deltaTheta1 = modelViewTransformMat3 *
                                      (glm::vec3(corner11 - corner01) * TileDelta);
        const glm::vec3 deltaPhi0 = modelViewTransformMat3 *
                                    (glm::vec3(corner01 - corner00) * TileDelta);
        const glm::vec3 deltaPhi1 = modelViewTransformMat3 *
                                    (glm::vec3(corner11 - corner10) * TileDelta);

        // Upload uniforms
        programObject.setUniform("deltaTheta0", glm::length(deltaTheta0));
        programObject.setUniform("deltaTheta1", glm::length(deltaTheta1));
        programObject.setUniform("deltaPhi0", glm::length(deltaPhi0));
        programObject.setUniform("deltaPhi1", glm::length(deltaPhi1));
        programObject.setUniform("tileDelta", TileDelta);
        // This should not be needed once the light calculations for the atmosphere
        // is performed in view space..
        programObject.setUniform(
            "invViewModelTransform",
            glm::inverse(
                glm::mat4(data.camera.combinedViewMatrix()) *
                glm::mat4(chunk.owner().modelTransform())
            )
        );
    }
}

void ChunkRenderer::renderChunkGlobally(const Chunk& chunk, const RenderData& data) {
    ghoul::opengl::ProgramObject* programObject = getActivatedProgramWithTileData(
        *_globalLayerShaderManager,
        *_globalGpuLayerManager,
        chunk
    );
    if (!programObject) {
        return;
    }

    const Ellipsoid& ellipsoid = chunk.owner().ellipsoid();

    if (_layerManager->hasAnyBlendingLayersEnabled()) {
        // Calculations are done in the reference frame of the globe. Hence, the
        // camera position needs to be transformed with the inverse model matrix
        const glm::dmat4 inverseModelTransform = chunk.owner().inverseModelTransform();
        const glm::dvec3 cameraPosition = glm::dvec3(
            inverseModelTransform * glm::dvec4(data.camera.positionVec3(), 1.0)
        );
        const float distanceScaleFactor = static_cast<float>(
            chunk.owner().generalProperties().lodScaleFactor * ellipsoid.minimumRadius()
        );
        programObject->setUniform("cameraPosition", glm::vec3(cameraPosition));
        programObject->setUniform("distanceScaleFactor", distanceScaleFactor);
        programObject->setUniform("chunkLevel", chunk.tileIndex().level);
    }

    // Calculate other uniform variables needed for rendering
    const Geodetic2 swCorner = chunk.surfacePatch().corner(Quad::SOUTH_WEST);
    const Geodetic2& patchSize = chunk.surfacePatch().size();

    const glm::dmat4 modelTransform = chunk.owner().modelTransform();
    const glm::dmat4 viewTransform = data.camera.combinedViewMatrix();
    const glm::mat4 modelViewTransform = glm::mat4(viewTransform * modelTransform);
    const glm::mat4 modelViewProjectionTransform =
        data.camera.sgctInternal.projectionMatrix() * modelViewTransform;

    // Upload the uniform variables
    programObject->setUniform(
        "modelViewProjectionTransform",
        modelViewProjectionTransform
    );
    programObject->setUniform("minLatLon", glm::vec2(swCorner.toLonLatVec2()));
    programObject->setUniform("lonLatScalingFactor", glm::vec2(patchSize.toLonLatVec2()));
    // Ellipsoid Radius (Model Space)
    programObject->setUniform("radiiSquared", glm::vec3(ellipsoid.radiiSquared()));

    const bool hasNightLayers = !_layerManager->layerGroup(
        layergroupid::GroupID::NightLayers
    ).activeLayers().empty();

    const bool hasWaterLayer = !_layerManager->layerGroup(
        layergroupid::GroupID::WaterMasks
    ).activeLayers().empty();

    const bool hasHeightLayer = !_layerManager->layerGroup(
        layergroupid::HeightLayers
    ).activeLayers().empty();

    if (hasNightLayers || hasWaterLayer ||
        chunk.owner().generalProperties().atmosphereEnabled ||
        chunk.owner().generalProperties().performShading)
    {
        programObject->setUniform("modelViewTransform", modelViewTransform);
    }

    if (chunk.owner().generalProperties().useAccurateNormals && hasHeightLayer) {
        // Apply an extra scaling to the height if the object is scaled
        programObject->setUniform(
            "heightScale",
            static_cast<float>(data.modelTransform.scale * data.camera.scaling())
        );
    }

    setCommonUniforms(*programObject, chunk, data);

    if (chunk.owner().ellipsoid().hasEclipseShadows()) {
        calculateEclipseShadows(chunk, programObject, data);
    }

    // OpenGL rendering settings
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);

    // render
    _grid->geometry().drawUsingActiveProgram();

    _globalGpuLayerManager->deactivate();

    // disable shader
    programObject->deactivate();
}

void ChunkRenderer::renderChunkLocally(const Chunk& chunk, const RenderData& data) {
    ghoul::opengl::ProgramObject* programObject = getActivatedProgramWithTileData(
        *_localLayerShaderManager,
        *_localGpuLayerManager,
        chunk
    );
    if (!programObject) {
        return;
    }

    const Ellipsoid& ellipsoid = chunk.owner().ellipsoid();

    if (_layerManager->hasAnyBlendingLayersEnabled()) {
        float distanceScaleFactor = static_cast<float>(
            chunk.owner().generalProperties().lodScaleFactor *
            chunk.owner().ellipsoid().minimumRadius()
        );

        programObject->setUniform("distanceScaleFactor", distanceScaleFactor);
        programObject->setUniform("chunkLevel", chunk.tileIndex().level);
    }

    // Calculate other uniform variables needed for rendering
    // Send the matrix inverse to the fragment for the global and local shader (JCC)
    const glm::dmat4 modelTransform = chunk.owner().modelTransform();
    const glm::dmat4 viewTransform = data.camera.combinedViewMatrix();
    const glm::dmat4 modelViewTransform = viewTransform * modelTransform;


    std::array<glm::dvec3, 4> cornersCameraSpace;
    std::array<glm::dvec3, 4> cornersModelSpace;
    for (int i = 0; i < 4; ++i) {
        constexpr const std::array<const char*, 4> CornerNames = {
            "p01", "p11", "p00", "p10"
        };

        const Quad q = static_cast<Quad>(i);
        const Geodetic2 corner = chunk.surfacePatch().corner(q);
        const glm::dvec3 cornerModelSpace = ellipsoid.cartesianSurfacePosition(corner);
        cornersModelSpace[i] = cornerModelSpace;
        const glm::dvec3 cornerCameraSpace = glm::dvec3(
            modelViewTransform * glm::dvec4(cornerModelSpace, 1)
        );
        cornersCameraSpace[i] = cornerCameraSpace;
        programObject->setUniform(CornerNames[i], glm::vec3(cornerCameraSpace));

    }

    // TODO: Patch normal can be calculated for all corners and then linearly
    // interpolated on the GPU to avoid cracks for high altitudes.
    const glm::vec3 patchNormalCameraSpace = normalize(
        cross(
            cornersCameraSpace[Quad::SOUTH_EAST] - cornersCameraSpace[Quad::SOUTH_WEST],
            cornersCameraSpace[Quad::NORTH_EAST] - cornersCameraSpace[Quad::SOUTH_WEST]
        )
    );

    // In order to improve performance, lets use the normal in object space (model space)
    // for deferred rendering.
    const glm::vec3 patchNormalModelSpace = normalize(
        cross(
            cornersModelSpace[Quad::SOUTH_EAST] - cornersModelSpace[Quad::SOUTH_WEST],
            cornersModelSpace[Quad::NORTH_EAST] - cornersModelSpace[Quad::SOUTH_WEST]
        )
    );

    programObject->setUniform("patchNormalModelSpace", patchNormalModelSpace);
    programObject->setUniform("patchNormalCameraSpace", patchNormalCameraSpace);
    programObject->setUniform(
        "projectionTransform",
        data.camera.sgctInternal.projectionMatrix()
    );

    if (!_layerManager->layerGroup(layergroupid::HeightLayers).activeLayers().empty()) {
        // Apply an extra scaling to the height if the object is scaled
        programObject->setUniform(
            "heightScale",
            static_cast<float>(data.modelTransform.scale * data.camera.scaling())
        );
    }

    setCommonUniforms(*programObject, chunk, data);

    if (chunk.owner().ellipsoid().hasEclipseShadows()) {
        calculateEclipseShadows(chunk, programObject, data);
    }

    // OpenGL rendering settings
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);

    // render
    _grid->geometry().drawUsingActiveProgram();

    _localGpuLayerManager->deactivate();

    // disable shader
    programObject->deactivate();
}

} // namespace openspace:;globebrowsing
