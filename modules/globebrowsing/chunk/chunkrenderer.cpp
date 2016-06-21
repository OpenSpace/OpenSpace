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


#include <modules/globebrowsing/chunk/chunkrenderer.h>
#include <modules/globebrowsing/chunk/chunkedlodglobe.h>
#include <modules/globebrowsing/tile/layeredtextures.h>

// open space includes
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/engine/openspaceengine.h> 
#include <openspace/rendering/renderengine.h>

// ghoul includes
#include <ghoul/misc/assert.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>

// STL includes
#include <sstream> 

#define _USE_MATH_DEFINES
#include <math.h>

namespace {
    const std::string _loggerCat = "PatchRenderer";

    const std::string keyFrame = "Frame";
    const std::string keyGeometry = "Geometry";
    const std::string keyShading = "PerformShading";

    const std::string keyBody = "Body";
}

namespace openspace {


    ChunkRenderer::ChunkRenderer(
        std::shared_ptr<Grid> grid,
        std::shared_ptr<TileProviderManager> tileProviderManager)
        : _tileProviderManager(tileProviderManager)
        , _grid(grid)
    {
        _globalRenderingShaderProvider = std::shared_ptr<LayeredTextureShaderProvider>
            (new LayeredTextureShaderProvider(
                "GlobalChunkedLodPatch",
                "${MODULE_GLOBEBROWSING}/shaders/globalchunkedlodpatch_vs.glsl",
                "${MODULE_GLOBEBROWSING}/shaders/globalchunkedlodpatch_fs.glsl"));

        _localRenderingShaderProvider = std::shared_ptr<LayeredTextureShaderProvider>
            (new LayeredTextureShaderProvider(
                "LocalChunkedLodPatch",
                "${MODULE_GLOBEBROWSING}/shaders/localchunkedlodpatch_vs.glsl",
                "${MODULE_GLOBEBROWSING}/shaders/localchunkedlodpatch_fs.glsl"));

        _globalProgramUniformHandler = std::shared_ptr<LayeredTextureShaderUniformIdHandler>
            (new LayeredTextureShaderUniformIdHandler());

        _localProgramUniformHandler = std::shared_ptr<LayeredTextureShaderUniformIdHandler>
            (new LayeredTextureShaderUniformIdHandler());

    }

    void ChunkRenderer::renderChunk(const Chunk& chunk, const RenderData& data) {
        if (chunk.index().level < 10) {
            renderChunkGlobally(chunk, data);
        }
        else {
            renderChunkLocally(chunk, data);
        }
    }

    void ChunkRenderer::update() {
        // unued atm. Could be used for caching or precalculating
    }


    void ChunkRenderer::setDepthTransformUniforms(
        std::shared_ptr<LayeredTextureShaderUniformIdHandler> uniformIdHandler,
        LayeredTextures::TextureCategory textureCategory,
        LayeredTextureShaderUniformIdHandler::BlendLayerSuffix blendLayerSuffix,
        size_t layerIndex,
        const TileDepthTransform& tileDepthTransform)
    {   
        uniformIdHandler->programObject().setUniform(
            uniformIdHandler->getId(
                textureCategory,
                blendLayerSuffix,
                layerIndex,
                LayeredTextureShaderUniformIdHandler::GlslTileDataId::depthTransform_depthScale),
            tileDepthTransform.depthScale);

        uniformIdHandler->programObject().setUniform(
            uniformIdHandler->getId(
                textureCategory,
                blendLayerSuffix,
                layerIndex,
                LayeredTextureShaderUniformIdHandler::GlslTileDataId::depthTransform_depthOffset),
            tileDepthTransform.depthOffset);

    }

    void ChunkRenderer::activateTileAndSetTileUniforms(
        std::shared_ptr<LayeredTextureShaderUniformIdHandler> uniformIdHandler,
        LayeredTextures::TextureCategory textureCategory,
        LayeredTextureShaderUniformIdHandler::BlendLayerSuffix blendLayerSuffix,
        size_t layerIndex,
        ghoul::opengl::TextureUnit& texUnit,
        const TileAndTransform& tileAndTransform)
    {

        // Blend tile with two parents
        // The texture needs a unit to sample from
        texUnit.activate();
        tileAndTransform.tile.texture->bind();

        uniformIdHandler->programObject().setUniform(
            uniformIdHandler->getId(
                textureCategory,
                blendLayerSuffix,
                layerIndex,
                LayeredTextureShaderUniformIdHandler::GlslTileDataId::textureSampler),
            texUnit);
        uniformIdHandler->programObject().setUniform(
            uniformIdHandler->getId(
                textureCategory,
                blendLayerSuffix,
                layerIndex,
                LayeredTextureShaderUniformIdHandler::GlslTileDataId::uvTransform_uvScale),
            tileAndTransform.uvTransform.uvScale);
        uniformIdHandler->programObject().setUniform(
            uniformIdHandler->getId(
                textureCategory,
                blendLayerSuffix,
                layerIndex,
                LayeredTextureShaderUniformIdHandler::GlslTileDataId::uvTransform_uvOffset),
            tileAndTransform.uvTransform.uvOffset);
    }


    ProgramObject* ChunkRenderer::getActivatedProgramWithTileData(
        LayeredTextureShaderProvider* layeredTextureShaderProvider,
        std::shared_ptr<LayeredTextureShaderUniformIdHandler> programUniformHandler,
        const Chunk& chunk)
    {
        const ChunkIndex& chunkIndex = chunk.index();

        std::array<std::vector<std::shared_ptr<TileProvider> >,
            LayeredTextures::NUM_TEXTURE_CATEGORIES> tileProviders;
        LayeredTexturePreprocessingData layeredTexturePreprocessingData;

        for (size_t category = 0; category < LayeredTextures::NUM_TEXTURE_CATEGORIES; category++)
        {
            tileProviders[category] = _tileProviderManager->getActivatedLayerCategory(
                LayeredTextures::TextureCategory(category));

            LayeredTextureInfo layeredTextureInfo;
            layeredTextureInfo.lastLayerIdx = tileProviders[category].size() - 1;
            layeredTextureInfo.layerBlendingEnabled = chunk.owner()->blendProperties[category];

            layeredTexturePreprocessingData.layeredTextureInfo[category] = layeredTextureInfo;
        }

        layeredTexturePreprocessingData.keyValuePairs.push_back(
            std::pair<std::string, std::string>(
                "useAtmosphere",
                std::to_string(chunk.owner()->atmosphereEnabled)));

        layeredTexturePreprocessingData.keyValuePairs.push_back(
            std::pair<std::string, std::string>(
                "showChunkEdges",
                std::to_string(chunk.owner()->showChunkEdges)));


        // Now the shader program can be accessed
        ProgramObject* programObject =
            layeredTextureShaderProvider->getUpdatedShaderProgram(
                layeredTexturePreprocessingData);

        programUniformHandler->updateIdsIfNecessary(layeredTextureShaderProvider);

        // Activate the shader program
        programObject->activate();

        // Initialize all texture units
        struct BlendTexUnits {
            ghoul::opengl::TextureUnit blendTexture0;
            ghoul::opengl::TextureUnit blendTexture1;
            ghoul::opengl::TextureUnit blendTexture2;
        };
        std::array<std::vector<BlendTexUnits>,
            LayeredTextures::NUM_TEXTURE_CATEGORIES> texUnits;
        for (size_t category = 0; category < LayeredTextures::NUM_TEXTURE_CATEGORIES; category++) {
            texUnits[category].resize(tileProviders[category].size());
        }

        // Go through all the categories
        for (size_t category = 0; category < LayeredTextures::NUM_TEXTURE_CATEGORIES; category++)
        {
            // Go through all the providers in this category
            int i = 0;
            for (auto it = tileProviders[category].begin(); it != tileProviders[category].end(); it++)
            {
                auto tileProvider = it->get();

                // Get the texture that should be used for rendering
                TileAndTransform tileAndTransform = TileSelector::getHighestResolutionTile(tileProvider, chunkIndex);
                if (tileAndTransform.tile.status == Tile::Status::Unavailable) {
                    // don't render if no tile was available
                    programObject->deactivate();
                    return nullptr;
                }

                activateTileAndSetTileUniforms(
                    programUniformHandler,
                    LayeredTextures::TextureCategory(category),
                    LayeredTextureShaderUniformIdHandler::BlendLayerSuffix::none,
                    i,
                    texUnits[category][i].blendTexture0,
                    tileAndTransform);

                // If blending is enabled, two more textures are needed
                if (layeredTexturePreprocessingData.layeredTextureInfo[category].layerBlendingEnabled) {
                    TileAndTransform tileAndTransformParent1 = TileSelector::getHighestResolutionTile(tileProvider, chunkIndex, 1);
                    if (tileAndTransformParent1.tile.status == Tile::Status::Unavailable) {
                        tileAndTransformParent1 = tileAndTransform;
                    }
                    activateTileAndSetTileUniforms(
                        programUniformHandler,
                        LayeredTextures::TextureCategory(category),
                        LayeredTextureShaderUniformIdHandler::BlendLayerSuffix::Parent1,
                        i,
                        texUnits[category][i].blendTexture1,
                        tileAndTransformParent1);

                    TileAndTransform tileAndTransformParent2 = TileSelector::getHighestResolutionTile(tileProvider, chunkIndex, 2);
                    if (tileAndTransformParent2.tile.status == Tile::Status::Unavailable) {
                        tileAndTransformParent2 = tileAndTransformParent1;
                    }
                    activateTileAndSetTileUniforms(
                        programUniformHandler,
                        LayeredTextures::TextureCategory(category),
                        LayeredTextureShaderUniformIdHandler::BlendLayerSuffix::Parent2,
                        i,
                        texUnits[category][i].blendTexture2,
                        tileAndTransformParent2);
                }
                i++;
            }
        }

        // Go through all the height maps and set depth tranforms
        int i = 0;
        for (auto it = tileProviders[LayeredTextures::HeightMaps].begin();
            it != tileProviders[LayeredTextures::HeightMaps].end(); it++) {
            auto tileProvider = *it;

            TileDepthTransform depthTransform = tileProvider->depthTransform();
            setDepthTransformUniforms(
                programUniformHandler,
                LayeredTextures::TextureCategory::HeightMaps,
                LayeredTextureShaderUniformIdHandler::BlendLayerSuffix::none,
                i,
                depthTransform);
            i++;
        }

        // The length of the skirts is proportional to its size
        programObject->setUniform("skirtLength", min(static_cast<float>(chunk.surfacePatch().halfSize().lat * 1000000), 8700.0f));
        programObject->setUniform("xSegments", _grid->xSegments());

        return programObject;
    }

    void ChunkRenderer::renderChunkGlobally(const Chunk& chunk, const RenderData& data){

        ProgramObject* programObject = getActivatedProgramWithTileData(
            _globalRenderingShaderProvider.get(),
            _globalProgramUniformHandler,
            chunk);
        if (programObject == nullptr) {
            return;
        }

        auto heightMapProviders = _tileProviderManager->getActivatedLayerCategory(LayeredTextures::HeightMaps);
        auto colorTextureProviders = _tileProviderManager->getActivatedLayerCategory(LayeredTextures::ColorTextures);
        auto nightTextureProviders = _tileProviderManager->getActivatedLayerCategory(LayeredTextures::NightTextures);
        auto overlayProviders = _tileProviderManager->getActivatedLayerCategory(LayeredTextures::Overlays);
        auto waterMaskProviders = _tileProviderManager->getActivatedLayerCategory(LayeredTextures::WaterMasks);
        const Ellipsoid& ellipsoid = chunk.owner()->ellipsoid();

        // This information is only needed when doing blending
        bool blendAny = false;
        for (size_t category = 0; category < LayeredTextures::NUM_TEXTURE_CATEGORIES; category++) {
            blendAny |= chunk.owner()->blendProperties[category];
        }
        if (blendAny &&
            ((heightMapProviders.size() > 0 ) ||
            (colorTextureProviders.size() > 0 ) ||
            (nightTextureProviders.size() > 0 ) ||
            (overlayProviders.size() > 0 ) ||
            (waterMaskProviders.size() > 0))) {
            float distanceScaleFactor = chunk.owner()->lodScaleFactor * ellipsoid.minimumRadius();
            programObject->setUniform("cameraPosition", vec3(data.camera.positionVec3()));
            programObject->setUniform("distanceScaleFactor", distanceScaleFactor);
            programObject->setUniform("chunkLevel", chunk.index().level);
        }
        
        // Calculate other uniform variables needed for rendering
        Geodetic2 swCorner = chunk.surfacePatch().getCorner(Quad::SOUTH_WEST);
        auto patchSize = chunk.surfacePatch().size();
        
        // TODO : Model transform should be fetched as a matrix directly.
        dmat4 modelTransform = dmat4(chunk.owner()->stateMatrix()); // Rotation
        modelTransform = translate(dmat4(1), data.position.dvec3()) * modelTransform; // Translation
        dmat4 viewTransform = data.camera.combinedViewMatrix();
        mat4 modelViewTransform = mat4(viewTransform * modelTransform);
        mat4 modelViewProjectionTransform = data.camera.projectionMatrix() * modelViewTransform;

        // Upload the uniform variables
        programObject->setUniform("modelViewProjectionTransform", modelViewProjectionTransform);
        programObject->setUniform("minLatLon", vec2(swCorner.toLonLatVec2()));
        programObject->setUniform("lonLatScalingFactor", vec2(patchSize.toLonLatVec2()));
        programObject->setUniform("radiiSquared", vec3(ellipsoid.radiiSquared()));

        if (nightTextureProviders.size() > 0) {
            programObject->setUniform("modelViewTransform", modelViewTransform);
        }

        
        // OpenGL rendering settings
        glEnable(GL_DEPTH_TEST);
        glEnable(GL_CULL_FACE);
        glCullFace(GL_BACK);

        // render
        _grid->geometry().drawUsingActiveProgram();

        // disable shader
        programObject->deactivate();
    }



    void ChunkRenderer::renderChunkLocally(const Chunk& chunk, const RenderData& data) {
        
        ProgramObject* programObject = getActivatedProgramWithTileData(
            _localRenderingShaderProvider.get(),
            _localProgramUniformHandler,
            chunk);
        if (programObject == nullptr) {
            return;
        }

        using namespace glm;

        auto heightMapProviders = _tileProviderManager->getActivatedLayerCategory(LayeredTextures::HeightMaps);
        auto colorTextureProviders = _tileProviderManager->getActivatedLayerCategory(LayeredTextures::ColorTextures);
        auto nightTextureProviders = _tileProviderManager->getActivatedLayerCategory(LayeredTextures::NightTextures);
        auto overlayProviders = _tileProviderManager->getActivatedLayerCategory(LayeredTextures::Overlays);
        auto waterMaskProviders = _tileProviderManager->getActivatedLayerCategory(LayeredTextures::WaterMasks);
        const Ellipsoid& ellipsoid = chunk.owner()->ellipsoid();

        // This information is only needed when doing blending
        bool blendAny = false;
        for (size_t category = 0; category < LayeredTextures::NUM_TEXTURE_CATEGORIES; category++) {
            blendAny |= chunk.owner()->blendProperties[category];
        }
        if (blendAny &&
            ((heightMapProviders.size() > 0) ||
                (colorTextureProviders.size() > 0) ||
                (nightTextureProviders.size() > 0) ||
                (overlayProviders.size() > 0) ||
                (waterMaskProviders.size() > 0))) {
            float distanceScaleFactor = chunk.owner()->lodScaleFactor * chunk.owner()->ellipsoid().minimumRadius();
            programObject->setUniform("distanceScaleFactor", distanceScaleFactor);
            programObject->setUniform("chunkLevel", chunk.index().level);
        }

        // Calculate other uniform variables needed for rendering

        // TODO : Model transform should be fetched as a matrix directly.
        dmat4 modelTransform = translate(dmat4(1), data.position.dvec3());
        dmat4 viewTransform = data.camera.combinedViewMatrix();
        dmat4 modelViewTransform = viewTransform * modelTransform;

        Geodetic2 sw = chunk.surfacePatch().getCorner(Quad::SOUTH_WEST);
        Geodetic2 se = chunk.surfacePatch().getCorner(Quad::SOUTH_EAST);
        Geodetic2 nw = chunk.surfacePatch().getCorner(Quad::NORTH_WEST);
        Geodetic2 ne = chunk.surfacePatch().getCorner(Quad::NORTH_EAST);

        // Get model space positions of the four control points
        Vec3 patchSwModelSpace = ellipsoid.cartesianSurfacePosition(sw);
        Vec3 patchSeModelSpace = ellipsoid.cartesianSurfacePosition(se);
        Vec3 patchNwModelSpace = ellipsoid.cartesianSurfacePosition(nw);
        Vec3 patchNeModelSpace = ellipsoid.cartesianSurfacePosition(ne);

        // Transform all control points to camera space
        Vec3 patchSwCameraSpace = Vec3(dmat4(modelViewTransform) * glm::dvec4(patchSwModelSpace, 1));
        Vec3 patchSeCameraSpace = Vec3(dmat4(modelViewTransform) * glm::dvec4(patchSeModelSpace, 1));
        Vec3 patchNwCameraSpace = Vec3(dmat4(modelViewTransform) * glm::dvec4(patchNwModelSpace, 1));
        Vec3 patchNeCameraSpace = Vec3(dmat4(modelViewTransform) * glm::dvec4(patchNeModelSpace, 1));

        // Send control points to shader
        programObject->setUniform("p00", vec3(patchSwCameraSpace));
        programObject->setUniform("p10", vec3(patchSeCameraSpace));
        programObject->setUniform("p01", vec3(patchNwCameraSpace));
        programObject->setUniform("p11", vec3(patchNeCameraSpace));

        vec3 patchNormalCameraSpace = normalize(
            cross(patchSeCameraSpace - patchSwCameraSpace,
                patchNwCameraSpace - patchSwCameraSpace));

        programObject->setUniform("patchNormalCameraSpace", patchNormalCameraSpace);
        programObject->setUniform("projectionTransform", data.camera.projectionMatrix());


        // OpenGL rendering settings
        glEnable(GL_DEPTH_TEST);
        glEnable(GL_CULL_FACE);
        glCullFace(GL_BACK);

        // render
        _grid->geometry().drawUsingActiveProgram();

        // disable shader
        programObject->deactivate();
    }
}  // namespace openspace
