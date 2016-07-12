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
        _globalRenderingShaderProvider = std::make_shared<LayeredTextureShaderProvider>(
                "GlobalChunkedLodPatch",
                "${MODULE_GLOBEBROWSING}/shaders/globalchunkedlodpatch_vs.glsl",
                "${MODULE_GLOBEBROWSING}/shaders/globalchunkedlodpatch_fs.glsl");

        _localRenderingShaderProvider = std::make_shared<LayeredTextureShaderProvider>(
                "LocalChunkedLodPatch",
                "${MODULE_GLOBEBROWSING}/shaders/localchunkedlodpatch_vs.glsl",
                "${MODULE_GLOBEBROWSING}/shaders/localchunkedlodpatch_fs.glsl");

        _globalProgramUniformHandler = std::make_shared<LayeredTextureShaderUniformIdHandler>();
        _localProgramUniformHandler = std::make_shared<LayeredTextureShaderUniformIdHandler>();

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

        for (size_t category = 0; category < LayeredTextures::NUM_TEXTURE_CATEGORIES; category++) {
            tileProviders[category] = _tileProviderManager->getTileProviderGroup(category).getActiveTileProviders();

            LayeredTextureInfo layeredTextureInfo;
            layeredTextureInfo.lastLayerIdx = tileProviders[category].size() - 1;
            layeredTextureInfo.layerBlendingEnabled = _tileProviderManager->getTileProviderGroup(category).levelBlendingEnabled;

            layeredTexturePreprocessingData.layeredTextureInfo[category] = layeredTextureInfo;
        }

        layeredTexturePreprocessingData.keyValuePairs.push_back(
            std::pair<std::string, std::string>(
                "useAtmosphere",
                std::to_string(chunk.owner()->atmosphereEnabled)));

        layeredTexturePreprocessingData.keyValuePairs.push_back(
            std::pair<std::string, std::string>(
                "showChunkEdges",
                std::to_string(chunk.owner()->debugOptions.showChunkEdges)));


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
        std::array<std::vector<BlendTexUnits>, LayeredTextures::NUM_TEXTURE_CATEGORIES> texUnits;
        for (size_t category = 0; category < LayeredTextures::NUM_TEXTURE_CATEGORIES; category++) {
            texUnits[category].resize(tileProviders[category].size());
        }

        // Go through all the categories
        for (size_t category = 0; category < LayeredTextures::NUM_TEXTURE_CATEGORIES; category++) {
            // Go through all the providers in this category
            int i = 0;
            for (auto it = tileProviders[category].begin(); it != tileProviders[category].end(); it++) {
                auto tileProvider = it->get();

                // Get the texture that should be used for rendering
                TileAndTransform tileAndTransform = TileSelector::getHighestResolutionTile(tileProvider, chunkIndex);
                if (tileAndTransform.tile.status == Tile::Status::Unavailable) {
                    tileAndTransform.tile = tileProvider->getDefaultTile();
                    tileAndTransform.uvTransform.uvOffset = { 0, 0 };
                    tileAndTransform.uvTransform.uvScale = { 1, 1 };
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

                /*
                if (category == LayeredTextures::HeightMaps && tileAndTransform.tile.preprocessData) {
                    //auto preprocessingData = tileAndTransform.tile.preprocessData;
                    //float noDataValue = preprocessingData->noDataValues[0];
                    programObject->setUniform(
                        "minimumValidHeight[" + std::to_string(i) + "]",
                        -100000);
                }
                */
                i++;
            }
        }

        // Go through all the height maps and set depth tranforms
        int i = 0;
        auto it = tileProviders[LayeredTextures::HeightMaps].begin();
        auto end = tileProviders[LayeredTextures::HeightMaps].end();
        for (; it != end; it++) {
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

        // Go through all the height map overlays and set depth tranforms
        i = 0;
        it = tileProviders[LayeredTextures::HeightMapOverlays].begin();
        end = tileProviders[LayeredTextures::HeightMapOverlays].end();
        for (; it != end; it++) {
            auto tileProvider = *it;

            TileDepthTransform depthTransform = tileProvider->depthTransform();
            setDepthTransformUniforms(
                programUniformHandler,
                LayeredTextures::TextureCategory::HeightMapOverlays,
                LayeredTextureShaderUniformIdHandler::BlendLayerSuffix::none,
                i,
                depthTransform);
            i++;
        }

        // The length of the skirts is proportional to its size
        programObject->setUniform("skirtLength", min(static_cast<float>(chunk.surfacePatch().halfSize().lat * 1000000), 8700.0f));
        programObject->setUniform("xSegments", _grid->xSegments());

        if (tileProviders[LayeredTextures::ColorTextures].size() == 0) {
            programObject->setUniform("vertexResolution", glm::vec2(_grid->xSegments(), _grid->ySegments()));
        }       
        
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

        const Ellipsoid& ellipsoid = chunk.owner()->ellipsoid();

        bool performAnyBlending = false;
        
        for (int i = 0; i < LayeredTextures::NUM_TEXTURE_CATEGORIES; ++i) {
            LayeredTextures::TextureCategory category = (LayeredTextures::TextureCategory)i;
            if(_tileProviderManager->getTileProviderGroup(i).levelBlendingEnabled && _tileProviderManager->getTileProviderGroup(category).getActiveTileProviders().size() > 0){
                performAnyBlending = true; 
                break;
            }
        }
        if (performAnyBlending) {
            // Calculations are done in the reference frame of the globe. Hence, the camera
            // position needs to be transformed with the inverse model matrix
            glm::dmat4 inverseModelTransform = chunk.owner()->inverseModelTransform();
            glm::dvec3 cameraPosition =
                glm::dvec3(inverseModelTransform * glm::dvec4(data.camera.positionVec3(), 1));
            float distanceScaleFactor = chunk.owner()->lodScaleFactor * ellipsoid.minimumRadius();
            programObject->setUniform("cameraPosition", vec3(cameraPosition));
            programObject->setUniform("distanceScaleFactor", distanceScaleFactor);
            programObject->setUniform("chunkLevel", chunk.index().level);
        }
        
        // Calculate other uniform variables needed for rendering
        Geodetic2 swCorner = chunk.surfacePatch().getCorner(Quad::SOUTH_WEST);
        auto patchSize = chunk.surfacePatch().size();
        
        // TODO : Model transform should be fetched as a matrix directly.
        dmat4 modelTransform = chunk.owner()->modelTransform();
        dmat4 viewTransform = data.camera.combinedViewMatrix();
        mat4 modelViewTransform = mat4(viewTransform * modelTransform);
        mat4 modelViewProjectionTransform = data.camera.projectionMatrix() * modelViewTransform;

        // Upload the uniform variables
        programObject->setUniform("modelViewProjectionTransform", modelViewProjectionTransform);
        programObject->setUniform("minLatLon", vec2(swCorner.toLonLatVec2()));
        programObject->setUniform("lonLatScalingFactor", vec2(patchSize.toLonLatVec2()));
        programObject->setUniform("radiiSquared", vec3(ellipsoid.radiiSquared()));

        if (_tileProviderManager->getTileProviderGroup(LayeredTextures::NightTextures).getActiveTileProviders().size() > 0) {
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

        const Ellipsoid& ellipsoid = chunk.owner()->ellipsoid();


        bool performAnyBlending = false;
        for (int i = 0; i < LayeredTextures::NUM_TEXTURE_CATEGORIES; ++i) {
            LayeredTextures::TextureCategory category = (LayeredTextures::TextureCategory)i;
            if (_tileProviderManager->getTileProviderGroup(i).levelBlendingEnabled && _tileProviderManager->getTileProviderGroup(category).getActiveTileProviders().size() > 0) {
                performAnyBlending = true;
                break;
            }
        }
        if (performAnyBlending) {
            float distanceScaleFactor = chunk.owner()->lodScaleFactor * chunk.owner()->ellipsoid().minimumRadius();
            programObject->setUniform("distanceScaleFactor", distanceScaleFactor);
            programObject->setUniform("chunkLevel", chunk.index().level);
        }

        // Calculate other uniform variables needed for rendering

        // TODO : Model transform should be fetched as a matrix directly.
        dmat4 modelTransform = chunk.owner()->modelTransform();
        dmat4 viewTransform = data.camera.combinedViewMatrix();
        dmat4 modelViewTransform = viewTransform * modelTransform;

        std::vector<std::string> cornerNames = { "p01", "p11", "p00", "p10" };
        std::vector<Vec3> cornersCameraSpace(4);
        for (int i = 0; i < 4; ++i) {
            Quad q = (Quad)i;
            Geodetic2 corner = chunk.surfacePatch().getCorner(q);
            Vec3 cornerModelSpace = ellipsoid.cartesianSurfacePosition(corner);
            Vec3 cornerCameraSpace = Vec3(dmat4(modelViewTransform) * glm::dvec4(cornerModelSpace, 1));
            cornersCameraSpace[i] = cornerCameraSpace;
            programObject->setUniform(cornerNames[i], vec3(cornerCameraSpace));
        }

        vec3 patchNormalCameraSpace = normalize(
            cross(cornersCameraSpace[Quad::SOUTH_EAST] - cornersCameraSpace[Quad::SOUTH_WEST],
                cornersCameraSpace[Quad::NORTH_EAST] - cornersCameraSpace[Quad::SOUTH_WEST]));

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
