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


#include <modules/globebrowsing/rendering/patchrenderer.h>
#include <modules/globebrowsing/globes/chunkedlodglobe.h>

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

    //////////////////////////////////////////////////////////////////////////////////////
    //							PATCH RENDERER											//
    //////////////////////////////////////////////////////////////////////////////////////
    PatchRenderer::PatchRenderer(shared_ptr<TileProviderManager> tileProviderManager)
        : _tileProviderManager(tileProviderManager)
    {

    }

    PatchRenderer::~PatchRenderer() {

    }

    void PatchRenderer::update() {

    }

    //////////////////////////////////////////////////////////////////////////////////////
    //								LATLON PATCH RENDERER								//
    //////////////////////////////////////////////////////////////////////////////////////
    ChunkRenderer::ChunkRenderer(
        shared_ptr<Grid> grid,
        shared_ptr<TileProviderManager> tileProviderManager)
        : PatchRenderer(tileProviderManager)
        , _grid(grid)
    {
        _globalRenderingShaderProvider = unique_ptr<LayeredTextureShaderProvider>
            (new LayeredTextureShaderProvider(
                "GlobalChunkedLodPatch",
                "${MODULE_GLOBEBROWSING}/shaders/globalchunkedlodpatch_vs.glsl",
                "${MODULE_GLOBEBROWSING}/shaders/globalchunkedlodpatch_fs.glsl"));

        _localRenderingShaderProvider = unique_ptr<LayeredTextureShaderProvider>
            (new LayeredTextureShaderProvider(
                "LocalChunkedLodPatch",
                "${MODULE_GLOBEBROWSING}/shaders/localchunkedlodpatch_vs.glsl",
                "${MODULE_GLOBEBROWSING}/shaders/localchunkedlodpatch_fs.glsl"));
    }

    void ChunkRenderer::renderChunk(const Chunk& chunk, const RenderData& data) {
        if (chunk.index().level < 9) {
            renderChunkGlobally(chunk, data);
        }
        else {
            renderChunkLocally(chunk, data);
        }
    }

    void ChunkRenderer::setDepthTransformUniforms(
        ProgramObject* programObject, const std::string& indexedTileKey, const TileDepthTransform& tileDepthTransform) 
    {   
        programObject->setUniform(
            indexedTileKey + ".depthTransform.depthScale",
            tileDepthTransform.depthScale);
        programObject->setUniform(
            indexedTileKey + ".depthTransform.depthOffset",
            tileDepthTransform.depthOffset);
    }

    void ChunkRenderer::activateTileAndSetTileUniforms(
        ProgramObject* programObject,
        ghoul::opengl::TextureUnit& texUnit,
        const std::string indexedTileKey,
        const TileAndTransform& tileAndTransform)
    {

        // Blend tile with two parents
        // The texture needs a unit to sample from
        texUnit.activate();
        tileAndTransform.tile.texture->bind();
        programObject->setUniform(indexedTileKey + ".textureSampler", texUnit);
        programObject->setUniform(
            indexedTileKey + ".uvTransform.uvScale",
            tileAndTransform.uvTransform.uvScale);
        programObject->setUniform(
            indexedTileKey + ".uvTransform.uvOffset",
            tileAndTransform.uvTransform.uvOffset);
    }


    ProgramObject* ChunkRenderer::getActivatedProgramWithTileData(
        LayeredTextureShaderProvider* layeredTextureShaderProvider,
        const Chunk& chunk)
    {
        const ChunkIndex& chunkIndex = chunk.index();

        auto heightMapProviders = _tileProviderManager->getActiveHeightMapProviders();
        auto colorTextureProviders = _tileProviderManager->getActiveColorTextureProviders();

        // Create information for the shader provider
        LayeredTextureInfo layeredTextureInfoHeight;
        LayeredTextureInfo layeredTextureInfoColor;
        layeredTextureInfoHeight.keyLastLayerIndex = "lastLayerIndexHeight";
        layeredTextureInfoHeight.lastLayerIndex = heightMapProviders.size() - 1;
        layeredTextureInfoHeight.keyUseThisLayerType = "useHeightMap";
        layeredTextureInfoHeight.keyLayerBlendingEnabled = "heightMapBlendingEnabled";
        layeredTextureInfoHeight.layerBlendingEnabled = chunk.owner()->blendHeightMap;

        layeredTextureInfoColor.keyLastLayerIndex = "lastLayerIndexColor";
        layeredTextureInfoColor.lastLayerIndex = colorTextureProviders.size() - 1;
        layeredTextureInfoColor.keyUseThisLayerType = "useColorTexture";
        layeredTextureInfoColor.keyLayerBlendingEnabled = "colorTextureBlendingEnabled";
        layeredTextureInfoColor.layerBlendingEnabled = chunk.owner()->blendColorMap;

        LayeredTexturePreprocessingData layeredTexturePreprocessingData;
        layeredTexturePreprocessingData.layeredTextureInfo.push_back(
            layeredTextureInfoHeight);
        layeredTexturePreprocessingData.layeredTextureInfo.push_back(
            layeredTextureInfoColor);

        // Now the shader program can be accessed
        ProgramObject* programObject =
            layeredTextureShaderProvider->getUpdatedShaderProgram(
                layeredTexturePreprocessingData);

        // Activate the shader program
        programObject->activate();



        // Create all the texture units
        std::vector<ghoul::opengl::TextureUnit> texUnitHeight;
        std::vector<ghoul::opengl::TextureUnit> texUnitHeightParent1;
        std::vector<ghoul::opengl::TextureUnit> texUnitHeightParent2;

        texUnitHeight.resize(heightMapProviders.size());
        texUnitHeightParent1.resize(heightMapProviders.size());
        texUnitHeightParent2.resize(heightMapProviders.size());

        std::vector<ghoul::opengl::TextureUnit> texUnitColor;
        std::vector<ghoul::opengl::TextureUnit> texUnitColorParent1;
        std::vector<ghoul::opengl::TextureUnit> texUnitColorParent2;

        texUnitColor.resize(colorTextureProviders.size());
        texUnitColorParent1.resize(colorTextureProviders.size());
        texUnitColorParent2.resize(colorTextureProviders.size());



        // Go through all the color texture providers
        int i = 0;
        for (auto it = heightMapProviders.begin(); it != heightMapProviders.end(); it++)
        {
            auto tileProvider = *it;

            // Get the texture that should be used for rendering
            TileAndTransform tileAndTransform = tileProvider->getHighestResolutionTile(chunkIndex);
            if (tileAndTransform.tile.status == Tile::Status::Unavailable) {
                // don't render if no tile was available
                programObject->deactivate();
                return nullptr;
            }

            TileDepthTransform depthTransform = tileProvider->depthTransform();
            std::string indexedTileKey = "heightTiles[" + std::to_string(i) + "]";

            setDepthTransformUniforms(programObject, indexedTileKey, depthTransform);
            activateTileAndSetTileUniforms(programObject, texUnitHeight[i], indexedTileKey, tileAndTransform);

            // If blending is enabled, two more textures are needed
            if (layeredTextureInfoHeight.layerBlendingEnabled) {
                TileAndTransform tileAndTransformParent1 = tileProvider->getHighestResolutionTile(chunkIndex, 1);
                if (tileAndTransformParent1.tile.status == Tile::Status::Unavailable) {
                    tileAndTransformParent1 = tileAndTransform;
                }
                std::string indexedTileKeyParent1 = "heightTilesParent1[" + std::to_string(i) + "]";
                activateTileAndSetTileUniforms(programObject, texUnitHeightParent1[i], indexedTileKeyParent1, tileAndTransformParent1);

                TileAndTransform tileAndTransformParent2 = tileProvider->getHighestResolutionTile(chunkIndex, 2);
                if (tileAndTransformParent2.tile.status == Tile::Status::Unavailable) {
                    tileAndTransformParent2 = tileAndTransformParent1;
                }
                std::string indexedTileKeyParent2 = "heightTilesParent2[" + std::to_string(i) + "]";
                activateTileAndSetTileUniforms(programObject, texUnitHeightParent2[i], indexedTileKeyParent2, tileAndTransformParent2);
            }
            i++;
        }


        // Go through all the color texture providers
        i = 0;
        for (auto it = colorTextureProviders.begin(); it != colorTextureProviders.end(); it++)
        {
            auto tileProvider = *it;

            TileAndTransform tileAndTransform = tileProvider->getHighestResolutionTile(chunkIndex);
            if (tileAndTransform.tile.status == Tile::Status::Unavailable) {
                // don't render if no tile was available
                programObject->deactivate();
                return nullptr;
            }

            std::string indexedTileKey = "colorTiles[" + std::to_string(i) + "]";

            // Blend tile with two parents
            // The texture needs a unit to sample from
            activateTileAndSetTileUniforms(programObject, texUnitColor[i], indexedTileKey, tileAndTransform);

            // If blending is enabled, two more textures are needed
            if (layeredTextureInfoColor.layerBlendingEnabled) {
                TileAndTransform tileAndTransformParent1 = tileProvider->getHighestResolutionTile(chunkIndex, 1);
                if (tileAndTransformParent1.tile.status == Tile::Status::Unavailable) {
                    tileAndTransformParent1 = tileAndTransform;
                }

                std::string indexedTileKeyParent1 = "colorTilesParent1[" + std::to_string(i) + "]";
                activateTileAndSetTileUniforms(programObject, texUnitColorParent1[i], indexedTileKeyParent1, tileAndTransformParent1);


                TileAndTransform tileAndTransformParent2 = tileProvider->getHighestResolutionTile(chunkIndex, 2);
                if (tileAndTransformParent2.tile.status == Tile::Status::Unavailable) {
                    tileAndTransformParent2 = tileAndTransformParent1;
                }
                std::string indexedTileKeyParent2 = "colorTilesParent2[" + std::to_string(i) + "]";
                activateTileAndSetTileUniforms(programObject, texUnitColorParent2[i], indexedTileKeyParent2, tileAndTransformParent2);

            }
            i++;
        }

        // The length of the skirts is proportional to its size
        programObject->setUniform("skirtLength", static_cast<float>(chunk.surfacePatch().halfSize().lat * 1000000));
        programObject->setUniform("xSegments", _grid->xSegments());


        return programObject;
    }

    void ChunkRenderer::renderChunkGlobally(const Chunk& chunk, const RenderData& data){

        ProgramObject* programObject = getActivatedProgramWithTileData(_globalRenderingShaderProvider.get(), chunk);
        if (programObject == nullptr) {
            return;
        }

        auto heightMapProviders = _tileProviderManager->getActiveHeightMapProviders();
        auto colorTextureProviders = _tileProviderManager->getActiveColorTextureProviders();
        const Ellipsoid& ellipsoid = chunk.owner()->ellipsoid();

        // This information is only needed when doing blending
        if ((heightMapProviders.size() > 0 && chunk.owner()->blendHeightMap) ||
            (colorTextureProviders.size() > 0 && chunk.owner()->blendColorMap)) {
            float distanceScaleFactor = chunk.owner()->lodScaleFactor * ellipsoid.minimumRadius();
            programObject->setUniform("cameraPosition", vec3(data.camera.positionVec3()));
            programObject->setUniform("distanceScaleFactor", distanceScaleFactor);
            programObject->setUniform("chunkLevel", chunk.index().level);
        }
        
        // Calculate other uniform variables needed for rendering
        Geodetic2 swCorner = chunk.surfacePatch().southWestCorner();
        auto patchSize = chunk.surfacePatch().size();
        
        // TODO : Model transform should be fetched as a matrix directly.
        dmat4 modelTransform = dmat4(chunk.owner()->stateMatrix()); // Rotation
        modelTransform = translate(dmat4(1), data.position.dvec3()) * modelTransform; // Translation
        dmat4 viewTransform = data.camera.combinedViewMatrix();
        mat4 modelViewProjectionTransform = data.camera.projectionMatrix()
            * mat4(viewTransform * modelTransform);

        // Upload the uniform variables
        programObject->setUniform("modelViewProjectionTransform", modelViewProjectionTransform);
        programObject->setUniform("minLatLon", vec2(swCorner.toLonLatVec2()));
        programObject->setUniform("lonLatScalingFactor", vec2(patchSize.toLonLatVec2()));
        programObject->setUniform("radiiSquared", vec3(ellipsoid.radiiSquared()));


        
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
        
        ProgramObject* programObject = getActivatedProgramWithTileData(_localRenderingShaderProvider.get(), chunk);
        if (programObject == nullptr) {
            return;
        }

        using namespace glm;
        const Ellipsoid& ellipsoid = chunk.owner()->ellipsoid();
        auto heightMapProviders = _tileProviderManager->getActiveHeightMapProviders();
        auto colorTextureProviders = _tileProviderManager->getActiveColorTextureProviders();
        if ((heightMapProviders.size() > 0 && chunk.owner()->blendHeightMap) ||
            (colorTextureProviders.size() > 0 && chunk.owner()->blendColorMap)) {
            float distanceScaleFactor = chunk.owner()->lodScaleFactor * chunk.owner()->ellipsoid().minimumRadius();
            programObject->setUniform("distanceScaleFactor", distanceScaleFactor);
            programObject->setUniform("chunkLevel", chunk.index().level);
        }

        // Calculate other uniform variables needed for rendering

        // TODO : Model transform should be fetched as a matrix directly.
        dmat4 modelTransform = translate(dmat4(1), data.position.dvec3());
        dmat4 viewTransform = data.camera.combinedViewMatrix();
        dmat4 modelViewTransform = viewTransform * modelTransform;

        Geodetic2 sw = chunk.surfacePatch().southWestCorner();
        Geodetic2 se = chunk.surfacePatch().southEastCorner();
        Geodetic2 nw = chunk.surfacePatch().northWestCorner();
        Geodetic2 ne = chunk.surfacePatch().northEastCorner();

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
