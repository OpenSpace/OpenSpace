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
        auto heightMapProviders = _tileProviderManager->getActiveHeightMapProviders();
        for (auto iter = heightMapProviders.begin(); iter != heightMapProviders.end(); iter++)
        {
            iter->get()->prerender();
        }
        auto colorTextureProviders = _tileProviderManager->getActiveColorTextureProviders();
        for (auto iter = colorTextureProviders.begin(); iter != colorTextureProviders.end(); iter++)
        {
            iter->get()->prerender();
        }
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

    void ChunkRenderer::renderChunkGlobally(const Chunk& chunk, const RenderData& data){
        using namespace glm;

        // All providers of tiles
        auto heightMapProviders = _tileProviderManager->getActiveHeightMapProviders();
        auto colorTextureProviders = _tileProviderManager->getActiveColorTextureProviders();
        
        int numHeightMapProviders = heightMapProviders.size();
        int numColorTextureProviders = colorTextureProviders.size();
        
        /*
        if (numHeightMapProviders == 0 || numColorTextureProviders == 0) {
            return;
        }
        */

        // Create information for the shader provider
        LayeredTextureInfo layeredTextureInfoHeight;
        LayeredTextureInfo layeredTextureInfoColor;
        layeredTextureInfoHeight.keyLastLayerIndex = "lastLayerIndexHeight";
        layeredTextureInfoHeight.lastLayerIndex = numHeightMapProviders - 1;
        layeredTextureInfoHeight.keyUseThisLayerType = "useHeightMap";
        layeredTextureInfoHeight.useThisLayerType = chunk.owner()->useHeightMap;
        layeredTextureInfoHeight.keyLayerBlendingEnabled = "heightMapBlendingEnabled";
        layeredTextureInfoHeight.layerBlendingEnabled = chunk.owner()->blendHeightMap;

        layeredTextureInfoColor.keyLastLayerIndex = "lastLayerIndexColor";
        layeredTextureInfoColor.lastLayerIndex = numColorTextureProviders - 1;
        layeredTextureInfoColor.keyUseThisLayerType = "useColorTexture";
        layeredTextureInfoColor.useThisLayerType = chunk.owner()->useColorMap;
        layeredTextureInfoColor.keyLayerBlendingEnabled = "colorTextureBlendingEnabled";
        layeredTextureInfoColor.layerBlendingEnabled = chunk.owner()->blendColorMap;

        LayeredTexturePreprocessingData layeredTexturePreprocessingData;
        layeredTexturePreprocessingData.layeredTextureInfo.push_back(
            layeredTextureInfoHeight);
        layeredTexturePreprocessingData.layeredTextureInfo.push_back(
            layeredTextureInfoColor);

        // Now the shader program can be accessed
        ProgramObject* programObject =
            _globalRenderingShaderProvider->getUpdatedShaderProgram(
                layeredTexturePreprocessingData);

        // Activate the shader program
        programObject->activate();

        // Create all the texture units
        std::vector<ghoul::opengl::TextureUnit> texUnitHeight;
        std::vector<ghoul::opengl::TextureUnit> texUnitHeightParent1;
        std::vector<ghoul::opengl::TextureUnit> texUnitHeightParent2;

        texUnitHeight.resize(numHeightMapProviders);
        texUnitHeightParent1.resize(numHeightMapProviders);
        texUnitHeightParent2.resize(numHeightMapProviders);

        std::vector<ghoul::opengl::TextureUnit> texUnitColor;
        std::vector<ghoul::opengl::TextureUnit> texUnitColorParent1;
        std::vector<ghoul::opengl::TextureUnit> texUnitColorParent2;

        texUnitColor.resize(numColorTextureProviders);
        texUnitColorParent1.resize(numColorTextureProviders);
        texUnitColorParent2.resize(numColorTextureProviders);


        if (layeredTextureInfoHeight.useThisLayerType) {

            // Go through all the color texture providers
            int i = 0;
            for (auto it = heightMapProviders.begin(); it != heightMapProviders.end(); it++)
            {
                auto tileProvider = it->get();
                // Get the texture that should be used for rendering
                Tile tile = tileProvider->getHighestResolutionTile(chunk.index());

                if (tile.texture == nullptr) {
                    // don't render if no tile was available
                    programObject->deactivate();
                    return;
                }
                TileDepthTransform depthTransform = tileProvider->depthTransform();


                // The texture needs a unit to sample from
                texUnitHeight[i].activate();
                tile.texture->bind();

                std::string indexedTileKey = "heightTiles[" + std::to_string(i) + "]";
                // Send uniforms for the tile to the shader
                programObject->setUniform(indexedTileKey + ".textureSampler", texUnitHeight[i]);

                programObject->setUniform(
                    indexedTileKey + ".uvTransform.uvScale",
                    tile.uvTransform.uvScale);
                programObject->setUniform(
                    indexedTileKey + ".uvTransform.uvOffset",
                    tile.uvTransform.uvOffset);

                programObject->setUniform(
                    indexedTileKey + ".depthTransform.depthScale",
                    depthTransform.depthScale);
                programObject->setUniform(
                    indexedTileKey + ".depthTransform.depthOffset",
                    depthTransform.depthOffset);

                // If blending is enabled, two more textures are needed
                if (layeredTextureInfoHeight.layerBlendingEnabled) {
                    Tile tileParent1 = tileProvider->getHighestResolutionTile(chunk.index(), 1);
                    Tile tileParent2 = tileProvider->getHighestResolutionTile(chunk.index(), 2);

                    if (tileParent1.texture == nullptr) {
                        tileParent1 = tile;
                    }
                    if (tileParent2.texture == nullptr) {
                        tileParent2 = tileParent1;
                    }

                    // Blend tile with two parents
                    // The texture needs a unit to sample from
                    texUnitHeightParent1[i].activate();
                    tileParent1.texture->bind();

                    std::string indexedTileKeyParent1 = "heightTilesParent1[" + std::to_string(i) + "]";
                    // Send uniforms for the tile to the shader
                    programObject->setUniform(indexedTileKeyParent1 + ".textureSampler", texUnitHeightParent1[i]);

                    programObject->setUniform(
                        indexedTileKeyParent1 + ".uvTransform.uvScale",
                        tileParent1.uvTransform.uvScale);
                    programObject->setUniform(
                        indexedTileKeyParent1 + ".uvTransform.uvOffset",
                        tileParent1.uvTransform.uvOffset);


                    // The texture needs a unit to sample from
                    texUnitHeightParent2[i].activate();
                    tileParent2.texture->bind();

                    std::string indexedTileKeyParent2 = "heightTilesParent2[" + std::to_string(i) + "]";
                    // Send uniforms for the tile to the shader
                    programObject->setUniform(indexedTileKeyParent2 + ".textureSampler", texUnitHeightParent2[i]);

                    programObject->setUniform(
                        indexedTileKeyParent2 + ".uvTransform.uvScale",
                        tileParent2.uvTransform.uvScale);
                    programObject->setUniform(
                        indexedTileKeyParent2 + ".uvTransform.uvOffset",
                        tileParent2.uvTransform.uvOffset);
                }

                i++;
            }
        }
        
        if (layeredTextureInfoColor.useThisLayerType) {

            // Go through all the color texture providers
            int i = 0;
            for (auto it = colorTextureProviders.begin(); it != colorTextureProviders.end(); it++)
            {
                auto tileProvider = it->get();
                // Get the texture that should be used for rendering
                Tile tile = tileProvider->getHighestResolutionTile(chunk.index());

                if (tile.texture == nullptr) {
                    // don't render if no tile was available
                    programObject->deactivate();
                    return;
                }

                // The texture needs a unit to sample from
                texUnitColor[i].activate();
                tile.texture->bind();

                std::string indexedTileKey = "colorTiles[" + std::to_string(i) + "]";
                // Send uniforms for the tile to the shader
                programObject->setUniform(indexedTileKey + ".textureSampler", texUnitColor[i]);

                programObject->setUniform(
                    indexedTileKey + ".uvTransform.uvScale",
                    tile.uvTransform.uvScale);
                programObject->setUniform(
                    indexedTileKey + ".uvTransform.uvOffset",
                    tile.uvTransform.uvOffset);

                // If blending is enabled, two more textures are needed
                if (layeredTextureInfoColor.layerBlendingEnabled) {
                    Tile tileParent1 = tileProvider->getHighestResolutionTile(chunk.index(), 1);
                    Tile tileParent2 = tileProvider->getHighestResolutionTile(chunk.index(), 2);

                    if (tileParent1.texture == nullptr) {
                        tileParent1 = tile;
                    }
                    if (tileParent2.texture == nullptr) {
                        tileParent2 = tileParent1;
                    }

                    // Blend tile with two parents
                    // The texture needs a unit to sample from
                    texUnitColorParent1[i].activate();
                    tileParent1.texture->bind();

                    std::string indexedTileKeyParent1 = "colorTilesParent1[" + std::to_string(i) + "]";
                    // Send uniforms for the tile to the shader
                    programObject->setUniform(indexedTileKeyParent1 + ".textureSampler", texUnitColorParent1[i]);

                    programObject->setUniform(
                        indexedTileKeyParent1 + ".uvTransform.uvScale",
                        tileParent1.uvTransform.uvScale);
                    programObject->setUniform(
                        indexedTileKeyParent1 + ".uvTransform.uvOffset",
                        tileParent1.uvTransform.uvOffset);


                    // The texture needs a unit to sample from
                    texUnitColorParent2[i].activate();
                    tileParent2.texture->bind();

                    std::string indexedTileKeyParent2 = "colorTilesParent2[" + std::to_string(i) + "]";
                    // Send uniforms for the tile to the shader
                    programObject->setUniform(indexedTileKeyParent2 + ".textureSampler", texUnitColorParent2[i]);

                    programObject->setUniform(
                        indexedTileKeyParent2 + ".uvTransform.uvScale",
                        tileParent2.uvTransform.uvScale);
                    programObject->setUniform(
                        indexedTileKeyParent2 + ".uvTransform.uvOffset",
                        tileParent2.uvTransform.uvOffset);
                }
                i++;
            }
        }
        
        // Calculate other uniform variables needed for rendering
        Geodetic2 swCorner = chunk.surfacePatch().southWestCorner();
        auto patchSize = chunk.surfacePatch().size();
        
        // TODO : Model transform should be fetched as a matrix directly.
        mat4 modelTransform = translate(mat4(1), data.position.vec3());
        mat4 viewTransform = data.camera.combinedViewMatrix();
        mat4 modelViewProjectionTransform = data.camera.projectionMatrix()
            * viewTransform * modelTransform;
        const Ellipsoid& ellipsoid = chunk.owner()->ellipsoid();

        // Upload the uniform variables
        programObject->setUniform("modelViewProjectionTransform", modelViewProjectionTransform);
        programObject->setUniform("minLatLon", vec2(swCorner.toLonLatVec2()));
        programObject->setUniform("lonLatScalingFactor", vec2(patchSize.toLonLatVec2()));
        programObject->setUniform("radiiSquared", vec3(ellipsoid.radiiSquared()));
        programObject->setUniform("xSegments", _grid->xSegments());

        // The length of the skirts is proportional to its size
        programObject->setUniform("skirtLength", static_cast<float>(chunk.surfacePatch().halfSize().lat * 1000000));
        
        // This information is only needed when doing blending
        if ((layeredTextureInfoHeight.useThisLayerType && layeredTextureInfoHeight.layerBlendingEnabled) ||
            (layeredTextureInfoColor.useThisLayerType && layeredTextureInfoColor.layerBlendingEnabled)) {
            float distanceScaleFactor = chunk.owner()->lodScaleFactor * ellipsoid.minimumRadius();
            programObject->setUniform("cameraPosition", vec3(data.camera.positionVec3()));
            programObject->setUniform("distanceScaleFactor", distanceScaleFactor);
            programObject->setUniform("chunkLevel", chunk.index().level);
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

    void ChunkRenderer::renderChunkLocally(const Chunk& chunk, const RenderData& data)
    {
        using namespace glm;

        // All providers of tiles
        auto heightMapProviders = _tileProviderManager->getActiveHeightMapProviders();
        auto colorTextureProviders = _tileProviderManager->getActiveColorTextureProviders();

        int numHeightMapProviders = heightMapProviders.size();
        int numColorTextureProviders = colorTextureProviders.size();


        if (numHeightMapProviders == 0 || numColorTextureProviders == 0) {
            return;
        }

        // Create information for the shader provider
        LayeredTextureInfo layeredTextureInfoHeight;
        LayeredTextureInfo layeredTextureInfoColor;
        layeredTextureInfoHeight.keyLastLayerIndex = "lastLayerIndexHeight";
        layeredTextureInfoHeight.lastLayerIndex = numHeightMapProviders - 1;
        layeredTextureInfoHeight.keyUseThisLayerType = "useHeightMap";
        layeredTextureInfoHeight.useThisLayerType = chunk.owner()->useHeightMap;
        layeredTextureInfoHeight.keyLayerBlendingEnabled = "heightMapBlendingEnabled";
        layeredTextureInfoHeight.layerBlendingEnabled = chunk.owner()->blendHeightMap;

        layeredTextureInfoColor.keyLastLayerIndex = "lastLayerIndexColor";
        layeredTextureInfoColor.lastLayerIndex = numColorTextureProviders - 1;
        layeredTextureInfoColor.keyUseThisLayerType = "useColorTexture";
        layeredTextureInfoColor.useThisLayerType = chunk.owner()->useColorMap;
        layeredTextureInfoColor.keyLayerBlendingEnabled = "colorTextureBlendingEnabled";
        layeredTextureInfoColor.layerBlendingEnabled = chunk.owner()->blendColorMap;

        LayeredTexturePreprocessingData layeredTexturePreprocessingData;
        layeredTexturePreprocessingData.layeredTextureInfo.push_back(
            layeredTextureInfoHeight);
        layeredTexturePreprocessingData.layeredTextureInfo.push_back(
            layeredTextureInfoColor);

        // Now the shader program can be accessed
        ProgramObject* programObject =
            _localRenderingShaderProvider->getUpdatedShaderProgram(
                layeredTexturePreprocessingData);

        // Activate the shader program
        programObject->activate();

        // Create all the texture units
        std::vector<ghoul::opengl::TextureUnit> texUnitHeight;
        std::vector<ghoul::opengl::TextureUnit> texUnitHeightParent1;
        std::vector<ghoul::opengl::TextureUnit> texUnitHeightParent2;

        texUnitHeight.resize(numHeightMapProviders);
        texUnitHeightParent1.resize(numHeightMapProviders);
        texUnitHeightParent2.resize(numHeightMapProviders);

        std::vector<ghoul::opengl::TextureUnit> texUnitColor;
        std::vector<ghoul::opengl::TextureUnit> texUnitColorParent1;
        std::vector<ghoul::opengl::TextureUnit> texUnitColorParent2;

        texUnitColor.resize(numColorTextureProviders);
        texUnitColorParent1.resize(numColorTextureProviders);
        texUnitColorParent2.resize(numColorTextureProviders);


        if (layeredTextureInfoHeight.useThisLayerType) {

            // Go through all the color texture providers
            int i = 0;
            for (auto it = heightMapProviders.begin(); it != heightMapProviders.end(); it++)
            {
                auto tileProvider = it->get();
                // Get the texture that should be used for rendering
                Tile tile = tileProvider->getHighestResolutionTile(chunk.index());

                if (tile.texture == nullptr) {
                    // don't render if no tile was available
                    programObject->deactivate();
                    return;
                }
                TileDepthTransform depthTransform = tileProvider->depthTransform();


                // The texture needs a unit to sample from
                texUnitHeight[i].activate();
                tile.texture->bind();

                std::string indexedTileKey = "heightTiles[" + std::to_string(i) + "]";
                // Send uniforms for the tile to the shader
                programObject->setUniform(indexedTileKey + ".textureSampler", texUnitHeight[i]);

                programObject->setUniform(
                    indexedTileKey + ".uvTransform.uvScale",
                    tile.uvTransform.uvScale);
                programObject->setUniform(
                    indexedTileKey + ".uvTransform.uvOffset",
                    tile.uvTransform.uvOffset);

                programObject->setUniform(
                    indexedTileKey + ".depthTransform.depthScale",
                    depthTransform.depthScale);
                programObject->setUniform(
                    indexedTileKey + ".depthTransform.depthOffset",
                    depthTransform.depthOffset);

                // If blending is enabled, two more textures are needed
                if (layeredTextureInfoHeight.layerBlendingEnabled) {
                    Tile tileParent1 = tileProvider->getHighestResolutionTile(chunk.index(), 1);
                    Tile tileParent2 = tileProvider->getHighestResolutionTile(chunk.index(), 2);

                    if (tileParent1.texture == nullptr) {
                        tileParent1 = tile;
                    }
                    if (tileParent2.texture == nullptr) {
                        tileParent2 = tileParent1;
                    }

                    // Blend tile with two parents
                    // The texture needs a unit to sample from
                    texUnitHeightParent1[i].activate();
                    tileParent1.texture->bind();

                    std::string indexedTileKeyParent1 = "heightTilesParent1[" + std::to_string(i) + "]";
                    // Send uniforms for the tile to the shader
                    programObject->setUniform(indexedTileKeyParent1 + ".textureSampler", texUnitHeightParent1[i]);

                    programObject->setUniform(
                        indexedTileKeyParent1 + ".uvTransform.uvScale",
                        tileParent1.uvTransform.uvScale);
                    programObject->setUniform(
                        indexedTileKeyParent1 + ".uvTransform.uvOffset",
                        tileParent1.uvTransform.uvOffset);


                    // The texture needs a unit to sample from
                    texUnitHeightParent2[i].activate();
                    tileParent2.texture->bind();

                    std::string indexedTileKeyParent2 = "heightTilesParent2[" + std::to_string(i) + "]";
                    // Send uniforms for the tile to the shader
                    programObject->setUniform(indexedTileKeyParent2 + ".textureSampler", texUnitHeightParent2[i]);

                    programObject->setUniform(
                        indexedTileKeyParent2 + ".uvTransform.uvScale",
                        tileParent2.uvTransform.uvScale);
                    programObject->setUniform(
                        indexedTileKeyParent2 + ".uvTransform.uvOffset",
                        tileParent2.uvTransform.uvOffset);
                }

                i++;
            }
        }

        if (layeredTextureInfoColor.useThisLayerType) {

            // Go through all the color texture providers
            int i = 0;
            for (auto it = colorTextureProviders.begin(); it != colorTextureProviders.end(); it++)
            {
                auto tileProvider = it->get();
                // Get the texture that should be used for rendering
                Tile tile = tileProvider->getHighestResolutionTile(chunk.index());

                if (tile.texture == nullptr) {
                    // don't render if no tile was available
                    programObject->deactivate();
                    return;
                }

                // The texture needs a unit to sample from
                texUnitColor[i].activate();
                tile.texture->bind();

                std::string indexedTileKey = "colorTiles[" + std::to_string(i) + "]";
                // Send uniforms for the tile to the shader
                programObject->setUniform(indexedTileKey + ".textureSampler", texUnitColor[i]);

                programObject->setUniform(
                    indexedTileKey + ".uvTransform.uvScale",
                    tile.uvTransform.uvScale);
                programObject->setUniform(
                    indexedTileKey + ".uvTransform.uvOffset",
                    tile.uvTransform.uvOffset);

                // If blending is enabled, two more textures are needed
                if (layeredTextureInfoColor.layerBlendingEnabled) {
                    Tile tileParent1 = tileProvider->getHighestResolutionTile(chunk.index(), 1);
                    Tile tileParent2 = tileProvider->getHighestResolutionTile(chunk.index(), 2);

                    if (tileParent1.texture == nullptr) {
                        tileParent1 = tile;
                    }
                    if (tileParent2.texture == nullptr) {
                        tileParent2 = tileParent1;
                    }

                    // Blend tile with two parents
                    // The texture needs a unit to sample from
                    texUnitColorParent1[i].activate();
                    tileParent1.texture->bind();

                    std::string indexedTileKeyParent1 = "colorTilesParent1[" + std::to_string(i) + "]";
                    // Send uniforms for the tile to the shader
                    programObject->setUniform(indexedTileKeyParent1 + ".textureSampler", texUnitColorParent1[i]);

                    programObject->setUniform(
                        indexedTileKeyParent1 + ".uvTransform.uvScale",
                        tileParent1.uvTransform.uvScale);
                    programObject->setUniform(
                        indexedTileKeyParent1 + ".uvTransform.uvOffset",
                        tileParent1.uvTransform.uvOffset);


                    // The texture needs a unit to sample from
                    texUnitColorParent2[i].activate();
                    tileParent2.texture->bind();

                    std::string indexedTileKeyParent2 = "colorTilesParent2[" + std::to_string(i) + "]";
                    // Send uniforms for the tile to the shader
                    programObject->setUniform(indexedTileKeyParent2 + ".textureSampler", texUnitColorParent2[i]);

                    programObject->setUniform(
                        indexedTileKeyParent2 + ".uvTransform.uvScale",
                        tileParent2.uvTransform.uvScale);
                    programObject->setUniform(
                        indexedTileKeyParent2 + ".uvTransform.uvOffset",
                        tileParent2.uvTransform.uvOffset);
                }
                i++;
            }
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

        const Ellipsoid& ellipsoid = chunk.owner()->ellipsoid();

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

        programObject->setUniform(
            "patchNormalCameraSpace",
            patchNormalCameraSpace);

        programObject->setUniform(
            "projectionTransform",
            data.camera.projectionMatrix());

        programObject->setUniform("xSegments", _grid->xSegments());
        // The length of the skirts is proportional to its size
        programObject->setUniform("skirtLength", static_cast<float>(chunk.surfacePatch().halfSize().lat * 1000000));

        // This information is only needed when doing blending
        if ((layeredTextureInfoHeight.useThisLayerType && layeredTextureInfoHeight.layerBlendingEnabled) ||
            (layeredTextureInfoColor.useThisLayerType && layeredTextureInfoColor.layerBlendingEnabled)) {
            float distanceScaleFactor = chunk.owner()->lodScaleFactor * chunk.owner()->ellipsoid().minimumRadius();
            programObject->setUniform("distanceScaleFactor", distanceScaleFactor);
            programObject->setUniform("chunkLevel", chunk.index().level);
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
}  // namespace openspace
