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
#include <modules/globebrowsing/meshes/clipmapgrid.h>

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
        auto heightMapProviders = _tileProviderManager->heightMapProviders();
        for (auto iter = heightMapProviders.begin(); iter != heightMapProviders.end(); iter++)
        {
            iter->second->prerender();
        }
        auto colorTextureProviders = _tileProviderManager->colorTextureProviders();
        for (auto iter = colorTextureProviders.begin(); iter != colorTextureProviders.end(); iter++)
        {
            iter->second->prerender();
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

        /*
        _programObjectGlobalRendering = OsEng.renderEngine().buildRenderProgram(
            "GlobalChunkedLodPatch",
            "${MODULE_GLOBEBROWSING}/shaders/globalchunkedlodpatch_vs.glsl",
            "${MODULE_GLOBEBROWSING}/shaders/globalchunkedlodpatch_fs.glsl");
        ghoul_assert(_programObjectGlobalRendering != nullptr, "Failed to initialize programObject!");

        _programObjectLocalRendering = OsEng.renderEngine().buildRenderProgram(
            "LocalChunkedLodPatch",
            "${MODULE_GLOBEBROWSING}/shaders/localchunkedlodpatch_vs.glsl",
            "${MODULE_GLOBEBROWSING}/shaders/localchunkedlodpatch_fs.glsl");
        ghoul_assert(_programObjectLocalRendering != nullptr, "Failed to initialize programObject!");
        using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
        _programObjectGlobalRendering->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
        _programObjectLocalRendering->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
        */

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
        auto heightMapProviders = _tileProviderManager->heightMapProviders();
        auto colorTextureProviders = _tileProviderManager->colorTextureProviders();
        
        int numHeightMapProviders = heightMapProviders.size();
        int numColorTextureProviders = colorTextureProviders.size();

        // Create information for the shader provider
        LayeredTextureInfo layeredTextureInfoHeight;
        LayeredTextureInfo layeredTextureInfoColor;
        layeredTextureInfoHeight.keyNumLayers = "numLayersHeight";
        layeredTextureInfoHeight.numLayers = numHeightMapProviders;
        layeredTextureInfoColor.keyNumLayers = "numLayersColor";
        layeredTextureInfoColor.numLayers = numColorTextureProviders;

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

        std::vector<ghoul::opengl::TextureUnit> texUnitHeight;
        std::vector<ghoul::opengl::TextureUnit> texUnitColor;

        texUnitHeight.resize(numHeightMapProviders);
        texUnitColor.resize(numColorTextureProviders);


        // Go through all the height map providers
        int i = 0;
        for (auto it = heightMapProviders.begin(); it != heightMapProviders.end(); it++)
        {
            texUnitHeight.push_back(ghoul::opengl::TextureUnit());
            auto tileProvider = it->second;
            // Get the texture that should be used for rendering
            Tile tile = tileProvider->getHighestResolutionTile(chunk.index());
            TileDepthTransform depthTransform = tileProvider->depthTransform();

            // The texture needs a unit to sample from
            texUnitHeight[i].activate();
            int hej = 0;
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

            i++;
        }

        // Go through all the color texture providers
        i = 0;
        for (auto it = colorTextureProviders.begin(); it != colorTextureProviders.end(); it++)
        {
            auto tileProvider = it->second;
            // Get the texture that should be used for rendering
            Tile tile = tileProvider->getHighestResolutionTile(chunk.index());

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
            
            i++;
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

        // OpenGL rendering settings
        glEnable(GL_DEPTH_TEST);
        glEnable(GL_CULL_FACE);
        glCullFace(GL_BACK);

        // render
        _grid->geometry().drawUsingActiveProgram();

        // disable shader
        programObject->deactivate();
        







        /*
        // activate shader
        _programObjectGlobalRendering->activate();


        // For now just pick the first one from height maps
        //auto heightMapProviders = _tileProviderManager->heightMapProviders();
        //auto tileProviderHeight = heightMapProviders.begin()->second;

        // Get the textures that should be used for rendering
        Tile heightTile = tileProviderHeight->getHighestResolutionTile(chunk.index());


        // Bind and use the texture
        ghoul::opengl::TextureUnit texUnitHeight;
        texUnitHeight.activate();
        heightTile.texture->bind();
        _programObjectGlobalRendering->setUniform("heightTile.textureSampler", texUnitHeight);

        _programObjectGlobalRendering->setUniform("heightTile.uvTransform.uvScale", heightTile.uvTransform.uvScale);
        _programObjectGlobalRendering->setUniform("heightTile.uvTransform.uvOffset", heightTile.uvTransform.uvOffset);

        TileDepthTransform depthTransformHeight = tileProviderHeight->depthTransform();
        _programObjectGlobalRendering->setUniform("heightTile.depthTransform.depthScale", depthTransformHeight.depthScale);
        _programObjectGlobalRendering->setUniform("heightTile.depthTransform.depthOffset", depthTransformHeight.depthOffset);


        // Pick the first color texture
        auto colorTextureProviders = _tileProviderManager->colorTextureProviders();
        auto tileProviderColor = colorTextureProviders.begin()->second;
        Tile colorTile = tileProviderColor->getHighestResolutionTile(chunk.index());


        // Bind and use the texture
        ghoul::opengl::TextureUnit texUnitColor;
        texUnitColor.activate();
        colorTile.texture->bind();
        _programObjectGlobalRendering->setUniform("colorTile.textureSampler", texUnitColor);
        _programObjectGlobalRendering->setUniform("colorTile.uvTransform.uvScale", colorTile.uvTransform.uvScale);
        _programObjectGlobalRendering->setUniform("colorTile.uvTransform.uvOffset", colorTile.uvTransform.uvOffset);

        Geodetic2 swCorner = chunk.surfacePatch().southWestCorner();
        auto patchSize = chunk.surfacePatch().size();
        const Ellipsoid& ellipsoid = chunk.owner()->ellipsoid();
        _programObjectGlobalRendering->setUniform("modelViewProjectionTransform", modelViewProjectionTransform);
        _programObjectGlobalRendering->setUniform("minLatLon", vec2(swCorner.toLonLatVec2()));
        _programObjectGlobalRendering->setUniform("lonLatScalingFactor", vec2(patchSize.toLonLatVec2()));
        _programObjectGlobalRendering->setUniform("radiiSquared", vec3(ellipsoid.radiiSquared()));

        glEnable(GL_DEPTH_TEST);
        glEnable(GL_CULL_FACE);
        glCullFace(GL_BACK);

        // render
        _grid->geometry().drawUsingActiveProgram();

        // disable shader
        _programObjectGlobalRendering->deactivate();
        */
    }

    void ChunkRenderer::renderChunkLocally(const Chunk& chunk, const RenderData& data)
    {
        using namespace glm;

        // All providers of tiles
        auto heightMapProviders = _tileProviderManager->heightMapProviders();
        auto colorTextureProviders = _tileProviderManager->colorTextureProviders();

        int numHeightMapProviders = heightMapProviders.size();
        int numColorTextureProviders = colorTextureProviders.size();

        // Create information for the shader provider
        LayeredTextureInfo layeredTextureInfoHeight;
        LayeredTextureInfo layeredTextureInfoColor;
        layeredTextureInfoHeight.keyNumLayers = "numLayersHeight";
        layeredTextureInfoHeight.numLayers = numHeightMapProviders;
        layeredTextureInfoColor.keyNumLayers = "numLayersColor";
        layeredTextureInfoColor.numLayers = numColorTextureProviders;

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


        std::vector<ghoul::opengl::TextureUnit> texUnitHeight;
        std::vector<ghoul::opengl::TextureUnit> texUnitColor;

        texUnitHeight.resize(numHeightMapProviders);
        texUnitColor.resize(numColorTextureProviders);


        // Go through all the height map providers
        int i = 0;
        for (auto it = heightMapProviders.begin(); it != heightMapProviders.end(); it++)
        {
            auto tileProvider = it->second;

            // Get the texture that should be used for rendering
            Tile tile = tileProvider->getHighestResolutionTile(chunk.index());
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

            i++;
        }

        // Go through all the color texture providers
        i = 0;
        for (auto it = colorTextureProviders.begin(); it != colorTextureProviders.end(); it++)
        {
            auto tileProvider = it->second;

            // Get the texture that should be used for rendering
            Tile tile = tileProvider->getHighestResolutionTile(chunk.index());

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

            i++;
        }








        // Calculate other uniform variables needed for rendering

        // TODO : Model transform should be fetched as a matrix directly.
        mat4 modelTransform = translate(mat4(1), data.position.vec3());
        mat4 viewTransform = data.camera.combinedViewMatrix();
        mat4 modelViewTransform = viewTransform * modelTransform;

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

        // OpenGL rendering settings
        glEnable(GL_DEPTH_TEST);
        glEnable(GL_CULL_FACE);
        glCullFace(GL_BACK);

        // render
        _grid->geometry().drawUsingActiveProgram();

        // disable shader
        programObject->deactivate();


        



        /*
        using namespace glm;

        // TODO : Model transform should be fetched as a matrix directly.
        mat4 modelTransform = translate(mat4(1), data.position.vec3());
        mat4 viewTransform = data.camera.combinedViewMatrix();
        mat4 modelViewTransform = viewTransform * modelTransform;

        // activate shader
        _programObjectLocalRendering->activate();


        // For now just pick the first one from height maps
        auto heightMapProviders = _tileProviderManager->heightMapProviders();
        auto tileProviderHeight = heightMapProviders.begin()->second;

        // Get the textures that should be used for rendering
        Tile heightTile = tileProviderHeight->getHighestResolutionTile(chunk.index());

        // Bind and use the texture
        ghoul::opengl::TextureUnit texUnitHeight;
        texUnitHeight.activate();
        heightTile.texture->bind();
        _programObjectLocalRendering->setUniform("heightTile.textureSampler", texUnitHeight);
        _programObjectLocalRendering->setUniform("heightTile.uvTransform.uvScale", heightTile.uvTransform.uvScale);
        _programObjectLocalRendering->setUniform("heightTile.uvTransform.uvOffset", heightTile.uvTransform.uvOffset);
        
        TileDepthTransform depthTransformHeight = tileProviderHeight->depthTransform();
        _programObjectLocalRendering->setUniform("heightTile.depthTransform.depthScale", depthTransformHeight.depthScale);
        _programObjectLocalRendering->setUniform("heightTile.depthTransform.depthOffset", depthTransformHeight.depthOffset);

        // Pick the first color texture
        auto colorTextureProviders = _tileProviderManager->colorTextureProviders();
        auto tileProviderColor = colorTextureProviders.begin()->second;
        Tile colorTile = tileProviderColor->getHighestResolutionTile(chunk.index());


        // Bind and use the texture
        ghoul::opengl::TextureUnit texUnitColor;
        texUnitColor.activate();
        colorTile.texture->bind();
        _programObjectLocalRendering->setUniform("colorTile.textureSampler", texUnitColor);
        _programObjectLocalRendering->setUniform("colorTile.uvTransform.uvScale", colorTile.uvTransform.uvScale);
        _programObjectLocalRendering->setUniform("colorTile.uvTransform.uvOffset", colorTile.uvTransform.uvOffset);


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
        _programObjectLocalRendering->setUniform("p00", vec3(patchSwCameraSpace));
        _programObjectLocalRendering->setUniform("p10", vec3(patchSeCameraSpace));
        _programObjectLocalRendering->setUniform("p01", vec3(patchNwCameraSpace));
        _programObjectLocalRendering->setUniform("p11", vec3(patchNeCameraSpace));

        vec3 patchNormalCameraSpace = normalize(
            cross(patchSeCameraSpace - patchSwCameraSpace,
                patchNwCameraSpace - patchSwCameraSpace));

        _programObjectLocalRendering->setUniform(
            "patchNormalCameraSpace",
            patchNormalCameraSpace);

        _programObjectLocalRendering->setUniform(
            "projectionTransform",
            data.camera.projectionMatrix());

        glEnable(GL_DEPTH_TEST);
        glEnable(GL_CULL_FACE);
        glCullFace(GL_BACK);

        // render
        _grid->geometry().drawUsingActiveProgram();

        // disable shader
        _programObjectLocalRendering->deactivate();
        */
    }
    
    //////////////////////////////////////////////////////////////////////////////////////
    //								CLIPMAP PATCH RENDERER								//
    //////////////////////////////////////////////////////////////////////////////////////
    ClipMapPatchRenderer::ClipMapPatchRenderer(
        shared_ptr<ClipMapGrid> grid,
        shared_ptr<TileProviderManager> tileProviderManager)
        : PatchRenderer(tileProviderManager)
        , _grid(grid)
        , _patchCoverageProvider(
            Geodetic2(M_PI * 2, M_PI * 2),
            Geodetic2(-M_PI -M_PI/2, -M_PI),
            10)
    {
        _programObjectGlobalRendering = OsEng.renderEngine().buildRenderProgram(
            "GlobalClipMapPatch",
            "${MODULE_GLOBEBROWSING}/shaders/globalclipmappatch_vs.glsl",
            "${MODULE_GLOBEBROWSING}/shaders/globalclipmappatch_fs.glsl");
        ghoul_assert(_programObjectGlobalRendering != nullptr, "Failed to initialize programObject!");
        
        _programObjectLocalRendering = OsEng.renderEngine().buildRenderProgram(
            "LocalClipMapPatch",
            "${MODULE_GLOBEBROWSING}/shaders/localclipmappatch_vs.glsl",
            "${MODULE_GLOBEBROWSING}/shaders/localclipmappatch_fs.glsl");
        ghoul_assert(_programObjectLocalRendering != nullptr, "Failed to initialize programObject!");

        using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
        _programObjectGlobalRendering->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
        _programObjectLocalRendering->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    }

    ClipMapPatchRenderer::~ClipMapPatchRenderer()
    {
        if (_programObjectGlobalRendering) {
            RenderEngine& renderEngine = OsEng.renderEngine();
            renderEngine.removeRenderProgram(_programObjectGlobalRendering);
            _programObjectGlobalRendering = nullptr;
        }
    }

    void ClipMapPatchRenderer::renderPatch(
        const Geodetic2& patchSize,
        const RenderData& data,
        const Ellipsoid& ellipsoid)
    {
        if (glm::max(patchSize.lat, patchSize.lon) > M_PI / 200) {
            renderPatchGlobally(patchSize, data, ellipsoid);
        }
        else {
            renderPatchLocally(patchSize, data, ellipsoid);
        }
    }

    void ClipMapPatchRenderer::renderPatchGlobally(
        const Geodetic2& patchSize,
        const RenderData& data,
        const Ellipsoid& ellipsoid)
    {
        // activate shader
        _programObjectGlobalRendering->activate();
        using namespace glm;

        mat4 viewTransform = data.camera.combinedViewMatrix();

        // TODO : Model transform should be fetched as a matrix directly.
        mat4 modelTransform = translate(mat4(1), data.position.vec3());

        // Snap patch position
        int segmentsPerPatch = _grid->segments();
        Geodetic2 stepSize = Geodetic2(
            patchSize.lat / segmentsPerPatch,
            patchSize.lon / segmentsPerPatch);
        ivec2 patchesToCoverGlobe = ivec2(
            M_PI / patchSize.lat + 0.5,
            M_PI * 2 / patchSize.lon + 0.5);
        Geodetic2 cameraPosLatLon = ellipsoid.cartesianToGeodetic2(data.camera.position().dvec3());
        ivec2 intSnapCoord = ivec2(
            cameraPosLatLon.lat / (M_PI * 2) * segmentsPerPatch * patchesToCoverGlobe.y,
            cameraPosLatLon.lon / (M_PI)* segmentsPerPatch * patchesToCoverGlobe.x);
        Geodetic2 newPatchCenter = Geodetic2(
            stepSize.lat * intSnapCoord.x,
            stepSize.lon * intSnapCoord.y);
        GeodeticPatch newPatch(
            newPatchCenter,
            Geodetic2(patchSize.lat / 2, patchSize.lon / 2));

        ivec2 contraction = ivec2(intSnapCoord.y % 2, intSnapCoord.x % 2);




        
        // For now just pick the first one from height maps
        auto heightMapProviders = _tileProviderManager->heightMapProviders();
        auto tileProviderHeight = heightMapProviders.begin()->second;
        PatchCoverage patchCoverageHeight = _patchCoverageProvider.getCoverage(newPatch, tileProviderHeight);

        // Bind and use the texture
        ghoul::opengl::TextureUnit texUnitHeight00;
        texUnitHeight00.activate();
        patchCoverageHeight.textureTransformPairs[0].first->bind(); // tile00
        _programObjectGlobalRendering->setUniform("textureSamplerHeight00", texUnitHeight00);

        ghoul::opengl::TextureUnit texUnitHeight10;
        texUnitHeight10.activate();
        patchCoverageHeight.textureTransformPairs[1].first->bind(); // tile10
        _programObjectGlobalRendering->setUniform("textureSamplerHeight10", texUnitHeight10);

        ghoul::opengl::TextureUnit texUnitHeight01;
        texUnitHeight01.activate();
        patchCoverageHeight.textureTransformPairs[2].first->bind(); // tile01
        _programObjectGlobalRendering->setUniform("textureSamplerHeight01", texUnitHeight01);

        ghoul::opengl::TextureUnit texUnitHeight11;
        texUnitHeight11.activate();
        patchCoverageHeight.textureTransformPairs[3].first->bind(); // tile11
        _programObjectGlobalRendering->setUniform("textureSamplerHeight11", texUnitHeight11);


        _programObjectGlobalRendering->setUniform(
            "uvTransformPatchToTileHeight00",
            patchCoverageHeight.textureTransformPairs[0].second);
        _programObjectGlobalRendering->setUniform(
            "uvTransformPatchToTileHeight10",
            patchCoverageHeight.textureTransformPairs[1].second);
        _programObjectGlobalRendering->setUniform(
            "uvTransformPatchToTileHeight01",
            patchCoverageHeight.textureTransformPairs[2].second);
        _programObjectGlobalRendering->setUniform(
            "uvTransformPatchToTileHeight11",
            patchCoverageHeight.textureTransformPairs[3].second);




        uvec2 texture00DimensionsHeight = patchCoverageHeight.textureTransformPairs[0].first->dimensions().xy();
        uvec2 texture10DimensionsHeight = patchCoverageHeight.textureTransformPairs[1].first->dimensions().xy();
        uvec2 texture01DimensionsHeight = patchCoverageHeight.textureTransformPairs[2].first->dimensions().xy();
        uvec2 texture11DimensionsHeight = patchCoverageHeight.textureTransformPairs[3].first->dimensions().xy();

        _programObjectGlobalRendering->setUniform("texture00DimensionsHeight", texture00DimensionsHeight);
        _programObjectGlobalRendering->setUniform("texture10DimensionsHeight", texture10DimensionsHeight);
        _programObjectGlobalRendering->setUniform("texture01DimensionsHeight", texture01DimensionsHeight);
        _programObjectGlobalRendering->setUniform("texture11DimensionsHeight", texture11DimensionsHeight);



        // Pick the first color texture
        auto colorTextureProviders = _tileProviderManager->colorTextureProviders();
        auto tileProviderColor = colorTextureProviders.begin()->second;
        PatchCoverage patchCoverageColor = _patchCoverageProvider.getCoverage(newPatch, tileProviderColor);

        // Bind and use the texture
        ghoul::opengl::TextureUnit texUnitColor00;
        texUnitColor00.activate();
        patchCoverageColor.textureTransformPairs[0].first->bind(); // tile00
        _programObjectGlobalRendering->setUniform("textureSamplerColor00", texUnitColor00);

        ghoul::opengl::TextureUnit texUnitColor10;
        texUnitColor10.activate();
        patchCoverageColor.textureTransformPairs[1].first->bind(); // tile10
        _programObjectGlobalRendering->setUniform("textureSamplerColor10", texUnitColor10);

        ghoul::opengl::TextureUnit texUnitColor01;
        texUnitColor01.activate();
        patchCoverageColor.textureTransformPairs[2].first->bind(); // tile01
        _programObjectGlobalRendering->setUniform("textureSamplerColor01", texUnitColor01);

        ghoul::opengl::TextureUnit texUnitColor11;
        texUnitColor11.activate();
        patchCoverageColor.textureTransformPairs[3].first->bind(); // tile11
        _programObjectGlobalRendering->setUniform("textureSamplerColor11", texUnitColor11);


        _programObjectGlobalRendering->setUniform(
            "uvTransformPatchToTileColor00",
            patchCoverageColor.textureTransformPairs[0].second);
        _programObjectGlobalRendering->setUniform(
            "uvTransformPatchToTileColor10",
            patchCoverageColor.textureTransformPairs[1].second);
        _programObjectGlobalRendering->setUniform(
            "uvTransformPatchToTileColor01",
            patchCoverageColor.textureTransformPairs[2].second);
        _programObjectGlobalRendering->setUniform(
            "uvTransformPatchToTileColor11",
            patchCoverageColor.textureTransformPairs[3].second);


        uvec2 texture00DimensionsColor = patchCoverageColor.textureTransformPairs[0].first->dimensions().xy();
        uvec2 texture10DimensionsColor = patchCoverageColor.textureTransformPairs[1].first->dimensions().xy();
        uvec2 texture01DimensionsColor = patchCoverageColor.textureTransformPairs[2].first->dimensions().xy();
        uvec2 texture11DimensionsColor = patchCoverageColor.textureTransformPairs[3].first->dimensions().xy();

        _programObjectGlobalRendering->setUniform("texture00DimensionsColor", texture00DimensionsColor);
        _programObjectGlobalRendering->setUniform("texture10DimensionsColor", texture10DimensionsColor);
        _programObjectGlobalRendering->setUniform("texture01DimensionsColor", texture01DimensionsColor);
        _programObjectGlobalRendering->setUniform("texture11DimensionsColor", texture11DimensionsColor);



        _programObjectGlobalRendering->setUniform(
            "modelViewProjectionTransform",
            data.camera.projectionMatrix() * viewTransform *  modelTransform);
        _programObjectGlobalRendering->setUniform("segmentsPerPatch", segmentsPerPatch);
        _programObjectGlobalRendering->setUniform("minLatLon", vec2(newPatch.southWestCorner().toLonLatVec2()));
        _programObjectGlobalRendering->setUniform("lonLatScalingFactor", vec2(patchSize.toLonLatVec2()));
        _programObjectGlobalRendering->setUniform("radiiSquared", vec3(ellipsoid.radiiSquared()));
        _programObjectGlobalRendering->setUniform("contraction", contraction);

        glEnable(GL_DEPTH_TEST);
        glEnable(GL_CULL_FACE);
        glCullFace(GL_BACK);

        // render
        _grid->geometry().drawUsingActiveProgram();

        // disable shader
        _programObjectGlobalRendering->deactivate();
    }

    void ClipMapPatchRenderer::renderPatchLocally(
        const Geodetic2& patchSize,
        const RenderData& data,
        const Ellipsoid& ellipsoid)
    {
        // activate shader
        _programObjectLocalRendering->activate();
        using namespace glm;

        mat4 viewTransform = data.camera.combinedViewMatrix();

        // TODO : Model transform should be fetched as a matrix directly.
        mat4 modelTransform = translate(mat4(1), data.position.vec3());

        mat4 modelViewTransform = viewTransform * modelTransform;

        // Snap patch position
        int segmentsPerPatch = _grid->segments();
        Geodetic2 stepSize = Geodetic2(
            patchSize.lat / segmentsPerPatch,
            patchSize.lon / segmentsPerPatch);
        ivec2 patchesToCoverGlobe = ivec2(
            M_PI / patchSize.lat + 0.5,
            M_PI * 2 / patchSize.lon + 0.5);
        Geodetic2 cameraPosLatLon = ellipsoid.cartesianToGeodetic2(data.camera.position().dvec3());
        ivec2 intSnapCoord = ivec2(
            cameraPosLatLon.lat / (M_PI * 2) * segmentsPerPatch * patchesToCoverGlobe.y,
            cameraPosLatLon.lon / (M_PI)* segmentsPerPatch * patchesToCoverGlobe.x);
        Geodetic2 newPatchCenter = Geodetic2(
            stepSize.lat * intSnapCoord.x,
            stepSize.lon * intSnapCoord.y);
        GeodeticPatch newPatch(
            newPatchCenter,
            Geodetic2(patchSize.lat / 2, patchSize.lon / 2));

        ivec2 contraction = ivec2(intSnapCoord.y % 2, intSnapCoord.x % 2);

        // Get global positions of the four control points
        Vec3 patchSw = ellipsoid.cartesianSurfacePosition(newPatch.southWestCorner());
        Vec3 patchSe = ellipsoid.cartesianSurfacePosition(newPatch.southEastCorner());
        Vec3 patchNw = ellipsoid.cartesianSurfacePosition(newPatch.northWestCorner());
        Vec3 patchNe = ellipsoid.cartesianSurfacePosition(newPatch.northEastCorner());

        // Transform all control points to camera space
        patchSw = Vec3(dmat4(modelViewTransform) * glm::dvec4(patchSw, 1));
        patchSe = Vec3(dmat4(modelViewTransform) * glm::dvec4(patchSe, 1));
        patchNw = Vec3(dmat4(modelViewTransform) * glm::dvec4(patchNw, 1));
        patchNe = Vec3(dmat4(modelViewTransform) * glm::dvec4(patchNe, 1));


        // Send control points to shader
        _programObjectLocalRendering->setUniform("p00", vec3(patchSw));
        _programObjectLocalRendering->setUniform("p10", vec3(patchSe));
        _programObjectLocalRendering->setUniform("p01", vec3(patchNw));
        _programObjectLocalRendering->setUniform("p11", vec3(patchNe));

        vec3 patchNormal = normalize(cross(patchSe - patchSw, patchNw - patchSw));
        _programObjectLocalRendering->setUniform("patchNormal", patchNormal);

        // For now just pick the first one from height maps
        auto heightMapProviders = _tileProviderManager->heightMapProviders();
        auto tileProviderHeight = heightMapProviders.begin()->second;
        PatchCoverage patchCoverageHeight = _patchCoverageProvider.getCoverage(newPatch, tileProviderHeight);

        // Bind and use the texture
        ghoul::opengl::TextureUnit texUnitHeight00;
        texUnitHeight00.activate();
        patchCoverageHeight.textureTransformPairs[0].first->bind(); // tile00
        _programObjectLocalRendering->setUniform("textureSamplerHeight00", texUnitHeight00);

        ghoul::opengl::TextureUnit texUnitHeight10;
        texUnitHeight10.activate();
        patchCoverageHeight.textureTransformPairs[1].first->bind(); // tile10
        _programObjectLocalRendering->setUniform("textureSamplerHeight10", texUnitHeight10);

        ghoul::opengl::TextureUnit texUnitHeight01;
        texUnitHeight01.activate();
        patchCoverageHeight.textureTransformPairs[2].first->bind(); // tile01
        _programObjectLocalRendering->setUniform("textureSamplerHeight01", texUnitHeight01);

        ghoul::opengl::TextureUnit texUnitHeight11;
        texUnitHeight11.activate();
        patchCoverageHeight.textureTransformPairs[3].first->bind(); // tile11
        _programObjectLocalRendering->setUniform("textureSamplerHeight11", texUnitHeight11);


        _programObjectLocalRendering->setUniform(
            "uvTransformPatchToTileHeight00",
            patchCoverageHeight.textureTransformPairs[0].second);
        _programObjectLocalRendering->setUniform(
            "uvTransformPatchToTileHeight10",
            patchCoverageHeight.textureTransformPairs[1].second);
        _programObjectLocalRendering->setUniform(
            "uvTransformPatchToTileHeight01",
            patchCoverageHeight.textureTransformPairs[2].second);
        _programObjectLocalRendering->setUniform(
            "uvTransformPatchToTileHeight11",
            patchCoverageHeight.textureTransformPairs[3].second);



        uvec2 texture00DimensionsHeight = patchCoverageHeight.textureTransformPairs[0].first->dimensions().xy();
        uvec2 texture10DimensionsHeight = patchCoverageHeight.textureTransformPairs[1].first->dimensions().xy();
        uvec2 texture01DimensionsHeight = patchCoverageHeight.textureTransformPairs[2].first->dimensions().xy();
        uvec2 texture11DimensionsHeight = patchCoverageHeight.textureTransformPairs[3].first->dimensions().xy();

        _programObjectLocalRendering->setUniform("texture00DimensionsHeight", texture00DimensionsHeight);
        _programObjectLocalRendering->setUniform("texture10DimensionsHeight", texture10DimensionsHeight);
        _programObjectLocalRendering->setUniform("texture01DimensionsHeight", texture01DimensionsHeight);
        _programObjectLocalRendering->setUniform("texture11DimensionsHeight", texture11DimensionsHeight);





        // Pick the first color texture
        auto colorTextureProviders = _tileProviderManager->colorTextureProviders();
        auto tileProviderColor = colorTextureProviders.begin()->second;
        PatchCoverage patchCoverageColor = _patchCoverageProvider.getCoverage(newPatch, tileProviderColor);

        // Bind and use the texture
        ghoul::opengl::TextureUnit texUnitColor00;
        texUnitColor00.activate();
        patchCoverageColor.textureTransformPairs[0].first->bind(); // tile00
        _programObjectLocalRendering->setUniform("textureSamplerColor00", texUnitColor00);

        ghoul::opengl::TextureUnit texUnitColor10;
        texUnitColor10.activate();
        patchCoverageColor.textureTransformPairs[1].first->bind(); // tile10
        _programObjectLocalRendering->setUniform("textureSamplerColor10", texUnitColor10);

        ghoul::opengl::TextureUnit texUnitColor01;
        texUnitColor01.activate();
        patchCoverageColor.textureTransformPairs[2].first->bind(); // tile01
        _programObjectLocalRendering->setUniform("textureSamplerColor01", texUnitColor01);

        ghoul::opengl::TextureUnit texUnitColor11;
        texUnitColor11.activate();
        patchCoverageColor.textureTransformPairs[3].first->bind(); // tile11
        _programObjectLocalRendering->setUniform("textureSamplerColor11", texUnitColor11);


        _programObjectLocalRendering->setUniform(
            "uvTransformPatchToTileColor00",
            patchCoverageColor.textureTransformPairs[0].second);
        _programObjectLocalRendering->setUniform(
            "uvTransformPatchToTileColor10",
            patchCoverageColor.textureTransformPairs[1].second);
        _programObjectLocalRendering->setUniform(
            "uvTransformPatchToTileColor01",
            patchCoverageColor.textureTransformPairs[2].second);
        _programObjectLocalRendering->setUniform(
            "uvTransformPatchToTileColor11",
            patchCoverageColor.textureTransformPairs[3].second);


        uvec2 texture00DimensionsColor = patchCoverageColor.textureTransformPairs[0].first->dimensions().xy();
        uvec2 texture10DimensionsColor = patchCoverageColor.textureTransformPairs[1].first->dimensions().xy();
        uvec2 texture01DimensionsColor = patchCoverageColor.textureTransformPairs[2].first->dimensions().xy();
        uvec2 texture11DimensionsColor = patchCoverageColor.textureTransformPairs[3].first->dimensions().xy();

        _programObjectLocalRendering->setUniform("texture00DimensionsColor", texture00DimensionsColor);
        _programObjectLocalRendering->setUniform("texture10DimensionsColor", texture10DimensionsColor);
        _programObjectLocalRendering->setUniform("texture01DimensionsColor", texture01DimensionsColor);
        _programObjectLocalRendering->setUniform("texture11DimensionsColor", texture11DimensionsColor);


        _programObjectLocalRendering->setUniform(
            "projectionTransform",
            data.camera.projectionMatrix());
        _programObjectLocalRendering->setUniform("segmentsPerPatch", segmentsPerPatch);
        _programObjectLocalRendering->setUniform("contraction", contraction);

        glEnable(GL_DEPTH_TEST);
        glEnable(GL_CULL_FACE);
        glCullFace(GL_BACK);

        // render
        _grid->geometry().drawUsingActiveProgram();

        // disable shader
        _programObjectLocalRendering->deactivate();
    }

}  // namespace openspace
