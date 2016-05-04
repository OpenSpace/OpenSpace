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

#include <modules/globebrowsing/meshes/clipmapgrid.h>

// open space includes
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>

// ghoul includes
#include <ghoul/misc/assert.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>


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
        if (_programObjectGlobalRendering) {
            RenderEngine& renderEngine = OsEng.renderEngine();
            renderEngine.removeRenderProgram(_programObjectGlobalRendering);
            _programObjectGlobalRendering = nullptr;
        }
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
    LatLonPatchRenderer::LatLonPatchRenderer(
        shared_ptr<Grid> grid,
        shared_ptr<TileProviderManager> tileProviderManager)
        : PatchRenderer(tileProviderManager)
        , _grid(grid)
    {
        _programObjectGlobalRendering = OsEng.renderEngine().buildRenderProgram(
            "GlobalChunkedLodPatch",
            "${MODULE_GLOBEBROWSING}/shaders/globalchunkedlodpatch_vs.glsl",
            "${MODULE_GLOBEBROWSING}/shaders/globalchunkedlodpatch_fs.glsl");
        ghoul_assert(_programObjectGlobalRendering != nullptr, "Failed to initialize programObject!");

        using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
        _programObjectGlobalRendering->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    }

    void LatLonPatchRenderer::renderPatch(
        const GeodeticPatch& patch,
        const RenderData& data,
        const Ellipsoid& ellipsoid,
        const GeodeticTileIndex& tileIndex)
    {
        using namespace glm;

        // PATCH DID NOT MATCH THE TILEINDEX SO I CREATED A NEW PATCH FROM THE INDEX

        GeodeticPatch newPatch = patch;//GeodeticPatch(tileIndex);

        //GeodeticTileIndex tmpTileIndex = _tileSet.getTileIndex(patch);


        // TODO : Model transform should be fetched as a matrix directly.
        mat4 modelTransform = translate(mat4(1), data.position.vec3());
        mat4 viewTransform = data.camera.combinedViewMatrix();
        mat4 modelViewProjectionTransform = data.camera.projectionMatrix()
            * viewTransform * modelTransform;

        // activate shader
        _programObjectGlobalRendering->activate();

        // Get the textures that should be used for rendering
        std::shared_ptr<ghoul::opengl::Texture> tile00;
        bool usingTile = true;

        // For now just pick the first one from height maps
        auto heightMapProviders = _tileProviderManager->heightMapProviders();
        auto tileProviderHeight = heightMapProviders.begin()->second;
        tile00 = tileProviderHeight->getTile(tileIndex);

        if (tile00 == nullptr) {
            tile00 = tileProviderHeight->getTemporaryTexture();
        }
        
        // Bind and use the texture
        ghoul::opengl::TextureUnit texUnitHeight;
        texUnitHeight.activate();
        tile00->bind();
        _programObjectGlobalRendering->setUniform("textureSamplerHeight", texUnitHeight);





        // Pick the first color texture
        auto colorTextureProviders = _tileProviderManager->colorTextureProviders();
        auto tileProviderColor = colorTextureProviders.begin()->second;
        tile00 = tileProviderColor->getTile(tileIndex);

        if (tile00 == nullptr) {
            tile00 = tileProviderColor->getTemporaryTexture();
        }

        // Now we use the same shader as for clipmap rendering so this uv transform is needed
        glm::mat3 uvTransformColor = glm::mat3(1);
        // Bind and use the texture
        ghoul::opengl::TextureUnit texUnitColor;
        texUnitColor.activate();
        tile00->bind();
        _programObjectGlobalRendering->setUniform("textureSamplerColor", texUnitColor);





        Geodetic2 swCorner = newPatch.southWestCorner();
        _programObjectGlobalRendering->setUniform("modelViewProjectionTransform", modelViewProjectionTransform);
        _programObjectGlobalRendering->setUniform("minLatLon", vec2(swCorner.toLonLatVec2()));
        _programObjectGlobalRendering->setUniform("lonLatScalingFactor", vec2(newPatch.size().toLonLatVec2()));
        _programObjectGlobalRendering->setUniform("radiiSquared", vec3(ellipsoid.radiiSquared()));

        glEnable(GL_DEPTH_TEST);
        glEnable(GL_CULL_FACE);
        glCullFace(GL_BACK);

        // render
        _grid->geometry().drawUsingActiveProgram();

        // disable shader
        _programObjectGlobalRendering->deactivate();
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
            "GlobalClipMapPatch",
            "${MODULE_GLOBEBROWSING}/shaders/globalclipmappatch_vs.glsl",
            "${MODULE_GLOBEBROWSING}/shaders/globalclipmappatch_fs.glsl");
        ghoul_assert(_programObjectLocalRendering != nullptr, "Failed to initialize programObject!");

        using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
        _programObjectGlobalRendering->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
        _programObjectLocalRendering->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    }

    void ClipMapPatchRenderer::renderPatch(
        const Geodetic2& patchSize,
        const RenderData& data,
        const Ellipsoid& ellipsoid)
    {
        renderPatchGlobally(patchSize, data, ellipsoid);
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
        PatchCoverage patchCoverage = _patchCoverageProvider.getCoverage(newPatch, tileProviderHeight);

        // Bind and use the texture
        ghoul::opengl::TextureUnit texUnitHeight00;
        texUnitHeight00.activate();
        patchCoverage.textureTransformPairs[0].first->bind(); // tile00
        _programObjectGlobalRendering->setUniform("textureSamplerHeight00", texUnitHeight00);

        ghoul::opengl::TextureUnit texUnitHeight10;
        texUnitHeight10.activate();
        patchCoverage.textureTransformPairs[1].first->bind(); // tile10
        _programObjectGlobalRendering->setUniform("textureSamplerHeight10", texUnitHeight10);

        ghoul::opengl::TextureUnit texUnitHeight01;
        texUnitHeight01.activate();
        patchCoverage.textureTransformPairs[2].first->bind(); // tile01
        _programObjectGlobalRendering->setUniform("textureSamplerHeight01", texUnitHeight01);

        ghoul::opengl::TextureUnit texUnitHeight11;
        texUnitHeight11.activate();
        patchCoverage.textureTransformPairs[3].first->bind(); // tile11
        _programObjectGlobalRendering->setUniform("textureSamplerHeight11", texUnitHeight11);


        _programObjectGlobalRendering->setUniform(
            "uvTransformPatchToTileHeight00",
            patchCoverage.textureTransformPairs[0].second);
        _programObjectGlobalRendering->setUniform(
            "uvTransformPatchToTileHeight10",
            patchCoverage.textureTransformPairs[1].second);
        _programObjectGlobalRendering->setUniform(
            "uvTransformPatchToTileHeight01",
            patchCoverage.textureTransformPairs[2].second);
        _programObjectGlobalRendering->setUniform(
            "uvTransformPatchToTileHeight11",
            patchCoverage.textureTransformPairs[3].second);










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

    void ClipMapPatchRenderer::renderPatchCameraSpace(
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
        PatchCoverage patchCoverage = _patchCoverageProvider.getCoverage(newPatch, tileProviderHeight);

        // Bind and use the texture
        ghoul::opengl::TextureUnit texUnitHeight00;
        texUnitHeight00.activate();
        patchCoverage.textureTransformPairs[0].first->bind(); // tile00
        _programObjectGlobalRendering->setUniform("textureSamplerHeight00", texUnitHeight00);

        ghoul::opengl::TextureUnit texUnitHeight10;
        texUnitHeight10.activate();
        patchCoverage.textureTransformPairs[1].first->bind(); // tile10
        _programObjectGlobalRendering->setUniform("textureSamplerHeight10", texUnitHeight10);

        ghoul::opengl::TextureUnit texUnitHeight01;
        texUnitHeight01.activate();
        patchCoverage.textureTransformPairs[2].first->bind(); // tile01
        _programObjectGlobalRendering->setUniform("textureSamplerHeight01", texUnitHeight01);

        ghoul::opengl::TextureUnit texUnitHeight11;
        texUnitHeight11.activate();
        patchCoverage.textureTransformPairs[3].first->bind(); // tile11
        _programObjectGlobalRendering->setUniform("textureSamplerHeight11", texUnitHeight11);


        _programObjectGlobalRendering->setUniform(
            "uvTransformPatchToTileHeight00",
            patchCoverage.textureTransformPairs[0].second);
        _programObjectGlobalRendering->setUniform(
            "uvTransformPatchToTileHeight10",
            patchCoverage.textureTransformPairs[1].second);
        _programObjectGlobalRendering->setUniform(
            "uvTransformPatchToTileHeight01",
            patchCoverage.textureTransformPairs[2].second);
        _programObjectGlobalRendering->setUniform(
            "uvTransformPatchToTileHeight11",
            patchCoverage.textureTransformPairs[3].second);










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

}  // namespace openspace
