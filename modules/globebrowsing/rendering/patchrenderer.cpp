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
    PatchRenderer::PatchRenderer()
        : _tileSet(Geodetic2(M_PI * 2, M_PI * 2), Geodetic2(M_PI, -M_PI), 1)
    {

    }

    PatchRenderer::~PatchRenderer() {
        if (_programObject) {
            RenderEngine& renderEngine = OsEng.renderEngine();
            renderEngine.removeRenderProgram(_programObject);
            _programObject = nullptr;
        }
    }



    //////////////////////////////////////////////////////////////////////////////////////
    //								LATLON PATCH RENDERER								//
    //////////////////////////////////////////////////////////////////////////////////////
    LatLonPatchRenderer::LatLonPatchRenderer(shared_ptr<Grid> grid)
        : PatchRenderer()
        , _grid(grid)
        , tileProvider("map_service_configs/TERRAIN.wms" , 5000)
        //, tileProvider("map_service_configs/frmt_wms_virtualearth.xml", 5000)
        //, tileProvider("textures/earth_bluemarble.jpg", 5000)
    {
        _programObject = OsEng.renderEngine().buildRenderProgram(
            "LatLonSphereMappingProgram",
            "${MODULE_GLOBEBROWSING}/shaders/latlonpatch_spheremapping_vs.glsl",
            "${MODULE_GLOBEBROWSING}/shaders/simple_fs.glsl");
        ghoul_assert(_programObject != nullptr, "Failed to initialize programObject!");

        using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
        _programObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    }

    void LatLonPatchRenderer::update() {
        tileProvider.prerender();
    }

    void LatLonPatchRenderer::renderPatch(
        const GeodeticPatch& patch, const RenderData& data, const Ellipsoid& ellipsoid)
    {
        
        // Get the textures that should be used for rendering
        GeodeticTileIndex ti = _tileSet.getTileIndex(patch);
        renderPatch(patch, data, ellipsoid, ti);
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
        _programObject->activate();

        // Get the textures that should be used for rendering
        std::shared_ptr<ghoul::opengl::Texture> tile00;
        bool usingTile = true;
        tile00 = tileProvider.getTile(tileIndex);

        if (tile00 == nullptr) {
            tile00 = _tileSet.getTile(tileIndex);
            usingTile = false;
        }

        glm::mat3 uvTransform = usingTile ? glm::mat3(1) : _tileSet.getUvTransformationPatchToTile(newPatch, tileIndex);

        // Bind and use the texture
        ghoul::opengl::TextureUnit texUnit;
        texUnit.activate();
        tile00->bind();
        _programObject->setUniform("textureSampler00", texUnit);
        _programObject->setUniform("uvTransformPatchToTile00", uvTransform);

        Geodetic2 swCorner = newPatch.southWestCorner();
        _programObject->setUniform("segmentsPerPatch", _grid->xSegments());
        _programObject->setUniform("modelViewProjectionTransform", modelViewProjectionTransform);
        _programObject->setUniform("minLatLon", vec2(swCorner.toLonLatVec2()));
        _programObject->setUniform("lonLatScalingFactor", vec2(newPatch.size().toLonLatVec2()));
        _programObject->setUniform("radiiSquared", vec3(ellipsoid.radiiSquared()));

        glEnable(GL_DEPTH_TEST);
        glEnable(GL_CULL_FACE);
        glCullFace(GL_BACK);

        // render
        _grid->geometry().drawUsingActiveProgram();

        // disable shader
        _programObject->deactivate();
    }
    


    //////////////////////////////////////////////////////////////////////////////////////
    //								CLIPMAP PATCH RENDERER								//
    //////////////////////////////////////////////////////////////////////////////////////
    ClipMapPatchRenderer::ClipMapPatchRenderer(shared_ptr<ClipMapGrid> grid)
        : PatchRenderer()
        , _grid(grid)
        , _tileProvider("map_service_configs/TERRAIN.wms", 5000)
        , _patchCoverageProvider(Geodetic2(M_PI * 2, M_PI * 2), Geodetic2(-M_PI -M_PI/2, -M_PI), 10)
    {
        _programObject = OsEng.renderEngine().buildRenderProgram(
            "LatLonSphereMappingProgram",
            "${MODULE_GLOBEBROWSING}/shaders/clipmappatch_spheremapping_vs.glsl",
            "${MODULE_GLOBEBROWSING}/shaders/simple_fs.glsl");
        ghoul_assert(_programObject != nullptr, "Failed to initialize programObject!");

        using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
        _programObject->setIgnoreSubroutineUniformLocationError(IgnoreError::Yes);
    }

    void ClipMapPatchRenderer::update() {
        _tileProvider.prerender();
    }



    void ClipMapPatchRenderer::renderPatch(
        const Geodetic2& patchSize,
        const RenderData& data,
        const Ellipsoid& ellipsoid)
    {
        // activate shader
        _programObject->activate();
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
            cameraPosLatLon.lon / (M_PI) * segmentsPerPatch * patchesToCoverGlobe.x);
        Geodetic2 newPatchCenter = Geodetic2(
            stepSize.lat * intSnapCoord.x,
            stepSize.lon * intSnapCoord.y);
        GeodeticPatch newPatch(
            newPatchCenter,
            Geodetic2(patchSize.lat / 2, patchSize.lon / 2));

        ivec2 contraction = ivec2(intSnapCoord.y % 2, intSnapCoord.x % 2);

        //LDEBUG("patch.center = [ " << patch.center.lat << " , " << patch.center.lon << " ]");
        //LDEBUG("intSnapCoord = [ " << intSnapCoord.x << " , " << intSnapCoord.y << " ]");
        //LDEBUG("contraction = [ " << contraction.x << " , " << contraction.y << " ]");


        // Get the textures that should be used for rendering
        GeodeticTileIndex tileIndex00 = _patchCoverageProvider.getTileIndex(newPatch);
        GeodeticTileIndex tileIndex10 = { tileIndex00.x + 1, tileIndex00.y, tileIndex00.level };
        GeodeticTileIndex tileIndex01 = { tileIndex00.x, tileIndex00.y + 1, tileIndex00.level };
        GeodeticTileIndex tileIndex11 = { tileIndex00.x + 1, tileIndex00.y + 1, tileIndex00.level };

        std::shared_ptr<ghoul::opengl::Texture> tile00 = _tileProvider.getTile(tileIndex00);
        std::shared_ptr<ghoul::opengl::Texture> tile10 = _tileProvider.getTile(tileIndex10);
        std::shared_ptr<ghoul::opengl::Texture> tile01 = _tileProvider.getTile(tileIndex01);
        std::shared_ptr<ghoul::opengl::Texture> tile11 = _tileProvider.getTile(tileIndex11);
        
        if (tile00 == nullptr) {
            tile00 = _tileSet.getTile(tileIndex00);
        }
        if (tile10 == nullptr) {
            tile10 = _tileSet.getTile(tileIndex01);
        }
        if (tile01 == nullptr) {
            tile01 = _tileSet.getTile(tileIndex10);
        }
        if (tile11 == nullptr) {
            tile11 = _tileSet.getTile(tileIndex11);
        }

        glm::mat3 uvTransform00 = _patchCoverageProvider.getUvTransformationPatchToTile(newPatch, tileIndex00);
        glm::mat3 uvTransform10 = _patchCoverageProvider.getUvTransformationPatchToTile(newPatch, tileIndex10);
        glm::mat3 uvTransform01 = _patchCoverageProvider.getUvTransformationPatchToTile(newPatch, tileIndex01);
        glm::mat3 uvTransform11 = _patchCoverageProvider.getUvTransformationPatchToTile(newPatch, tileIndex11);

        //std::shared_ptr<ghoul::opengl::Texture> tile00 = _tileSet.getTile(tileIndex);
        


        // Bind and use the texture
        ghoul::opengl::TextureUnit texUnit00;
        texUnit00.activate();
        tile00->bind();
        _programObject->setUniform("textureSampler00", texUnit00);

        ghoul::opengl::TextureUnit texUnit10;
        texUnit10.activate();
        tile10->bind();
        _programObject->setUniform("textureSampler10", texUnit10);

        ghoul::opengl::TextureUnit texUnit01;
        texUnit01.activate();
        tile01->bind();
        _programObject->setUniform("textureSampler01", texUnit01);

        ghoul::opengl::TextureUnit texUnit11;
        texUnit11.activate();
        tile11->bind();
        _programObject->setUniform("textureSampler11", texUnit11);


        _programObject->setUniform("uvTransformPatchToTile00", mat3(uvTransform00));
        _programObject->setUniform("uvTransformPatchToTile10", mat3(uvTransform10));
        _programObject->setUniform("uvTransformPatchToTile01", mat3(uvTransform01));
        _programObject->setUniform("uvTransformPatchToTile11", mat3(uvTransform11));

        _programObject->setUniform(
            "modelViewProjectionTransform",
            data.camera.projectionMatrix() * viewTransform *  modelTransform);
        _programObject->setUniform("segmentsPerPatch", segmentsPerPatch);
        _programObject->setUniform("minLatLon", vec2(newPatch.southWestCorner().toLonLatVec2()));
        _programObject->setUniform("lonLatScalingFactor", vec2(patchSize.toLonLatVec2()));
        _programObject->setUniform("radiiSquared", vec3(ellipsoid.radiiSquared()));
        _programObject->setUniform("contraction", contraction);

        glEnable(GL_DEPTH_TEST);
        glEnable(GL_CULL_FACE);
        glCullFace(GL_BACK);

        // render
        _grid->geometry().drawUsingActiveProgram();

        // disable shader
        _programObject->deactivate();
    }

}  // namespace openspace
