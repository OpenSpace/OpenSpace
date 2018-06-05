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

#include <modules/marsrover/rendering/renderableheightmap.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>	//use ghoul to write to file?

#include <openspace/engine/openspaceengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scene.h>
#include <modules/roverterrainrenderer/model/modelprovider.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <modules/marsrover/marsrovermodule.h>
#include <ghoul/fmt.h>
#include <ghoul/io/texture/texturereader.h>

namespace {
    constexpr const char* ProgramName = "HeightMap";
	const char* _loggerCat = "RenderableHeightMap";

    static const openspace::properties::Property::PropertyInfo FrustumInfo = {
        "FrustumSize",
        "FrustumSize",
        ""
    };

} // namespace
namespace ghoul::filesystem { class File; } //maybe not needed?
namespace openspace {


documentation::Documentation RenderableHeightMap::Documentation() {
    using namespace documentation;
    return {
        "RenderableHeightMap",
        "marsrover_renderable_marsrover",
        {
            {
                FrustumInfo.identifier,
                new DoubleVerifier,
                Optional::No,
                FrustumInfo.description
            }
        }
    };
}

    RenderableHeightMap::RenderableHeightMap(const ghoul::Dictionary& dictionary)
    :Renderable(dictionary)
    , _frustumSize(FrustumInfo)
    , _shader(nullptr)
    {


        _frustumSize = dictionary.value<glm::vec2>(FrustumInfo.identifier);
		addProperty(_frustumSize);

        //use _frustumSize as size of camera frustum of the camera used for creatign the height map

    }


    bool RenderableHeightMap::isReady() const {
        return true;
    }


    void RenderableHeightMap::initializeGL() {

        _shader = MarsroverModule::ProgramObjectManager.requestProgramObject(
            ProgramName,
            []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
                return OsEng.renderEngine().buildRenderProgram(
                    ProgramName,
                    absPath("${MODULE_MARSROVER}/shaders/heightmap_vs.glsl"),
                    absPath("${MODULE_MARSROVER}/shaders/heightmap_fs.glsl")
                );
            }
        );
        _parent = OsEng.renderEngine().scene()->sceneGraphNode("Submodules");
        _globe = (globebrowsing::RenderableGlobe*)_parent->renderable();


        // Create and compile our GLSL program from the shaders
        //_uniformCache.cameraFrustumSize = _shader->uniformLocation("cameraFrustumSize");
        //_uniformCache.directionToSurfaceViewSpace = _shader->uniformLocation("directionToSurfaceViewSpace");
        _uniformCache.heightmapTexture = _shader->uniformLocation("heightmapTexture");
        _uniformCache.modelViewTransform = _shader->uniformLocation("modelViewProjectionTransform");


        //***USING THE RENDERED TEXTURE***/

        //initialize and upload to graphics card

        glGenVertexArrays(1, &_vaHeightMap);
        //glBindVertexArray(_vaHeightMap);

        glGenBuffers(1,&_vbHeightMap);

        //FIX:bind buffers, might want to put in an own function
        glBindVertexArray(_vaHeightMap);
        glBindBuffer(GL_ARRAY_BUFFER, _vbHeightMap);

        static const GLfloat coordinates[] = {
            -1.0f, -1.0f, 0.0f,
            1.0f, -1.0f, 0.0f,
            -1.0f,  1.0f, 0.0f,
            -1.0f,  1.0f, 0.0f,
            1.0f, -1.0f, 0.0f,
            1.0f,  1.0f, 0.0f,
        };
        glBufferData(GL_ARRAY_BUFFER, sizeof(coordinates), coordinates, GL_STATIC_DRAW);

        glEnableVertexAttribArray(0);
        glVertexAttribPointer(0,4,GL_FLOAT, GL_FALSE, sizeof(coordinates[0]), 0);
        glBindVertexArray(0);


        renderOrthoCamera();
    }

    void RenderableHeightMap::deinitializeGL() {
        _heightmap = 0;
        _vertexPositionBuffer = 0;

        MarsroverModule::ProgramObjectManager.releaseProgramObject(
            ProgramName,
            [](ghoul::opengl::ProgramObject* p) {
                OsEng.renderEngine().removeRenderProgram(p);
            }
        );

        glDeleteVertexArrays(1, &_vaHeightMap);
        glDeleteBuffers(1, &_vbHeightMap);

        _shader = nullptr;
    }

    void RenderableHeightMap::render(const RenderData& data, RendererTasks&) {

        //we want the camera to be positioned abowe the models, 
        //then we need to get the position data from the models, right?

        //send the position data for the models to the vertex file
        //send the camera info to the vertex file 
        // find the coordinates with height 

        // Activate shader
        _shader->activate();
        //glm::dvec3 modelPathPosition;
        //modelPathPosition = {-2491.9696, 2291.3289, -272.16459};    //not correct position right now


        //_shader->setIgnoreUniformLocationError(IgnoreError::Yes);   
        
        glm::dmat4 modelViewTransformMatrix = data.camera.combinedViewMatrix();
        glm::dmat4 projectionTransformMatrix = data.camera.projectionMatrix();

        _shader->setUniform(_uniformCache.heightmapTexture, _heightmap);
        //_shader->setUniform(_uniformCache.cameraFrustumSize, _frustumSize);
        _shader->setUniform(_uniformCache.modelViewTransform, glm::dmat4(modelViewTransformMatrix));
        _shader->setUniform(_uniformCache.projectionTransform, glm::dmat4(projectionTransformMatrix));
        //_shader->setUniform(
        //    _uniformCache.directionToSurfaceViewSpace,
        //    directionToSurfaceViewSpace
        //);

   
        //pixelsArray is a pointer to the image data in memory
        //const void pixelsArray[_heightmap];
        //float pixels[2000]; //2000 is the width and height

        //glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT24, 1024, 768, 0, GL_DEPTH_COMPONENT, GL_FLOAT, nullptr);
        //FROM EXAMPLE: glTexImage2D(GL_TEXTURE_2D, 0,GL_DEPTH_COMPONENT24, 1024, 768, 0,GL_DEPTH_COMPONENT, GL_FLOAT, 0);


        //glGetTexImage(GL_TEXTURE_2D, 1, GL_DEPTH_COMPONENT, GL_FLOAT, &pixels);
        //(target, level of detail, format, type, pointer to array of the same type as "type" (ie float))
        _shader->deactivate();
    }

    void RenderableHeightMap::update(const UpdateData&) {
        //_uniformCache.cameraFrustumSize = _shader->uniformLocation("cameraFrustumSize");
        //_uniformCache.directionToSurfaceViewSpace = _shader->uniformLocation("directionToSurfaceViewSpace");
        _uniformCache.heightmapTexture = _shader->uniformLocation("heightmapTexture");
        _uniformCache.modelViewTransform = _shader->uniformLocation("modelViewProjectionTransform");
        //to get the position for every submodule, this is not the way we want it right now, but maybe we can improve our code later
        //_modelPathPosition = OsEng.renderEngine().scene()->sceneGraphNode("Submodules")->worldPosition();
       
       // renderOrthoCamera();
    } 

    void RenderableHeightMap::renderOrthoCamera() {
        glMatrixMode(GL_PROJECTION);
        glLoadIdentity();   
        glViewport(0, 0, 200, 200);   //200 = screenwith/screenheight
        glMatrixMode(GL_MODELVIEW);
        glLoadIdentity();
        glOrtho(0, 200, 0, 200, 1, -1); // Origin in lower-left corner
        glOrtho(0, 200, 200, 0, 1, -1); // Origin in upper-left corner
    }

    void RenderableHeightMap::drawTexture() {
        // /TextureReader::loadTexture();


    }



} // namespace openspace
