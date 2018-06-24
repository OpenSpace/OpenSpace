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
#include <ghoul/filesystem/filesystem.h>    //use ghoul to write to file?

#include <openspace/engine/openspaceengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scene.h>
#include <modules/roverterrainrenderer/model/modelprovider.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <modules/marsrover/marsrovermodule.h>
#include <ghoul/fmt.h>

#include <freeimage.h>
#include <modules/marsrover/heighthandler/lodepng.h>


#include <modules/globebrowsing/geometry/geodetic2.h>
#include <modules/globebrowsing/geometry/geodetic3.h>
#include <modules/globebrowsing/globes/renderableglobe.h>



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
namespace ghoul::opengl { class Texture; }
namespace globebrowsing{ class globebrowsing; }
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

        //load shader
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
        
        _mars = OsEng.renderEngine().scene()->sceneGraphNode("Mars"); //change name?
        _globe = (globebrowsing::RenderableGlobe*)_mars->renderable();
        _camera = OsEng.navigationHandler().camera();   //gets the main camera object for the scene
  
 

        glGenVertexArrays(1, &_vertexArrayID);
        glGenBuffers(1, &_vertexArrayID);

        bool success = renderTexture();
        if(!success) {
            LERROR("Could not create depth texture");
        }
        else if(success) {
            LERROR("SUCCESS");
        }





    }
 


    void RenderableHeightMap::render(const RenderData& data, RendererTasks&) {  //data is the camera object

        _shader->activate();


        glEnable(GL_DEPTH_TEST);
        //cull triangles which normals is not towards the camera
        glEnable(GL_CULL_FACE);


        double latitude = 4.5895;
        double longitude = 137.4417;        //calculate the height vector to send as third argument of lookAt-function
        double altitude = 0.0;          //the above comment is done, need to be zero now!

        globebrowsing::Geodetic3 pos = {
            { latitude, longitude}, altitude    //check altitude, done 
        };

        globebrowsing::Geodetic3 posNorth = {
            { latitude + 0.001, longitude}, altitude    //check altitude, done 
        };

        glm::dvec3 marsPos = _mars->worldPosition();
        glm::dmat4 modelTransform = _globe->modelTransform();// Camera is described in world OpenSpace//generates unit matrix
        glm::dvec3 positionModelSpace =  _globe->ellipsoid().cartesianPosition(pos);
        glm::dvec3 cameraPosition = data.camera.positionVec3();//camera position in model space

        glm::dvec3 slightlyNorth = _globe->ellipsoid().cartesianPosition(posNorth);
        glm::dvec3 lookUpModelSpace = glm::normalize(slightlyNorth - positionModelSpace);
        glm::dvec3 lookUpWorldSpace = glm::dmat3(modelTransform) * lookUpModelSpace;

        // Lookat vector
        glm::dvec3 lookAtWorldSpace = glm::dvec3(modelTransform *
                                  glm::dvec4(positionModelSpace, 1.0));

        //LERROR(fmt::format("marsPos  '{}'", marsPos));
        //LERROR(fmt::format("modeltransform    '{}'",  modelTransform));
        //LERROR(fmt::format("positionModelSpace  '{}'", positionModelSpace));    //positionModelSpace is correct pos to look at
        //LERROR(fmt::format("cameraPosition    '{}'",  cameraPosition));
        //LERROR(fmt::format("slightlyNorth    '{}'",  slightlyNorth));
        //LERROR(fmt::format("lookUpModelSpace    '{}'",  lookUpModelSpace));
        //LERROR(fmt::format("lookUpWorldSpace    '{}'",  lookUpWorldSpace));
        //LERROR(fmt::format("lookAtWorldSpace    '{}'",  lookAtWorldSpace));
/*        
        glMatrixMode(GL_PROJECTION);
        glLoadIdentity();

        gluLookAt(
            cameraPosition[0], 
            cameraPosition[1], 
            cameraPosition[2], 
            lookAtWorldSpace[0], 
            lookAtWorldSpace[1], 
            lookAtWorldSpace[2], 
            lookUpWorldSpace[0],
            lookUpWorldSpace[1],
            lookUpWorldSpace[2]);
        glOrtho(0.0, 1920.0, 0.0, 1080.0, 1.0, -1000000.0);
        glPushMatrix();

*/
        glm::dmat4 lookAtCamera = glm::lookAt(    //creates a matrix that will look from cameraPosition to marsPos
            cameraPosition, 
            lookAtWorldSpace, 
            lookUpWorldSpace //find the normal of normal texture of Mars flat surface
        ); //location of camera, location of object you are looking at, location of up in scene
        //LERROR(fmt::format("lookAtCamera        '{}'", lookAtCamera)); 
        //glMatrixMode(GL_PROJECTION);//Applies subsequent matrix operations to the projection matrix stack.
        //glLoadIdentity();  //enhetsmatris
        //glViewport(0, 0, 1920, 1080);   //1000 = screenwith/screenheight
        glm::dmat4 orthoView = glm::ortho(0.0, 1280.0, 0.0, 720.0, -10000.0, -1000000.0);
        //left, right, bottom, top, near, far
        //LERROR(fmt::format("orthoView        '{}'", orthoView)); 
 

        //glm::dmat4 test1 = lookAtCamera * orthoView;

        //glm::dquat rotation = glm::quat_cast(test1);//converts to quaternions

        //_camera->setRotation(rotation);  //rotates from current rotation,


        _shader->setUniform(_uniformCache.viewmatte, lookAtCamera);
        _shader->setUniform(_uniformCache.projectionMatrix, orthoView);
 
      


        //bool success = renderTexture();
        //if(!success) {
        //    LERROR("Could not create depth texture");
        //}
        //else if(success) {


             //dimension of current window
            GLint dimensions[4];
            glGetIntegerv(GL_VIEWPORT, dimensions);
            GLint width = dimensions[2];
            GLint height = dimensions[3];
            
            GLenum err = glGetError();
            //LERROR(fmt::format("error:   '{}'", err));

            //dimension of current window
            //GLint dimensions[4];
            //glGetIntegerv(GL_VIEWPORT, dimensions);
            //GLint width = dimensions[2];
            //GLint height = dimensions[3];


            //glViewport(0,0,width,height);
            //glMatrixMode(GL_PROJECTION);
            //glLoadIdentity();
            //glOrtho(0.0, width, 0.0, height, -1.0, 1.0); 
            //glMatrixMode(GL_MODELVIEW);
            //glLoadIdentity();



            //not working?? GLint size = 0;
            //not working?? glGetBufferParameteriv(GL_FRAMEBUFFER, GL_BUFFER_SIZE, &size);
            //not working?? LERROR(fmt::format("size   '{}'", size));
//            GLubyte* pixels = (GLubyte*)malloc(4 * width * height* sizeof(GLubyte));
            //GLsizei buffsize = 8;
            //GLfloat pek[2];
            //GLubyte pixels[4 * width * height];
            //GLfloat windowDepth [4];

            //glReadPixels(0, 0, width, height, GL_RGBA, GL_UNSIGNED_BYTE, pixels); // read a block of pixels from the frame buffer, THIS ONE GIVES THE BLURRY
//            glReadPixels(0, 0, width, height, GL_DEPTH_COMPONENT, GL_FLOAT, pixels);
            //glGetFloatv(GL_DEPTH_SCALE, windowDepth);
            //glGetTexImage (GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT, GL_FLOAT, windowDepth);
            

            //pointer to image array with glByte
            //GLubyte ptrImage[4];
            //glGetTexImage (GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT, GL_FLOAT, ptrImage);
            //LERROR(fmt::format("texture:   '{}'", ptrImage));


            //TO FIX: maybe linearize before sending to writeTexture
            //for( size_t i = 0; i < (4*width*height); ++i )
            //{
            //    pixels[i] = ( 2.0 * near ) / ( far + near - pixels[i] * ( far - near ) );
            //}
            //LERROR(fmt::format("pek values:   '{}'", pek[0]));
            //LERROR(fmt::format("pek values:   '{}'", pek[1]));

 //           LERROR(fmt::format("pixel values:   '{}'", pixels[0]));
 //           LERROR(fmt::format("pixel values:   '{}'", pixels[1]));

 //           ghoul::io::TextureWriterFreeImage::writeTexture(pixels);
            

  //          free(pixels);
        //}


        _shader->deactivate();
    }

    void RenderableHeightMap::update(const UpdateData&) {
        //updatera alla uniformlocations here

    } 

    void RenderableHeightMap::deinitializeGL() {

        MarsroverModule::ProgramObjectManager.releaseProgramObject(
            ProgramName,
            [](ghoul::opengl::ProgramObject* p) {
                OsEng.renderEngine().removeRenderProgram(p);
            }
        );


        glDeleteTextures(1, &_zbuffer);
        //Bind 0, which means render to back buffer, as a result, fb is unbound
        
        glDeleteFramebuffers(1, &_fbo);
        //FIX: glDeleteRenderbuffers(1, &_rbo);
        glBindTexture(GL_TEXTURE_2D, 0);//unbind texture
        glBindRenderbuffer(GL_RENDERBUFFER, 0);
        glDisable(GL_DEPTH_TEST);
        _shader = nullptr;
    }


    bool RenderableHeightMap::renderTexture() {

        
        //do not need the color buffer at all
        GLint dimensions[4];
        glGetIntegerv(GL_VIEWPORT, dimensions);
        GLint width = dimensions[2];
        GLint height = dimensions[3];
        //******************Create Buffers****************//
        
        glGenFramebuffers(1, &_fbo);
        glBindFramebuffer(GL_FRAMEBUFFER, _fbo);


        //create a texture object
        glGenTextures(1, &_zbuffer);    //generate texture names
        glBindTexture(GL_TEXTURE_2D, _zbuffer); //bind a named texture to a texturing target
        
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);    //set texture parameters
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_DEPTH_STENCIL_TEXTURE_MODE, GL_DEPTH_COMPONENT);
        //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_MODE, GL_COMPARE_R_TO_TEXTURE);
        //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_FUNC, GL_LEQUAL);

        glTexImage2D(GL_TEXTURE_2D, 2, GL_DEPTH_COMPONENT, width, height, 0, 
            GL_DEPTH_COMPONENT, GL_FLOAT, NULL);     //specify a two-dimensional texture image
        //glGenerateMipmap(GL_TEXTURE_2D);
        

        //create a render buffer object _rbo, to store depth info
        GLuint _rbo;
        glGenRenderbuffers(1, &_rbo);
        glBindRenderbuffer(GL_RENDERBUFFER, _rbo);
        glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT, width, height);
        

        glPixelStorei(GL_PACK_ALIGNMENT, 8);

        //GLenum DrawBuffers[1] = {GL_COLOR_ATTACHMENT1};
        //glDrawBuffers(1, DrawBuffers);

        //glViewport(0,0,width,height);
        //******************Attach Buffers****************//

        glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, _zbuffer, 0);
        //glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, _rbo);


        GLubyte* pixelsDOWN = (GLubyte*)malloc(4 * width * height* sizeof(GLubyte));
        glReadPixels(0, 0, width, height, GL_DEPTH_COMPONENT, GL_FLOAT, pixelsDOWN);
        LERROR(fmt::format("pixel valuesDOWN:   '{}'", pixelsDOWN[1000]));
        LERROR(fmt::format("pixel valuesDOWN:   '{}'", pixelsDOWN[599]));




        //_uniformCache.texture = _shader->uniformLocation("heightmapTexture");
        //const GLint locationHeightmapTexture = _shader->uniformLocation("heightmapTexture");
        //_shader->setUniform(_uniformCache.texture, _fbo);
        //glUniform1i(locationHeightmapTexture, 0);

        glBindFramebuffer(GL_FRAMEBUFFER, 0);
        /******************************************************************************************/
       
        GLenum status;  
        status = glCheckFramebufferStatus(GL_FRAMEBUFFER);        
        if(status != GL_FRAMEBUFFER_COMPLETE)
            return false;
        else
            return true;
        
        
    }


    void RenderableHeightMap::renderOrthoCamera() {



        glMatrixMode(GL_PROJECTION);//Applies subsequent matrix operations to the projection matrix stack.
        glLoadIdentity();  //enhetsmatris

        //glViewport(0, 0, 1920, 1080);   //1000 = screenwith/screenheight
        //glOrtho(0, 1920, 0, 1080, 1, -1000000000000); // Origin in lower-left corner      //check if angle of camera???
        glOrtho(0, 1920, 0, 1080, 1, 1000000);
        //glTranslatef(-1.5f,0.0f,-995.0f);
        //glOrtho(0, 1000, 1000, 0, 1, -1); // Origin in upper-left corner
        //left, right, bottom, top, near, far

        //GLboolean pek = false;
        GLfloat pek;
        //glGetBooleanv(GL_MATRIX_MODE, &pek);
        glGetFloatv(GL_PROJECTION_STACK_DEPTH, &pek);
        //if(pek ){
            LERROR("HURRA");
            LERROR(fmt::format("peeek   '{}'", pek));
        //}
        //else
            //LERROR("NOOO");

        //glMatrixMode(GL_MODELVIEW);
        
        GLdouble orthoMatrix[16]; 
        glGetDoublev(GL_PROJECTION_MATRIX, orthoMatrix); 

        for (int i = 0; i < 16; i++){
            LERROR(fmt::format("ortho matrix: '{}'", orthoMatrix[i]));
        }
        


    }


} // namespace openspace



//Was on row 170

        //glm::dquat rotation = glm::quat_cast(lookAtCamera);//converts to quaternions


        //_camera->setFocusPositionVec3(focusOnPath); //no need to change at the moment
        //_camera->setPositionVec3(cameraPos);
        //_camera->setRotation(rotation);  //rotates from current rotation,
        //with lookat = -90 about z

        //_camera->setViewMatrix(lookAtCamera);
        //LERROR(fmt::format("camera: position after changed        '{}'", _camera->positionVec3()));
        //LERROR(fmt::format("camera: viewMarix?        '{}'", _camera->viewMatrix()));//enhetsmatris
        //LERROR(fmt::format("camera: viewdirection        '{}'", _camera->viewDirectionWorldSpace()));//bra        
        //LERROR(fmt::format("camera: focusPosition        '{}'", _camera->focusPositionVec3()));//0,0,0
