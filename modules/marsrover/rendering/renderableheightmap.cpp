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
#include <openspace/engine/globals.h>
#include <ghoul/fmt.h>

#include <freeimage.h>

#include <modules/globebrowsing/geometry/geodetic2.h>
#include <modules/globebrowsing/geometry/geodetic3.h>
#include <modules/globebrowsing/globes/renderableglobe.h>

#include <openspace/util/camera.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/framebufferrenderer.h>
#include <openspace/util/syncbuffer.h>
#include <openspace/query/query.h>



namespace {
    constexpr const char* ProgramName = "HeightMap";
    const char* _loggerCat = "RenderableHeightMap";

    static const openspace::properties::Property::PropertyInfo FrustumInfo = {
        "FrustumSize",
        "FrustumSize",
        ""
    };    

    static const openspace::properties::Property::PropertyInfo TriggerHeightmap = {
        "TriggerHeightmap",
        "TriggerHeightmap",
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
,_triggerHeightmap(TriggerHeightmap)
{


    _frustumSize = dictionary.value<glm::vec2>(FrustumInfo.identifier);

    addProperty(_frustumSize);

    //use _frustumSize as size of camera frustum of the camera used for creating the height map
}

bool RenderableHeightMap::isReady() const {
    return true;
}

void RenderableHeightMap::initializeGL() {

    ////Load shader
    //_shader = MarsroverModule::ProgramObjectManager.requestProgramObject(
    //    ProgramName,
    //    []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
    //        return OsEng.renderEngine().buildRenderProgram(
    //            ProgramName,
    //            //absPath("${MODULE_MARSROVER}/shaders/heightmap_vs.glsl"),
    //            //absPath("${MODULE_MARSROVER}/shaders/heightmap_fs.glsl")                
    //            absPath("${MODULE_BASE}/shaders/model_vs.glsl"),
    //            absPath("${MODULE_BASE}/shaders/model_fs.glsl")
    //        );
    //    }
    //);

    _shader = global::renderEngine.buildRenderProgram("HeightMap",
        absPath("${MODULE_MARSROVER}/shaders/heightmap_vs.glsl"),
        absPath("${MODULE_MARSROVER}/shaders/heightmap_fs.glsl") 
    );
    
    _mars = global::renderEngine.scene()->sceneGraphNode("Mars"); //change name?
    _globe = (globebrowsing::RenderableGlobe*)_mars->renderable();
    _orthoCamera = global::navigationHandler.camera();   //gets the main camera object for the scene

    /***************************************/


    glm::dvec3 orthoCameraPosition = glm::dvec3(350000, 350000, 350000);    //change to 4 and 137 degrees (lat/lon)
   
    //_orthoCamera->setParent(_mars);
    //_orthoCamera->setFocusPositionVec3(_mars->worldPosition());
    //_orthoCamera->setPositionVec3(orthoCameraPosition);

    //_orthoCamera->Camera::SgctInternal::setViewMatrix(_globe->modelTransform());
    //_orthoCamera->sgctInternal.setViewMatrix(_globe->modelTransform());

    /***************************************/



    glGenVertexArrays(1, &_vertexArrayID);
    glGenBuffers(1, &_vertexArrayID);
    

    _triggerHeightmap.onChange([this] () {
        succeded = renderTexture();
        if(!succeded) {
            LERROR("Could not create depth texture");
        }
        else 
            LERROR("Finished creating depth image");
    });
    addProperty(_triggerHeightmap);
}


void RenderableHeightMap::render(const RenderData& data, RendererTasks&) {  //data is the camera object

    _shader->activate();


    //glEnable(GL_DEPTH_TEST);
    //dont include triangles which normals is not towards the camera
    //glEnable(GL_CULL_FACE);

    //gale crater
    double latitude = 4.5895;
    double longitude = 137.4417;    //calculate the height vector to send as third argument of lookAt-function
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
    glm::dvec3 lookAtWorldSpace = glm::dvec3(modelTransform * glm::dvec4(positionModelSpace, 1.0));

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
    //creates a matrix that will look from cameraPosition to marsPos
    //lookAt(camera location, object location to look at, up in scene vector)
    glm::dmat4 lookAtCamera = glm::lookAt(    
        cameraPosition, 
        lookAtWorldSpace, 
        lookUpWorldSpace //find the normal of normal texture of Mars flat surface.
                         //Should be negative lookAtWorldSpace
    ); 
    

    //ortho(left, right, bottom, top, near, far)
    glm::dmat4 orthoView = glm::ortho(0.0, 1280.0, 0.0, 720.0, -10.0, -1000000.0);

    _shader->setUniform(_uniformCache.viewmatte, lookAtCamera);
    _shader->setUniform(_uniformCache.projectionMatrix, orthoView);



    


    /*
    if(!succeded) {
        LERROR("Could not create depth texture");
    }
    else if(succeded) {
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
        
        //use vector instead of malloc. 
        //GLubyte* pixels = vector.data
        GLubyte* pixels = (GLubyte*)malloc(4 * width * height* sizeof(GLubyte));
        //GLsizei buffsize = 8;
        //GLfloat test_value[2];
        //GLubyte pixels[4 * width * height];
        //GLfloat windowDepth [4];

        double *testDepth;
        glGetDoublev( DEPTH, testDepth );

        //glReadPixels(0, 0, width, height, GL_RGBA, GL_UNSIGNED_BYTE, pixels); // read a block of pixels from the frame buffer, THIS ONE GIVES THE BLURRY
        glReadPixels(0, 0, width, height, GL_DEPTH_COMPONENT, GL_FLOAT, pixels);
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
        //LERROR(fmt::format("test_value values:   '{}'", test_value[0]));
        //LERROR(fmt::format("test_value values:   '{}'", test_value[1]));
        //LERROR(fmt::format("pixel values: '{}'", pixels[0]));
        //LERROR(fmt::format("pixel values: '{}'", pixels[1]));

        ghoul::io::TextureWriterFreeImage::writeTexture(pixels);
        
        free(pixels);
        succeded = false;
    } */

    
    //}

   _shader->deactivate();
}


void RenderableHeightMap::update(const UpdateData&) {
    //Update all uniform locations 

} 

void RenderableHeightMap::deinitializeGL() {

    MarsroverModule::ProgramObjectManager.release(
        ProgramName,
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine.removeRenderProgram(p);
        }
    );

    glDeleteTextures(1, &_zbuffer);
    //Bind 0, which means render to back buffer, as a result, fb is unbound
    
    glDeleteFramebuffers(1, &_fbo);
    //FIX: glDeleteRenderbuffers(1, &_rbo);
    glBindTexture(GL_TEXTURE_2D, 0);
    //unbind texture
    glBindRenderbuffer(GL_RENDERBUFFER, 0);
    glDisable(GL_DEPTH_TEST);
    _shader = nullptr;
}


bool RenderableHeightMap::renderTexture() {


    /*
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_CULL_FACE);

    //Don't need the color buffer
    GLint dimensions[4];
    glGetIntegerv(GL_VIEWPORT, dimensions);
    GLint width = dimensions[2];
    GLint height = dimensions[3];


    // ********** Create Buffers **********
    
    glGenFramebuffers(1, &_fbo);
    glBindFramebuffer(GL_FRAMEBUFFER, _fbo);

    //Create a texture object
    glGenTextures(1, &_zbuffer);            //generate texture names
    glBindTexture(GL_TEXTURE_2D, _zbuffer); //bind a named texture to a texturing target
    
    //Set texture parameters
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE); 
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_DEPTH_STENCIL_TEXTURE_MODE, GL_DEPTH_COMPONENT);

    //Specify a two-dimensional texture image
    glTexImage2D(GL_TEXTURE_2D, 2, GL_DEPTH_COMPONENT, width, height, 0, GL_DEPTH_COMPONENT, GL_FLOAT, NULL);     

    //Create a render buffer object _rbo, to store depth info
    //GLuint _rbo;
    //glGenRenderbuffers(1, &_rbo);
    //glBindRenderbuffer(GL_RENDERBUFFER, _rbo);
    //glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT, width, height);
    //glPixelStorei(GL_PACK_ALIGNMENT, 8);

    // ********** Attach Buffers **********

    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, _zbuffer, 0);
    glBindFramebuffer(GL_FRAMEBUFFER, 0);

    */

    /***************************************/

    //std::unique_ptr<Renderer> newRenderer = nullptr;
    //newRenderer = std::make_unique<FramebufferRenderer>();
    //const std::string renderingMethod = "Framebuffer";
    //OsEng.renderEngine().setRendererFromString(renderingMethod);

    //dimension of current window
    GLint dimensions[4];
    glGetIntegerv(GL_VIEWPORT, dimensions);
    GLint width = dimensions[2];
    GLint height = dimensions[3];
    
    LERROR(fmt::format("width   '{}'", width));
    LERROR(fmt::format("height   '{}'", height));


    FramebufferRenderer renderer;
    renderer.setResolution(glm::ivec2(width, height));
    renderer.setNAaSamples(1);
    renderer.initialize();
    //renderer.render(OsEng.renderEngine().scene(), _orthoCamera, 1.0, false);
    renderer.render(global::renderEngine.scene(), global::navigationHandler.camera(), 1.0);



    /***************************************/
    // ***********************************************
    /*
    GLenum status;  
    status = glCheckFramebufferStatus(GL_FRAMEBUFFER);        
    if (status != GL_FRAMEBUFFER_COMPLETE) 
        return false;
    */
    



    //Write to file
    //else {

        //GLenum err = glGetError();

        //std::vector<GLubyte> pixels;
        //pixels.resize(4 * width * height * sizeof(GLubyte));
        //
        ////GLubyte* pixels = (GLubyte*)malloc(4 * width * height * sizeof(GLubyte));
//
//        //glReadPixels(0, 0, width, height, GL_DEPTH_COMPONENT, GL_FLOAT, pixels.data());
//        //LERROR(fmt::format("pixelsdata    '{}'", pixels.data()));
        //LERROR(fmt::format("pixelssize    '{}'", pixels.size()));


        //float* fdata = reinterpret_cast<float*>(pixels.data());
        //LERROR(fmt::format("fData  size   '{}'", length(fdata)));   //sizeof

        //while(fdata) {
        //    LERROR(fmt::format("fData   '{}'", *fdata));  
        //    fdata = fdata+4;
        //}

        //treat the data as floats so we can read the entire pixel value
        //map values
        //float max = fdata[0];
        //float min = fdata[0];
        //int size = width*height;
        //for ( int i = 0; i < size; i++)          
        //{
        //    //LERROR(fmt::format("fdata    '{}'", *fdata));
        //    if (fdata[i] > max) {
        //        max = fdata[i];
        //    }
        //    else if (fdata[i] < min) {
        //        min = fdata[i];
        //    }
//
//        //}
//        //LERROR(fmt::format("max fdata    '{}'", max));
        //LERROR(fmt::format("min fdata    '{}'", min));

        std::vector<float> pixels;

        pixels = renderer.getDepthTexture(global::renderEngine.scene(), _orthoCamera, glm::ivec2(width, height));

        //float max = pixels[0];
        //float min = pixels[0];
        //int size = width*height;
        //for ( int i = 0; i < size; i++)          
        //{
        //    if (pixels[i] > max) {
        //        max = pixels[i];
        //    }
        //    else if (pixels[i] < min) {
        //        min = pixels[i];
        //    }
        //}
        //LERROR(fmt::format("max pixels    '{}'", max));
        //LERROR(fmt::format("min pixels    '{}'", min));

        //, width, height
        //ghoul::io::TextureWriterFreeImage::writeTexture(pixels, width, height);
        
        //free(pixels);
        
        return true;
    //}
}


void RenderableHeightMap::renderOrthoCamera() {

    //Applies subsequent matrix operations to the projection matrix stack.
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity(); //Unit Matrix
    
    glOrtho(0, 1920, 0, 1080, 1, 1000000);

    GLfloat test_value;
    glGetFloatv(GL_PROJECTION_STACK_DEPTH, &test_value);
    //glGetBooleanv(GL_MATRIX_MODE, &test_value);
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
