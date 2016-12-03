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

#include <modules/base/rendering/screenspaceframebuffer.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/camera.h>

#include <openspace/rendering/renderer.h>
#include <openspace/rendering/abufferrenderer.h>
#include <openspace/rendering/framebufferrenderer.h>
#include <openspace/engine/wrapper/windowwrapper.h>

namespace openspace {

ScreenSpaceFramebuffer::ScreenSpaceFramebuffer(const ghoul::Dictionary& dictionary) 
    : ScreenSpaceRenderable(dictionary)
    , _size("size", "Size", glm::vec4(0), glm::vec4(0), glm::vec4(2000))
    , _framebuffer(nullptr)
{
    _id = id();
    setName("ScreenSpaceFramebuffer" + std::to_string(_id));

    glm::vec2 resolution = OsEng.windowWrapper().currentWindowResolution();
    addProperty(_size);
    _size.set(glm::vec4(0, 0, resolution.x,resolution.y));

    _scale.setValue(1.0f);
}

ScreenSpaceFramebuffer::~ScreenSpaceFramebuffer(){}

bool ScreenSpaceFramebuffer::initialize(){
    _originalViewportSize = OsEng.windowWrapper().currentWindowResolution();

    createPlane();
    createShaders();
    createFragmentbuffer();

    return isReady();
}

bool ScreenSpaceFramebuffer::deinitialize(){
    glDeleteVertexArrays(1, &_quad);
    _quad = 0;

    glDeleteBuffers(1, &_vertexPositionBuffer);
    _vertexPositionBuffer = 0;

    _texture = nullptr;

     RenderEngine& renderEngine = OsEng.renderEngine();
    if (_shader) {
        renderEngine.removeRenderProgram(_shader);
        _shader = nullptr;
    }

    _framebuffer->detachAll();
    removeAllRenderFunctions();

    return true;
}

void ScreenSpaceFramebuffer::render(){
    glm::vec2 resolution = OsEng.windowWrapper().currentWindowResolution();
    glm::vec4 size = _size.value();

    float xratio = _originalViewportSize.x / (size.z-size.x);
    float yratio = _originalViewportSize.y / (size.w-size.y);;

    if(!_renderFunctions.empty()){
        glViewport (-size.x*xratio, -size.y*yratio, _originalViewportSize.x*xratio, _originalViewportSize.y*yratio);
        GLint defaultFBO = _framebuffer->getActiveObject();
        _framebuffer->activate();
        
        glClearColor (0.0f, 0.0f, 0.0f, 0.0f);
        glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        glEnable(GL_ALPHA_TEST);
        for(auto renderFunction : _renderFunctions){
            (*renderFunction)();
        }
        _framebuffer->deactivate();

        glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);
        glViewport (0, 0, resolution.x, resolution.y);
        
        glm::mat4 rotation = rotationMatrix();
        glm::mat4 translation = translationMatrix();
        glm::mat4 scale = scaleMatrix();
        scale = glm::scale(scale, glm::vec3((1.0/xratio), -(1.0/yratio), 1.0f));
        glm::mat4 modelTransform = rotation*translation*scale;
        draw(modelTransform);
    }
}


void ScreenSpaceFramebuffer::update(){}

bool ScreenSpaceFramebuffer::isReady() const{
    bool ready = true;
    if (!_shader)
        ready &= false;
    if(!_texture)
        ready &= false;
    return ready;
}


void ScreenSpaceFramebuffer::setSize(glm::vec4 size){
    _size.set(size);
}

void ScreenSpaceFramebuffer::addRenderFunction(std::shared_ptr<std::function<void()>> renderFunction){
    _renderFunctions.push_back(renderFunction);
}

void ScreenSpaceFramebuffer::removeAllRenderFunctions(){
    _renderFunctions.clear();
}

void ScreenSpaceFramebuffer::createFragmentbuffer(){
    _framebuffer = std::make_unique<ghoul::opengl::FramebufferObject>();
    _framebuffer->activate();
    _texture = std::make_unique<ghoul::opengl::Texture>(glm::uvec3(_originalViewportSize.x, _originalViewportSize.y, 1));
    _texture->uploadTexture();
    _texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
    _framebuffer->attachTexture(_texture.get(), GL_COLOR_ATTACHMENT0);
    _framebuffer->deactivate();
}

int ScreenSpaceFramebuffer::id(){
    static int id = 0;
    return id++;
}
} //namespace openspace