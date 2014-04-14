/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014                                                                    *
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

#include <openspace/rendering/volumeraycastercl.h>
#include <openspace/engine/openspaceengine.h>

#include <glm/glm.hpp>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opencl/clworksize.h>
#include <ghoul/logging/logmanager.h>

#include <iostream>
#include <cmath>
#include <cstdio>

namespace {
    std::string _loggerCat = "VolumeRaycasterCL";
}

namespace openspace {

VolumeRaycasterCL::VolumeRaycasterCL(const ghoul::Dictionary& dictionary):
    _fbo(nullptr), _backTexture(nullptr), _frontTexture(nullptr), _volume(nullptr),
    _fboProgram(nullptr), _boundingBox(nullptr), _screenQuad(0),
    _kernelPath(""), _kernelSourceFile(nullptr), _kernelUpdateOnSave(false) {
        
    if (dictionary.hasKey("Filepath")) {
        std::string filename;
        if(dictionary.getValue("Filepath", filename)) {
            if (FileSys.ref().fileExists(absPath(filename))) {
                _filename = absPath(filename);
            }
        }
    }
    
    if (dictionary.hasKey("Hints")) {
        if(dictionary.getValue("Hints", _hints)) {
        }
    }
    
    if (dictionary.hasKey("Kernel")) {
        std::string filename;
        if(dictionary.getValue("Kernel", filename)) {
            if (FileSys.ref().fileExists(absPath(filename))) {
                _kernelPath = absPath(filename);
            }
        }
    }
    
    if (dictionary.hasKey("KernelUpdateOnSave")) {
        if(dictionary.getValue("KernelUpdateOnSave", _kernelUpdateOnSave)) {
        }
    }
    
    
}

VolumeRaycasterCL::~VolumeRaycasterCL() {
}

bool VolumeRaycasterCL::initialize() {
    assert(_filename != "");
    //	------ VOLUME READING ----------------
	ghoul::RawVolumeReader rawReader(_hints);
	_volume = rawReader.read(_filename);
    glm::size3_t d = _volume->dimensions();
    
    //	------ SETUP GEOMETRY ----------------
	const GLfloat size = 1.0f;
	const GLfloat vertex_texcoord_data[] = { // square of two triangles (sigh)
        //	  x      y     z     s     t
        -size, -size, 0.0f, 0.0f, 0.0f,
        size,	size, 0.0f, 1.0f, 1.0f,
        -size,  size, 0.0f, 0.0f, 1.0f,
        -size, -size, 0.0f, 0.0f, 0.0f,
        size, -size, 0.0f, 1.0f, 0.0f,
        size,	size, 0.0f, 1.0f, 1.0f
    };
    
	GLuint vertexPositionBuffer;
	glGenVertexArrays(1, &_screenQuad); // generate array
	glBindVertexArray(_screenQuad); // bind array
	glGenBuffers(1, &vertexPositionBuffer); // generate buffer
	glBindBuffer(GL_ARRAY_BUFFER, vertexPositionBuffer); // bind buffer
	glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_texcoord_data), vertex_texcoord_data, GL_STATIC_DRAW);
    
	// Vertex positions
	GLuint vertexLocation = 2;
	glEnableVertexAttribArray(vertexLocation);
	glVertexAttribPointer(vertexLocation, 3, GL_FLOAT, GL_FALSE, 5*sizeof(GLfloat), reinterpret_cast<void*>(0));
    
	// Texture coordinates
	GLuint texcoordLocation = 0;
	glEnableVertexAttribArray(texcoordLocation);
	glVertexAttribPointer(texcoordLocation, 2, GL_FLOAT, GL_FALSE, 5*sizeof(GLfloat), (void*)(3*sizeof(GLfloat)));
    
	glBindBuffer(GL_ARRAY_BUFFER, 0); //unbind buffer
	glBindVertexArray(0); //unbind array
    
	_boundingBox = new sgct_utils::SGCTBox(1.0f, sgct_utils::SGCTBox::Regular);
    
    //	------ SETUP SHADERS -----------------
    // TODO error control or better design pattern
	OsEng.ref().configurationManager().getValue("RaycastProgram", _fboProgram);
	OsEng.ref().configurationManager().getValue("Quad", _quadProgram);
    
    //	------ SETUP FBO ---------------------
	_fbo = new ghoul::opengl::FramebufferObject();
	_fbo->activate();
    
	int x = sgct::Engine::instance()->getActiveXResolution();
	int y = sgct::Engine::instance()->getActiveYResolution();
	_backTexture = new ghoul::opengl::Texture(glm::size3_t(x,y,1));
	_frontTexture = new ghoul::opengl::Texture(glm::size3_t(x,y,1));
	_output = new ghoul::opengl::Texture(glm::size3_t(x,y,1));
	_backTexture->uploadTexture();
	_frontTexture->uploadTexture();
	_output->uploadTexture();
	_fbo->attachTexture(_backTexture, GL_COLOR_ATTACHMENT0);
	_fbo->attachTexture(_frontTexture, GL_COLOR_ATTACHMENT1);
    
	_fbo->deactivate();
    
    _context = OsEng.clContext();
    _commands = _context.createCommandQueue();
    
    _clBackTexture = _context.createTextureFromGLTexture(CL_MEM_READ_ONLY, *_backTexture);
    _clFrontTexture = _context.createTextureFromGLTexture(CL_MEM_READ_ONLY, *_frontTexture);
    _clVolume = _context.createTextureFromGLTexture(CL_MEM_READ_ONLY, *_volume);
    _clOutput = _context.createTextureFromGLTexture(CL_MEM_WRITE_ONLY, *_output);
    
    auto privateCallback = [this](const ghoul::filesystem::File& file) {
        _safeKernelCompilation();
    };
    _kernelSourceFile = new ghoul::filesystem::File(_kernelPath, false);
    if(_kernelUpdateOnSave)
        _kernelSourceFile->setCallback(privateCallback);
    privateCallback(*_kernelSourceFile);
    
    return true;
}

void VolumeRaycasterCL::_safeKernelCompilation() {
    if(_context.isValidContext()) {
    
        ghoul::opencl::CLProgram tmpProgram = _context.createProgram(_kernelSourceFile->path());
        tmpProgram.setOption(ghoul::opencl::CLProgram::Option::OptDisable, true);
        if(tmpProgram.build()) {
            ghoul::opencl::CLKernel tmpKernel = tmpProgram.createKernel("volumeraycaster");
            if(tmpKernel.isValidKernel()) {
                tmpKernel.setArgument(0, &_clFrontTexture);
                tmpKernel.setArgument(1, &_clBackTexture);
                tmpKernel.setArgument(2, &_clVolume);
                tmpKernel.setArgument(3, &_clOutput);
                
                // do the actual assignment behind locked doors
                _kernelMutex.lock();
                _program = tmpProgram;
                _kernel = tmpKernel;
                _kernelMutex.unlock();
                
                LDEBUG("Done updating kernel");
            }
        }
    }
}

void VolumeRaycasterCL::render(const glm::mat4& modelViewProjection) {
    
    
    if(_kernel.isValidKernel()) {
        _stepSize = 0.01f;
        
        //	------ DRAW TO FBO -------------------
        GLuint sgctFBO = ghoul::opengl::FramebufferObject::getActiveObject(); // Save SGCTs main FBO
        _fbo->activate();
        _fboProgram->activate();
        _fboProgram->setUniform("modelViewProjection", modelViewProjection);
        
        //	Draw backface
        glDrawBuffer(GL_COLOR_ATTACHMENT0);
        glClearColor(0.2f, 0.2f, 0.2f, 0);
        glClear(GL_COLOR_BUFFER_BIT);
        glEnable(GL_CULL_FACE);
        glCullFace(GL_FRONT);
        _boundingBox->draw();
        glDisable(GL_CULL_FACE);
        
        //	Draw frontface
        glDrawBuffer(GL_COLOR_ATTACHMENT1);
        glClear(GL_COLOR_BUFFER_BIT);
        glClearColor(0.2f, 0.2f, 0.2f, 0);
        glEnable(GL_CULL_FACE);
        glCullFace(GL_BACK);
        _boundingBox->draw();
        glDisable(GL_CULL_FACE);
        
        _fboProgram->deactivate();
        _fbo->deactivate();
        
        //	------ DRAW TO SCREEN ----------------
        glBindFramebuffer(GL_FRAMEBUFFER, sgctFBO); // Re-bind SGCTs main FBO
        
        size_t x = sgct::Engine::instance()->getActiveXResolution();
        size_t y = sgct::Engine::instance()->getActiveYResolution();
        size_t local_x = 32;
        size_t local_y = 32;
        while (local_x > 1) {
            if(x % local_x == 0)
                break;
            local_x /= 2;
        }
        while (local_y > 1) {
            if(y % local_y == 0)
                break;
            local_y /= 2;
        }
        ghoul::opencl::CLWorkSize ws({x,y}, {local_x,local_y});
        
        glFinish();
        _commands.enqueueKernelBlocking(_kernel, ws);
        _commands.finish();
        _quadProgram->activate();
        glActiveTexture(GL_TEXTURE0);
        _output->bind();
        glClearColor(0.0f, 0.0f, 0.0f, 0);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        glBindVertexArray(_screenQuad);
        glDrawArrays(GL_TRIANGLES, 0, 6);
        glBindVertexArray(0);
        
        _quadProgram->deactivate();
        
        /*
         _twopassProgram->activate();
         _twopassProgram->setUniform("stepSize", _stepSize);
         
         //	 Set textures
         glActiveTexture(GL_TEXTURE0);
         _backTexture->bind();
         glActiveTexture(GL_TEXTURE1);
         _frontTexture->bind();
         glActiveTexture(GL_TEXTURE2);
         _volume->bind();
         
         //	Draw screenquad
         glClearColor(0.2f, 0.2f, 0.2f, 0);
         glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
         glBindVertexArray(_screenQuad);
         glDrawArrays(GL_TRIANGLES, 0, 6);
         glBindVertexArray(0);
         
         _twopassProgram->deactivate();
         */
    }
    

}

}// namespace openspace
