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

#include <openspace/rendering/volumeraycastergl.h>
#include <openspace/engine/openspaceengine.h>

#include <glm/glm.hpp>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/rawvolumereader.h>

#include <iostream>
#include <cmath>
#include <cstdio>

namespace {
    std::string _loggerCat = "VolumeRaycasterGL";
}

namespace openspace {

VolumeRaycasterGL::VolumeRaycasterGL(const ghoul::Dictionary& dictionary):
    _fbo(nullptr), _backTexture(nullptr), _frontTexture(nullptr), _volume(nullptr),
    _fboProgram(nullptr), _twopassProgram(nullptr), _boundingBox(nullptr), _screenQuad(0) {
    
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
    _vshaderpath = "";
    _fshaderpath = "";
    
    if (dictionary.hasKey("VertexShader")) {
        dictionary.getValue("VertexShader", _vshaderpath);
    }
    if (dictionary.hasKey("FragmentShader")) {
        dictionary.getValue("FragmentShader", _fshaderpath);
    }
        
    _twopassProgram = new ProgramObject("TwoPassProgram");
    ShaderObject* vertexShader = new ShaderObject(ShaderObject::ShaderTypeVertex,_vshaderpath);
    ShaderObject* fragmentShader = new ShaderObject(ShaderObject::ShaderTypeFragment,_fshaderpath);
    _twopassProgram->attachObject(vertexShader);
    _twopassProgram->attachObject(fragmentShader);

}

VolumeRaycasterGL::~VolumeRaycasterGL() {
    if(_fbo)
        delete _fbo;
    if(_backTexture)
        delete _backTexture;
    if(_frontTexture)
        delete _frontTexture;
    if(_volume)
        delete _volume;
    if(_boundingBox)
        delete _boundingBox;
}

bool VolumeRaycasterGL::initialize() {
    assert(_filename != "");
//	------ VOLUME READING ----------------
	ghoul::RawVolumeReader rawReader(_hints);
	_volume = rawReader.read(_filename);

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
    
    auto privateCallback = [this](const ghoul::filesystem::File& file) {
        _safeShaderCompilation();
    };
    _vertexSourceFile = new ghoul::filesystem::File(_vshaderpath, false);
    _fragmentSourceFile = new ghoul::filesystem::File(_fshaderpath, false);
    _vertexSourceFile->setCallback(privateCallback);
    _fragmentSourceFile->setCallback(privateCallback);

    _twopassProgram->compileShaderObjects();
    _twopassProgram->linkProgramObject();
    _twopassProgram->setUniform("texBack", 0);
    _twopassProgram->setUniform("texFront", 1);
    _twopassProgram->setUniform("texVolume", 2);
	//OsEng.ref().configurationManager().getValue("TwoPassProgram", _twopassProgram);
    
    //	------ SETUP FBO ---------------------
	_fbo = new FramebufferObject();
	_fbo->activate();
    
	int x = sgct::Engine::instance()->getActiveXResolution();
	int y = sgct::Engine::instance()->getActiveYResolution();
	_backTexture = new Texture(glm::size3_t(x,y,1));
	_frontTexture = new Texture(glm::size3_t(x,y,1));
	_backTexture->uploadTexture();
	_frontTexture->uploadTexture();
	_fbo->attachTexture(_backTexture, GL_COLOR_ATTACHMENT0);
	_fbo->attachTexture(_frontTexture, GL_COLOR_ATTACHMENT1);
    
	_fbo->deactivate();

    return true;
}

void VolumeRaycasterGL::render(const glm::mat4& modelViewProjection) {
    _stepSize = 0.01f;
	
    //	------ DRAW TO FBO -------------------
	GLuint sgctFBO = FramebufferObject::getActiveObject(); // Save SGCTs main FBO
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
}

void VolumeRaycasterGL::_safeShaderCompilation() {
    _shaderMutex.lock();
    _twopassProgram->rebuildFromFile();
    _twopassProgram->compileShaderObjects();
    _twopassProgram->linkProgramObject();
    _twopassProgram->setUniform("texBack", 0);
    _twopassProgram->setUniform("texFront", 1);
    _twopassProgram->setUniform("texVolume", 2);
    _shaderMutex.unlock();
}

}// namespace openspace
