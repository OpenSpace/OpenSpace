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

/*
#include <openspace/rendering/volumeraycastersinglepass.h>
#include <openspace/engine/openspaceengine.h>

#include <glm/glm.hpp>
#include <ghoul/filesystem/filesystem.h>

#include <iostream>
#include <cmath>
#include <cstdio>

namespace openspace {


VolumeRaycasterSinglePass::VolumeRaycasterSinglePass(const ghoul::Dictionary& dictionary):
    _volume(nullptr), _singlepassProgram(nullptr), _boundingBox(nullptr),
    _cubeCenterVBO(0) {

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
}

VolumeRaycasterSinglePass::~VolumeRaycasterSinglePass() {
    if(_volume)
        delete _volume;
    if(_boundingBox)
        delete _boundingBox;
}

bool VolumeRaycasterSinglePass::initialize() {

    assert(_filename != "");

//	------ VOLUME READING ----------------
	ghoul::RawVolumeReader rawReader(_hints);
	_volume = rawReader.read(_filename);

    float p[] = {0, 0, 0};
	glGenBuffers(1, &_cubeCenterVBO);
	glBindBuffer(GL_ARRAY_BUFFER, _cubeCenterVBO);
	glBufferData(GL_ARRAY_BUFFER, sizeof(p), &p[0], GL_STATIC_DRAW);
    
	glDisable(GL_DEPTH_TEST);
	glEnable(GL_BLEND);
	glEnable(GL_CULL_FACE);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    
	float topPlane, nearPlane;
	topPlane = sgct::Engine::instance()->getActiveWindowPtr()->getCurrentViewport()->getFrustum()->getTop();
	nearPlane = sgct::Engine::instance()->getActiveWindowPtr()->getCurrentViewport()->getFrustum()->getNear();
	float FOV = float(atan(topPlane / nearPlane)) * (180.0 / 3.141592653589793) * 2.0f;
	float focalLength = 1.0f / tan(FOV / 2);
	int x = sgct::Engine::instance()->getActiveXResolution();
	int y = sgct::Engine::instance()->getActiveYResolution();
    
	_singlepassProgram = new ProgramObject("SinglePassProgram");
	ShaderObject* vertexShader = new ShaderObject(ShaderObject::ShaderTypeVertex,
                                                  absPath("${BASE_PATH}/shaders/singlepassraycaster.vert"));
	ShaderObject* fragmentShader = new ShaderObject(ShaderObject::ShaderTypeFragment,
                                                    absPath("${BASE_PATH}/shaders/singlepassraycaster.frag"));
	ShaderObject* geometryShader = new ShaderObject(ShaderObject::ShaderTypeGeometry,
                                                    absPath("${BASE_PATH}/shaders/singlepassraycaster.gs"));
	_singlepassProgram->attachObject(vertexShader);
	_singlepassProgram->attachObject(fragmentShader);
	_singlepassProgram->attachObject(geometryShader);
	_singlepassProgram->compileShaderObjects();
	_singlepassProgram->linkProgramObject();
	_singlepassProgram->setUniform("FocalLength", focalLength);
	_singlepassProgram->setUniform("WindowSize", glm::vec2(x,y));
	_singlepassProgram->setUniform("Density", 2);


    return true;
}

void VolumeRaycasterSinglePass::render(const glm::mat4& modelViewProjection) {

	glm::mat4 modelViewMatrix = sgct::Engine::instance()->getActiveModelViewMatrix();
	glm::vec3 eyePos = *sgct::Engine::instance()->getUserPtr()->getPosPtr();
	_singlepassProgram->setUniform("modelViewProjection", modelViewProjection);
	_singlepassProgram->setUniform("Modelview", modelViewMatrix);
	glm::vec4 rayOrigin = glm::transpose(modelViewMatrix)*glm::vec4(eyePos, 1.0);
	_singlepassProgram->setUniform("RayOrigin", glm::vec3(rayOrigin.x, rayOrigin.y, rayOrigin.z));
    
	_singlepassProgram->activate();
    
	glBindBuffer(GL_ARRAY_BUFFER, _cubeCenterVBO);
	GLuint SlotPosition = 0;
	glEnableVertexAttribArray(SlotPosition);
	glVertexAttribPointer(SlotPosition, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(float), 0);
	glEnableVertexAttribArray(SlotPosition);
	glClearColor(0.2f, 0.2f, 0.2f, 0);
	glClear(GL_COLOR_BUFFER_BIT);
    
	glActiveTexture(GL_TEXTURE2);
	_volume->bind();
    
	glDrawArrays(GL_POINTS, 0, 1);
    
	_singlepassProgram->deactivate();
}

}// namespace openspace
*/