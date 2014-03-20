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

#include <rendering/volumeraycaster.h>

#include <glm/glm.hpp>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/rawvolumereader.h>

#include <iostream>
#include <cmath>
#include <cstdio>

namespace openspace {

VolumeRaycaster::VolumeRaycaster() : _stepSize(0.01f) , _type(TWOPASS) {
	initialize();
}

VolumeRaycaster::~VolumeRaycaster() {}

// Initializes the data and setups the correct type of ray caster
void VolumeRaycaster::initialize() {
//	------ VOLUME READING ----------------
	std::string filename = absPath("${BASE_PATH}/openspace-data/skull.raw");

	ghoul::RawVolumeReader::ReadHints hints; // TODO: Read hints from .dat file
	hints._dimensions = glm::ivec3(256, 256, 256);
	hints._format = Texture::Format::Red;
	hints._internalFormat = GL_R8;
	ghoul::RawVolumeReader rawReader(hints);
	_volume = rawReader.read(filename);

//	------ SETUP RAYCASTER ---------------
	if (_type == SINGLEPASS) 	setupSinglepassRaycaster();
	if (_type == TWOPASS) 		setupTwopassRaycaster();
}

// Calculate MVP and use it to render with the chosen raycaster
void VolumeRaycaster::render() {
	float speed = 50.0f;
	float time = sgct::Engine::getTime();
	glm::mat4 yRotation = glm::rotate(glm::mat4(1.0f), time*speed, glm::vec3(0.0f, 1.0f, 0.0f));
	glm::mat4 MVP = sgct::Engine::instance()->getActiveModelViewProjectionMatrix()*yRotation;

	if (_type == SINGLEPASS) 	renderWithSinglepassRaycaster(MVP);
	if (_type == TWOPASS) 		renderWithTwopassRaycaster(MVP);
}

// Initialize the two pass raycaster by specifying the bounding box,
// full screen quad, FBO and the constant uniforms needed.
void VolumeRaycaster::setupTwopassRaycaster() {
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
	_fboProgram = new ProgramObject("RaycastProgram");
	ShaderObject* vertexShader = new ShaderObject(ShaderObject::ShaderTypeVertex,
			absPath("${BASE_PATH}/shaders/exitpoints.vert"));
	ShaderObject* fragmentShader = new ShaderObject(ShaderObject::ShaderTypeFragment,
			absPath("${BASE_PATH}/shaders/exitpoints.frag"));
	_fboProgram->attachObject(vertexShader);
	_fboProgram->attachObject(fragmentShader);
	_fboProgram->compileShaderObjects();
	_fboProgram->linkProgramObject();

	_twopassProgram = new ProgramObject("TwoPassProgram");
	vertexShader = new ShaderObject(ShaderObject::ShaderTypeVertex,
			absPath("${BASE_PATH}/shaders/twopassraycaster.vert"));
	fragmentShader = new ShaderObject(ShaderObject::ShaderTypeFragment,
			absPath("${BASE_PATH}/shaders/twopassraycaster.frag"));
	_twopassProgram->attachObject(vertexShader);
	_twopassProgram->attachObject(fragmentShader);
	_twopassProgram->compileShaderObjects();
	_twopassProgram->linkProgramObject();
	_twopassProgram->setUniform("texBack", 0);
	_twopassProgram->setUniform("texFront", 1);
	_twopassProgram->setUniform("texVolume", 2);
	_twopassProgram->setUniform("stepSize", _stepSize);

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
}


// Initialize the single pass raycaster by specifying the VBO for the
// bounding box center, calculating the focallength and setting it as a uniform
void VolumeRaycaster::setupSinglepassRaycaster() {
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
}

// First renders a SGCT box to a FBO and then uses it as entry and exit points
// for the second pass which does the actual ray casting.
void VolumeRaycaster::renderWithTwopassRaycaster(glm::mat4 modelViewProjectionMatrix) {
//	------ DRAW TO FBO -------------------
	_fbo->activate();
	_fboProgram->activate();
	_fboProgram->setUniform("modelViewProjection", modelViewProjectionMatrix);

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
	glBindFramebuffer(GL_FRAMEBUFFER,
			sgct::Engine::instance()->getActiveWindowPtr()->getFBOPtr()->getBufferID());
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

// FIXME Get it working
// Uses the cube center VBO with a geometry shader to create a bounding box
// and then does the ray casting in the same pass in the fragment shader.
void VolumeRaycaster::renderWithSinglepassRaycaster(glm::mat4 modelViewProjectionMatrix) {
	glm::mat4 modelViewMatrix = sgct::Engine::instance()->getActiveModelViewMatrix();
	glm::vec3 eyePos = *sgct::Engine::instance()->getUserPtr()->getPosPtr();
	_singlepassProgram->setUniform("modelViewProjection", modelViewProjectionMatrix);
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
