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

#include <openspace/abuffer/abufferSingleLinked.h>
#include <openspace/engine/openspaceengine.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>

#include <iostream>
#include <fstream>
#include <string>

#define MAX_LAYERS 10
#define BUFFER_OFFSET(i) ((char *)NULL + (i))

namespace {
	std::string _loggerCat = "ABufferSingleLinked";
}

namespace openspace {

ABufferSingleLinked::ABufferSingleLinked(): _data(0), _anchorPointerTexture(0), 
	_anchorPointerTextureInitializer(0), _atomicCounterBuffer(0), _fragmentBuffer(0), 
	_fragmentTexture(0) 
{}

ABufferSingleLinked::~ABufferSingleLinked() {
	if(_data != 0)
		delete _data;

	if(_fragmentShaderFile)
		delete _fragmentShaderFile;

	if(_resolveShader)
		delete _resolveShader;

	glDeleteTextures(1,&_anchorPointerTexture);
	glDeleteTextures(1,&_fragmentTexture);
	glDeleteBuffers(1,&_anchorPointerTextureInitializer);
	glDeleteBuffers(1,&_atomicCounterBuffer);
	glDeleteBuffers(1,&_anchorPointerTextureInitializer);
}

bool ABufferSingleLinked::initialize() {
	// ============================
	//          BUFFERS
	// ============================
	glGenTextures(1, &_anchorPointerTexture);
	glBindTexture(GL_TEXTURE_2D, _anchorPointerTexture);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_R32UI, _width, _height, 0, GL_RED_INTEGER, GL_UNSIGNED_INT, NULL);

	glGenBuffers(1, &_anchorPointerTextureInitializer);
	glBindBuffer(GL_PIXEL_UNPACK_BUFFER, _anchorPointerTextureInitializer);
	glBufferData(GL_PIXEL_UNPACK_BUFFER, _totalPixels * sizeof(GLuint), NULL, GL_STATIC_DRAW);

	_data = (GLuint*)glMapBuffer(GL_PIXEL_UNPACK_BUFFER, GL_WRITE_ONLY);
	memset(_data, 0x00, _totalPixels * sizeof(GLuint));
	glUnmapBuffer(GL_PIXEL_UNPACK_BUFFER);
	glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);

	glGenBuffers(1, &_atomicCounterBuffer);
	glBindBuffer(GL_ATOMIC_COUNTER_BUFFER, _atomicCounterBuffer);
	glBufferData(GL_ATOMIC_COUNTER_BUFFER, sizeof(GLuint), NULL, GL_DYNAMIC_COPY);

	glGenBuffers(1, &_fragmentBuffer);
	glBindBuffer(GL_TEXTURE_BUFFER, _fragmentBuffer);
	glBufferData(GL_TEXTURE_BUFFER, MAX_LAYERS*_totalPixels*sizeof(GLfloat)*4, NULL, GL_DYNAMIC_COPY);

    glGenTextures(1, &_fragmentTexture);
    glBindTexture(GL_TEXTURE_BUFFER, _fragmentTexture);
    glTexBuffer(GL_TEXTURE_BUFFER, GL_RGBA32UI, _fragmentBuffer);
    glBindTexture(GL_TEXTURE_BUFFER, 0);

    glBindImageTexture(1, _fragmentTexture, 0, GL_FALSE, 0, GL_WRITE_ONLY, GL_RGBA32UI);

	// ============================
    // Volume and transferfunction data
    // ============================
	ghoul::opengl::Texture* volume 	= nullptr;
	ghoul::opengl::Texture* tf 		= nullptr;
	OsEng.configurationManager().getValue("firstVolume", volume);
	OsEng.configurationManager().getValue("firstTransferFunction", tf);
	if(volume)
		_volumes.push_back(std::make_pair(std::string("volume1"), volume));
	if(tf)
		_transferFunctions.push_back(std::make_pair(std::string("transferFunction1"), tf));
	_samplers.push_back(R"(
void sampleVolume1(inout vec4 finalColor, vec3 position) {
	float intensity = texture(volume1, position).x;
	vec4 color = texture(transferFunction1, intensity);
	blendStep(finalColor, color, stepSize);
}
)");

	return initializeABuffer();
}

void ABufferSingleLinked::clear() {
	glBindBuffer(GL_PIXEL_UNPACK_BUFFER, _anchorPointerTextureInitializer);
	glBindTexture(GL_TEXTURE_2D, _anchorPointerTexture);

	glTexImage2D(GL_TEXTURE_2D, 0, GL_R32UI, _width, _height, 0, GL_RED_INTEGER, GL_UNSIGNED_INT, NULL);
	glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);

	static const GLuint zero = 1;
	glBindBufferBase(GL_ATOMIC_COUNTER_BUFFER, 0, _atomicCounterBuffer);
	glBufferSubData(GL_ATOMIC_COUNTER_BUFFER, 0, sizeof(zero), &zero);
	glBindBufferBase(GL_ATOMIC_COUNTER_BUFFER, 0, 0);
}

void ABufferSingleLinked::preRender() {

	// Bind head-pointer image for read-write
	glBindBufferBase(GL_ATOMIC_COUNTER_BUFFER, 0, _atomicCounterBuffer);
    glBindImageTexture(0, _anchorPointerTexture, 0, GL_FALSE, 0, GL_READ_WRITE, GL_R32UI);
    glBindImageTexture(1, _fragmentTexture, 0, GL_FALSE, 0, GL_READ_WRITE, GL_RGBA32UI);
}

void ABufferSingleLinked::postRender() {

}

std::string ABufferSingleLinked::settings() {
	return R"(#define SINGLE_LINKED)";
}


} // openspace