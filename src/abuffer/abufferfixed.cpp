/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2015                                                               *
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

#include <openspace/abuffer/abufferfixed.h>
#include <openspace/engine/openspaceengine.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>

#include <cstring>
#include <iostream>
#include <fstream>
#include <string>

namespace {
	std::string _loggerCat = "ABufferFixed";
}

namespace openspace {

ABufferFixed::ABufferFixed(): _data(0), _anchorPointerTexture(0), 
	_anchorPointerTextureInitializer(0), _atomicCounterBuffer(0), _fragmentBuffer(0), 
	_fragmentTexture(0) 
{}

ABufferFixed::~ABufferFixed() {
	if(_data != 0)
		delete _data;

	glDeleteTextures(1,&_anchorPointerTexture);
	glDeleteTextures(1,&_fragmentTexture);
	// glDeleteTextures(1,&_atomicCounterTexture);
	glDeleteBuffers(1,&_anchorPointerTextureInitializer);
	// glDeleteBuffers(1,&_atomicCounterBuffer);
	//glDeleteBuffers(1,&_anchorPointerTextureInitializer);
}

bool ABufferFixed::initialize() {
	// ============================
	//          BUFFERS
	// ============================
	glGenTextures(1, &_anchorPointerTexture);
	glGenBuffers(1, &_anchorPointerTextureInitializer);
	glGenBuffers(1, &_fragmentBuffer);
	glGenTextures(1, &_fragmentTexture);

	reinitialize();

	return initializeABuffer();
}

bool ABufferFixed::reinitializeInternal() {
	glBindTexture(GL_TEXTURE_2D, _anchorPointerTexture);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_R32UI, _width, _height, 0, GL_RED_INTEGER, GL_UNSIGNED_INT, NULL);

	glBindBuffer(GL_PIXEL_UNPACK_BUFFER, _anchorPointerTextureInitializer);
	glBufferData(GL_PIXEL_UNPACK_BUFFER, _totalPixels * sizeof(GLuint), NULL, GL_STATIC_DRAW);

	_data = (GLuint*)glMapBuffer(GL_PIXEL_UNPACK_BUFFER, GL_WRITE_ONLY);
	memset(_data, 0x00, _totalPixels * sizeof(GLuint));
	glUnmapBuffer(GL_PIXEL_UNPACK_BUFFER);
	glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);


	// glGenBuffers(1, &_atomicCounterBuffer);
	// glBindBuffer(GL_TEXTURE_BUFFER, _atomicCounterBuffer);
	// glBufferData(GL_TEXTURE_BUFFER, _totalPixels*sizeof(GLuint), NULL, GL_DYNAMIC_COPY);

	// glGenTextures(1, &_atomicCounterTexture);
	// glBindTexture(GL_TEXTURE_2D, _atomicCounterTexture);
	//    glTexBuffer(GL_TEXTURE_BUFFER, GL_R32UI, _atomicCounterBuffer);
	//    glBindTexture(GL_TEXTURE_BUFFER, 0);


	glBindBuffer(GL_TEXTURE_BUFFER, _fragmentBuffer);
	glBufferData(GL_TEXTURE_BUFFER, MAX_LAYERS*_totalPixels*sizeof(GLuint) * 4, NULL, GL_DYNAMIC_COPY);

	glBindTexture(GL_TEXTURE_BUFFER, _fragmentTexture);
	glTexBuffer(GL_TEXTURE_BUFFER, GL_RGBA32UI, _fragmentBuffer);
	glBindTexture(GL_TEXTURE_BUFFER, 0);

	glBindImageTexture(1, _fragmentTexture, 0, GL_FALSE, 0, GL_WRITE_ONLY, GL_RGBA32UI);
	return false;
}

void ABufferFixed::clear() {

	// Bind texture initializer
	glBindBuffer(GL_PIXEL_UNPACK_BUFFER, _anchorPointerTextureInitializer);

	// clear _anchorPointerTexture
	glBindTexture(GL_TEXTURE_2D, _anchorPointerTexture);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_R32UI, _width, _height, 0, GL_RED_INTEGER, GL_UNSIGNED_INT, NULL);
	
	// // clear _atomicCounterTexture
	// glBindTexture(GL_TEXTURE_2D, _atomicCounterTexture);
	// glTexImage2D(GL_TEXTURE_2D, 0, GL_R32UI, _width, _height, 0, GL_RED_INTEGER, GL_UNSIGNED_INT, NULL);

	// reset GL_PIXEL_UNPACK_BUFFER
	glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);

}

void ABufferFixed::preRender() {

	// Bind head-pointer image for read-write
    glBindImageTexture(0, _anchorPointerTexture, 0, GL_FALSE, 0, GL_READ_WRITE, GL_R32UI);
    glBindImageTexture(1, _fragmentTexture, 0, GL_FALSE, 0, GL_READ_WRITE, GL_RGBA32UI);
    // glBindImageTexture(2, _atomicCounterTexture, 0, GL_FALSE, 0, GL_READ_WRITE, GL_R32UI);
}

void ABufferFixed::postRender() {

}

std::vector<ABuffer::fragmentData> ABufferFixed::pixelData() {
	LWARNING("pixelData() not working properly for ABufferFixed");
	std::vector<ABuffer::fragmentData> d;

	unsigned int* anchorTexture = new unsigned int[_totalPixels];
	unsigned int* fragmentBuffer = new unsigned int[_totalPixels*MAX_LAYERS * 4];

	glBindTexture(GL_TEXTURE_2D, _anchorPointerTexture);
	glGetTexImage(GL_TEXTURE_2D, 0, GL_RED_INTEGER, GL_UNSIGNED_INT, anchorTexture);

	glBindBuffer(GL_TEXTURE_BUFFER, _fragmentBuffer);
	glGetBufferSubData(GL_TEXTURE_BUFFER, 0, _totalPixels*MAX_LAYERS * 4, fragmentBuffer);

	// iterate over every pixel
	for (size_t x = 0; x < _width; ++x){
		const float fx = static_cast<float>(x) / _width;
		for (size_t y = 0; y < _height; ++y){
			const float fy = static_cast<float>(y) / _height;
			unsigned int fragments = anchorTexture[y*_width + x];

			// loop until last in list
			for (size_t current = 0; current < fragments; ++current) {
				//LDEBUG("(" << x << ", " << y << "): " << current);
				size_t index = (y * _width + x)*MAX_LAYERS + current;
				// RGBA
				index *= 4;

				glm::uvec4 fragment;
				for (size_t j = 0; j < 4; ++j) {
					fragment[j] = fragmentBuffer[index + j];
				}

				float z = glm::uintBitsToFloat(fragment[0]);
				glm::vec4 color(glm::unpackUnorm2x16(fragment[2]), glm::unpackUnorm2x16(fragment[3]));

				fragmentData fd;
				fd._position[0] = fx;
				fd._position[1] = fy;
				fd._position[2] = z;
				fd._color[0] = color[0];
				fd._color[1] = color[1];
				fd._color[2] = color[2];
				fd._color[3] = color[3];
				d.emplace_back(fd);
			}

		}
	}

	delete[] anchorTexture;
	delete[] fragmentBuffer;
	return d;
}

} // openspace
