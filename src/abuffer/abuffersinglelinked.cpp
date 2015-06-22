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

#include <openspace/abuffer/abuffersinglelinked.h>
#include <openspace/engine/openspaceengine.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/sharedmemory.h>

#include <cstring>
#include <iostream>
#include <fstream>
#include <string>

namespace {
	std::string _loggerCat = "ABufferSingleLinked";

	const unsigned int mask_29 = 536870911;
	const unsigned int mask_id_next = mask_29;
	const unsigned int shift_id_next = 0;
	unsigned int bitextract_u(unsigned int pack, unsigned int mask, unsigned int shift) {
		return (pack >> shift) & (mask >> shift);
	}
}

namespace openspace {

ABufferSingleLinked::ABufferSingleLinked()
	: _anchorPointerTexture(0)
	, _anchorPointerTextureInitializer(0)
	, _atomicCounterBuffer(0)
	, _fragmentBuffer(0)
	, _fragmentTexture(0) 
{}

ABufferSingleLinked::~ABufferSingleLinked() {
	glDeleteTextures(1, &_anchorPointerTexture);
	glDeleteTextures(1, &_fragmentTexture);
	glDeleteBuffers(1, &_anchorPointerTextureInitializer);
	glDeleteBuffers(1, &_atomicCounterBuffer);
	glDeleteBuffers(1, &_anchorPointerTextureInitializer);
}

bool ABufferSingleLinked::initialize() {
	// ============================
	//          BUFFERS
	// ============================
	glGenTextures(1, &_anchorPointerTexture);
    LDEBUG("AnchorPointerTexture ID: " << _anchorPointerTexture);
	glGenBuffers(1, &_anchorPointerTextureInitializer);
    LDEBUG("AnchorPointerTextureInitializer ID: " << _anchorPointerTextureInitializer);
	glGenBuffers(1, &_atomicCounterBuffer);
    LDEBUG("AtomicCounterBuffer ID: " << _atomicCounterBuffer);
	glBindBuffer(GL_ATOMIC_COUNTER_BUFFER, _atomicCounterBuffer);
	glBufferData(GL_ATOMIC_COUNTER_BUFFER, sizeof(GLuint), NULL, GL_DYNAMIC_COPY);
	glGenBuffers(1, &_fragmentBuffer);
    LDEBUG("FragmentBuffer ID: " << _fragmentBuffer);
    glGenTextures(1, &_fragmentTexture);
    LDEBUG("FragmentTexture ID: " << _fragmentTexture);

	reinitialize();

	return initializeABuffer();
}

bool ABufferSingleLinked::reinitializeInternal() {
	glBindTexture(GL_TEXTURE_2D, _anchorPointerTexture);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_R32UI, _width, _height, 0, GL_RED_INTEGER, GL_UNSIGNED_INT, NULL);

	glBindBuffer(GL_PIXEL_UNPACK_BUFFER, _anchorPointerTextureInitializer);
	glBufferData(GL_PIXEL_UNPACK_BUFFER, _totalPixels * sizeof(GLuint), NULL, GL_STATIC_DRAW);

	GLuint*  _data = (GLuint*)glMapBuffer(GL_PIXEL_UNPACK_BUFFER, GL_WRITE_ONLY);
	memset(_data, 0x00, _totalPixels * sizeof(GLuint));
	glUnmapBuffer(GL_PIXEL_UNPACK_BUFFER);
	glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);

	glBindBuffer(GL_TEXTURE_BUFFER, _fragmentBuffer);
	glBufferData(GL_TEXTURE_BUFFER, MAX_LAYERS*_totalPixels*sizeof(GLuint) * 4, NULL, GL_DYNAMIC_COPY);

	glBindTexture(GL_TEXTURE_BUFFER, _fragmentTexture);
	glTexBuffer(GL_TEXTURE_BUFFER, GL_RGBA32UI, _fragmentBuffer);
	glBindTexture(GL_TEXTURE_BUFFER, 0);

	glBindImageTexture(1, _fragmentTexture, 0, GL_FALSE, 0, GL_WRITE_ONLY, GL_RGBA32UI);

	return true;
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

std::vector<ABufferSingleLinked::fragmentData> ABufferSingleLinked::pixelData() {
	unsigned int* anchorTexture = new unsigned int[_totalPixels];
	unsigned int* fragmentBuffer = new unsigned int[_totalPixels*MAX_LAYERS*4];

	glBindTexture(GL_TEXTURE_2D, _anchorPointerTexture);
	glGetTexImage(GL_TEXTURE_2D, 0, GL_RED_INTEGER, GL_UNSIGNED_INT, anchorTexture);

	glBindBuffer(GL_TEXTURE_BUFFER, _fragmentBuffer);
	glGetBufferSubData(GL_TEXTURE_BUFFER, 0, _totalPixels*MAX_LAYERS * 4, fragmentBuffer);

	std::vector<ABufferSingleLinked::fragmentData> d;

	// iterate over every pixel
	for (size_t x = 0; x < _width; ++x){
		const float fx = static_cast<float>(x) / _width;
		for (size_t y = 0; y < _height; ++y){
			const float fy = static_cast<float>(y) / _height;
			unsigned int current = anchorTexture[y*_width+x];
			
			// loop until last in list
			while (current != 0) {
				//LDEBUG("(" << x << ", " << y << "): " << current);

				// RGBA
				current *= 4;

				glm::uvec4 fragment;
				for (size_t j = 0; j < 4; ++j) {
					fragment[j] = fragmentBuffer[current + j];
				}

				float z = glm::uintBitsToFloat(fragment[0]);
				unsigned int next = bitextract_u(fragment[1], mask_id_next, shift_id_next);
				glm::vec4 color(glm::unpackUnorm2x16(fragment[2]), glm::unpackUnorm2x16(fragment[3]));
				//LDEBUG("(" << x << ", " << y << "): " << z);

				fragmentData fd;
				fd._position[0] = fx;
				fd._position[1] = fy;
				fd._position[2] = z;
				fd._color[0] = color[0];
				fd._color[1] = color[1];
				fd._color[2] = color[2];
				fd._color[3] = color[3];
				d.emplace_back(fd);

				if (next >= _totalPixels*MAX_LAYERS * 4) {
					LWARNING("Getting index out of bounds, ignoring");
					//LDEBUG("(" << x << ", " << y << "): fragment[1]: " << fragment[1]);
					//LDEBUG("(" << x << ", " << y << "): next: " << next);
					current = 0;
				}
				else {
					current = next;
				}
			}
			
		}
	}

	delete[] anchorTexture;
	delete[] fragmentBuffer;
	
	return d;
}

} // openspace
