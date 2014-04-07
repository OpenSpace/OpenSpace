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

#ifndef VOLUMERAYCASTER_H
#define VOLUMERAYCASTER_H

#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/framebufferobject.h>
#include <ghoul/opengl/texture.h>

#include <sgct.h>

namespace openspace {
using namespace ghoul::opengl;

class VolumeRaycaster {
public:
	VolumeRaycaster();
	~VolumeRaycaster();
	void initialize();
	void render();

private:
	void setupTwopassRaycaster();
	void setupSinglepassRaycaster();

	void renderWithTwopassRaycaster(glm::mat4 modelViewProjectionMatrix);
	void renderWithSinglepassRaycaster(glm::mat4 modelViewProjectionMatrix);

	FramebufferObject* _fbo;
	Texture* _backTexture;
	Texture* _frontTexture;
	Texture* _volume;
	ProgramObject *_fboProgram, *_twopassProgram, *_singlepassProgram;
	sgct_utils::SGCTBox* _boundingBox;
	GLuint _screenQuad, _cubeCenterVBO;
};

} // namespace openspace

#endif // VOLUMERAYCASTER_H
