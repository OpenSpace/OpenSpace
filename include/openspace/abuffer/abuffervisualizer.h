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

#ifndef __ABUFFERVISUALIZER_H__
#define __ABUFFERVISUALIZER_H__

#include <openspace/abuffer/abuffer.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/glm.h>

#include <string>
#include <vector>

namespace ghoul {
	namespace opengl {
        class ProgramObject;
	}
}

namespace openspace {

class ABufferVisualizer {
public:

	ABufferVisualizer();
	~ABufferVisualizer();

	void updateData(const std::vector<ABuffer::fragmentData>& data);

	void render();

private:

	void initializeMarkers();

	GLuint _pointcloud;
	GLsizei _pointcloudSize;
	GLuint _markers;
	GLsizei _markersSize;
	GLuint _imarkers;
	GLsizei _imarkersSize;
	ghoul::opengl::ProgramObject* _pointcloudProgram;

}; // ABufferVisualizer
}  // namespace openspace

#endif // __ABUFFER_H__