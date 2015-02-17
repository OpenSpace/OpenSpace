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

#ifndef __RENDERABLESPHERICALGRID_H__
#define __RENDERABLESPHERICALGRID_H__

// more or less a brutal adaptation of powerscaledsphere class

// open space includes
#include <openspace/rendering/renderable.h>
#include <openspace/properties/stringproperty.h>

// ghoul includes
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>

namespace openspace {
class RenderableSphericalGrid : public Renderable{
public:
	RenderableSphericalGrid(const ghoul::Dictionary& dictionary);
	~RenderableSphericalGrid();

	bool initialize()   override;
	bool deinitialize() override;

	bool isReady() const override;

	void render(const RenderData& data) override;
	void update(const UpdateData& data) override;
private:
protected:
	typedef struct {
		GLfloat location[4];
		GLfloat tex[2];
		GLfloat normal[3];
		GLubyte padding[28];  // Pads the struct out to 64 bytes for performance increase
	} Vertex;


	ghoul::opengl::ProgramObject* _gridProgram;
	std::string _gridType;
	glm::vec4 _gridColor;
	glm::mat4 _gridMatrix;
	int _segments;

	GLuint _vaoID = 3;
	GLuint _vBufferID = 4;
	GLuint _iBufferID = 5;

	GLenum _mode;
	unsigned int _isize;
	unsigned int _vsize;
	Vertex* _varray;
	int* _iarray;
};
}// namespace openspace
#endif