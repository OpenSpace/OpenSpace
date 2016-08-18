/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2016                                                               *
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

#ifndef __TRIANGLESOUP_H__
#define __TRIANGLESOUP_H__

#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/logging/logmanager.h>

#include <glm/glm.hpp>

#include <vector>

namespace openspace {

/**
	Class to hold vertex data and handling OpenGL interfacing and rendering. A Geometry
	has all data needed such as position buffer and normal buffer but all data is not
	necessarily needed for all purpouses so the Geometry can disable use of normals for
	example.
*/

	// TODO : Possibly render triangle strips in this class instead of triangles since
	// that is faster

class TriangleSoup
{
public:
	enum class Positions { Yes, No };
	enum class TextureCoordinates { Yes, No };
	enum class Normals { Yes, No };

	TriangleSoup(
		std::vector<unsigned int> elements, // At least elements are required
		Positions usePositions = Positions::No,
		TextureCoordinates useTextures = TextureCoordinates::No,
		Normals useNormals = Normals::No);
	~TriangleSoup();

	// Setters
	void setVertexPositions(std::vector<glm::vec4> positions);
	void setVertexTextureCoordinates(std::vector<glm::vec2> textures);
	void setVertexNormals(std::vector<glm::vec3> normals);
	void setElements(std::vector<unsigned int> elements);


	
	void drawUsingActiveProgram();

protected:
	// Determines what attribute data is in use
	bool _useVertexPositions;
	bool _useTextureCoordinates;
	bool _useVertexNormals;

	typedef struct {
	public:
		GLfloat position[4];
		GLfloat texture[2];
		GLfloat normal[3];
	private:
		GLubyte padding[28];  // Pads the struct out to 64 bytes for performance increase
	} Vertex;

	// Vertex data
	std::vector<Vertex> _vertexData;
	std::vector<GLuint> _elementData;
private:

	bool updateDataInGPU();

	// GL handles
	GLuint _vaoID;
	GLuint _vertexBufferID;
	GLuint _elementBufferID;

	bool _gpuDataNeedUpdate;
};

} // namespace openspace

#endif // __TRIANGLESOUP_H__
