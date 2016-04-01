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

#ifndef __GEOMETRY_H__
#define __GEOMETRY_H__

#include <ghoul/opengl/ghoul_gl.h>
#include <glm/glm.hpp>

#include <vector>

namespace openspace {

/**
	Class to hold vertex data and handling OpenGL interfacing and rendering. A Geometry
	has all data needed such as position buffer and normal buffer but all data is not
	necessarily needed for all purpouses so the Geometry can disable use of normals for
	example.
*/
class Geometry
{
public:
	enum class Positions { Yes, No };
	enum class Textures { Yes, No };
	enum class Normals { Yes, No };

	Geometry(
		std::vector<unsigned int> elements, // At least elements are required
		Positions usePositions,
		Textures useTextures,
		Normals useNormals);
	~Geometry();

	// Setters
	void setPositionData(std::vector<glm::vec4> positions);
	void setTextureData(std::vector<glm::vec2> textures);
	void setNormalData(std::vector<glm::vec3> normals);
	void setElementData(std::vector<unsigned int> elements);

	/**
		Initialize GPU handles. Before calling this function, the data must be set.
	*/
	bool initialize();
	void drawUsingActiveProgram() const;

private:
	typedef struct {
		GLfloat position[4];
		GLfloat texture[2];
		GLfloat normal[3];
		GLubyte padding[28];  // Pads the struct out to 64 bytes for performance increase
	} Vertex;
	// Vertex data
	std::vector<Vertex> _vertexData;
	std::vector<GLuint> _elementData;

	// GL handles
	GLuint _vaoID;
	GLuint _vertexBufferID;
	GLuint _elementBufferID;

	// Determines what attribute data is in use
	const bool _usePositions;
	const bool _useTextures;
	const bool _useNormals;
};

} // namespace openspace

#endif // __GEOMETRY_H__