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

#include <modules/globebrowsing/meshes/trianglesoup.h>

namespace {
	const std::string _loggerCat = "TriangleSoup";
}

namespace openspace
{

TriangleSoup::TriangleSoup(std::vector<unsigned int> elements,
	Positions usePositions, TextureCoordinates useTextures, Normals useNormals)
	: _vaoID(0)
	,_vertexBufferID(0)
	,_elementBufferID(0)
	,_useVertexPositions(usePositions == Positions::Yes)
	,_useTextureCoordinates(useTextures == TextureCoordinates::Yes)
	,_useVertexNormals(useNormals == Normals::Yes)
{
	setElements(elements);
}

TriangleSoup::~TriangleSoup() {
	glDeleteBuffers(1, &_vertexBufferID);
	glDeleteBuffers(1, &_elementBufferID);
	glDeleteVertexArrays(1, &_vaoID);
}


void TriangleSoup::setVertexPositions(std::vector<glm::vec4> positions) {
	_useVertexPositions = true;
	_gpuDataNeedUpdate = true;
	_vertexData.resize(positions.size());
	for (size_t i = 0; i < positions.size(); i++)
	{
		_vertexData[i].position[0] = static_cast<GLfloat>(positions[i].x);
		_vertexData[i].position[1] = static_cast<GLfloat>(positions[i].y);
		_vertexData[i].position[2] = static_cast<GLfloat>(positions[i].z);
		_vertexData[i].position[3] = static_cast<GLfloat>(positions[i].w);
	}
}


void TriangleSoup::setVertexTextureCoordinates(std::vector<glm::vec2> textures) {
	_useTextureCoordinates = true;
	_gpuDataNeedUpdate = true;
	_vertexData.resize(textures.size());
	for (size_t i = 0; i < textures.size(); i++)
	{
		_vertexData[i].texture[0] = static_cast<GLfloat>(textures[i].s);
		_vertexData[i].texture[1] = static_cast<GLfloat>(textures[i].t);
	}
}


void TriangleSoup::setVertexNormals(std::vector<glm::vec3> normals) {
	_useVertexNormals = true;
	_gpuDataNeedUpdate = true;
	_vertexData.resize(normals.size());
	for (size_t i = 0; i < normals.size(); i++)
	{
		_vertexData[i].normal[0] = static_cast<GLfloat>(normals[i].x);
		_vertexData[i].normal[1] = static_cast<GLfloat>(normals[i].y);
		_vertexData[i].normal[2] = static_cast<GLfloat>(normals[i].z);
	}
}


void TriangleSoup::setElements(std::vector<unsigned int> elements) {
	_elementData.resize(elements.size());
	_gpuDataNeedUpdate = true;
	for (size_t i = 0; i < elements.size(); i++)
	{
		_elementData[i] = static_cast<GLuint>(elements[i]);
	}
}

bool TriangleSoup::updateDataInGPU() {
	// Create VAO
	if (_vaoID == 0)
		glGenVertexArrays(1, &_vaoID);

	// Create VBOs
	if (_vertexBufferID == 0 && _vertexData.size() > 0) {
		glGenBuffers(1, &_vertexBufferID);
		if (_vertexBufferID == 0) {
			LERROR("Could not create vertex buffer");
			return false;
		}
	}
	if (_elementBufferID == 0 && _elementData.size() > 0) {
		glGenBuffers(1, &_elementBufferID);
		if (_elementBufferID == 0) {
			LERROR("Could not create vertex element buffer");
			return false;
		}
	}


	// First VAO setup
	glBindVertexArray(_vaoID);

	// Vertex buffer
	glBindBuffer(GL_ARRAY_BUFFER, _vertexBufferID);
	glBufferData(
		GL_ARRAY_BUFFER,
		_vertexData.size() * sizeof(Vertex),
		&_vertexData[0],
		GL_STATIC_DRAW);

	// Positions at location 0
	if (_useVertexPositions) {
		glEnableVertexAttribArray(0);
		glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(Vertex),
			reinterpret_cast<const GLvoid*>(offsetof(Vertex, position)));
	}
	// Textures at location 1
	if (_useTextureCoordinates) {
		glEnableVertexAttribArray(1);
		glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, sizeof(Vertex),
			reinterpret_cast<const GLvoid*>(offsetof(Vertex, texture)));
	}
	// Normals at location 2
	if (_useVertexNormals) {
		glEnableVertexAttribArray(2);
		glVertexAttribPointer(2, 3, GL_FLOAT, GL_FALSE, sizeof(Vertex),
			reinterpret_cast<const GLvoid*>(offsetof(Vertex, normal)));
	}

	// Element buffer
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _elementBufferID);
	glBufferData(
		GL_ELEMENT_ARRAY_BUFFER,
		_elementData.size() * sizeof(GLint),
		&_elementData[0],
		GL_STATIC_DRAW);

	glBindVertexArray(0);

    _gpuDataNeedUpdate = false;

	return true;
}

void TriangleSoup::drawUsingActiveProgram() {
	if (_gpuDataNeedUpdate) {
		updateDataInGPU();
	}
	glBindVertexArray(_vaoID);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _elementBufferID);
	glDrawElements(GL_TRIANGLES, _elementData.size(), GL_UNSIGNED_INT, 0);
	glBindVertexArray(0);
}

} // namespace openspace