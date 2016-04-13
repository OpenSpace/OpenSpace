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

#include <modules/globebrowsing/rendering/clipmapgeometry.h>

#include <ghoul/opengl/ghoul_gl.h>

namespace {
	const std::string _loggerCat = "ClipMapGeometry";
}

namespace openspace {

ClipMapGeometry::ClipMapGeometry(
	unsigned int resolution,
	Positions usePositions, 
	TextureCoordinates useTextures, 
	Normals useNormals)
	: Geometry(CreateElements(resolution), usePositions, useTextures, useNormals)
	, _resolution(resolution)
{
	if(_useVertexPositions){
		setVertexPositions(CreatePositions(_resolution));
	}
	if (_useTextureCoordinates) {
		setVertexTextureCoordinates(CreateTextureCoordinates(_resolution));
	}
	if (_useVertexNormals) {
		setVertexNormals(CreateNormals(_resolution));
	}
}

ClipMapGeometry::~ClipMapGeometry()
{

}

const unsigned int ClipMapGeometry::resolution() const {
	return _resolution;
}

size_t ClipMapGeometry::numElements(unsigned int resolution)
{
	return 6 * ((resolution + 1) * (resolution + 1) - (resolution / 4 * resolution / 4));
}

size_t ClipMapGeometry::numVerticesBottom(unsigned int resolution)
{
	return (resolution + 1 + 2) * (resolution / 4 + 1 + 1);
}

size_t ClipMapGeometry::numVerticesLeft(unsigned int resolution)
{
	return (resolution / 4 + 1 + 1) * (resolution / 2 + 1);
}

size_t ClipMapGeometry::numVerticesRight(unsigned int resolution)
{
	return (resolution / 4 + 1 + 1) * (resolution / 2 + 1);
}

size_t ClipMapGeometry::numVerticesTop(unsigned int resolution)
{
	return (resolution + 1 + 2) * (resolution / 4 + 1 + 1);
}

size_t ClipMapGeometry::numVertices(unsigned int resolution)
{
	return	numVerticesBottom(resolution) +
			numVerticesLeft(resolution) +
			numVerticesRight(resolution) +
			numVerticesTop(resolution);
}

void ClipMapGeometry::validate(unsigned int resolution) {
	ghoul_assert(resolution >= 8,
		"Resolution must be at least 8. (" << resolution << ")");
	ghoul_assert(resolution == pow(2, int(log2(resolution))),
		"Resolution must be a power of 2. (" << resolution << ")");
}

std::vector<GLuint> ClipMapGeometry::CreateElements(unsigned int resolution) {
	validate(resolution);
	std::vector<GLuint> elements;
	elements.reserve(numElements(resolution));
	
	// x    v01---v11   x ..
	//       |  /  |
	// x    v00---v10   x ..
	//
	// x	x     x     x ..
	// :    :     :     :

	unsigned int previousVerts[4];
	previousVerts[0] = 0;
	previousVerts[1] = previousVerts[0] + numVerticesBottom(resolution);
	previousVerts[2] = previousVerts[1] + numVerticesLeft(resolution);
	previousVerts[3] = previousVerts[2] + numVerticesRight(resolution);

	// Build the bottom part of the clipmap geometry
	for (unsigned int y = 0; y < resolution / 4 + 1; y++) {
		for (unsigned int x = 0; x < resolution + 2; x++) {
			GLuint v00 = previousVerts[0] + (y + 0) * (resolution + 3) + x + 0;
			GLuint v10 = previousVerts[0] + (y + 0) * (resolution + 3) + x + 1;
			GLuint v01 = previousVerts[0] + (y + 1) * (resolution + 3) + x + 0;
			GLuint v11 = previousVerts[0] + (y + 1) * (resolution + 3) + x + 1;

			elements.push_back(v00);
			elements.push_back(v10);
			elements.push_back(v11);

			elements.push_back(v00);
			elements.push_back(v11);
			elements.push_back(v01);
		}
	}

	// Build the left part of the clipmap geometry
	for (unsigned int y = 0; y < resolution / 2; y++) {
		for (unsigned int x = 0; x < resolution / 4 + 1; x++) {
			GLuint v00 = previousVerts[1] + (y + 0) * (resolution / 4 + 2) + x + 0;
			GLuint v10 = previousVerts[1] + (y + 0) * (resolution / 4 + 2) + x + 1;
			GLuint v01 = previousVerts[1] + (y + 1) * (resolution / 4 + 2) + x + 0;
			GLuint v11 = previousVerts[1] + (y + 1) * (resolution / 4 + 2) + x + 1;

			elements.push_back(v00);
			elements.push_back(v10);
			elements.push_back(v11);

			elements.push_back(v00);
			elements.push_back(v11);
			elements.push_back(v01);
		}
	}

	// Build the left part of the clipmap geometry
	for (unsigned int y = 0; y < resolution / 2; y++) {
		for (unsigned int x = 0; x < resolution / 4 + 1; x++) {
			GLuint v00 = previousVerts[2] + (y + 0) * (resolution / 4 + 2) + x + 0;
			GLuint v10 = previousVerts[2] + (y + 0) * (resolution / 4 + 2) + x + 1;
			GLuint v01 = previousVerts[2] + (y + 1) * (resolution / 4 + 2) + x + 0;
			GLuint v11 = previousVerts[2] + (y + 1) * (resolution / 4 + 2) + x + 1;

			elements.push_back(v00);
			elements.push_back(v10);
			elements.push_back(v11);

			elements.push_back(v00);
			elements.push_back(v11);
			elements.push_back(v01);
		}
	}

	// Build the left part of the clipmap geometry
	for (unsigned int y = 0; y < resolution / 4 + 1; y++) {
		for (unsigned int x = 0; x < resolution + 2; x++) {
			GLuint v00 = previousVerts[3] + (y + 0) * (resolution + 3) + x + 0;
			GLuint v10 = previousVerts[3] + (y + 0) * (resolution + 3) + x + 1;
			GLuint v01 = previousVerts[3] + (y + 1) * (resolution + 3) + x + 0;
			GLuint v11 = previousVerts[3] + (y + 1) * (resolution + 3) + x + 1;

			elements.push_back(v00);
			elements.push_back(v10);
			elements.push_back(v11);

			elements.push_back(v00);
			elements.push_back(v11);
			elements.push_back(v01);
		}
	}
	return elements;
}

std::vector<glm::vec4> ClipMapGeometry::CreatePositions(unsigned int resolution)
{
	validate(resolution);
	std::vector<glm::vec4> positions;
	positions.reserve(numVertices(resolution));
	std::vector<glm::vec2> textureCoordinates = CreateTextureCoordinates(resolution);

	// Copy from texture coordinates
	for (unsigned int i = 0; i < textureCoordinates.size(); i++) {
		positions.push_back(
			glm::vec4(
				textureCoordinates[i].x,
				textureCoordinates[i].y,
				0,
				1));
	}
	return positions;
}


std::vector<glm::vec2> ClipMapGeometry::CreateTextureCoordinates(int resolution){
	validate(resolution);
	std::vector<glm::vec2> textureCoordinates;
	textureCoordinates.reserve(numVertices(resolution));

	// Build the bottom part of the clipmap geometry
	for (int y = -1; y < resolution / 4 + 1; y++) {
		for (int x = -1; x < resolution + 2; x++) {
			textureCoordinates.push_back(glm::vec2(
				static_cast<float>(x) / resolution,
				static_cast<float>(y) / resolution));
		}
	}
	
	// Build the left part of the clipmap geometry
	for (int y = resolution / 4; y < 3 * resolution / 4 + 1; y++) {
		for (int x = -1; x < resolution / 4 + 1; x++) {
			textureCoordinates.push_back(glm::vec2(
				static_cast<float>(x) / resolution,
				static_cast<float>(y) / resolution));
		}
	}
	
	// Build the right part of the clipmap geometry
	for (int y = resolution / 4; y < 3 * resolution / 4 + 1; y++) {
		for (int x = 3 * resolution / 4; x < resolution + 2; x++) {
			float u = static_cast<float>(x) / resolution;
			float v = static_cast<float>(y) / resolution;
			textureCoordinates.push_back(glm::vec2(u, v));
		}
	}
	
	// Build the top part of the clipmap geometry
	for (int y = 3 * resolution / 4; y < resolution + 2; y++) {
		for (int x = -1; x < resolution + 2; x++) {
			textureCoordinates.push_back(glm::vec2(
				static_cast<float>(x) / resolution,
				static_cast<float>(y) / resolution));
		}
	}
	
	return textureCoordinates;
}



std::vector<glm::vec3> ClipMapGeometry::CreateNormals(unsigned int resolution) {
	validate(resolution);
	std::vector<glm::vec3> normals;
	normals.reserve(numVertices(resolution));

	for (unsigned int y = 0; y < resolution + 1; y++) {
		for (unsigned int x = 0; x < resolution + 1; x++) {
			normals.push_back(glm::vec3(0, 0, 1));
		}
	}

	return normals;
}
}// namespace openspace