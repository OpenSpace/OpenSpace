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
	return resolution * resolution / 2 * 9; 
}

size_t ClipMapGeometry::numVertices(unsigned int resolution)
{
	return 
		(resolution + 1)		* (resolution / 4 + 1) + // Bottom
		(resolution / 4 + 1)	* (resolution / 2 + 1) + // Left
		(resolution / 4 + 1)	* (resolution / 2 + 1) + // Right
		(resolution + 1)		* (resolution / 4 + 1);  // Top
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

	unsigned int numVerticesBottom	= (resolution + 1)		* (resolution / 4 + 1);
	unsigned int numVerticesLeft	= (resolution / 4 + 1)	* (resolution / 2 + 1);
	unsigned int numVerticesRight	= (resolution / 4 + 1)	* (resolution / 2 + 1);
	unsigned int numVerticesTop		= (resolution + 1)		* (resolution / 4 + 1);

	// Build the bottom part of the clipmap geometry
	for (unsigned int y = 0; y < resolution / 4; y++) {
		for (unsigned int x = 0; x < resolution; x++) {
			GLuint v00 = (y + 0) * (resolution + 1) + x + 0;
			GLuint v10 = (y + 0) * (resolution + 1) + x + 1;
			GLuint v01 = (y + 1) * (resolution + 1) + x + 0;
			GLuint v11 = (y + 1) * (resolution + 1) + x + 1;

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
		for (unsigned int x = 0; x < resolution / 4; x++) {
			GLuint v00 = numVerticesBottom + (y + 0) * (resolution / 4 + 1) + x + 0;
			GLuint v10 = numVerticesBottom + (y + 0) * (resolution / 4 + 1) + x + 1;
			GLuint v01 = numVerticesBottom + (y + 1) * (resolution / 4 + 1) + x + 0;
			GLuint v11 = numVerticesBottom + (y + 1) * (resolution / 4 + 1) + x + 1;

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
		for (unsigned int x = 0; x < resolution / 4; x++) {
			GLuint v00 = numVerticesBottom + numVerticesLeft + (y + 0) * (resolution / 4 + 1) + x + 0;
			GLuint v10 = numVerticesBottom + numVerticesLeft + (y + 0) * (resolution / 4 + 1) + x + 1;
			GLuint v01 = numVerticesBottom + numVerticesLeft + (y + 1) * (resolution / 4 + 1) + x + 0;
			GLuint v11 = numVerticesBottom + numVerticesLeft + (y + 1) * (resolution / 4 + 1) + x + 1;

			elements.push_back(v00);
			elements.push_back(v10);
			elements.push_back(v11);

			elements.push_back(v00);
			elements.push_back(v11);
			elements.push_back(v01);
		}
	}

	// Build the left part of the clipmap geometry
	for (unsigned int y = 0; y < resolution / 4; y++) {
		for (unsigned int x = 0; x < resolution; x++) {
			GLuint v00 = numVerticesBottom + numVerticesLeft + numVerticesRight + (y + 0) * (resolution + 1) + x + 0;
			GLuint v10 = numVerticesBottom + numVerticesLeft + numVerticesRight + (y + 0) * (resolution + 1) + x + 1;
			GLuint v01 = numVerticesBottom + numVerticesLeft + numVerticesRight + (y + 1) * (resolution + 1) + x + 0;
			GLuint v11 = numVerticesBottom + numVerticesLeft + numVerticesRight + (y + 1) * (resolution + 1) + x + 1;

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


std::vector<glm::vec2> ClipMapGeometry::CreateTextureCoordinates(unsigned int resolution){
	validate(resolution);
	std::vector<glm::vec2> textureCoordinates;
	textureCoordinates.reserve(numVertices(resolution));

	// Build the bottom part of the clipmap geometry
	for (unsigned int y = 0; y < resolution / 4 + 1; y++) {
		for (unsigned int x = 0; x < resolution + 1; x++) {
			textureCoordinates.push_back(glm::vec2(
				static_cast<float>(x) / resolution,
				static_cast<float>(y) / resolution));
		}
	}
	
	// Build the left part of the clipmap geometry
	for (unsigned int y = resolution / 4; y < 3 * resolution / 4 + 1; y++) {
		for (unsigned int x = 0; x < resolution / 4 + 1; x++) {
			float u = static_cast<float>(x) / resolution;
			float v = static_cast<float>(y) / resolution;
			textureCoordinates.push_back(glm::vec2(u, v));
		}
	}
	
	// Build the right part of the clipmap geometry
	for (unsigned int y = resolution / 4; y < 3 * resolution / 4 + 1; y++) {
		for (unsigned int x = 3 * resolution / 4; x < resolution + 1; x++) {
			float u = static_cast<float>(x) / resolution;
			float v = static_cast<float>(y) / resolution;
			textureCoordinates.push_back(glm::vec2(u, v));
		}
	}
	
	// Build the top part of the clipmap geometry
	for (unsigned int y = 3 * resolution / 4; y < resolution + 1; y++) {
		for (unsigned int x = 0; x < resolution + 1; x++) {
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