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

#include <modules/globebrowsing/rendering/basicgrid.h>

namespace {
	const std::string _loggerCat = "BasicGrid";
}

namespace openspace {

BasicGrid::BasicGrid(
	unsigned int xRes,
	unsigned int yRes,
	Geometry::Positions usePositions,
	Geometry::TextureCoordinates useTextureCoordinates,
	Geometry::Normals useNormals)
	: Grid(
		xRes, 
		yRes,
		usePositions,
		useTextureCoordinates,
		useNormals)
{
	_geometry = std::unique_ptr<Geometry>(new Geometry(
		CreateElements(xRes, yRes),
		usePositions,
		useTextureCoordinates,
		useNormals));

	if (usePositions == Geometry::Positions::Yes) {
		_geometry->setVertexPositions(CreatePositions(_xRes, _yRes));
	}
	if (useTextureCoordinates == Geometry::TextureCoordinates::Yes) {
		_geometry->setVertexTextureCoordinates(CreateTextureCoordinates(_xRes, _yRes));
	}
	if (useNormals == Geometry::Normals::Yes) {
		_geometry->setVertexNormals(CreateNormals(_xRes, _yRes));
	}
}

BasicGrid::~BasicGrid()
{

}

int BasicGrid::xResolution() const {
	return _xRes;
}

int BasicGrid::yResolution() const {
	return _yRes;
}

void BasicGrid::validate(int xRes, int yRes) {
	ghoul_assert(xRes > 0 && yRes > 0,
		"Resolution must be at least 1x1. (" << xRes << ", " << yRes << ")");
}

inline size_t BasicGrid::numElements(int xRes, int yRes){
	return 3 * 2 * (xRes - 1)*(yRes - 1);
}

inline size_t BasicGrid::numVertices(int xRes, int yRes) {
	return xRes * yRes;
}

std::vector<GLuint> BasicGrid::CreateElements(int xRes, int yRes) {
	validate(xRes, yRes);

	std::vector<GLuint> elements;
	elements.reserve(numElements(xRes, yRes));
	for (unsigned int y = 0; y < yRes-1; y++) {
		for (unsigned int x = 0; x < xRes-1; x++) {

			// x    v01---v11   x ..
			//       |  /  |
			// x    v00---v10   x ..
			//
			// x	x     x     x ..
			// :    :     :     :

			GLuint v00 = (y + 0) * xRes + x + 0;
			GLuint v10 = (y + 0) * xRes + x + 1;
			GLuint v01 = (y + 1) * xRes + x + 0;
			GLuint v11 = (y + 1) * xRes + x + 1;

			// add upper triangle
			elements.push_back(v00);
			elements.push_back(v10);
			elements.push_back(v11);

			// add lower triangle
			elements.push_back(v00);
			elements.push_back(v11);
			elements.push_back(v01);
		}
	}

	return elements;
}

std::vector<glm::vec4> BasicGrid::CreatePositions(
	int xRes,
	int yRes) 
{
	validate(xRes, yRes);
	std::vector<glm::vec4> positions;
	positions.reserve(numVertices(xRes, yRes));

	// Copy from 2d texture coordinates and use as template to create positions
	std::vector<glm::vec2> templateTextureCoords = CreateTextureCoordinates(xRes, yRes);
	for (unsigned int i = 0; i < templateTextureCoords.size(); i++)
	{
		positions.push_back(glm::vec4(
			templateTextureCoords[i],
			0.0f,
			1.0f
			));
	}
	return positions;
}

std::vector<glm::vec2> BasicGrid::CreateTextureCoordinates(int xRes, int yRes){
	validate(xRes, yRes);
	std::vector<glm::vec2> textureCoordinates;
	textureCoordinates.reserve(numVertices(xRes, yRes));

	for (unsigned int y = 0; y < yRes; y++) {
		for (unsigned int x = 0; x < xRes; x++) {
			textureCoordinates.push_back(glm::vec2(
				static_cast<float>(x) / static_cast<float>(xRes - 1),
				static_cast<float>(y) / static_cast<float>(yRes - 1)
			));
		}
	}
	return textureCoordinates;
}

std::vector<glm::vec3> BasicGrid::CreateNormals(int xRes, int yRes) {
	validate(xRes, yRes);
	std::vector<glm::vec3> normals;
	normals.reserve(numVertices(xRes, yRes));

	for (unsigned int y = 0; y < yRes; y++) {
		for (unsigned int x = 0; x < xRes; x++) {
			normals.push_back(glm::vec3(0, 0, 1));
		}
	}

	return normals;
}

}// namespace openspace