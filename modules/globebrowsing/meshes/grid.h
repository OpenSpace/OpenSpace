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

#ifndef __GRIDGEOMETRY_H__
#define __GRIDGEOMETRY_H__

#include <ghoul/opengl/ghoul_gl.h>
#include <glm/glm.hpp>

#include <modules/globebrowsing/meshes/trianglesoup.h>

#include <vector>

namespace openspace {

class Grid
{
public:
	Grid(
		int xSegments,
		int ySegments,
		TriangleSoup::Positions usePositions = TriangleSoup::Positions::No,
		TriangleSoup::TextureCoordinates useTextures = TriangleSoup::TextureCoordinates::No,
		TriangleSoup::Normals useNormals = TriangleSoup::Normals::No);
	~Grid();

	TriangleSoup& geometry();

	/**
	Returns the number of grid cells in the x direction. Hence the number of vertices
	in the x direction is xResolution + 1.
	*/
	virtual int xSegments() const = 0;
	
	/**
	Returns the number of grid cells in the y direction. Hence the number of vertices
	in the y direction is xResolution + 1.
	*/
	virtual int ySegments() const = 0;

protected:
	virtual std::vector<GLuint>		CreateElements(				int xSegments, int ySegments) = 0;
	virtual std::vector<glm::vec4>	CreatePositions(			int xSegments, int ySegments) = 0;
	virtual std::vector<glm::vec2>	CreateTextureCoordinates(	int xSegments, int ySegments) = 0;
	virtual std::vector<glm::vec3>	CreateNormals(				int xSegments, int ySegments) = 0;

	std::unique_ptr<TriangleSoup> _geometry;

	const int _xSegments;
	const int _ySegments;
};
} // namespace openspace
#endif // __GRIDGEOMETRY_H__