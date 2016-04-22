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

#include <modules/globebrowsing/rendering/geometry.h>

#include <vector>

namespace openspace {

class Grid
{
public:
	Grid(
		int xRes,
		int yRes,
		Geometry::Positions usePositions = Geometry::Positions::No,
		Geometry::TextureCoordinates useTextures = Geometry::TextureCoordinates::No,
		Geometry::Normals useNormals = Geometry::Normals::No);
	~Grid();

	Geometry& geometry();

	virtual int xResolution() const = 0;
	virtual int yResolution() const = 0;

protected:
	virtual std::vector<GLuint>		CreateElements(				int xRes, int yRes) = 0;
	virtual std::vector<glm::vec4>	CreatePositions(			int xRes, int yRes) = 0;
	virtual std::vector<glm::vec2>	CreateTextureCoordinates(	int xRes, int yRes) = 0;
	virtual std::vector<glm::vec3>	CreateNormals(				int xRes, int yRes) = 0;

	std::unique_ptr<Geometry> _geometry;

	const int _xRes;
	const int _yRes;
};
} // namespace openspace
#endif // __GRIDGEOMETRY_H__