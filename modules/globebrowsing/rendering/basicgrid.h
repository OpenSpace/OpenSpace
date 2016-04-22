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


#ifndef __BASICGRIDGEOMETRY_H__
#define __BASICGRIDGEOMETRY_H__

#include <glm/glm.hpp>

#include <modules/globebrowsing/rendering/grid.h>

#include <vector>

namespace openspace {

class BasicGrid : public Grid
{
public:
	/**
	\param xRes is the number of grid cells in the x direction.
	\param yRes is the number of grid cells in the y direction.
	\param usePositions determines whether or not to upload any vertex position data
	to the GPU.
	\param useTextureCoordinates determines whether or not to upload any vertex texture
	coordinate data to the GPU.
	\param useNormals determines whether or not to upload any vertex normal data
	to the GPU.
	*/
	BasicGrid(
		unsigned int xRes,
		unsigned int yRes,
		Geometry::Positions usePositions,
		Geometry::TextureCoordinates useTextureCoordinates,
		Geometry::Normals useNormals);
	~BasicGrid();

	/**
	Returns the number of grid cells in the x direction. Hence the number of vertices
	in the x direction is xResolution + 1.
	*/
	virtual int xResolution() const;
	
	/**
	Returns the number of grid cells in the y direction. Hence the number of vertices
	in the y direction is xResolution + 1.
	*/
	virtual int yResolution() const;

private:
	virtual std::vector<GLuint>		CreateElements(				int xRes, int yRes);
	virtual std::vector<glm::vec4>	CreatePositions(			int xRes, int yRes);
	virtual std::vector<glm::vec2>	CreateTextureCoordinates(	int xRes, int yRes);
	virtual std::vector<glm::vec3>	CreateNormals(				int xRes, int yRes);

	void validate(int xRes, int yRes);

	inline size_t numElements(int xRes, int yRes);
	inline size_t numVertices(int xRes, int yRes);
};
} // namespace openspace
#endif // __BASICGRIDGEOMETRY_H__