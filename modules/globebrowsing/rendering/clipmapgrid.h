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

#ifndef __CLIPMAPGEOMETRY_H__
#define __CLIPMAPGEOMETRY_H__

#include <modules/globebrowsing/rendering/grid.h>

#include <vector>
#include <glm/glm.hpp>

namespace openspace {

//////////////////////////////////////////////////////////////////////////////////////////
//							CLIPMAP GRID	(Abstract class)							//
//////////////////////////////////////////////////////////////////////////////////////////

/**
This class defines a grid used for the layers of a geometry clipmap. A geometry
clipmap is built up from a pyramid of clipmaps so the majority of the grids used
are of the class OuterClipMapGrid. The vertex positions and texture coordinated are
defined differently than for a normal BasicGrid. Other than having the basic grid
vertices it also creates padding of one grid cell on the perimeter. This padding can be
used when rendering a ClipMapGrid so that an inner layer of the pyramid can move and
snap to the outer layers grid cells.
*/
class ClipMapGrid : public Grid
{
public:
	ClipMapGrid(unsigned int segments);

	~ClipMapGrid();

	virtual int xSegments() const;
	virtual int ySegments() const;

	/**
	Returns the segments of the grid. A ClipMapGrid must have the segments in x and
	y direction equal so this function works as a wrapper for xSegments() and
	ySegments().
	*/
	int segments() const;
};

//////////////////////////////////////////////////////////////////////////////////////////
//									OUTER CLIPMAP GRID									//
//////////////////////////////////////////////////////////////////////////////////////////

/**
The outer layers of a geometry clipmap pyramid can be built up by grids with sizes
increasing with the power of 2. The OuterClipMapGrid has a whole in the middle where
a smaller ClipMapGrid of half the size can fit.
*/
class OuterClipMapGrid : public ClipMapGrid
{
public:
	OuterClipMapGrid(unsigned int segments);

	~OuterClipMapGrid();

protected:
	virtual std::vector<GLuint>		CreateElements(int xSegments, int ySegments);
	virtual std::vector<glm::vec4>	CreatePositions(int xSegments, int ySegments);
	virtual std::vector<glm::vec2>	CreateTextureCoordinates(int xSegments, int ySegments);
	virtual std::vector<glm::vec3>	CreateNormals(int xSegments, int ySegments);

private:
	void validate(int xSegments, int ySegments);

	static size_t numVerticesBottom(int segments);
	static size_t numVerticesLeft(int segments);
	static size_t numVerticesRight(int segments);
	static size_t numVerticesTop(int segments);

	static size_t numElements(int segments);
	static size_t numVertices(int segments);
};

//////////////////////////////////////////////////////////////////////////////////////////
//									INNER CLIPMAP GRID									//
//////////////////////////////////////////////////////////////////////////////////////////

/**
The InnerClipMapGrid can be used for the inner most (smallest) grid of a geometry clipmap
pyramid. The only difference from a OuterClipMapGrid is that this grid does not have
a whole where a smaller ClipMapGrid can be positioned.
*/
class InnerClipMapGrid : public ClipMapGrid
{
public:
	InnerClipMapGrid(unsigned int segments);

	~InnerClipMapGrid();

private:
	virtual std::vector<GLuint>		CreateElements(	int xSegments, int ySegments);
	virtual std::vector<glm::vec4>	CreatePositions(int xSegments, int ySegments);
	virtual std::vector<glm::vec2>	CreateTextureCoordinates(int xSegments, int ySegments);
	virtual std::vector<glm::vec3>	CreateNormals(int xSegments, int ySegments);

	void validate(int xSegments, int ySegments);

	static size_t numElements(int segments);
	static size_t numVertices(int segments);
};

} // namespace openspace
#endif // __CLIPMAPGEOMETRY_H__