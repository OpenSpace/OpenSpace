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

class GridGeometry : public Geometry
{
public:
	GridGeometry(unsigned int xRes, unsigned int yRes,
		Positions usePositions = Positions::No, 
		TextureCoordinates useTextures = TextureCoordinates::No, 
		Normals useNormals = Normals::No
	);

	~GridGeometry();

	inline const unsigned int xResolution() const;
	inline const unsigned int yResolution() const;

	inline static size_t numElements(unsigned int xRes, unsigned int yRes);
	static size_t numVertices(unsigned int xRes, unsigned int yRes);

private:
	static std::vector<GLuint> CreateElements(unsigned int xRes, unsigned int yRes);
	static std::vector<glm::vec4> CreatePositions(unsigned int xRes, unsigned int yRes,
		float xSize = 1.0f, float ySize = 1.0f, float xOffset = 0.0f, float yOffset = 0.0f);
	static std::vector<glm::vec2> CreateTextureCoordinates(unsigned int xRes, unsigned int yRes);
	static std::vector<glm::vec3> CreateNormals(unsigned int xRes, unsigned int yRes);

	inline static void validate(unsigned int xRes, unsigned int yRes);
	inline void validateIndices(unsigned int x, unsigned int y);

	unsigned int _xRes;
	unsigned int _yRes;
};
} // namespace openspace
#endif // __GRIDGEOMETRY_H__