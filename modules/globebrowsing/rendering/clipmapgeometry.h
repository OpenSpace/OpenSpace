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

#include <modules/globebrowsing/rendering/geometry.h>

#include <vector>
#include <glm/glm.hpp>

namespace openspace {

class ClipmapGeometry : public Geometry
{
public:
	ClipmapGeometry(
		unsigned int resolution,
		Positions usePositions = Positions::No, 
		TextureCoordinates useTextures = TextureCoordinates::No, 
		Normals useNormals = Normals::No
	);

	~ClipmapGeometry();

	const unsigned int resolution() const;

	static size_t numElements(unsigned int resolution);
	static size_t numVertices(unsigned int resolution);
private:
	static std::vector<GLuint> CreateElements(unsigned int resoluion);
	static std::vector<glm::vec4> CreatePositions(unsigned int resolution);
	static std::vector<glm::vec2> CreateTextureCoordinates(unsigned int resolution);
	static std::vector<glm::vec3> CreateNormals(unsigned int resolution);

	static void validate(unsigned int resolution);

	unsigned int _resolution;
};
} // namespace openspace
#endif // __CLIPMAPGEOMETRY_H__