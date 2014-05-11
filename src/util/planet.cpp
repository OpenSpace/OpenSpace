/**
Copyright (C) 2012-2014 Jonas Strandstedt

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

// open space includes
#include <openspace/util/planet.h>

// sgct includes
#include "sgct.h"

// std includes
#include <cstdio>

namespace openspace
{

Planet::Planet(PowerScaledScalar radius, int levels) {
	
	// describe datatype and Vertex structure to the vbo template
	std::vector<std::tuple<int, GLenum, int> > descriptor;
	descriptor.push_back(std::make_tuple(4, GL_FLOAT, 0));
	descriptor.push_back(std::make_tuple(2, GL_FLOAT, 16));
	descriptor.push_back(std::make_tuple(3, GL_FLOAT, 24));

	int vsize = levels * levels + 1;
	int isize = 2 * levels * 3 + (levels -1) * (levels +1) * 6-3;
	Vertex *varray = new Vertex[vsize];
	int *iarray = new int[isize];

	// define PI
	const float PI = 3.14159265f;
	int nr = 0;

	// define top vertex
	varray[nr].location[0] = 0.0f;
	varray[nr].location[1] = static_cast<GLfloat>(radius[0]);
	varray[nr].location[2] = 0.0f;
	varray[nr].location[3] = static_cast<GLfloat>(radius[1]);
	varray[nr].normal[0] = 0.0f;
	varray[nr].normal[1] = 1.0f;
	varray[nr].normal[2] = 0.0f;
	varray[nr].tex[0] = 0.5f;
	varray[nr].tex[1] = 1.0f;

	nr++;
	for (int i = 1; i < levels; i++)
	{
		// define an extra vertex around the y-axis due to texture mapping
		for (int j = 0; j <= levels; j++)
		{

			float theta = static_cast<float>(i)*PI/static_cast<float>(levels);
			float phi = static_cast<float>(j)*PI*2.0f/static_cast<float>(levels);

			float z = static_cast<float>(radius[0]*cos(phi)*sin(theta));
			float x = static_cast<float>(radius[0]*sin(phi)*sin(theta));
			float y = static_cast<float>(radius[0]*cos(theta));

            glm::vec3 normal = glm::vec3(x,y,z);
            if (!(x == 0.f && y == 0.f && z == 0.f))
                normal = glm::normalize(normal);

			float t1 = static_cast<float>(j)/static_cast<float>(levels);
			float t2 = 1.0f-static_cast<float>(i)/static_cast<float>(levels);

			varray[nr].location[0] = x;
			varray[nr].location[1] = y;
			varray[nr].location[2] = z;
			varray[nr].location[3] = static_cast<GLfloat>(radius[1]);
			varray[nr].normal[0] = normal[0];
			varray[nr].normal[1] = normal[1];
			varray[nr].normal[2] = normal[2];

			varray[nr].tex[0] = t1;
			varray[nr].tex[1] = t2;

			nr++;

		}
	}
	// define bottom vertex
	varray[nr].location[0] = 0.0f;
	varray[nr].location[1] = static_cast<GLfloat>(-radius[0]);
	varray[nr].location[2] = 0.0f;
	varray[nr].location[3] = static_cast<GLfloat>(radius[1]);
	varray[nr].normal[0] = 0.0f;
	varray[nr].normal[1] = -1.0f;
	varray[nr].normal[2] = 0.0f;
	varray[nr].tex[0] = 0.5f;
	varray[nr].tex[1] = 0.0f;
	nr++;

	nr = 0;
	// define indicies for top cap
	for (int i = 0; i < levels; ++i)
	{
		iarray[nr] = 0;
		nr++;
		iarray[nr] = 1 + i;
		nr++;
		iarray[nr] = 2 + i;
		nr++;
	}

	// define indicies for middle
	for (int i = 1; i < levels ; ++i)
	{
		for (int j = 0; j < levels; ++j)
		{
			iarray[nr] = (levels+1)*(i-1) + j +1;
			nr++;
			iarray[nr] = (levels+1)*i + 1 + j;
			nr++;
			iarray[nr] = (levels+1)*i+ 1 + j+1;
			nr++;

			iarray[nr] = (levels+1)*(i-1) + j +2;
			nr++;
			iarray[nr] = (levels+1)*(i-1) + j +1;
			nr++;
			iarray[nr] = (levels+1)*i+ 1 + j+1;
			nr++;
		}	
	}

	// define indices for bottom cap
	for (int i = 0; i < levels; ++i)
	{
		iarray[nr] = vsize - levels + i-2;
		nr++;
		iarray[nr] = vsize - levels + i -1;
		nr++;
		iarray[nr] = vsize -1;
		nr++;
	}


	vbo = new VBO<Vertex>(descriptor, varray, vsize, iarray, isize);
	vbo->init();

	delete[] varray;
	delete[] iarray;

}

Planet::~Planet()
{
	if(vbo)
		delete vbo;
}

void Planet::setHeightMap(const std::string &path) {

}

void Planet::render() {
	assert(vbo);
	vbo->render();
}


} // namespace openspace

