/**
Copyright (C) 2012-2014 Jonas Strandstedt

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

// open space includes
#include "util/sphere.h"

gl4::Sphere::Sphere(float radius, int segments, bool tessellation)
{
	// use patches if using a tessellation shader
	if (tessellation)
	{
		_mode = GL_PATCHES;
	} else {
		_mode = GL_TRIANGLES;
	}

	// calculate and allocate memory for number of vertices and incicies
	_vsize = segments * segments + 1;
	_isize = 2 * segments * 3 + (segments -1) * (segments +1) * 6-3;
	_varray = (Vertex*) std::malloc(_vsize*sizeof(Vertex));
	_iarray = (int*) std::malloc(_isize*sizeof(int));


	//LOG("Initializing sphere with:\n");
	//LOG("   segments = %d\n", segments);
	//LOG("   radius   = %f\n", radius);
	//LOG("   _vsize   = %d\n", _vsize);
	//LOG("   _isize   = %d\n", _isize);

	// define PI
	const float PI = 3.14159265f;
	int nr = 0;

	// define top vertex
	_varray[nr].location[0] = 0.0f;
	_varray[nr].location[1] = radius;
	_varray[nr].location[2] = 0.0f;
	_varray[nr].normal[0] = 0.0f;
	_varray[nr].normal[1] = 1.0f;
	_varray[nr].normal[2] = 0.0f;
	_varray[nr].tex[0] = 0.5f;
	_varray[nr].tex[1] = 1.0f;
	_varray[nr].color[0] = 1.0f;
	_varray[nr].color[1] = 0.0f;
	_varray[nr].color[2] = 0.0f;
	_varray[nr].color[3] = 1.0f;
	_varray[nr].attribute[0] = 0.0f;
	_varray[nr].attribute[1] = 0.0f;
	_varray[nr].attribute[2] = 0.0f;
	_varray[nr].float_attribute = 0.0f;

	nr++;
	for (int i = 1; i < segments; i++)
	{
		// define an extra vertex around the y-axis due to texture mapping
		for (int j = 0; j <= segments; j++)
		{

			float theta = static_cast<float>(i)*PI/static_cast<float>(segments);
			float phi = static_cast<float>(j)*PI*2.0f/static_cast<float>(segments);

			float z = radius*cos(phi)*sin(theta);
			float x = radius*sin(phi)*sin(theta);
			float y = radius*cos(theta);

			glm::vec3 normal = glm::normalize(glm::vec3(x,y,z));

			float t1 = static_cast<float>(j)/static_cast<float>(segments);
			float t2 = 1.0f-static_cast<float>(i)/static_cast<float>(segments);

			_varray[nr].location[0] = x;
			_varray[nr].location[1] = y;
			_varray[nr].location[2] = z;
			_varray[nr].normal[0] = normal[0];
			_varray[nr].normal[1] = normal[1];
			_varray[nr].normal[2] = normal[2];

			_varray[nr].tex[0] = t1;
			_varray[nr].tex[1] = t2;

			_varray[nr].color[0] = 1.0f;
			_varray[nr].color[1] = 0.0f;
			_varray[nr].color[2] = 0.0f;
			_varray[nr].color[3] = 1.0f;
			_varray[nr].attribute[0] = 0.0f;
			_varray[nr].attribute[1] = 0.0f;
			_varray[nr].attribute[2] = 0.0f;
			_varray[nr].float_attribute = 0.0f;

			nr++;

		}
	}
	// define bottom vertex
	_varray[nr].location[0] = 0.0f;
	_varray[nr].location[1] = -radius;
	_varray[nr].location[2] = 0.0f;
	_varray[nr].normal[0] = 0.0f;
	_varray[nr].normal[1] = -1.0f;
	_varray[nr].normal[2] = 0.0f;
	_varray[nr].tex[0] = 0.5f;
	_varray[nr].tex[1] = 0.0f;
	_varray[nr].color[0] = 1.0f;
	_varray[nr].color[1] = 0.0f;
	_varray[nr].color[2] = 0.0f;
	_varray[nr].color[3] = 1.0f;
	_varray[nr].attribute[0] = 0.0f;
	_varray[nr].attribute[1] = 0.0f;
	_varray[nr].attribute[2] = 0.0f;
	_varray[nr].float_attribute = 0.0f;
	nr++;

	nr = 0;
	// define indicies for top cap
	for (int i = 0; i < segments; ++i)
	{
		_iarray[nr] = 0;
		nr++;
		_iarray[nr] = 1 + i;
		nr++;
		_iarray[nr] = 2 + i;
		nr++;
	}

	// define indicies for middle
	for (int i = 1; i < segments ; ++i)
	{
		for (int j = 0; j < segments; ++j)
		{
			_iarray[nr] = (segments+1)*(i-1) + j +1;
			nr++;
			_iarray[nr] = (segments+1)*i + 1 + j;
			nr++;
			_iarray[nr] = (segments+1)*i+ 1 + j+1;
			nr++;

			_iarray[nr] = (segments+1)*(i-1) + j +2;
			nr++;
			_iarray[nr] = (segments+1)*(i-1) + j +1;
			nr++;
			_iarray[nr] = (segments+1)*i+ 1 + j+1;
			nr++;
		}	
	}

	// define indices for bottom cap
	for (int i = 0; i < segments; ++i)
	{
		_iarray[nr] = _vsize - segments + i-2;
		nr++;
		_iarray[nr] = _vsize - segments + i -1;
		nr++;
		_iarray[nr] = _vsize -1;
		nr++;
	}
	
}

gl4::Sphere::~Sphere()
{

}
