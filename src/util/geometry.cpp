/**
Copyright (C) 2012-2014 Jonas Strandstedt

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

// open space includes
#include "util/geometry.h"

gl4::Geometry::Geometry() 
{

}
gl4::Geometry::~Geometry() 
{

}
void gl4::Geometry::init() 
{
	VBO::init();
	//LOG("Geometry Init()\n");

	// if there is an geometry
	if(_vsize > 0 && _isize > 0 && _varray != NULL && _iarray != NULL) {

		// calculate xyz max and mins
		for (int i = 0; i < 3; ++i)
		{
			_limits[i][0] = _varray[0].location[i];
			_limits[i][1] = _varray[0].location[i];
		}
		for (unsigned int i = 0; i < _vsize; ++i)
		{

			for (unsigned int j = 0; j < 3; ++j)
			{
				if (_varray[i].location[j] < _limits[j][0])
				{
					 _limits[j][0] = _varray[i].location[j];
				}
				if (_varray[i].location[j] < _limits[j][1])
				{
					_limits[j][1] = _varray[i].location[j];
				}
			}
		}
	}

}

glm::mat4 gl4::Geometry::getTransform() 
{
	glm::mat4 transform = glm::translate(glm::mat4(1.0), _position);
	return transform * _rotation;
}
