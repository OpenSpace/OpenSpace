/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2015                                                               *
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

// open space includes
#include <openspace/util/powerscaledsphere.h>

#include <ghoul/logging/logmanager.h>

#define _USE_MATH_DEFINES
#include <math.h>

namespace {
const std::string _loggerCat = "PowerScaledSphere";
}

namespace openspace {

PowerScaledSphere::PowerScaledSphere(const PowerScaledScalar& radius, int segments)
    :  _isize(6 * segments * segments)
    , _vsize((segments + 1) * (segments + 1))
    , _varray(new Vertex[_vsize])
    , _iarray(new int[_isize])
{
    static_assert(sizeof(Vertex) == 64,
                  "The size of the Vertex needs to be 64 for performance");

    int nr = 0;
    const float fsegments = static_cast<float>(segments);
    const float r = static_cast<float>(radius[0]);

	
    for (int i = 0; i <= segments; i++) {
        // define an extra vertex around the y-axis due to texture mapping
        for (int j = 0; j <= segments; j++) {
            const float fi = static_cast<float>(i);
            const float fj = static_cast<float>(j);
            // inclination angle (north to south)
            const float theta = fi * float(M_PI) / fsegments;  // 0 -> PI

            // azimuth angle (east to west)
            const float phi = fj * float(M_PI) * 2.0f / fsegments;  // 0 -> 2*PI

            const float x = r * sin(phi) * sin(theta);  //
            const float y = r * cos(theta);             // up
            const float z = r * cos(phi) * sin(theta);  //

            glm::vec3 normal = glm::vec3(x, y, z);
            if (!(x == 0.f && y == 0.f && z == 0.f))
                normal = glm::normalize(normal);

            const float t1 = fj / fsegments;
            const float t2 = fi / fsegments;

			//double tp = 1.0 / pow(10, static_cast<GLfloat>(radius[1]));

            _varray[nr].location[0] = x;
            _varray[nr].location[1] = y;
            _varray[nr].location[2] = z;
            _varray[nr].location[3] = static_cast<GLfloat>(radius[1]);
            _varray[nr].normal[0] = normal[0];
            _varray[nr].normal[1] = normal[1];
            _varray[nr].normal[2] = normal[2];

			//std::cout << _varray[nr].location[0] << " " << _varray[nr].location[1] << " " << _varray[nr].location[2] << " " << _varray[nr].location[3] << std::endl;
            _varray[nr].tex[0] = t1;
            _varray[nr].tex[1] = t2;
            ++nr;
        }
    }

    nr = 0;
    // define indices for all triangles
    for (int i = 1; i <= segments; ++i) {
        for (int j = 0; j < segments; ++j) {
            const int t = segments + 1;
            _iarray[nr] = t * (i - 1) + j + 0; //1
            ++nr;
            _iarray[nr] = t * (i + 0) + j + 0; //2 
            ++nr;
            _iarray[nr] = t * (i + 0) + j + 1; //3
            ++nr;

            _iarray[nr] = t * (i - 1) + j + 0; //4 
            ++nr;
            _iarray[nr] = t * (i + 0) + j + 1; //5
            ++nr;
            _iarray[nr] = t * (i - 1) + j + 1; //6
            ++nr;
			
			/*
			_iarray[nr] = t * (i - 1) + j + 0; //1
			++nr;
			_iarray[nr] = t * (i + 0) + j + 0; //2 
			++nr;
			_iarray[nr] = t * (i + 0) + j + 1; //3
			++nr;
			_iarray[nr] = t * (i - 1) + j + 1; //6
			++nr;
			_iarray[nr] = t * (i - 1) + j + 0; //4 
			++nr;
			*/
        }
    }
}

PowerScaledSphere::~PowerScaledSphere()
{
	if (_varray)
	    delete[] _varray;
	if (_iarray)
	    delete[] _iarray;

	_vbo.deinitialize();
}

bool PowerScaledSphere::initialize()
{

	std::vector<Vertex> varray(_vsize);
	std::vector<int> iarray(_isize);
	for (size_t i = 0; i < _vsize; ++i) {
		varray.at(i) = _varray[i];
	}
	for (size_t i = 0; i < _isize; ++i) {
		iarray.at(i) = _iarray[i];
	}

	_vbo.initialize(varray, iarray);
	_vbo.vertexAttribPointer(0, 4, GL_FLOAT, sizeof(Vertex), offsetof(Vertex, location));
	_vbo.vertexAttribPointer(1, 2, GL_FLOAT, sizeof(Vertex), offsetof(Vertex, tex));
	_vbo.vertexAttribPointer(2, 3, GL_FLOAT, sizeof(Vertex), offsetof(Vertex, normal));

    return true;
}

void PowerScaledSphere::render()
{
	_vbo.render();
}

}  // namespace openspace
