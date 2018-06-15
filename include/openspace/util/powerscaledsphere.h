/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#ifndef __OPENSPACE_CORE___POWERSCALEDSPHERE___H__
#define __OPENSPACE_CORE___POWERSCALEDSPHERE___H__

#include <ghoul/glm.h>
#include <ghoul/opengl/ghoul_gl.h>

namespace openspace {

class PowerScaledScalar;
class PowerScaledSphere;

class PowerScaledSphere {
public:
    PowerScaledSphere(const PowerScaledScalar& radius, int segments = 8);
    PowerScaledSphere(glm::vec3 radius, int segments);
    PowerScaledSphere(const PowerScaledSphere& cpy);
    ~PowerScaledSphere();

    bool initialize();

    void render();

//private:
    struct Vertex {
        GLfloat location[4];
        GLfloat tex[2];
        GLfloat normal[3];
    };

    GLuint _vaoID = 0;
    GLuint _vBufferID = 0;
    GLuint _iBufferID = 0;

    unsigned int _isize;
    unsigned int _vsize;
    Vertex* _varray;
    int* _iarray;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___POWERSCALEDSPHERE___H__
