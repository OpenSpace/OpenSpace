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

#ifndef __OPENSPACE_CORE___BLOCKPLANEINTERSECTIONGEOMETRY___H__
#define __OPENSPACE_CORE___BLOCKPLANEINTERSECTIONGEOMETRY___H__

#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/glm.h>
#include <vector>

namespace openspace {

class BlockPlaneIntersectionGeometry {
public:
    // initializers
    BlockPlaneIntersectionGeometry(glm::vec3 blockSize, glm::vec3 planeNormal,
        float planeDistance);
    ~BlockPlaneIntersectionGeometry();

    bool initialize();
    void render();

    void setBlockSize(glm::vec3 size);
    void setPlane(glm::vec3 normal, float distance);

private:
    void updateVertices();
    std::vector<float> _vertices;
    // bool _isInitialized;
    GLuint _vaoId = 0;
    GLuint _vBufferId = 0;
    glm::vec3 _size;
    glm::vec3 _normal;
    float _planeDistance;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___BLOCKPLANEINTERSECTIONGEOMETRY___H__
