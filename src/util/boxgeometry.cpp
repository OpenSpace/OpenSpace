/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <openspace/util/boxgeometry.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <string>

namespace openspace {

BoxGeometry::BoxGeometry(glm::vec3 size) : _size(std::move(size)) {}

BoxGeometry::~BoxGeometry() {
    glDeleteBuffers(1, &_vBufferId);
    glDeleteVertexArrays(1, &_vaoId);
}

bool BoxGeometry::initialize() {
    // Initialize and upload to GPU
    const float x = _size.x * 0.5f;
    const float y = _size.y * 0.5f;
    const float z = _size.z * 0.5f;

    const std::array<GLfloat, 108> vertices = {
        -x, -y,  z, // blue corner
         x,  y,  z,  // white corner
        -x,  y,  z, // cyan corner
        -x, -y,  z, // blue corner
         x, -y,  z,  // magenta corner
         x,  y,  z,  // white corner

        -x, -y, -z, // black corner
        -x,  y, -z, // green
         x,  y, -z,  // yellow corner
        -x, -y, -z, // black
         x,  y, -z, // yellow
         x, -y, -z, // red

         x, -y, -z, // red
         x,  y,  z, // yellow
         x, -y,  z, // magenta
         x, -y, -z, // red
         x,  y, -z, // yellow
         x,  y,  z, // white

        -x, -y, -z, // black
        -x, -y,  z, // blue
        -x,  y,  z, // cyan
        -x, -y, -z, // black
        -x,  y,  z, // cyan
        -x,  y, -z, // green

         x,  y,  z, // white
        -x,  y, -z, // green
        -x,  y,  z, // cyan
        -x,  y, -z, // green
         x,  y,  z, // white
         x,  y, -z, // yellow

        -x, -y, -z, // black
         x, -y,  z, // magenta
        -x, -y,  z, // blue
        -x, -y, -z, // black
         x, -y, -z, // red
         x, -y,  z // magenta
    };

    if (_vaoId == 0) {
        glGenVertexArrays(1, &_vaoId);
    }

    if (_vBufferId == 0) {
        glGenBuffers(1, &_vBufferId);
    }

    glBindVertexArray(_vaoId);

    glBindBuffer(GL_ARRAY_BUFFER, _vBufferId);
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices.data(), GL_STATIC_DRAW);

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 3, nullptr);

    glBindVertexArray(0);
    return true;
}

void BoxGeometry::render() const {
    glBindVertexArray(_vaoId);  // select first VAO
    glDrawArrays(GL_TRIANGLES, 0, 6*6);
    glBindVertexArray(0);
}

} // namespace openspace
