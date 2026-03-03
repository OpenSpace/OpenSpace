/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <array>
#include <utility>

namespace openspace {

BoxGeometry::BoxGeometry(glm::vec3 size) : _size(std::move(size)) {}

BoxGeometry::~BoxGeometry() {
    glDeleteVertexArrays(1, &_vao);
    glDeleteBuffers(1, &_vbo);
}

void BoxGeometry::initialize() {
    // Initialize and upload to GPU
    const float x = _size.x * 0.5f;
    const float y = _size.y * 0.5f;
    const float z = _size.z * 0.5f;

    struct Vertex {
        float r;
        float g;
        float b;
    };
    const std::array<Vertex, 36> Vertices = {
        Vertex { -x, -y,  z}, // blue corner
        Vertex {  x,  y,  z}, // white corner
        Vertex { -x,  y,  z}, // cyan corner
        Vertex { -x, -y,  z}, // blue corner
        Vertex {  x, -y,  z}, // magenta corner
        Vertex {  x,  y,  z}, // white corner

        Vertex { -x, -y, -z}, // black corner
        Vertex { -x,  y, -z}, // green
        Vertex {  x,  y, -z}, // yellow corner
        Vertex { -x, -y, -z}, // black
        Vertex {  x,  y, -z}, // yellow
        Vertex {  x, -y, -z}, // red

        Vertex {  x, -y, -z}, // red
        Vertex {  x,  y,  z}, // yellow
        Vertex {  x, -y,  z}, // magenta
        Vertex {  x, -y, -z}, // red
        Vertex {  x,  y, -z}, // yellow
        Vertex {  x,  y,  z}, // white

        Vertex { -x, -y, -z}, // black
        Vertex { -x, -y,  z}, // blue
        Vertex { -x,  y,  z}, // cyan
        Vertex { -x, -y, -z}, // black
        Vertex { -x,  y,  z}, // cyan
        Vertex { -x,  y, -z}, // green

        Vertex {  x,  y,  z}, // white
        Vertex { -x,  y, -z}, // green
        Vertex { -x,  y,  z}, // cyan
        Vertex { -x,  y, -z}, // green
        Vertex {  x,  y,  z}, // white
        Vertex {  x,  y, -z}, // yellow

        Vertex { -x, -y, -z}, // black
        Vertex {  x, -y,  z}, // magenta
        Vertex { -x, -y,  z}, // blue
        Vertex { -x, -y, -z}, // black
        Vertex {  x, -y, -z}, // red
        Vertex {  x, -y,  z}  // magenta
    };

    glCreateBuffers(1, &_vbo);
    glNamedBufferStorage(_vbo, 36 * sizeof(Vertex), Vertices.data(), GL_NONE_BIT);

    glCreateVertexArrays(1, &_vao);
    glVertexArrayVertexBuffer(_vao, 0, _vbo, 0, sizeof(Vertex));

    glEnableVertexArrayAttrib(_vao, 0);
    glVertexArrayAttribFormat(_vao, 0, 3, GL_FLOAT, GL_FALSE, 0);
    glVertexArrayAttribBinding(_vao, 0, 0);
}

void BoxGeometry::render() const {
    glBindVertexArray(_vao);
    glDrawArrays(GL_TRIANGLES, 0, 6*6);
    glBindVertexArray(0);
}

} // namespace openspace
