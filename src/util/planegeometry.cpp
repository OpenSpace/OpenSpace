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

#include <openspace/util/planegeometry.h>

#include <array>
#include <cstddef>
#include <utility>

namespace {
    struct VertexData {
        GLfloat x;
        GLfloat y;
        GLfloat s;
        GLfloat t;
    };
} // namespace

namespace openspace {

PlaneGeometry::PlaneGeometry(glm::vec2 size) : _size(std::move(size)) {}

PlaneGeometry::PlaneGeometry(float size) : PlaneGeometry(glm::vec2(size, size)) {}

void PlaneGeometry::initialize() {
    glCreateBuffers(1, &_vBufferId);
    glCreateVertexArrays(1, &_vaoId);
    glVertexArrayVertexBuffer(_vaoId, 0, _vBufferId, 0, 4 * sizeof(float));

    glEnableVertexArrayAttrib(_vaoId, 0);
    glVertexArrayAttribFormat(_vaoId, 0, 2, GL_FLOAT, GL_FALSE, 0);
    glVertexArrayAttribBinding(_vaoId, 0, 0);

    glEnableVertexArrayAttrib(_vaoId, 1);
    glVertexArrayAttribFormat(_vaoId, 1, 2, GL_FLOAT, GL_FALSE, offsetof(VertexData, s));
    glVertexArrayAttribBinding(_vaoId, 1, 0);

    updateGeometry();
}

void PlaneGeometry::deinitialize() {
    glDeleteVertexArrays(1, &_vaoId);
    _vaoId = 0;

    glDeleteBuffers(1, &_vBufferId);
    _vBufferId = 0;
}

void PlaneGeometry::render() const {
    glBindVertexArray(_vaoId);
    glDrawArrays(GL_TRIANGLES, 0, 6);
    glBindVertexArray(0);
}

void PlaneGeometry::updateSize(const glm::vec2& size) {
    _size = size;
    updateGeometry();
}

void PlaneGeometry::updateSize(float size) {
    updateSize(glm::vec2(size));
}

void PlaneGeometry::updateGeometry() const {
    const glm::vec2 size = _size;
    const std::array<VertexData, 6> vertices = {
        VertexData{ -size.x, -size.y, 0.f, 0.f },
        VertexData{  size.x,  size.y, 1.f, 1.f },
        VertexData{ -size.x,  size.y, 0.f, 1.f },
        VertexData{ -size.x, -size.y, 0.f, 0.f },
        VertexData{  size.x, -size.y, 1.f, 0.f },
        VertexData{  size.x,  size.y, 1.f, 1.f }
    };

    glNamedBufferData(_vBufferId, sizeof(vertices), vertices.data(), GL_STATIC_DRAW);
}

} // namespace openspace
