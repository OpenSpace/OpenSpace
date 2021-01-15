/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <ghoul/logging/logmanager.h>
#include <string>

namespace {
    constexpr const char* _loggerCat = "PlaneGeometry";
} // namespace

namespace openspace {

PlaneGeometry::PlaneGeometry(glm::vec2 size) : _size(std::move(size)) {}

PlaneGeometry::PlaneGeometry(float size) : _size(size, size) {}

PlaneGeometry::~PlaneGeometry() {
    glDeleteBuffers(1, &_vBufferId);
    glDeleteVertexArrays(1, &_vaoId);
}

bool PlaneGeometry::initialize() {
    // Initialize and upload to GPU
    const glm::vec2 size = _size;

    struct VertexData {
        GLfloat x;
        GLfloat y;
        GLfloat s;
        GLfloat t;
    };

    VertexData vertices[] = {
        { -size.x, -size.y, 0.f, 0.f },
        {  size.x,  size.y, 1.f, 1.f },
        { -size.x,  size.y, 0.f, 1.f },
        { -size.x, -size.y, 0.f, 0.f },
        {  size.x, -size.y, 1.f, 0.f },
        {  size.x,  size.y, 1.f, 1.f },
    };

    if (_vaoId == 0) {
        glGenVertexArrays(1, &_vaoId);
    }

    if (_vBufferId == 0) {
        glGenBuffers(1, &_vBufferId);

        if (_vBufferId == 0) {
            LERROR("Could not create vertex buffer");
            return false;
        }
    }

    // First VAO setup
    glBindVertexArray(_vaoId);

    glBindBuffer(GL_ARRAY_BUFFER, _vBufferId);
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), &vertices, GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, sizeof(VertexData), nullptr);
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(
        1,
        2,
        GL_FLOAT,
        GL_FALSE,
        sizeof(VertexData),
        reinterpret_cast<void*>(offsetof(VertexData, s)) // NOLINT
    );

    glBindVertexArray(0);
    return true;
}

void PlaneGeometry::deinitialize() {
    glDeleteVertexArrays(1, &_vaoId);
    _vaoId = 0;

    glDeleteBuffers(1, &_vBufferId);
    _vBufferId = 0;
}

void PlaneGeometry::render() {
    glBindVertexArray(_vaoId);  // select first VAO
    glDrawArrays(GL_TRIANGLES, 0, 6);
    glBindVertexArray(0);
}

void PlaneGeometry::updateSize(const glm::vec2 size) {
    _size = size;
    initialize();
}

void PlaneGeometry::updateSize(const float size) {
    _size = glm::vec2(size, size);
    initialize();
}

} // namespace openspace
