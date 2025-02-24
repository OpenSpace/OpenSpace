/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <modules/globebrowsing/src/skirtedgrid.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>

namespace {
    size_t numElements(int xSegments, int ySegments) {
        return 3 * 2 * xSegments * ySegments;
    }

    size_t numVertices(int xSegments, int ySegments) {
        return (xSegments + 1) * (ySegments + 1);
    }

    void validate([[maybe_unused]] int xSegments, [[maybe_unused]] int ySegments) {
        ghoul_assert(
            xSegments > 0 && ySegments > 0,
            "Resolution must be at least 1x1. (" + std::to_string(xSegments) + ", " +
            std::to_string(ySegments) + ")"
        );
    }

    std::vector<GLushort> createElements(int xSegments, int ySegments) {
        validate(xSegments, ySegments);

        std::vector<GLushort> elements;
        elements.reserve(numElements(xSegments + 2, ySegments + 2));
        for (int y = 0; y < ySegments + 2; y++) {
            for (int x = 0; x < xSegments + 2; x++) {

                // x    v01---v11   x ..
                //       |  /  |
                // x    v00---v10   x ..
                //
                // x    x     x     x ..
                // :    :     :     :

                const GLuint v00 = (y + 0) * (xSegments + 2 + 1) + x + 0;
                const GLuint v10 = (y + 0) * (xSegments + 2 + 1) + x + 1;
                const GLuint v01 = (y + 1) * (xSegments + 2 + 1) + x + 0;
                const GLuint v11 = (y + 1) * (xSegments + 2 + 1) + x + 1;

                // add upper triangle
                elements.push_back(static_cast<GLushort>(v00));
                elements.push_back(static_cast<GLushort>(v10));
                elements.push_back(static_cast<GLushort>(v11));

                // add lower triangle
                elements.push_back(static_cast<GLushort>(v00));
                elements.push_back(static_cast<GLushort>(v11));
                elements.push_back(static_cast<GLushort>(v01));
            }
        }

        return elements;
    }

    std::vector<glm::vec2> createTextureCoordinates(int xSegments, int ySegments) {
        validate(xSegments, ySegments);

        std::vector<glm::vec2> textureCoordinates;
        textureCoordinates.reserve(numVertices(xSegments + 2, ySegments + 2));
        for (int y = -1; y < ySegments + 2; y++) {
            for (int x = -1; x < xSegments + 2; x++) {
                textureCoordinates.emplace_back(
                    std::clamp(
                        static_cast<float>(x) / static_cast<float>(xSegments),
                        0.f - 1.f / (2.f * xSegments),
                        1.f + 1.f / (2.f * xSegments)
                    ),
                    std::clamp(
                        static_cast<float>(y) / static_cast<float>(ySegments),
                        0.f - 1.f / (2.f * ySegments),
                        1.f + 1.f / (2.f * ySegments)
                    )
                );
            }
        }
        return textureCoordinates;
    }

} // namespace

namespace openspace::globebrowsing {

SkirtedGrid::SkirtedGrid(unsigned int xSeg, unsigned int ySeg)
    : xSegments(xSeg)
    , ySegments(ySeg)
    , _elementSize(static_cast<GLsizei>(numElements(xSegments + 2, ySegments + 2)))
{}

void SkirtedGrid::initializeGL() {
    std::vector<GLushort> elementData = createElements(xSegments, ySegments);

    struct Vertex {
        std::array<GLfloat, 2> texture;
    };


    std::vector<glm::vec2> textures = createTextureCoordinates(xSegments, ySegments);
    std::vector<Vertex> vertexData(textures.size());
    for (size_t i = 0; i < textures.size(); i++) {
        vertexData[i].texture[0] = textures[i][0];
        vertexData[i].texture[1] = textures[i][1];
    }


    glGenVertexArrays(1, &_vaoID);
    glGenBuffers(1, &_vertexBufferID);
    glGenBuffers(1, &_elementBufferID);

    // First VAO setup
    glBindVertexArray(_vaoID);

    // Vertex buffer
    glBindBuffer(GL_ARRAY_BUFFER, _vertexBufferID);
    glBufferData(GL_ARRAY_BUFFER,
        vertexData.size() * sizeof(Vertex),
        vertexData.data(),
        GL_STATIC_DRAW
    );

    // Textures at location 1
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, sizeof(Vertex), nullptr);

    // Element buffer
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _elementBufferID);
    glBufferData(
        GL_ELEMENT_ARRAY_BUFFER,
        elementData.size() * sizeof(GLushort),
        elementData.data(),
        GL_STATIC_DRAW
    );

    glBindVertexArray(0);

    ghoul_assert(
        static_cast<int>(elementData.size()) == _elementSize,
        "Wrong element size. The correct number is assumed in the render method"
    );
}

void SkirtedGrid::deinitializeGL() {
    glDeleteBuffers(1, &_vertexBufferID);
    glDeleteBuffers(1, &_elementBufferID);
    glDeleteVertexArrays(1, &_vaoID);
}

void SkirtedGrid::drawUsingActiveProgram() const {
    glBindVertexArray(_vaoID);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _elementBufferID);
    glDrawElements(GL_TRIANGLES, _elementSize, GL_UNSIGNED_SHORT, nullptr);
    glBindVertexArray(0);
}

} // namespace openspace::globebrowsing
