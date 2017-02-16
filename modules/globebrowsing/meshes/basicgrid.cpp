/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <modules/globebrowsing/meshes/basicgrid.h>

#include <ghoul/misc/assert.h>

namespace openspace {
namespace globebrowsing {

BasicGrid::BasicGrid(unsigned int xSegments, unsigned int ySegments,
                     TriangleSoup::Positions usePositions,
                     TriangleSoup::TextureCoordinates useTextureCoordinates,
                     TriangleSoup::Normals useNormals)
    : Grid(xSegments, ySegments, usePositions, useTextureCoordinates, useNormals)
{
    _geometry = std::make_unique<TriangleSoup>(
        createElements(xSegments, ySegments),
        usePositions,
        useTextureCoordinates,
        useNormals
    );

    if (usePositions) {
        _geometry->setVertexPositions(createPositions(_xSegments, _ySegments));
    }
    if (useTextureCoordinates) {
        _geometry->setVertexTextureCoordinates(
            createTextureCoordinates(_xSegments, _ySegments)
        );
    }
    if (useNormals) {
        _geometry->setVertexNormals(createNormals(_xSegments, _ySegments));
    }
}

int BasicGrid::xSegments() const {
    return _xSegments;
}

int BasicGrid::ySegments() const {
    return _ySegments;
}

void BasicGrid::validate(int xSegments, int ySegments) {
    ghoul_assert(xSegments > 0 && ySegments > 0,
        "Resolution must be at least 1x1. (" << xSegments << ", " << ySegments << ")");
}

inline size_t BasicGrid::numElements(int xSegments, int ySegments) {
    return 3 * 2 * xSegments * ySegments;
}

inline size_t BasicGrid::numVertices(int xSegments, int ySegments) {
    return (xSegments + 1) * (ySegments + 1);
}

std::vector<GLuint> BasicGrid::createElements(int xSegments, int ySegments) {
    validate(xSegments, ySegments);

    std::vector<GLuint> elements;
    elements.reserve(numElements(xSegments, ySegments));
    for (unsigned int y = 0; y < ySegments; y++) {
        for (unsigned int x = 0; x < xSegments; x++) {

            // x    v01---v11   x ..
            //       |  /  |
            // x    v00---v10   x ..
            //
            // x    x     x     x ..
            // :    :     :     :

            GLuint v00 = (y + 0) * (xSegments + 1) + x + 0;
            GLuint v10 = (y + 0) * (xSegments + 1) + x + 1;
            GLuint v01 = (y + 1) * (xSegments + 1) + x + 0;
            GLuint v11 = (y + 1) * (xSegments + 1) + x + 1;

            // add upper triangle
            elements.push_back(v00);
            elements.push_back(v10);
            elements.push_back(v11);

            // add lower triangle
            elements.push_back(v00);
            elements.push_back(v11);
            elements.push_back(v01);
        }
    }

    return elements;
}

std::vector<glm::vec4> BasicGrid::createPositions(int xSegments, int ySegments) {
    validate(xSegments, ySegments);
    std::vector<glm::vec4> positions;
    positions.reserve(numVertices(xSegments, ySegments));

    // Copy from 2d texture coordinates and use as template to create positions
    std::vector<glm::vec2> templateTextureCoords = createTextureCoordinates(
        xSegments, ySegments
    );
    for (const glm::vec2& coords : templateTextureCoords) {
        positions.push_back(glm::vec4(coords, 0.f, 1.f));
    }
    //for (unsigned int i = 0; i < templateTextureCoords.size(); i++) {
    //    positions.push_back(glm::vec4(
    //        templateTextureCoords[i],
    //        0.0f,
    //        1.0f
    //        ));
    //}
    return positions;
}

std::vector<glm::vec2> BasicGrid::createTextureCoordinates(int xSegments, int ySegments) {
    validate(xSegments, ySegments);
    std::vector<glm::vec2> textureCoordinates;
    textureCoordinates.reserve(numVertices(xSegments, ySegments));

    for (unsigned int y = 0; y < ySegments + 1; y++) {
        for (unsigned int x = 0; x < xSegments + 1; x++) {
            textureCoordinates.push_back(glm::vec2(
                static_cast<float>(x) / static_cast<float>(xSegments),
                static_cast<float>(y) / static_cast<float>(ySegments)
            ));
        }
    }
    return textureCoordinates;
}

std::vector<glm::vec3> BasicGrid::createNormals(int xSegments, int ySegments) {
    validate(xSegments, ySegments);
    std::vector<glm::vec3> normals;
    normals.reserve(numVertices(xSegments, ySegments));

    for (unsigned int y = 0; y < ySegments + 1; y++) {
        for (unsigned int x = 0; x < xSegments + 1; x++) {
            normals.push_back(glm::vec3(0, 0, 1));
        }
    }

    return normals;
}

} // namespace globebrowsing
} // namespace openspace
