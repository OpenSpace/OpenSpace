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

#include <openspace/util/blockplaneintersectiongeometry.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <algorithm>

namespace {
    constexpr std::string_view _loggerCat = "BlockPlaneIntersectionGeometry";
} // namespace

namespace openspace {

BlockPlaneIntersectionGeometry::BlockPlaneIntersectionGeometry(glm::vec3 blockSize,
                                                               glm::vec3 planeNormal,
                                                               float planeDistance)
    : _size(std::move(blockSize))
    , _normal(std::move(planeNormal))
    , _planeDistance(planeDistance)
{}

BlockPlaneIntersectionGeometry::~BlockPlaneIntersectionGeometry() {
    glDeleteBuffers(1, &_vBufferId);
    glDeleteVertexArrays(1, &_vaoId);
}

void BlockPlaneIntersectionGeometry::setBlockSize(glm::vec3 size) {
    _size = std::move(size);
    updateVertices();
}

void BlockPlaneIntersectionGeometry::setPlane(const glm::vec3& normal, float distance) {
    _normal = glm::normalize(normal);
    _planeDistance = distance;
    updateVertices();
}

void BlockPlaneIntersectionGeometry::updateVertices() {
    _vertices.clear();

    constexpr std::array<int, 24> CornersInLines = {
        0, 1,
        1, 5,
        5, 4,
        4, 0,

        2, 3,
        3, 7,
        7, 6,
        6, 2,

        0, 2,
        1, 3,
        5, 7,
        4, 6
    };

    const glm::vec3 halfSize = _size * 0.5f;
    std::array<glm::vec3, 12> intersections;
    std::fill(intersections.begin(), intersections.end(), glm::vec3(0.f));
    int nIntersections = 0;

    for (int i = 0; i < 12; i++) {
        const int iCorner0 = CornersInLines[i * 2];
        const int iCorner1 = CornersInLines[i * 2 + 1];

        const glm::vec3 corner0 = glm::vec3(
            iCorner0 % 2,
            (iCorner0 / 2) % 2,
            iCorner0 / 4
        ) - halfSize;
        const glm::vec3 corner1 = glm::vec3(
            iCorner1 % 2,
            (iCorner1 / 2) % 2,
            iCorner1 / 4
        ) - halfSize;

        const glm::vec3 line = corner1 - corner0;

        const float t =
            (_planeDistance - glm::dot(corner0, _normal)) / glm::dot(line, _normal);
        if (t >= 0.0 && t <= 1.0) {
            intersections[nIntersections++] = corner0 + t * line;
        }
    }

    // Gotta love intersections
    if (nIntersections <3) {
        return;
    }

    // Construct vectors vectors (vectors1 .. vectorsN) between the N points

    std::vector<std::pair<int, float>> angles(nIntersections - 1);

    const glm::vec3 vec1 = glm::normalize(intersections[1] - intersections[0]);
    angles[0] = std::pair<int, float>(1, 0.f);

    for (int i = 2; i < nIntersections; i++) {
        const glm::vec3 vectorI = glm::normalize(intersections[i] - intersections[0]);
        const float sinA = glm::dot(glm::cross(vec1, vectorI), _normal);
        const float cosA = glm::dot(vec1, vectorI);
        angles[i - 1] = { i, glm::sign(sinA) * (1.f - cosA) };
    }

    // Sort the vectors by angle in the plane
    std::sort(angles.begin(), angles.end(),
        [](const std::pair<int, float>& a, const std::pair<int, float>& b) -> bool {
            return a.second < b.second;
        }
    );

    _vertices.push_back(intersections[0].x);
    _vertices.push_back(intersections[0].y);
    _vertices.push_back(intersections[0].z);
    //_vertices.push_back(_w);
    for (int i = 0; i < nIntersections - 1; i++) {
        const int j = angles[i].first;
        _vertices.push_back(intersections[j].x);
        _vertices.push_back(intersections[j].y);
        _vertices.push_back(intersections[j].z);
        //_vertices.push_back(_w);
    }

    // First VAO setup
    glBindVertexArray(_vaoId);

    glBindBuffer(GL_ARRAY_BUFFER, _vBufferId);
    glBufferData(
        GL_ARRAY_BUFFER,
        _vertices.size() * sizeof(GLfloat),
        _vertices.data(),
        GL_STATIC_DRAW
    );

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(GLfloat), nullptr);

    glBindVertexArray(0);
}

bool BlockPlaneIntersectionGeometry::initialize() {
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

    updateVertices();
    return true;
}

void BlockPlaneIntersectionGeometry::render() {
    glBindVertexArray(_vaoId);
    //glDisable(GL_CULL_FACE);
    glDrawArrays(GL_TRIANGLE_FAN, 0, static_cast<GLsizei>(_vertices.size() / 3));
    //glEnable(GL_CULL_FACE);
}

} // namespace openspace
