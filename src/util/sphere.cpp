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

#include <openspace/util/sphere.h>

#include <ghoul/logging/logmanager.h>
#include <cstddef>
#include <cstring>
#include <string_view>
#include <vector>

namespace openspace {

Sphere::Sphere(float radius, int segments)
    : Sphere(glm::vec3(radius), segments)
{}

Sphere::Sphere(glm::vec3 radius, int segments)
    : _isize(6 * segments * segments)
    , _vsize((segments + 1) * (segments + 1))
    , _radius(std::move(radius))
    , _nSegments(segments)
{}

Sphere::~Sphere() {
    glDeleteBuffers(1, &_vbo);
    glDeleteBuffers(1, &_ibo);
    glDeleteVertexArrays(1, &_vao);
}

void Sphere::initialize() {
    std::vector<Vertex> vertices;
    vertices.resize(_vsize);
    std::vector<int> indices;
    indices.resize(_isize);

    int nr = 0;
    const float fsegments = static_cast<float>(_nSegments);

    for (int i = 0; i <= _nSegments; i++) {
        // Define an extra vertex around the y-axis due to texture mapping
        for (int j = 0; j <= _nSegments; j++) {
            const float fi = static_cast<float>(i);
            const float fj = static_cast<float>(j);
            // Inclination angle (north to south)
            const float theta = fi * glm::pi<float>() / fsegments;  // 0 -> PI
            // Azimuth angle (east to west)
            const float phi = fj * glm::pi<float>() * 2.f / fsegments;  // 0 -> 2*PI

            // Spherical coordinates based on ISO standard.
            // https://en.wikipedia.org/wiki/Spherical_coordinate_system

            // Z points towards pole (theta = 0)
            const float x = _radius[0] * std::sin(theta) * std::cos(phi);
            const float y = _radius[1] * std::sin(theta) * std::sin(phi);
            const float z = _radius[2] * std::cos(theta);

            vertices[nr].location[0] = x;
            vertices[nr].location[1] = y;
            vertices[nr].location[2] = z;
            vertices[nr].location[3] = 0.0;

            glm::vec3 normal = glm::vec3(x, y, z);
            if (x != 0.f || y != 0.f || z != 0.f) {
                normal = glm::vec3(glm::normalize(glm::dvec3(normal)));
            }

            vertices[nr].normal[0] = normal[0];
            vertices[nr].normal[1] = normal[1];
            vertices[nr].normal[2] = normal[2];

            const float t1 = fj / fsegments;
            const float t2 = 1.f - (fi / fsegments);

            vertices[nr].tex[0] = t1;
            vertices[nr].tex[1] = t2;
            nr++;
        }
    }

    nr = 0;
    // Define indices for all triangles
    for (int i = 1; i <= _nSegments; i++) {
        for (int j = 0; j < _nSegments; j++) {
            const int t = _nSegments + 1;
            indices[nr] = t * (i - 1) + j + 0; //1
            nr++;
            indices[nr] = t * (i + 0) + j + 0; //2
            nr++;
            indices[nr] = t * (i + 0) + j + 1; //3
            nr++;

            indices[nr] = t * (i - 1) + j + 0; //4
            nr++;
            indices[nr] = t * (i + 0) + j + 1; //5
            nr++;
            indices[nr] = t * (i - 1) + j + 1; //6
            nr++;
        }
    }

    glCreateBuffers(1, &_vbo);
    glNamedBufferStorage(_vbo, _vsize * sizeof(Vertex), vertices.data(), GL_NONE_BIT);

    glCreateBuffers(1, &_ibo);
    glNamedBufferStorage(_ibo, _isize * sizeof(int), indices.data(), GL_NONE_BIT);

    glCreateVertexArrays(1, &_vao);
    glVertexArrayVertexBuffer(_vao, 0, _vbo, 0, sizeof(Vertex));
    glVertexArrayElementBuffer(_vao, _ibo);

    glEnableVertexArrayAttrib(_vao, 0);
    glVertexArrayAttribFormat(_vao, 0, 4, GL_FLOAT, GL_FALSE, 0);
    glVertexArrayAttribBinding(_vao, 0, 0);
    glVertexArrayAttribBinding(_vao, 0, 0);

    glEnableVertexArrayAttrib(_vao, 1);
    glVertexArrayAttribFormat(_vao, 1, 2, GL_FLOAT, GL_FALSE, offsetof(Vertex, tex));
    glVertexArrayAttribBinding(_vao, 1, 0);

    glEnableVertexArrayAttrib(_vao, 2);
    glVertexArrayAttribFormat(_vao, 2, 3, GL_FLOAT, GL_FALSE, offsetof(Vertex, normal));
    glVertexArrayAttribBinding(_vao, 2, 0);
}

void Sphere::render() const {
    glBindVertexArray(_vao);
    glDrawElements(GL_TRIANGLES, _isize, GL_UNSIGNED_INT, nullptr);
    glBindVertexArray(0);
}

} // namespace openspace
