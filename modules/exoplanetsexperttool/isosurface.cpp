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

#include <modules/exoplanetsexperttool/isosurface.h>

#include <array>
#include <cmath>
#include <utility>

namespace {
    using namespace openspace::exoplanets;

    // Kuhn triangulation of a cube into six tetrahedra sharing the 0-7 diagonal. Corner
    // c has the offset (c & 1, (c >> 1) & 1, (c >> 2) & 1). Since every cell is split
    // the same way, the faces of neighboring cells match and the surface is watertight
    constexpr std::array<std::array<int, 4>, 6> Tetrahedra = {{
        { 0, 1, 3, 7 },
        { 0, 3, 2, 7 },
        { 0, 2, 6, 7 },
        { 0, 6, 4, 7 },
        { 0, 4, 5, 7 },
        { 0, 5, 1, 7 }
    }};

    void addTriangle(std::vector<glm::dvec3>& triangles, const glm::dvec3& first,
                     glm::dvec3 second, glm::dvec3 third, const glm::dvec3& outward)
    {
        const glm::dvec3 normal = glm::cross(second - first, third - first);
        if (glm::dot(normal, outward) < 0.0) {
            std::swap(second, third);
        }
        triangles.insert(triangles.end(), { first, second, third });
    }

    void polygonizeTetrahedron(std::vector<glm::dvec3>& triangles,
                               const std::array<glm::dvec3, 4>& positions,
                               const std::array<float, 4>& values, float isoValue)
    {
        std::array<int, 4> inside;
        std::array<int, 4> outside;
        int nInside = 0;
        int nOutside = 0;
        glm::dvec3 insideCenter = glm::dvec3(0.0);
        glm::dvec3 outsideCenter = glm::dvec3(0.0);
        for (int i = 0; i < 4; i++) {
            if (values[i] >= isoValue) {
                inside[nInside++] = i;
                insideCenter += positions[i];
            }
            else {
                outside[nOutside++] = i;
                outsideCenter += positions[i];
            }
        }
        if (nInside == 0 || nOutside == 0) {
            return;
        }
        const glm::dvec3 outward = outsideCenter / static_cast<double>(nOutside) -
            insideCenter / static_cast<double>(nInside);

        const auto crossing = [&](int in, int out) {
            const double t = (static_cast<double>(isoValue) - values[in]) /
                (static_cast<double>(values[out]) - values[in]);
            return glm::mix(positions[in], positions[out], t);
        };

        if (nInside == 1) {
            addTriangle(
                triangles,
                crossing(inside[0], outside[0]),
                crossing(inside[0], outside[1]),
                crossing(inside[0], outside[2]),
                outward
            );
        }
        else if (nInside == 3) {
            addTriangle(
                triangles,
                crossing(inside[0], outside[0]),
                crossing(inside[1], outside[0]),
                crossing(inside[2], outside[0]),
                outward
            );
        }
        else {
            // Consecutive corners share a tetrahedron vertex, so this is a planar loop
            const glm::dvec3 a = crossing(inside[0], outside[0]);
            const glm::dvec3 b = crossing(inside[0], outside[1]);
            const glm::dvec3 c = crossing(inside[1], outside[1]);
            const glm::dvec3 d = crossing(inside[1], outside[0]);
            addTriangle(triangles, a, b, c, outward);
            addTriangle(triangles, a, c, d, outward);
        }
    }
} // namespace

namespace openspace::exoplanets {

float VolumeGrid::interpolate(const glm::dvec3& position) const {
    const glm::dvec3 upper = glm::dvec3(dimensions) - 1.0;
    if (glm::any(glm::lessThan(dimensions, glm::uvec3(2))) ||
        glm::any(glm::lessThan(position, glm::dvec3(0.0))) ||
        glm::any(glm::greaterThan(position, upper)))
    {
        return 0.f;
    }
    const glm::uvec3 node = glm::min(glm::uvec3(position), dimensions - glm::uvec3(2));
    const glm::dvec3 t = position - glm::dvec3(node);

    const double c00 = glm::mix<double>(
        value(node.x, node.y, node.z), value(node.x + 1, node.y, node.z), t.x
    );
    const double c10 = glm::mix<double>(
        value(node.x, node.y + 1, node.z), value(node.x + 1, node.y + 1, node.z), t.x
    );
    const double c01 = glm::mix<double>(
        value(node.x, node.y, node.z + 1), value(node.x + 1, node.y, node.z + 1), t.x
    );
    const double c11 = glm::mix<double>(
        value(node.x, node.y + 1, node.z + 1),
        value(node.x + 1, node.y + 1, node.z + 1),
        t.x
    );
    const double c0 = glm::mix(c00, c10, t.y);
    const double c1 = glm::mix(c01, c11, t.y);
    return static_cast<float>(glm::mix(c0, c1, t.z));
}

std::vector<glm::dvec3> extractIsosurface(const VolumeGrid& grid, float isoValue) {
    std::vector<glm::dvec3> triangles;
    const glm::uvec3 dims = grid.dimensions;
    if (glm::any(glm::lessThan(dims, glm::uvec3(2)))) {
        return triangles;
    }

    for (unsigned int z = 0; z < dims.z - 1; z++) {
        for (unsigned int y = 0; y < dims.y - 1; y++) {
            for (unsigned int x = 0; x < dims.x - 1; x++) {
                std::array<glm::dvec3, 8> positions;
                std::array<float, 8> values;
                int nInside = 0;
                for (int c = 0; c < 8; c++) {
                    const glm::uvec3 node = glm::uvec3(
                        x + (c & 1), y + ((c >> 1) & 1), z + ((c >> 2) & 1)
                    );
                    positions[c] = glm::dvec3(node);
                    values[c] = grid.value(node.x, node.y, node.z);
                    nInside += values[c] >= isoValue ? 1 : 0;
                }
                if (nInside == 0 || nInside == 8) {
                    continue;
                }

                for (const std::array<int, 4>& tetrahedron : Tetrahedra) {
                    polygonizeTetrahedron(
                        triangles,
                        {
                            positions[tetrahedron[0]], positions[tetrahedron[1]],
                            positions[tetrahedron[2]], positions[tetrahedron[3]]
                        },
                        {
                            values[tetrahedron[0]], values[tetrahedron[1]],
                            values[tetrahedron[2]], values[tetrahedron[3]]
                        },
                        isoValue
                    );
                }
            }
        }
    }
    return triangles;
}

} // namespace openspace::exoplanets
