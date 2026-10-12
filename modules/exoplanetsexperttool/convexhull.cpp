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

#include <modules/exoplanetsexperttool/convexhull.h>

#include <algorithm>
#include <array>
#include <cmath>
#include <numeric>
#include <set>
#include <utility>

namespace {
    // Relative to the extent of the point set
    constexpr double RelativeTolerance = 1e-9;

    struct Face {
        std::array<int, 3> vertices;
        glm::dvec3 normal;
        double offset;
    };

    // Orients the face away from interior, which stays inside the hull while it grows
    std::optional<Face> makeFace(const std::vector<glm::dvec3>& points,
                                 const glm::dvec3& interior, int a, int b, int c)
    {
        glm::dvec3 normal = glm::cross(points[b] - points[a], points[c] - points[a]);
        const double length = glm::length(normal);
        if (!(length > 0.0)) {
            return std::nullopt;
        }
        normal /= length;
        Face face = { { a, b, c }, normal, glm::dot(normal, points[a]) };
        if (glm::dot(face.normal, interior) > face.offset) {
            std::swap(face.vertices[1], face.vertices[2]);
            face.normal = -face.normal;
            face.offset = -face.offset;
        }
        return face;
    }
} // namespace

namespace openspace::exoplanets {

bool ConvexHull::contains(const glm::dvec3& point) const {
    if (planes.empty()) {
        return false;
    }
    for (const glm::dvec4& plane : planes) {
        if (glm::dot(glm::dvec3(plane), point) - plane.w > tolerance) {
            return false;
        }
    }
    return true;
}

std::optional<ConvexHull> computeConvexHull(const std::vector<glm::dvec3>& points) {
    if (points.size() < 4) {
        return std::nullopt;
    }

    // Initial tetrahedron: the two axis-extreme points farthest apart, then the point
    // farthest from their line, then the point farthest from that plane
    std::array<int, 6> extremes = { 0, 0, 0, 0, 0, 0 };
    for (int i = 0; i < static_cast<int>(points.size()); i++) {
        for (int axis = 0; axis < 3; axis++) {
            if (points[i][axis] < points[extremes[2 * axis]][axis]) {
                extremes[2 * axis] = i;
            }
            if (points[i][axis] > points[extremes[2 * axis + 1]][axis]) {
                extremes[2 * axis + 1] = i;
            }
        }
    }
    int i0 = 0;
    int i1 = 0;
    double maxDistance = 0.0;
    for (int a : extremes) {
        for (int b : extremes) {
            const double distance = glm::distance(points[a], points[b]);
            if (distance > maxDistance) {
                maxDistance = distance;
                i0 = a;
                i1 = b;
            }
        }
    }
    const double tolerance = RelativeTolerance * maxDistance;
    if (!(maxDistance > 0.0)) {
        return std::nullopt;
    }

    const glm::dvec3 direction = (points[i1] - points[i0]) / maxDistance;
    int i2 = 0;
    maxDistance = 0.0;
    for (int i = 0; i < static_cast<int>(points.size()); i++) {
        const double distance =
            glm::length(glm::cross(points[i] - points[i0], direction));
        if (distance > maxDistance) {
            maxDistance = distance;
            i2 = i;
        }
    }
    if (maxDistance <= tolerance) {
        return std::nullopt;
    }

    const glm::dvec3 normal = glm::normalize(
        glm::cross(points[i1] - points[i0], points[i2] - points[i0])
    );
    int i3 = 0;
    maxDistance = 0.0;
    for (int i = 0; i < static_cast<int>(points.size()); i++) {
        const double distance = std::abs(glm::dot(points[i] - points[i0], normal));
        if (distance > maxDistance) {
            maxDistance = distance;
            i3 = i;
        }
    }
    if (maxDistance <= tolerance) {
        return std::nullopt;
    }

    const glm::dvec3 interior =
        (points[i0] + points[i1] + points[i2] + points[i3]) / 4.0;

    std::vector<Face> faces;
    for (const std::array<int, 3>& f : std::array<std::array<int, 3>, 4>{{
        { i0, i1, i2 }, { i0, i1, i3 }, { i0, i2, i3 }, { i1, i2, i3 }
    }})
    {
        faces.push_back(*makeFace(points, interior, f[0], f[1], f[2]));
    }

    // Adding far points first means fewer faces are created only to be removed again
    std::vector<int> order(points.size());
    std::iota(order.begin(), order.end(), 0);
    std::sort(
        order.begin(), order.end(),
        [&](int a, int b) {
            return glm::distance(points[a], interior) > glm::distance(points[b], interior);
        }
    );

    std::vector<Face> kept;
    std::set<std::pair<int, int>> visibleEdges;
    for (int p : order) {
        if (p == i0 || p == i1 || p == i2 || p == i3) {
            continue;
        }

        kept.clear();
        visibleEdges.clear();
        for (const Face& face : faces) {
            if (glm::dot(face.normal, points[p]) - face.offset > tolerance) {
                const std::array<int, 3>& v = face.vertices;
                visibleEdges.insert({ v[0], v[1] });
                visibleEdges.insert({ v[1], v[2] });
                visibleEdges.insert({ v[2], v[0] });
            }
            else {
                kept.push_back(face);
            }
        }
        if (visibleEdges.empty()) {
            continue;
        }

        // The horizon consists of the visible edges that are not shared by two visible
        // faces, i.e. whose reverse edge belongs to a face that is kept
        for (const std::pair<int, int>& edge : visibleEdges) {
            if (!visibleEdges.contains({ edge.second, edge.first })) {
                std::optional<Face> face =
                    makeFace(points, interior, edge.first, edge.second, p);
                if (face.has_value()) {
                    kept.push_back(*face);
                }
            }
        }
        std::swap(faces, kept);
    }

    ConvexHull hull;
    hull.tolerance = tolerance;
    hull.planes.reserve(faces.size());
    hull.triangles.reserve(3 * faces.size());
    for (const Face& face : faces) {
        hull.planes.emplace_back(face.normal, face.offset);
        for (int v : face.vertices) {
            hull.triangles.push_back(points[v]);
        }
    }
    return hull;
}

} // namespace openspace::exoplanets
