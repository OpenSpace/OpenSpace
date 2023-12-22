/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#include <modules/tetramesh/include/datastructures/volumetetramesh.h>
#include <modules/tetramesh/include/util/tetrameshutils.h>
#include <glm/gtc/type_ptr.hpp>
#include <glm/gtx/transform.hpp>
#include <numeric>
#include <unordered_map>

namespace openspace {

namespace utiltetra {

namespace detail {
// Function to combine several hash values
// http://stackoverflow.com/questions/2590677/how-do-i-combine-hash-values-in-c0x
template <class T>
constexpr void hash_combine(std::size_t& seed, const T& v) noexcept {
    std::hash<T> hasher;
    seed ^= hasher(v) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
}

struct TriangleKey {
    TriangleKey(const glm::ivec3& indices) : tri{indices} {
        std::sort(glm::value_ptr(tri), glm::value_ptr(tri) + 3);
    }
    TriangleKey(int a, int b, int c) : tri{a, b, c} {
        std::sort(glm::value_ptr(tri), glm::value_ptr(tri) + 3);
    }

    bool operator==(const TriangleKey& rhs) const { return tri == rhs.tri; }

    glm::ivec3 tri;
};

struct TriangleHash {
    size_t operator()(const TriangleKey& key) const {
        size_t seed = 0;
        hash_combine(seed, key.tri[0]);
        hash_combine(seed, key.tri[1]);
        hash_combine(seed, key.tri[2]);
        return seed;
    }
};

int globalFaceId(int tetra, int face) { return tetra * 4 + face; }

}  // namespace detail

std::vector<glm::ivec4> getOpposingFaces(const std::vector<glm::ivec4>& nodeIds) {
    using TetraId = int;
    using FaceId = int;

    std::unordered_map<detail::TriangleKey, std::pair<TetraId, FaceId>,
                                                        detail::TriangleHash> adjacency;         
    std::vector<glm::ivec4> opposingFaces(nodeIds.size(), glm::ivec4(-1));

    for (int tetra = 0; tetra < std::ssize(nodeIds); ++tetra) {
        for (int face = 0; face < 4; ++face) {
            // determine indices of half face opposing the current one
            const int nextHf = (face + 1) % 4;
            const int midHf = (face + 2) % 4;
            const int prevHf = (face + 3) % 4;

            detail::TriangleKey tri{nodeIds[tetra][nextHf], nodeIds[tetra][midHf],
                                    nodeIds[tetra][prevHf]};

            if (auto adjIt = adjacency.find(tri); adjIt != adjacency.end()) {
                auto&& [opposingTetra, opposingFace] = adjIt->second;
                opposingFaces[tetra][face] = detail::globalFaceId(
                    opposingTetra,
                    opposingFace
                );
                opposingFaces[opposingTetra][opposingFace] = detail::globalFaceId(
                    tetra,
                    face
                );
                adjacency.erase(adjIt);
            } else {
                adjacency.insert({tri, {tetra, face}});
            }
        }
    }
    return opposingFaces;
}

std::vector<int> getBoundaryFaces(const std::vector<glm::ivec4>& opposingFaces) {
    std::vector<int> boundaryFaces;
    for (int tetra = 0; tetra < std::ssize(opposingFaces); ++tetra) {
        for (int face = 0; face < 4; ++face) {
            if (opposingFaces[tetra][face] < 0) {
                const int faceId = detail::globalFaceId(tetra, face);
                boundaryFaces.push_back(faceId);
            }
        }
    }
    return boundaryFaces;
}

std::shared_ptr<Mesh> createBoundaryMesh(const std::vector<glm::vec4>& nodes,
                                         const std::vector<glm::ivec4>& nodeIds,
                                         const std::vector<int>& boundaryFaces)
{
    std::vector<glm::vec3> vertices;
    std::vector<int> faceIds;

    const glm::imat4x3 triIndices{ glm::ivec3{1, 2, 3}, glm::ivec3{2, 0, 3},
                                   glm::ivec3{3, 0, 1}, glm::ivec3{0, 2, 1} };

    for (const auto& faceId : boundaryFaces) {
        const int tetraId = faceId / 4;
        glm::ivec3 triIndex = triIndices[faceId % 4];

        for (int i = 0; i < 3; ++i) {
            vertices.push_back(glm::vec3{ nodes[nodeIds[tetraId][triIndex[i]]] });
            faceIds.push_back(faceId);
        }
    }

    std::vector<std::uint32_t> indices(vertices.size());
    std::iota(indices.begin(), indices.end(), 0);

    std::shared_ptr<Mesh> mesh = std::make_shared<Mesh>(std::move(vertices),
                                                        std::move(faceIds),
                                                        std::move(indices));
    return mesh;
}

void fixFaceOrientation(const std::vector<glm::vec4>& nodes,
                        std::vector<glm::ivec4>& nodeIds) {
    for (auto& ids : nodeIds) {
        glm::vec3 v0{ nodes[ids[0]] };
        glm::vec3 v1{ nodes[ids[1]] };
        glm::vec3 v2{ nodes[ids[2]] };
        glm::vec3 v3{ nodes[ids[3]] };

        // check if tetrahedron is inside out, that is two indices are swapped
        if (glm::dot(v3 - v0, glm::cross(v2 - v0, v1 - v0)) > 0.0f) {
            // vertex 3 is facing the front-face of triangle 0-2-1
            std::swap(ids[2], ids[3]);
        }
    }
}

}  // namespace utiltetra

}  // namespace openspace
