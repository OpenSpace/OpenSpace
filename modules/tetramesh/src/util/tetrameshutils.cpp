/*********************************************************************************
 *
 * Inviwo - Interactive Visualization Workshop
 *
 * Copyright (c) 2023 Inviwo Foundation
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *********************************************************************************/

#include <inviwo/tetramesh/util/tetrameshutils.h>
#include <inviwo/tetramesh/datastructures/tetramesh.h>

#include <inviwo/core/datastructures/geometry/mesh.h>
#include <inviwo/core/datastructures/buffer/bufferram.h>
#include <inviwo/core/util/zip.h>

#include <glm/gtx/transform.hpp>
#include <glm/gtc/type_ptr.hpp>

#include <algorithm>
#include <unordered_map>

namespace openspace {

namespace utiltetra {

namespace detail {

struct TriangleKey {
    TriangleKey(const ivec3& indices) : tri{indices} {
        std::sort(glm::value_ptr(tri), glm::value_ptr(tri) + 3);
    }
    TriangleKey(int a, int b, int c) : tri{a, b, c} {
        std::sort(glm::value_ptr(tri), glm::value_ptr(tri) + 3);
    }

    bool operator==(const TriangleKey& rhs) const { return tri == rhs.tri; }

    ivec3 tri;
};

struct TriangleHash {
    size_t operator()(const TriangleKey& key) const {
        size_t seed = 0;
        openspace::util::hash_combine(seed, key.tri[0]);
        openspace::util::hash_combine(seed, key.tri[1]);
        openspace::util::hash_combine(seed, key.tri[2]);
        return seed;
    }
};

int globalFaceId(int tetra, int face) { return tetra * 4 + face; }

}  // namespace detail

std::vector<ivec4> getOpposingFaces(const std::vector<ivec4>& nodeIds) {
    using TetraId = int;
    using FaceId = int;

    std::unordered_map<detail::TriangleKey, std::pair<TetraId, FaceId>, detail::TriangleHash>
        adjacency;
    std::vector<ivec4> opposingFaces(nodeIds.size(), ivec4(-1));

    for (int tetra = 0; tetra < std::ssize(nodeIds); ++tetra) {
        for (int face = 0; face < 4; ++face) {
            // determine indices of half face opposing the current one
            int nextHf = (face + 1) % 4;
            int midHf = (face + 2) % 4;
            int prevHf = (face + 3) % 4;

            detail::TriangleKey tri{nodeIds[tetra][nextHf], nodeIds[tetra][midHf],
                                    nodeIds[tetra][prevHf]};

            if (auto adjIt = adjacency.find(tri); adjIt != adjacency.end()) {
                auto&& [opposingTetra, opposingFace] = adjIt->second;
                opposingFaces[tetra][face] = detail::globalFaceId(opposingTetra, opposingFace);
                opposingFaces[opposingTetra][opposingFace] = detail::globalFaceId(tetra, face);
                adjacency.erase(adjIt);
            } else {
                adjacency.insert({tri, {tetra, face}});
            }
        }
    }
    return opposingFaces;
}

std::vector<int> getBoundaryFaces(const std::vector<ivec4>& opposingFaces) {
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

std::shared_ptr<Mesh> createBoundaryMesh(const TetraMesh& tetraMesh, const std::vector<vec4>& nodes,
                                         const std::vector<ivec4>& nodeIds,
                                         const std::vector<int>& boundaryFaces) {
    std::vector<vec3> vertices;
    std::vector<int> faceIds;

    const glm::imat4x3 triIndices{ivec3{1, 2, 3}, ivec3{2, 0, 3}, ivec3{3, 0, 1}, ivec3{0, 2, 1}};

    for (const auto& faceId : boundaryFaces) {
        const int tetraId = faceId / 4;
        ivec3 triIndex = triIndices[faceId % 4];

        for (int i = 0; i < 3; ++i) {
            vertices.push_back(vec3{nodes[nodeIds[tetraId][triIndex[i]]]});
            faceIds.push_back(faceId);
        }
    }

    std::vector<std::uint32_t> indices;
    util::append(indices, util::make_sequence<std::uint32_t>(
                              0, static_cast<std::uint32_t>(vertices.size())));

    auto mesh = std::make_shared<Mesh>(DrawType::Triangles, ConnectivityType::None);
    mesh->addBuffer(BufferType::PositionAttrib, util::makeBuffer(std::move(vertices)));
    mesh->addBuffer(BufferType::IndexAttrib, util::makeBuffer(std::move(faceIds)));
    mesh->addIndices(Mesh::MeshInfo(DrawType::Triangles, ConnectivityType::None),
                     util::makeIndexBuffer(std::move(indices)));
    // copy all transformations to the boundary mesh
    mesh->SpatialEntity<3>::operator=(tetraMesh);
    return mesh;
}

std::shared_ptr<Mesh> createBoundaryMesh(const TetraMesh& mesh) {
    std::vector<vec4> nodes;
    std::vector<ivec4> nodeIds;
    mesh.get(nodes, nodeIds);
    return createBoundaryMesh(mesh, nodes, nodeIds, getBoundaryFaces(getOpposingFaces(nodeIds)));
}

void fixFaceOrientation(const std::vector<vec4>& nodes, std::vector<ivec4>& nodeIds) {
    for (auto& ids : nodeIds) {
        vec3 v0{nodes[ids[0]]};
        vec3 v1{nodes[ids[1]]};
        vec3 v2{nodes[ids[2]]};
        vec3 v3{nodes[ids[3]]};

        // check if tetrahedron is inside out, that is two indices are swapped
        if (glm::dot(v3 - v0, glm::cross(v2 - v0, v1 - v0)) > 0.0f) {
            // vertex 3 is facing the front-face of triangle 0-2-1
            std::swap(ids[2], ids[3]);
        }
    }
}

}  // namespace utiltetra

}  // namespace openspace
