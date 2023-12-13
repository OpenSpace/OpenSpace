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
#pragma once

#ifndef __OPENSPACE_MODULE_TETRAMESH___TETRAMESHUTILS___H__
#define __OPENSPACE_MODULE_TETRAMESH___TETRAMESHUTILS___H__

//#include <inviwo/core/util/glmvec.h>
//#include <inviwo/core/util/glmmat.h>

#include <glm/glm.hpp>
#include <vector>
#include <memory>

namespace openspace {

class VolumeTetraMesh;

namespace utiltetra {

struct Mesh {
    Mesh(std::vector<glm::vec3> _vertices,
        std::vector<int> _faceIds,
        std::vector<uint32_t> _indices) : vertices{ std::move(_vertices) },
                                          faceIds{ std::move(_faceIds) },
                                          indices{ std::move(_indices) } {}

    std::vector<glm::vec3> vertices;
    std::vector<int> faceIds;
    std::vector<std::uint32_t> indices;
};

struct TetraBufferIds {
    GLuint nodesBuffer = 0;
    GLuint nodeIdsBuffer = 0;
    GLuint opposingFaceIdsBuffer = 0;
    GLuint boundaryMeshVAO = 0;
    GLuint indicesEBO = 0;
    GLuint vertsVBO = 0;
    GLuint faceIdVBO = 0;

    void deinitialize() {
        nodesBuffer = 0;
        nodeIdsBuffer = 0;
        opposingFaceIdsBuffer = 0;
        boundaryMeshVAO = 0;
        indicesEBO = 0;
        vertsVBO = 0;
        faceIdVBO = 0;
    }
};

/**
 * Determine the opposing faces of each tetradhedron by identifying faces with shared nodes.
 * The four face IDs of a single tetrahedron are stored in an ivec4. The order matches the vertex
 * IDs in \p nodeIds so that the corresponding node is the apex of the face.
 *
 * @param nodeIds        contains four node IDs for each tetrahedron
 * @return opposing faces where a negative index indicates a boundary face, that is no neighboring
 *         tetrahedron
 */
std::vector<glm::ivec4> getOpposingFaces(const std::vector<glm::ivec4>& nodeIds);

/**
 * Derive boundary faces from the \p opposingFaces where negative IDs indicate a boundary face.
 *
 * @param opposingFaces  list of opposing face IDs
 * @return list of boundary face IDs
 */
std::vector<int> getBoundaryFaces(const std::vector<glm::ivec4>& opposingFaces);

/**
 * Create a triangular mesh from a tetrahedral mesh that consists only of the boundary faces and no
 * interior triangles. Note that holes in the tetra mesh also feature boundary faces.
 *
 * @param mesh           tetrahedra mesh used only for transformations
 * @param nodes          vertex positions of the tetrahedra
 * @param nodeIds        contains four node IDs for each tetrahedron
 * @param boundaryFaces  list of face IDs belonging to the boundary
 * @return triangle mesh representing the boundary faces
 *
 * \see getOpposingFaces
 */
std::shared_ptr<Mesh> createBoundaryMesh(
    const std::vector<glm::vec4>& nodes, const std::vector<glm::ivec4>& nodeIds,
    const std::vector<int>& boundaryFaces);

/**
 * Create a triangular mesh from a TetraMesh \p mesh that consists only of the boundary faces and no
 * interior triangles. Note that holes in the tetra mesh also feature boundary faces.
 *
 * @param mesh    tetrahedra mesh
 * @return triangle mesh representing the boundary faces
 *
 * \see getOpposingFaces
 * \see createBoundaryMesh(const TetraMesh&, const std::vector<vec4>&, const std::vector<ivec4>&,
 * const std::vector<int>&)
 */
std::shared_ptr<Mesh> createBoundaryMesh(const VolumeTetraMesh& mesh);

/**
 * Check and fix the face orientations of the tetrahedral mesh. Afterward, the faces of each
 * tetrahedra are facing outward, that is the vertices of each triangle are ordered counter-clock
 * wise.
 *
 * @param nodes     vertex positions of the tetrahedra
 * @param nodeIds   contains four node IDs for each tetrahedron which might get reordered in-place
 */
void fixFaceOrientation(const std::vector<glm::vec4>& nodes,
                                                 std::vector<glm::ivec4>& nodeIds);

}  // namespace utiltetra

}  // namespace openspace

#endif // !__OPENSPACE_MODULE_TETRAMESH___TETRAMESHUTILS___H__
