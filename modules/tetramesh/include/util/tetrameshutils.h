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

struct VoxelData {
    VoxelData() = default;
    VoxelData(float x, float y, float z, float v) : position{ x,y,z }, value{ v } {}
    glm::vec3 position = glm::vec3(0.f);
    float value = 0.f;
};

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
