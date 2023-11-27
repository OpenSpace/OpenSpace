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

#include <inviwo/tetramesh/tetrameshmoduledefine.h>

#include <inviwo/core/util/glmvec.h>
#include <modules/opengl/buffer/bufferobject.h>

#include <vector>

namespace openspace {

class TetraMesh;

/**
 * This is an OpenGL representation of the data required for rendering tetrahedral meshes on the GPU
 * using GLSL shaders. The data is stored in Shader Storage Buffers.
 */
struct IVW_MODULE_TETRAMESH_API TetraMeshBuffers {
    TetraMeshBuffers();

    /**
     * Bind the Shader Storage Buffers and tie them to specific layout locations. They can then be
     * accessed in the shader as follows:
     *
     * \code{.glsl}
     * struct VertexPosition {
     *     vec3 pos;
     *     float scalar;
     * };
     *
     * layout(std430, binding=0) readonly buffer nodeBuffer {
     *   VertexPosition vertexPositions[];
     * };
     * layout(std430, binding=1) readonly buffer nodeIdsBuffer {
     *    ivec4 vertexIds[];
     * };
     * layout(std430, binding=2) readonly buffer opposingFaceIdsBuffer {
     *     ivec4 faceIds[];
     * };
     * \endcode
     *
     * \see glBindBufferBase
     */
    void bind() const;
    /**
     * Unbind the Shader Storage Buffer.
     */
    void unbind() const;

    /**
     * Update the buffer contents of the Shader Storage Buffers on the GPU.
     */
    void upload(const TetraMesh& mesh);

    /**
     * @copydoc upload(const TetraMesh&)
     * @see upload(const TetraMesh&)
     */
    void upload(const std::vector<vec4>& nodes, const std::vector<ivec4>& nodeIds,
                const std::vector<ivec4>& opposingFaces);

    /**
     * The nodes buffer holds the 3D coordinates of each node along with its scalar value (vec4).
     * The scalar is stored in the w component.
     */
    BufferObject nodesBuffer;
    /**
     * A list of node/vertex IDs for each tetrahedron (ivec4). The faces opposite of each node are
     * implicitly encoded.
     */
    BufferObject nodeIdsBuffer;
    /**
     * The four IDs of the opposing half faces of a single tetrahedron are stored in an ivec4. The
     * order matches the one of the nodeIds so that the corresponding node is the apex of the face.
     * A negative index indicates a boundary face with no opposing half face.
     */
    BufferObject opposingFaceIdsBuffer;
};

}  // namespace openspace
