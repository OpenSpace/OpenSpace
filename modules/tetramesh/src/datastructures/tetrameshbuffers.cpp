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

#include <inviwo/tetramesh/datastructures/tetrameshbuffers.h>
#include <inviwo/tetramesh/datastructures/tetramesh.h>
#include <inviwo/tetramesh/util/tetrameshutils.h>

namespace openspace {

TetraMeshBuffers::TetraMeshBuffers()
    : nodesBuffer{0, GLFormats::getGLFormat(GL_FLOAT, 4), GL_DYNAMIC_DRAW, GL_SHADER_STORAGE_BUFFER}
    , nodeIdsBuffer{0, GLFormats::getGLFormat(GL_INT, 4), GL_DYNAMIC_DRAW, GL_SHADER_STORAGE_BUFFER}
    , opposingFaceIdsBuffer{0, GLFormats::getGLFormat(GL_INT, 4), GL_DYNAMIC_DRAW,
                            GL_SHADER_STORAGE_BUFFER} {}

void TetraMeshBuffers::bind() const {
    nodesBuffer.bindBase(0);
    nodeIdsBuffer.bindBase(1);
    opposingFaceIdsBuffer.bindBase(2);
}

void TetraMeshBuffers::unbind() const { glBindBuffer(GL_SHADER_STORAGE_BUFFER, 0); }

void TetraMeshBuffers::upload(const TetraMesh& mesh) {
    std::vector<vec4> nodes;
    std::vector<ivec4> nodeIds;
    mesh.get(nodes, nodeIds);
    upload(nodes, nodeIds, utiltetra::getOpposingFaces(nodeIds));
}

void TetraMeshBuffers::upload(const std::vector<vec4>& nodes, const std::vector<ivec4>& nodeIds,
                              const std::vector<ivec4>& opposingFaces) {
    nodesBuffer.upload(nodes, BufferObject::SizePolicy::ResizeToFit);
    nodeIdsBuffer.upload(nodeIds, BufferObject::SizePolicy::ResizeToFit);
    opposingFaceIdsBuffer.upload(opposingFaces, BufferObject::SizePolicy::ResizeToFit);
    unbind();
}

}  // namespace openspace
