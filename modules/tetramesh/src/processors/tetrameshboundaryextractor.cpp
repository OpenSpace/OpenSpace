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

#include <inviwo/tetramesh/processors/tetrameshboundaryextractor.h>
#include <inviwo/tetramesh/datastructures/tetramesh.h>
#include <inviwo/tetramesh/util/tetrameshutils.h>

#include <inviwo/core/datastructures/buffer/buffer.h>

namespace openspace {

// The Class Identifier has to be globally unique. Use a reverse DNS naming scheme
const ProcessorInfo TetraMeshBoundaryExtractor::processorInfo_{
    "org.inviwo.TetraMeshBoundaryExtractor",  // Class identifier
    "TetraMesh Boundary Extractor",           // Display name
    "Unstructured Grids",                     // Category
    CodeState::Stable,                        // Code state
    Tag::CPU | Tag{"Unstructured"},           // Tags
    R"(Extracts the boundary faces of a tetrahedral mesh)"_unindentHelp};

const ProcessorInfo TetraMeshBoundaryExtractor::getProcessorInfo() const { return processorInfo_; }

TetraMeshBoundaryExtractor::TetraMeshBoundaryExtractor()
    : Processor{}
    , inport_{"inport", "Input tetra mesh data used for extracting the boundary faces"_help}
    , outport_{"outport",
               "Triangular mesh consisting only of the boundary faces and no interior triangles. Note that holes in the tetra mesh also feature boundary faces."_help}
    , meshColor_{
          "meshColor", "Mesh Color",
          util::ordinalColor(vec4{1.0f, 0.7f, 0.2f, 1.0f}).set("Color of the boundary mesh"_help)} {

    addPorts(inport_, outport_);
    addProperties(meshColor_);
}

void TetraMeshBoundaryExtractor::process() {
    auto mesh = utiltetra::createBoundaryMesh(*inport_.getData());

    if (mesh && !mesh->hasBuffer(BufferType::ColorAttrib)) {
        mesh = std::shared_ptr<Mesh>(mesh->clone());
        if (auto posBuffer = mesh->getBuffer(BufferType::PositionAttrib); posBuffer) {
            std::vector<vec4> colors(posBuffer->getSize(), meshColor_.get());
            mesh->addBuffer(BufferType::ColorAttrib, util::makeBuffer(std::move(colors)));
        }
    }
    outport_.setData(mesh);
}

}  // namespace openspace
