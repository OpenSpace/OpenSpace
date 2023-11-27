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

#include <inviwo/tetramesh/processors/tetrameshboundingbox.h>
#include <inviwo/tetramesh/datastructures/tetramesh.h>
#include <modules/base/algorithm/meshutils.h>

namespace openspace {

// The Class Identifier has to be globally unique. Use a reverse DNS naming scheme
const ProcessorInfo TetraMeshBoundingBox::processorInfo_{
    "org.inviwo.TetraMeshBoundingBox",  // Class identifier
    "TetraMesh Bounding Box",           // Display name
    "Unstructured Grids",               // Category
    CodeState::Stable,                  // Code state
    Tag::CPU | Tag{"Unstructured"},     // Tags
    R"(Creates a mesh containing the bounding box of the tetra mesh, that is lines with adjacency "
    "information.)"_unindentHelp};

const ProcessorInfo TetraMeshBoundingBox::getProcessorInfo() const { return processorInfo_; }

TetraMeshBoundingBox::TetraMeshBoundingBox()
    : Processor{}
    , tetraMesh_("tetramesh", "Tetra mesh data"_help)
    , mesh_("mesh", "The bounding box mesh"_help)
    , color_("color", "Color", util::ordinalColor(vec4{1.0f}).set("Line color of the mesh"_help)) {

    addPorts(tetraMesh_, mesh_);
    addProperty(color_);
}

void TetraMeshBoundingBox::process() {
    auto mesh = meshutil::boundingBoxAdjacency(tetraMesh_.getData()->getModelMatrix(), color_);
    mesh->setWorldMatrix(tetraMesh_.getData()->getWorldMatrix());
    mesh_.setData(mesh);
}

}  // namespace openspace
