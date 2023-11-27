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

#include <inviwo/tetramesh/processors/volumetotetramesh.h>
#include <inviwo/tetramesh/datastructures/volumetetramesh.h>

#include <inviwo/core/datastructures/volume/volume.h>

namespace openspace {

// The Class Identifier has to be globally unique. Use a reverse DNS naming scheme
const ProcessorInfo VolumeToTetraMesh::processorInfo_{
    "org.inviwo.VolumeToTetraMesh",                   // Class identifier
    "Volume To TetraMesh",                            // Display name
    "Unstructured Grids",                             // Category
    CodeState::Experimental,                          // Code state
    Tags::CPU | Tag{"Volume"} | Tag{"Unstructured"},  // Tags
    R"(Use an Inviwo Volume as input for an Inviwo TetraMesh)"_unindentHelp};

const ProcessorInfo VolumeToTetraMesh::getProcessorInfo() const { return processorInfo_; }

namespace {

const std::vector<OptionPropertyIntOption> channelsList = {{"channel1", "Channel 1", 0},
                                                           {"channel2", "Channel 2", 0},
                                                           {"channel3", "Channel 3", 0},
                                                           {"channel4", "Channel 4", 0}};

}

VolumeToTetraMesh::VolumeToTetraMesh()
    : Processor{}
    , inport_{"volume", "Volume dataset to be represented as tetrahedral mesh"_help}
    , outport_{"tetramesh", "Tetrahedral mesh information extracted from the input volume"_help}
    , channel_("channel", "Scalar Values", channelsList, 0) {

    addPorts(inport_, outport_);
    addProperties(channel_);

    inport_.onChange([this]() {
        if (inport_.hasData()) {
            const auto channels = inport_.getData()->getDataFormat()->getComponents();
            if (channels != channel_.size()) {
                channel_.replaceOptions(std::vector<OptionPropertyIntOption>{
                    channelsList.begin(), channelsList.begin() + channels});
                channel_.setCurrentStateAsDefault();
            }
        }
    });
}

void VolumeToTetraMesh::process() {
    auto tetraMesh =
        std::make_shared<VolumeTetraMesh>(inport_.getData(), channel_.getSelectedValue());

    outport_.setData(tetraMesh);
}

}  // namespace openspace
