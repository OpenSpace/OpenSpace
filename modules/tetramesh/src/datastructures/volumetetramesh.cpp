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

#include <inviwo/tetramesh/datastructures/volumetetramesh.h>

#include <inviwo/core/datastructures/volume/volume.h>
#include <inviwo/core/datastructures/volume/volumeram.h>
#include <inviwo/core/util/glmcomp.h>
#include <inviwo/core/util/zip.h>
#include <inviwo/core/util/indexmapper.h>
#include <inviwo/core/util/exception.h>

#include <glm/gtx/component_wise.hpp>

#include <span>

namespace openspace {

namespace detail {

mat4 tetraBoundingBox(const Volume& volume) {
    mat3 basis{volume.getBasis()};
    vec3 offset{volume.getOffset()};
    // adjust basis and offset by half a voxel inward since the tetramesh nodes are located in
    // between voxels
    size3_t dims{volume.getDimensions()};
    mat3 voxelOffset{basis[0] / static_cast<float>(dims.x), basis[1] / static_cast<float>(dims.y),
                     basis[2] / static_cast<float>(dims.z)};

    basis -= voxelOffset;
    offset += (voxelOffset[0] + voxelOffset[1] + voxelOffset[2]) * 0.5f;

    mat4 bbox{basis};
    bbox[3] = vec4{offset, 1.0f};

    return bbox;
}

}  // namespace detail

VolumeTetraMesh::VolumeTetraMesh(const std::shared_ptr<const Volume>& volume, int channel)
    : channel_{0} {
    setData(volume, channel);
}

VolumeTetraMesh* VolumeTetraMesh::clone() const { return new VolumeTetraMesh{*this}; }

void VolumeTetraMesh::setData(const std::shared_ptr<const Volume>& volume, int channel) {
    if (volume && glm::any(glm::lessThanEqual(volume->getDimensions(), size3_t{1}))) {
        throw Exception(IVW_CONTEXT,
                        "Volumes with one or more dimensions equal to 1 cannot be converted to a "
                        "TetraMesh ({})",
                        volume->getDimensions());
    }

    volume_ = volume;
    channel_ = channel;
    setModelMatrix(detail::tetraBoundingBox(*volume_));
    setWorldMatrix(mat4(1.0f));
}

int VolumeTetraMesh::getNumberOfCells() const {
    if (volume_) {
        return static_cast<int>(glm::compMul(volume_.get()->getDimensions() - size3_t{1}) * 6);
    } else {
        return 0;
    }
}

int VolumeTetraMesh::getNumberOfPoints() const {
    if (volume_) {
        return static_cast<int>(glm::compMul(volume_.get()->getDimensions()));
    } else {
        return 0;
    }
}

void VolumeTetraMesh::get(std::vector<vec4>& nodes, std::vector<ivec4>& nodeIds) const {
    nodes.clear();
    nodeIds.clear();

    if (!volume_) {
        return;
    }

    // regular tetrahedralization with six tetrahedra per four-voxel cube with
    // node positions being voxel-centered
    const size3_t dims = volume_->getDimensions();

    // transform all coordinates to [0,1]
    const mat4 indexMatrix{glm::scale(dims - size3_t{1})};
    mat4 m = glm::inverse(indexMatrix);

    nodes.reserve(getNumberOfPoints());
    for (size_t z = 0; z < dims.z; ++z) {
        for (size_t y = 0; y < dims.y; ++y) {
            for (size_t x = 0; x < dims.x; ++x) {
                vec3 v{m * vec4{x, y, z, 1.0f}};
                nodes.emplace_back(v, 0.0f);
            }
        }
    }
    volume_->getRepresentation<VolumeRAM>()->dispatch<void>([&](const auto* vrprecision) {
        using ValueType = util::PrecisionValueType<decltype(vrprecision)>;
        const ValueType* data = vrprecision->getDataTyped();
        for (auto&& [node, value] : util::zip(nodes, std::span{data, glm::compMul(dims)})) {
            node.w = static_cast<float>(util::glmcomp(value, channel_));
        }
    });

    nodeIds.reserve(getNumberOfCells());
    util::IndexMapper<3, int> indexMapper{dims};
    for (int z = 0; z < dims.z - 1; ++z) {
        for (int y = 0; y < dims.y - 1; ++y) {
            for (int x = 0; x < dims.x - 1; ++x) {
                int v0 = indexMapper(x, y, z);
                int v1 = indexMapper(x + 1, y, z);
                int v2 = indexMapper(x, y + 1, z);
                int v3 = indexMapper(x + 1, y + 1, z);
                int v4 = indexMapper(x, y, z + 1);
                int v5 = indexMapper(x + 1, y, z + 1);
                int v6 = indexMapper(x, y + 1, z + 1);
                int v7 = indexMapper(x + 1, y + 1, z + 1);

                nodeIds.emplace_back(v0, v1, v2, v6);
                nodeIds.emplace_back(v0, v6, v4, v1);
                nodeIds.emplace_back(v1, v4, v5, v6);
                nodeIds.emplace_back(v1, v3, v2, v6);
                nodeIds.emplace_back(v1, v7, v3, v6);
                nodeIds.emplace_back(v1, v5, v7, v6);
            }
        }
    }
}

mat4 VolumeTetraMesh::getBoundingBox() const {
    return getCoordinateTransformer().getDataToWorldMatrix();
}

dvec2 VolumeTetraMesh::getDataRange() const {
    if (volume_) {
        return volume_->dataMap_.dataRange;
    } else {
        return dvec2{0.0, 1.0};
    }
}

}  // namespace openspace
