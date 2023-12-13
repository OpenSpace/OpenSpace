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


#include <modules/tetramesh/include/datastructures/volumetetramesh.h>
#include <glm/gtx/component_wise.hpp>
#include <glm/gtx/transform.hpp>
#include <ghoul/logging/logmanager.h>
#include <ghoul/fmt.h>
#include <span>

namespace {
    constexpr std::string_view _loggerCat = "VolumeTetraMesh";
}

namespace openspace {

//namespace detail {


glm::mat4 VolumeTetraMesh::tetraBoundingBox() const {
    // TODO: convert to make it work 
    glm::mat3 basis{ 1.f };
    glm::vec3 offset{ -0.5f };
    // adjust basis and offset by half a voxel inward since the tetramesh nodes are located in
    // between voxels
    glm::uvec3 dims{ _volume.get()->dimensions() };
    glm::mat3 voxelOffset{basis[0] / static_cast<float>(dims.x), basis[1] / static_cast<float>(dims.y),
                     basis[2] / static_cast<float>(dims.z)};

    basis -= voxelOffset;
    offset += (voxelOffset[0] + voxelOffset[1] + voxelOffset[2]) * 0.5f;

    glm::mat4 bbox{basis};
    bbox[3] = glm::vec4{offset, 1.0f};

    return bbox;
}

//}  // namespace detail

VolumeTetraMesh::VolumeTetraMesh(const std::shared_ptr<const volume::RawVolume<gaiavolume::GaiaVolumeDataLayout>>& volume, int channel)
    : _channel{0} {
    setData(volume, channel);
}

void VolumeTetraMesh::setData(const std::shared_ptr<const volume::RawVolume<gaiavolume::GaiaVolumeDataLayout>>& volume, int channel) {
    if (volume && glm::any(glm::lessThanEqual(volume->dimensions(), glm::uvec3{ 1 }))) {
        throw ghoul::RuntimeError(fmt::format("{}: Volumes with one or more dimensions "
            "equal to 1 cannot be converted to a TetraMesh ({},{},{})", _loggerCat,
            volume->dimensions().x, volume->dimensions().y, volume->dimensions().z)
        );
    }

    _volume = volume;
    _channel = channel;
    // TODO investigate here inviwo set model and world matricies
}

int VolumeTetraMesh::getNumberOfCells() const {
    if (_volume) {
        return static_cast<int>(
            glm::compMul(_volume.get()->dimensions() - glm::uvec3{ 1 }) * 6);
    } else {
        return 0;
    }
}

int VolumeTetraMesh::getNumberOfPoints() const {
    if (_volume) {
        return static_cast<int>(_volume.get()->nCells());
    } else {
        return 0;
    }
}

void VolumeTetraMesh::get(std::vector<glm::vec4>& nodes, std::vector<glm::ivec4>& nodeIds) const {
    nodes.clear();
    nodeIds.clear();

    if (!_volume) {
        return;
    }

    // regular tetrahedralization with six tetrahedra per four-voxel cube with
    // node positions being voxel-centered
    const glm::uvec3 dims = _volume->dimensions();

    // transform all coordinates to [0,1]
    const glm::mat4 indexMatrix{ glm::scale(dims - glm::uvec3{1}) };
    glm::mat4 m = glm::inverse(indexMatrix);

    nodes.reserve(getNumberOfPoints());
    for (size_t z = 0; z < dims.z; ++z) {
        for (size_t y = 0; y < dims.y; ++y) {
            for (size_t x = 0; x < dims.x; ++x) {
                glm::vec3 v{ m * glm::vec4{x, y, z, 1.0f} };
                float value = 0.f;
                auto voxel = _volume->get(glm::uvec3{ x,y,z });
                if (voxel.containData()) {
                    value = voxel.data[3].avgData;
                }
                // TODO: temporary only!
                nodes.emplace_back(v, value);
            }
        }
    }

    nodeIds.reserve(getNumberOfCells());
    for (int z = 0; z < dims.z - 1; ++z) {
        for (int y = 0; y < dims.y - 1; ++y) {
            for (int x = 0; x < dims.x - 1; ++x) {
                size_t v0 = _volume->coordsToIndex(glm::uvec3{ x, y, z });
                size_t v1 = _volume->coordsToIndex(glm::uvec3{ x + 1, y, z });
                size_t v2 = _volume->coordsToIndex(glm::uvec3{ x, y + 1, z });
                size_t v3 = _volume->coordsToIndex(glm::uvec3{ x + 1, y + 1, z });
                size_t v4 = _volume->coordsToIndex(glm::uvec3{ x, y, z + 1 });
                size_t v5 = _volume->coordsToIndex(glm::uvec3{ x + 1, y, z + 1 });
                size_t v6 = _volume->coordsToIndex(glm::uvec3{ x, y + 1, z + 1 });
                size_t v7 = _volume->coordsToIndex(glm::uvec3{ x + 1, y + 1, z + 1 });

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

glm::mat4 VolumeTetraMesh::getBoundingBox() const {
    throw "GetBoundingBox not yet implemented";
    //return getCoordinateTransformer().getDataToWorldMatrix();
}

glm::vec2 VolumeTetraMesh::getDataRange() const {
    throw "getDataRange not yet implemented";

    //if (_volume) {
    // TODO: This information is available in the volume metadata so should use 
        //return _volume->dataMap_.dataRange;
    //} else {
        return glm::vec2{0.0, 1.0};
    //}
}

}  // namespace openspace
