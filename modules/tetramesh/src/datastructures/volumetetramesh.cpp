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

#include <modules/tetramesh/include/datastructures/volumetetramesh.h>
#include <modules/tetramesh/include/util/tetrameshutils.h>
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

VolumeTetraMesh::VolumeTetraMesh(const std::shared_ptr<const volume::RawVolume<utiltetra::VoxelData>>& volume, int channel)
    : _channel{0} {
    setData(volume, channel);
}

void VolumeTetraMesh::setData(const std::shared_ptr<const volume::RawVolume<utiltetra::VoxelData>>& volume, int channel) {
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
    //const glm::mat4 indexMatrix{ glm::scale(dims - glm::uvec3{1}) };
    //glm::mat4 m = glm::inverse(indexMatrix);

    glm::vec3 lowerDomainBound{ std::numeric_limits<float>::max() };
    glm::vec3 upperDomainBound{ std::numeric_limits<float>::min() };

    nodes.reserve(getNumberOfPoints());
    for (size_t z = 0; z < dims.z; ++z) {
        for (size_t y = 0; y < dims.y; ++y) {
            for (size_t x = 0; x < dims.x; ++x) {
                const utiltetra::VoxelData voxel = _volume->get(glm::uvec3{ x,y,z });

                glm::vec3 pos = voxel.position;
                lowerDomainBound = glm::vec3{
                    std::min(pos.x, lowerDomainBound.x),
                    std::min(pos.y, lowerDomainBound.y),
                    std::min(pos.z, lowerDomainBound.z)
                };

                upperDomainBound = glm::vec3{
                    std::max(pos.x, upperDomainBound.x),
                    std::max(pos.y, upperDomainBound.y),
                    std::max(pos.z, upperDomainBound.z)
                };

                nodes.emplace_back(pos, voxel.value);
            }
        }
    }

    const glm::vec3 diff = upperDomainBound - lowerDomainBound;
    //(pos - _lowerDomainBound) / (_upperDomainBound - _lowerDomainBound)
    for (glm::vec4& node : nodes) {
        glm::vec3 pos = glm::vec3(node.x, node.y, node.z);
        pos = (pos - lowerDomainBound) / diff;
        node = glm::vec4(pos, node.w);
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

    utiltetra::fixFaceOrientation(nodes, nodeIds);
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
