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

VolumeTetraMesh::VolumeTetraMesh(
    const std::shared_ptr<const volume::RawVolume<utiltetra::VoxelData>>& volume)
{
    setData(volume);
}

void VolumeTetraMesh::setData(
    const std::shared_ptr<const volume::RawVolume<utiltetra::VoxelData>>& volume)
{
    const glm::uvec3 dimensions = volume->dimensions();
    if (volume && glm::any(glm::lessThanEqual(dimensions, glm::uvec3{ 1 }))) {
        throw ghoul::RuntimeError(fmt::format("{}: Volumes with one or more dimensions "
            "equal to 1 cannot be converted to a TetraMesh ({},{},{})", _loggerCat,
            dimensions.x, dimensions.y, dimensions.z)
        );
    }

    _volume = volume;
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

void VolumeTetraMesh::get(std::vector<glm::vec4>& nodes,
    std::vector<glm::ivec4>& nodeIds) const
{
    nodes.clear();
    nodeIds.clear();

    if (!_volume) {
        return;
    }

    // regular tetrahedralization with six tetrahedra per four-voxel cube with
    // node positions being voxel-centered
    const glm::uvec3 dims = _volume->dimensions();

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

    // TODO: Ask Martin if we can avoid normalizing coordinates without messing
    // up the rendering
    
    // transform all coordinates to [0,1]
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

glm::vec2 VolumeTetraMesh::getDataRange() const {
    throw ghoul::RuntimeError("getDataRange not yet implemented");

    //if (_volume) {
        // TODO: Could fetch the range in ´get´ function, however that means it must
        // have been called before this function can be called -> bad?
    //} else {
        //return glm::vec2{0.0, 1.0};
    //}
}

}  // namespace openspace
