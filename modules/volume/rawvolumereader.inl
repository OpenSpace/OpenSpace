/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <ghoul/misc/exception.h>
#include <ghoul/misc/profiling.h>
#include <fstream>

namespace openspace::volume {

template <typename VoxelType>
RawVolumeReader<VoxelType>::RawVolumeReader(const std::filesystem::path& path,
                                            const glm::uvec3& dimensions)
    : _dimensions(dimensions)
    , _path(std::move(path))
{}

template <typename VoxelType>
glm::uvec3 RawVolumeReader<VoxelType>::dimensions() const {
    return _dimensions;
}

template <typename VoxelType>
void RawVolumeReader<VoxelType>::setDimensions(const glm::uvec3& dimensions) {
    _dimensions = dimensions;
}

template <typename VoxelType>
std::filesystem::path RawVolumeReader<VoxelType>::path() const {
    return _path;
}

template <typename VoxelType>
void RawVolumeReader<VoxelType>::setPath(std::filesystem::path path) {
    _path = std::move(path);
}

/*
TODO: Implement these methods for random access in raw volume file
template <typename VoxelType>
VoxelType RawVolumeReader<VoxelType>::get(const glm::ivec3& coordinates) const {
    return get(coordsToIndex(coordinates, dimensions()));
}

template <typename VoxelType>
VoxelType RawVolumeReader<VoxelType>::get(size_t index) const {
    // TODO: read from specific position.
    return VoxelType();
}*/

template <typename VoxelType>
size_t RawVolumeReader<VoxelType>::coordsToIndex(const glm::uvec3& cartesian) const {
    return coordsToIndex(cartesian, dimensions());
}

template <typename VoxelType>
glm::uvec3 RawVolumeReader<VoxelType>::indexToCoords(size_t linear) const {
    return indexToCoords(linear, dimensions());
}

template <typename VoxelType>
std::unique_ptr<RawVolume<VoxelType>> RawVolumeReader<VoxelType>::read(bool invertZ) {
    ZoneScoped;

    std::ifstream file = std::ifstream(_path, std::ios::binary);
    if (file.fail()) {
        throw ghoul::FileNotFoundError("Volume file not found");
    }

    glm::uvec3 dims = dimensions();
    auto volume = std::make_unique<RawVolume<VoxelType>>(dims);

    char* buffer = reinterpret_cast<char*>(volume->data());
    size_t length = glm::compMul(dims) * sizeof(VoxelType);
    {
        ZoneScopedN("read");
        file.read(buffer, length);
    }

    if (file.fail()) {
        throw ghoul::RuntimeError("Error reading volume file");
    }

    if (invertZ) {
        std::unique_ptr<RawVolume<VoxelType>> newVolume =
            std::make_unique<RawVolume<VoxelType>>(dims);

        for (size_t i = 0; i < volume->nCells(); i++) {
            const glm::uvec3& coords = volume->indexToCoords(i);
            glm::uvec3 newcoords = glm::uvec3(coords.x, coords.y, dims.z - coords.z - 1);

            // @TODO (emmbr, 2022-02-10) Not sure why they are here, but commented them
            // out for now as they aren't used
            //size_t newIndex = volume->coordsToIndex(newcoords);
            //size_t oldIndex = volume->coordsToIndex(coords);

            newVolume->set(newcoords, volume->get(coords));
        }
        return newVolume;
    }
    else {
        return volume;
    }
}

} // namespace openspace::volume
