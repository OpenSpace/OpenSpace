/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <fstream>

namespace openspace::volume {

template <typename VoxelType>
RawVolumeReader<VoxelType>::RawVolumeReader(const std::string& path,
                                            const glm::uvec3& dimensions)
    : _dimensions(dimensions)
    , _path(path)
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
std::string RawVolumeReader<VoxelType>::path() const {
    return _path;
}

template <typename VoxelType>
void RawVolumeReader<VoxelType>::setPath(const std::string& path) {
    _path = path;
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
std::unique_ptr<RawVolume<VoxelType>> RawVolumeReader<VoxelType>::read() {
    glm::uvec3 dims = dimensions();
    std::unique_ptr<RawVolume<VoxelType>> volume = std::make_unique<RawVolume<VoxelType>>(
        dims
    );

    std::ifstream file(_path, std::ios::binary);
    char* buffer = reinterpret_cast<char*>(volume->data());

    if (file.fail()) {
        throw ghoul::FileNotFoundError("Volume file not found");
    }

    size_t length = static_cast<size_t>(dims.x) *
                    static_cast<size_t>(dims.y) *
                    static_cast<size_t>(dims.z) *
                    sizeof(VoxelType);

    file.read(buffer, length);

    if (file.fail()) {
        throw ghoul::RuntimeError("Error reading volume file");
    }

    return volume;
}

} // namespace openspace::volume
