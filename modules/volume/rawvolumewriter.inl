/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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
#include <modules/volume/volumeutils.h>

namespace openspace {
namespace volume {

template <typename VoxelType>
RawVolumeWriter<VoxelType>::RawVolumeWriter(std::string path, size_t bufferSize)
    : _path(path)
    , _bufferSize(bufferSize)
{}

template <typename VoxelType>
size_t RawVolumeWriter<VoxelType>::coordsToIndex(const glm::uvec3& cartesian) const {
    return coordsToIndex(cartesian, dimensions());
}

template <typename VoxelType>
glm::ivec3 RawVolumeWriter<VoxelType>::indexToCoords(size_t linear) const {
    return indexToCoords(linear, dimensions());
}

template <typename VoxelType>
void RawVolumeWriter<VoxelType>::setDimensions(const glm::uvec3& dimensions) {
    _dimensions = dimensions;
}

template <typename VoxelType>
glm::uvec3 RawVolumeWriter<VoxelType>::dimensions() const {
    return _dimensions;
}

template <typename VoxelType>
void RawVolumeWriter<VoxelType>::write(const std::function<VoxelType(const glm::uvec3&)>& fn,
                                       const std::function<void(float t)>& onProgress)
{
    glm::uvec3 dims = dimensions();

    size_t size = static_cast<size_t>(dims.x) *
        static_cast<size_t>(dims.y) *
        static_cast<size_t>(dims.z);
    
    std::vector<VoxelType> buffer(_bufferSize);
    std::ofstream file(_path, std::ios::binary);

    int nChunks = size / _bufferSize;
    if (size % _bufferSize > 0) {
        nChunks++;
    }

    size_t i = 0;
    for (int c = 0; c < nChunks; c++) {
        size_t bufferPos = 0;
        size_t bufferSize = std::min(_bufferSize, size - i);
        for (bufferPos = 0; bufferPos < bufferSize; bufferPos++, i++) {
            buffer[bufferPos] = fn(indexToCoords(i));
        }
        file.write(reinterpret_cast<char*>(buffer.data()), bufferSize * sizeof(VoxelType));
        onProgress(static_cast<float>(c + 1) / nChunks);
    }
    file.close();
}

template <typename VoxelType>    
void RawVolumeWriter<VoxelType>::write(const RawVolume<VoxelType>& volume) {
    setDimensions(volume.dimensions());

    const char* const buffer = reinterpret_cast<const char*>(volume.data());
    size_t length = volume.nCells() * sizeof(VoxelType);
    
    std::ofstream file(_path, std::ios::binary);
    file.write(buffer, length);
    file.close();
}

} // namespace volume
} // namespace openspace
