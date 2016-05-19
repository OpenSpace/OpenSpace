#include <fstream>
#include <modules/volume/volumeutils.h>

namespace openspace {

template <typename VoxelType>
    RawVolumeWriter<VoxelType>::RawVolumeWriter(std::string path, size_t bufferSize)
    : _path(path)
    , _bufferSize(bufferSize) {}

template <typename VoxelType>
size_t RawVolumeWriter<VoxelType>::coordsToIndex(const glm::ivec3& cartesian) const {
    return volumeutils::coordsToIndex(cartesian, dimensions());
}

template <typename VoxelType>
glm::ivec3 RawVolumeWriter<VoxelType>::indexToCoords(size_t linear) const {
    return volumeutils::indexToCoords(linear, dimensions());
}

template <typename VoxelType>
    void RawVolumeWriter<VoxelType>::setDimensions(const glm::ivec3& dimensions) {
    _dimensions = dimensions;
}

template <typename VoxelType>
    glm::ivec3 RawVolumeWriter<VoxelType>::dimensions() const {
    return _dimensions;
}


template <typename VoxelType>
void RawVolumeWriter<VoxelType>::write(const std::function<VoxelType(const glm::ivec3&)>& fn,
                                       const std::function<void(float t)>& onProgress)
{
    glm::ivec3 dims = dimensions();

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
    glm::ivec3 dims = dimensions();
    ghoul_assert(dims == volume.dims());

    const char* buffer = reinterpret_cast<char*>(volume.data());
    size_t length = static_cast<size_t>(dims.x) *
        static_cast<size_t>(dims.y) *
        static_cast<size_t>(dims.z) *
        sizeof(VoxelType);
    
    std::ofstream file(_path, std::ios::binary);
    file.write(buffer, length);
    file.close();
}

    
}
