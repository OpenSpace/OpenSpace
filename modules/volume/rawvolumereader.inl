#include <fstream>

namespace openspace {

template <typename VoxelType>
RawVolumeReader<VoxelType>::RawVolumeReader(const std::string& path,
                                            const glm::ivec3& dimensions)
    : _path(path)
    , _dimensions(dimensions) {}

template <typename VoxelType>
glm::ivec3 RawVolumeReader<VoxelType>::dimensions() const {
    return _dimensions;
}

template <typename VoxelType>
void RawVolumeReader<VoxelType>::setDimensions(const glm::ivec3& dimensions) {
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
size_t RawVolumeReader<VoxelType>::coordsToIndex(const glm::ivec3& cartesian) const {
    return volumeutils::coordsToIndex(cartesian, dimensions());
}

template <typename VoxelType>
glm::ivec3 RawVolumeReader<VoxelType>::indexToCoords(size_t linear) const {
    return volumeutils::indexToCoords(linear, dimensions());
}


template <typename VoxelType>
std::unique_ptr<RawVolume<VoxelType>> RawVolumeReader<VoxelType>::read() {
    glm::ivec3 dims = dimensions();
    std::unique_ptr<RawVolume<VoxelType>> volume =
        std::make_unique<RawVolume<VoxelType>>(dims);

    std::ifstream file(_path, std::ios::binary);
    char *buffer = reinterpret_cast<char*>(volume->data());
    size_t length = static_cast<size_t>(dims.x) *
        static_cast<size_t>(dims.y) *
        static_cast<size_t>(dims.z) *
        sizeof(VoxelType);

    file.read(buffer, length);
    return volume;
}

}
