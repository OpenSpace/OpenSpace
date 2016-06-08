#include <ghoul/io/texture/texturereader.h>

namespace openspace {

template <typename VoxelType>
VoxelType TextureSliceVolumeReader<VoxelType>::get(const glm::ivec3& coordinates) const {
    ghoul::opengl::Texture& slice = getSlice(coordinates.z);
    return slice.texel<VoxelType>(coordinates.xy());
}

template <typename VoxelType>
glm::ivec3 TextureSliceVolumeReader<VoxelType>::dimensions() const {
    return glm::ivec3(_sliceDimensions, _paths.size());
}

template <typename VoxelType>
TextureSliceVolumeReader<VoxelType>::TextureSliceVolumeReader(std::vector<std::string> paths,
                                                              size_t sliceCacheNIndices,
                                                              size_t sliceCacheCapacity)
    : _initialized(false)
    , _paths(paths)
    , _cache(sliceCacheCapacity, sliceCacheNIndices) {}
    
template <typename VoxelType>
void TextureSliceVolumeReader<VoxelType>::initialize() {
    ghoul_assert(_paths.size() > 0, "No paths to read slices from.");

    std::shared_ptr<ghoul::opengl::Texture> firstSlice =
        ghoul::io::TextureReader::ref().loadTexture(_paths[0]);
    
    _sliceDimensions = firstSlice->dimensions().xy();
    _initialized = true;
    _cache.set(0, firstSlice);
}

template <typename VoxelType>
void TextureSliceVolumeReader<VoxelType>::setPaths(const std::vector<std::string> paths) {
    _paths = paths;
}

template <typename VoxelType>
ghoul::opengl::Texture& TextureSliceVolumeReader<VoxelType>::getSlice(int sliceIndex) const {
    ghoul_assert(_initialized, "Volume is not initialized");
    ghoul_assert(sliceIndex >= 0 && sliceIndex < _paths.size(),
                 "Slice index " + std::to_string(sliceIndex) + "is outside the range.");
    
    if (!_cache.has(sliceIndex)) {
        std::shared_ptr<ghoul::opengl::Texture> texture =
            ghoul::io::TextureReader::ref().loadTexture(_paths[sliceIndex]);
        
        glm::ivec2 dims = texture->dimensions().xy();
        ghoul_assert(dims == _sliceDimensions, "Slice dimensions do not agree.");
        _cache.set(sliceIndex, std::move(texture));
    }
    return *_cache.get(sliceIndex).get();
}

}
