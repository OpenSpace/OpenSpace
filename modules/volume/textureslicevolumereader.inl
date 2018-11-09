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

#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/texture.h>

namespace openspace::volume {

template <typename VoxelType>
TextureSliceVolumeReader<VoxelType>::TextureSliceVolumeReader(
                                                           std::vector<std::string> paths,
                                                           size_t sliceCacheNIndices,
                                                           size_t sliceCacheCapacity)
    : _paths(std::move(paths))
    , _cache(sliceCacheCapacity, sliceCacheNIndices)
{}

template <typename VoxelType>
TextureSliceVolumeReader<VoxelType>::~TextureSliceVolumeReader() {}

template <typename VoxelType>
void TextureSliceVolumeReader<VoxelType>::initialize() {
    ghoul_assert(_paths.size() > 0, "No paths to read slices from.");

    std::shared_ptr<ghoul::opengl::Texture> firstSlice =
        ghoul::io::TextureReader::ref().loadTexture(_paths[0]);

    glm::uvec3 dimensions = firstSlice->dimensions();
    _sliceDimensions = glm::uvec2(dimensions.x, dimensions.y);
    _isInitialized = true;
    _cache.set(0, firstSlice);
}

template <typename VoxelType>
VoxelType TextureSliceVolumeReader<VoxelType>::get(const glm::ivec3& coordinates) const {
    ghoul::opengl::Texture& slice = getSlice(coordinates.z);
    return slice.texel<VoxelType>(glm::uvec2(coordinates.x, coordinates.y));
}

template <typename VoxelType>
glm::ivec3 TextureSliceVolumeReader<VoxelType>::dimensions() const {
    return glm::ivec3(_sliceDimensions, _paths.size());
}

template <typename VoxelType>
void TextureSliceVolumeReader<VoxelType>::setPaths(std::vector<std::string> paths) {
    _paths = std::move(paths);
}

template <typename VoxelType>
ghoul::opengl::Texture&
TextureSliceVolumeReader<VoxelType>::getSlice(int sliceIndex) const
{
    ghoul_assert(_isInitialized, "Volume is not initialized");
    ghoul_assert(
        sliceIndex >= 0 && sliceIndex < static_cast<int>(_paths.size()),
        "Slice index " + std::to_string(sliceIndex) + "is outside the range."
    );

    if (!_cache.has(sliceIndex)) {
        std::shared_ptr<ghoul::opengl::Texture> texture =
            ghoul::io::TextureReader::ref().loadTexture(_paths[sliceIndex]);

        glm::ivec2 dimensions = glm::uvec2(texture->dimensions());
        ghoul_assert(dimensions == _sliceDimensions, "Slice dimensions do not agree.");
        _cache.set(sliceIndex, std::move(texture));
    }
    return *_cache.get(sliceIndex).get();
}

} // namespace openspace::volume
