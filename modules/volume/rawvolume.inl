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

#include <modules/volume/volumeutils.h>

namespace openspace::volume {

template <typename VoxelType>
RawVolume<VoxelType>::RawVolume(const glm::uvec3& dimensions)
    : _dimensions(dimensions)
    , _data(static_cast<size_t>(dimensions.x) *
            static_cast<size_t>(dimensions.y) *
            static_cast<size_t>(dimensions.z))
{}

template <typename VoxelType>
glm::uvec3 RawVolume<VoxelType>::dimensions() const {
    return _dimensions;
}

template <typename VoxelType>
void RawVolume<VoxelType>::setDimensions(const glm::uvec3& dimensions) {
    _dimensions = dimensions;
    _data.resize(nCells());
}

template<typename VoxelType>
size_t RawVolume<VoxelType>::nCells() const
{
    return static_cast<size_t>(
        static_cast<size_t>(_dimensions.x) *
        static_cast<size_t>(_dimensions.y) *
        static_cast<size_t>(_dimensions.z)
    );
}

template <typename VoxelType>
VoxelType RawVolume<VoxelType>::get(const glm::uvec3& coordinates) const {
    return get(coordsToIndex(coordinates));
}

template <typename VoxelType>
VoxelType RawVolume<VoxelType>::get(size_t index) const {
    return _data[index];
}

template <typename VoxelType>
void RawVolume<VoxelType>::set(const glm::uvec3& coordinates, const VoxelType& value) {
    return set(coordsToIndex(coordinates), value);
}

template <typename VoxelType>
void RawVolume<VoxelType>::set(size_t index, const VoxelType& value) {
    _data[index] = value;
}

template <typename VoxelType>
void RawVolume<VoxelType>::forEachVoxel(
                       const std::function<void(const glm::uvec3&, const VoxelType&)>& fn)
{
    for (size_t i = 0; i < nCells(); i++) {
        const glm::ivec3& coords = indexToCoords(i);
        fn(coords, _data[i]);
    }
}

template <typename VoxelType>
size_t RawVolume<VoxelType>::coordsToIndex(const glm::uvec3& cartesian) const {
    return volume::coordsToIndex(cartesian, dimensions());
}

template <typename VoxelType>
glm::uvec3 RawVolume<VoxelType>::indexToCoords(size_t linear) const {
    return volume::indexToCoords(linear, dimensions());
}

template <typename VoxelType>
VoxelType* RawVolume<VoxelType>::data() {
    return _data.data();
}

template <typename VoxelType>
const VoxelType* RawVolume<VoxelType>::data() const {
    return _data.data();
}

} // namespace openspace::volume
