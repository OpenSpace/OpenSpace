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

#ifndef __OPENSPACE_MODULE_VOLUME___RAWVOLUME___H__
#define __OPENSPACE_MODULE_VOLUME___RAWVOLUME___H__

#include <ghoul/glm.h>
#include <functional>
#include <vector>

namespace openspace::volume {

template <typename Type>
class RawVolume {
    using VoxelType = Type;

public:
    RawVolume(const glm::uvec3& dimensions);

    glm::uvec3 dimensions() const;
    void setDimensions(const glm::uvec3& dimensions);
    size_t nCells() const;
    VoxelType get(const glm::uvec3& coordinates) const;
    VoxelType get(const size_t index) const;
    void set(const glm::uvec3& coordinates, const VoxelType& value);
    void set(size_t index, const VoxelType& value);
    void forEachVoxel(const std::function<void(const glm::uvec3&, const VoxelType&)>& fn);
    const VoxelType* data() const;
    size_t coordsToIndex(const glm::uvec3& cartesian) const;
    glm::uvec3 indexToCoords(size_t linear) const;
    VoxelType* data();

private:
    glm::uvec3 _dimensions;
    std::vector<VoxelType> _data;
};

} // namespace openspace::volume

#include "rawvolume.inl"

#endif // __OPENSPACE_MODULE_VOLUME___RAWVOLUME___H__
