/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014 - 2016                                                             *
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

#ifndef __RAWVOLUMEREADER_H__
#define __RAWVOLUMEREADER_H__

#include <functional>
#include <modules/volume/rawvolume.h>

namespace openspace {

template <typename Voxel>
class RawVolumeReader {
public:
    typedef Voxel VoxelType;
    RawVolumeReader(const std::string& path, const glm::ivec3& dimensions);
    glm::ivec3 dimensions() const;
    std::string path() const;
    void setPath(const std::string& path);
    void setDimensions(const glm::ivec3& dimensions);
    //VoxelType get(const glm::ivec3& coordinates) const; // TODO: Implement this
    //VoxelType get(const size_t index) const; // TODO: Implement this
    std::unique_ptr<RawVolume<VoxelType>> read();
private:
    size_t coordsToIndex(const glm::ivec3& cartesian) const;
    glm::ivec3 indexToCoords(size_t linear) const;
    glm::ivec3 _dimensions;
    std::string _path;
};

}

#include "rawvolumereader.inl"

#endif // __RAWVOLUMEREADER_H__
