/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#ifndef __OPENSPACE_MODULE_VOLUME___RAWVOLUMEWRITER___H__
#define __OPENSPACE_MODULE_VOLUME___RAWVOLUMEWRITER___H__

#include <functional>
#include <string>

namespace openspace::volume {

template <typename T> class RawVolume;

template <typename VoxelType>
class RawVolumeWriter {
public:
    RawVolumeWriter(std::string path, size_t bufferSize = 1024);

    void setPath(const std::string& path);
    glm::uvec3 dimensions() const;
    void setDimensions(glm::uvec3 dimensions);
    void write(const std::function<VoxelType(const glm::uvec3&)>& fn,
               const std::function<void(float)>& onProgress = [](float) {});
    void write(const RawVolume<VoxelType>& volume);

    size_t coordsToIndex(const glm::uvec3& coords) const;
    glm::ivec3 indexToCoords(size_t linear) const;

private:
    glm::ivec3 _dimensions = glm::ivec3(0);
    std::string _path;
    size_t _bufferSize = 0;
};

} // namespace openspace::volume

#include "rawvolumewriter.inl"

#endif // __OPENSPACE_MODULE_VOLUME___RAWVOLUMEWRITER___H__
