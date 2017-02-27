/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#ifndef __OPENSPACE_MODULE_VOLUME___TEXTURESLICEVOLUMEREADER___H__
#define __OPENSPACE_MODULE_VOLUME___TEXTURESLICEVOLUMEREADER___H__

#include <vector>
#include <ghoul/opengl/texture.h>
#include <memory>
#include <modules/volume/linearlrucache.h>
#include <map>
#include <unordered_map>

namespace openspace {

template <typename Voxel>
class TextureSliceVolumeReader {
public:
    typedef Voxel VoxelType;
    TextureSliceVolumeReader(std::vector<std::string> paths, size_t sliceCacheMaxItems, size_t sliceCacheSize);
    VoxelType get(const glm::ivec3& coordinates) const;
    virtual glm::ivec3 dimensions() const;
    void setPaths(const std::vector<std::string> paths);
    void initialize();
private:
    ghoul::opengl::Texture& getSlice(int sliceIndex) const;
    std::vector<std::string> _paths;
    mutable LinearLruCache<std::shared_ptr<ghoul::opengl::Texture>> _cache;
    glm::ivec2 _sliceDimensions;
    bool _initialized;
};

} // namespace openspace

#include "textureslicevolumereader.inl"

#endif // __OPENSPACE_MODULE_VOLUME___TEXTURESLICEVOLUMEREADER___H__
