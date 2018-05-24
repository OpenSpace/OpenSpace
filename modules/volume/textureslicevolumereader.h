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

#ifndef __OPENSPACE_MODULE_VOLUME___TEXTURESLICEVOLUMEREADER___H__
#define __OPENSPACE_MODULE_VOLUME___TEXTURESLICEVOLUMEREADER___H__

#include <modules/volume/linearlrucache.h>
#include <ghoul/glm.h>
#include <memory>
#include <vector>

namespace ghoul::opengl { class Texture; }

namespace openspace::volume {

template <typename Type>
class TextureSliceVolumeReader {
public:
    using VoxelType = Type;

    TextureSliceVolumeReader(std::vector<std::string> paths, size_t sliceCacheMaxItems,
        size_t sliceCacheSize);
    virtual ~TextureSliceVolumeReader();

    void initialize();

    VoxelType get(const glm::ivec3& coordinates) const;
    virtual glm::ivec3 dimensions() const;
    void setPaths(std::vector<std::string> paths);

private:
    ghoul::opengl::Texture& getSlice(int sliceIndex) const;
    std::vector<std::string> _paths;
    mutable LinearLruCache<std::shared_ptr<ghoul::opengl::Texture>> _cache;
    glm::ivec2 _sliceDimensions;
    bool _isInitialized = false;
};

} // namespace openspace::volume

#include "textureslicevolumereader.inl"

#endif // __OPENSPACE_MODULE_VOLUME___TEXTURESLICEVOLUMEREADER___H__
