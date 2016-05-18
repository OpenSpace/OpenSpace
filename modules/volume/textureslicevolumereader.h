#ifndef __TEXTURESLICEVOLUMEREADER_H__
#define __TEXTURESLICEVOLUMEREADER_H__

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
    void setCacheSize(size_t size);    
    void initialize();
private:
    ghoul::opengl::Texture& getSlice(int sliceIndex) const;
    std::vector<std::string> _paths;
    mutable LinearLruCache<std::shared_ptr<ghoul::opengl::Texture>> _cache;
    glm::ivec2 _sliceDimensions;
    bool _initialized;
};

}

#include "textureslicevolumereader.inl"

#endif // __TEXTURESLICEVOLUME_H__
