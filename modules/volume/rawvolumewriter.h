#ifndef __RAWVOLUMEWRITER_H__
#define __RAWVOLUMEWRITER_H__

#include <functional>
#include <string>
#include <modules/volume/rawvolume.h>

namespace openspace {

template <typename VoxelType>
class RawVolumeWriter {
public:
    RawVolumeWriter(std::string path, size_t bufferSize = 1024);
    void setPath(const std::string& path);    
    glm::ivec3 dimensions() const;
    void setDimensions(const glm::ivec3& dimensions);
    void write(const std::function<VoxelType(const glm::ivec3&)>& fn,
               const std::function<void(float t)>& onProgress = [](float t) {});
    void write(const RawVolume<VoxelType>& volume);

    size_t coordsToIndex(const glm::ivec3& coords) const;
    glm::ivec3 indexToCoords(size_t linear) const;
private:
    glm::ivec3 _dimensions;
    std::string _path;
    size_t _bufferSize;    
};

}

#include "rawvolumewriter.inl";

#endif
