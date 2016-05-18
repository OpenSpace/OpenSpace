#ifndef __VOLUMESAMPLER_H__
#define __VOLUMESAMPLER_H__

namespace openspace {

template <typename VolumeType>
class VolumeSampler {
public:
    VolumeSampler(const VolumeType& volume, const glm::vec3& filterSize);
    typename VolumeType::VoxelType sample(const glm::vec3& position) const;
private:
    glm::ivec3 _filterSize;
    const VolumeType* _volume;
};

}

#include "volumesampler.inl"

#endif
