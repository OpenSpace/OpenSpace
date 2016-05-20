namespace openspace {
    
template <typename VolumeType>
VolumeSampler<VolumeType>::VolumeSampler(const VolumeType& volume, const glm::vec3& filterSize) {
    // Only accept filter sizes of size 1, 3, 5, 7...
    // Round down to closest odd number.
    _filterSize = static_cast<glm::ivec3>((filterSize - glm::vec3(1.0)) * glm::vec3(0.5)) *
        glm::ivec3(2) + glm::ivec3(1);
    
    _volume = &volume;
}

template <typename VolumeType>
typename VolumeType::VoxelType VolumeSampler<VolumeType>::sample(const glm::vec3& position) const {
    glm::ivec3 flooredPos = static_cast<glm::ivec3>(glm::floor(position));
    glm::vec3 t = glm::fract(position);

    // t is now in interval [0, 1[ (never 1)
    glm::ivec3 minCoords = flooredPos - _filterSize / 2; // min coord to sample from
    glm::ivec3 maxCoords = minCoords + _filterSize; // max coords to sample from, including interpolation.
    glm::ivec3 clampCeiling = _volume->dimensions() - glm::ivec3(1);

    typename VolumeType::VoxelType value;
    for (int z = minCoords.z; z <= maxCoords.z; z++) {
        for (int y = minCoords.y; y <= maxCoords.y; y++) {
            for (int x = minCoords.x; x <= maxCoords.x; x++) {
                glm::ivec3 sampleCoords = glm::ivec3(x, y, z);
                float filterCoefficient = 1.0;

                if (x == minCoords.x) {
                    filterCoefficient *= (1.0 - t.x);
                } else if (x == maxCoords.x) {
                    filterCoefficient *= t.x;
                }
                if (y == minCoords.y) {
                    filterCoefficient *= (1.0 - t.y);
                } else if (y == maxCoords.y) {
                    filterCoefficient *= t.y;
                }
                if (z == minCoords.z) {
                    filterCoefficient *= (1.0 - t.z);
                } else if (z == maxCoords.z) {
                    filterCoefficient *= t.z;
                }

                glm::ivec3 clampedCoords = glm::clamp(sampleCoords, glm::ivec3(0), clampCeiling);
                value += filterCoefficient * _volume->get(clampedCoords);

            }
        }
    }
    
    value /= static_cast<float>(_filterSize.x * _filterSize.y * _filterSize.z);
    return value;
}

}
