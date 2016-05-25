#include "volumeutils.h"

namespace openspace {
namespace volumeutils {
    
size_t coordsToIndex(const glm::vec3& coords, const glm::ivec3& dims) {
    size_t w = dims.x;
    size_t h = dims.y;
    size_t d = dims.z;
    
    size_t x = coords.x;
    size_t y = coords.y;
    size_t z = coords.z;
    
    return coords.z * (h * w) + coords.y * w + coords.x;
}

glm::vec3 indexToCoords(size_t index, const glm::ivec3& dims) {
    size_t w = dims.x;
    size_t h = dims.y;
    size_t d = dims.z;
    
    size_t x = index % w;
    size_t y = (index / w) % h;
    size_t z = index / w / h;
    
    return glm::ivec3(x, y, z);
}
    
}
}
