#ifndef __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___ISOSURFACE___H__
#define __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___ISOSURFACE___H__

#include <openspace/glm.h>
#include <array>
#include <cstdint>
#include <optional>
#include <vector>

namespace openspace::exoplanets {

struct IsoSurface {
    glm::uvec3 dimensions = glm::uvec3(0);
    glm::ivec3 origin = glm::ivec3(0);
    std::vector<float> values;

    // The threshold value used to determine whether a point is inside the surface
    float threshold = 0.5f;

    size_t index(const glm::uvec3& node) const;
    double valueAt(const glm::dvec3& position) const;
    bool contains(const glm::dvec3& position) const;
    std::vector<glm::dvec3> triangles() const;
};

std::optional<IsoSurface> smoothSelectionSurface(const glm::uvec3& dimensions,
    const std::vector<uint8_t>& mask, const std::vector<glm::dvec3>& points);

} // namespace openspace::exoplanets

#endif
