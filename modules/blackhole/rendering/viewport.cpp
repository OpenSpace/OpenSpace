#include <modules/blackhole/rendering/viewport.h>

#include <glm/gtc/constants.hpp>
#include <glm/gtc/matrix_transform.hpp>

namespace openspace {

    ViewPort::ViewPort(Camera* camera) : _camera(camera) {}

    glm::dvec3 ViewPort::sphericalPosition() const {
        if (!_camera) {
            return glm::dvec3(0.0);
        }

        glm::dvec3 cartesian = _camera->positionVec3();
        double r = glm::length(cartesian);
        double theta = (r > 0.0) ? std::acos(cartesian.z / r) : 0.0;
        double phi = std::atan2(cartesian.y, cartesian.x);

        return glm::dvec3(r, theta, phi);
    }
} // openspace namespace
