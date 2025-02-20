#ifndef __OPENSPACE_MODULE_BLACKHOLE___BLACKHOLECAMERA___H__
#define __OPENSPACE_MODULE_BLACKHOLE___BLACKHOLECAMERA___H__

#include <openspace/camera/camera.h>
#include <glm/glm.hpp>

namespace openspace {
    class ViewPort {
    public:
        ViewPort() {};
        ViewPort(Camera* camera);

        glm::dvec3 sphericalPosition() const;

    private:
        Camera* _camera = nullptr;
    };
} // namespace openspace

#endif // __OPENSPACE_MODULE_BLACKHOLE___BLACKHOLECAMERA___H__
