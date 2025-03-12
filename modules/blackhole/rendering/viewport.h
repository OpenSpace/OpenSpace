#ifndef __OPENSPACE_MODULE_BLACKHOLE___BLACKHOLECAMERA___H__
#define __OPENSPACE_MODULE_BLACKHOLE___BLACKHOLECAMERA___H__

#include <openspace/camera/camera.h>
#include <ghoul/opengl/uniformcache.h>
#include <ghoul/opengl/texture.h>
#include <glm/glm.hpp>

namespace openspace {
    class ViewPort {
    public:
        ViewPort() {};
        ~ViewPort();

        void uploadViewGrid(const glm::ivec2& screenSize);
        void updateViewGridTexture(void* data, const glm::vec2& screenSize);
        std::unique_ptr<ghoul::opengl::Texture> viewGrid;

    private:
        float fovHorizontel = glm::radians(90.f);
    };
} // namespace openspace

#endif // __OPENSPACE_MODULE_BLACKHOLE___BLACKHOLECAMERA___H__
