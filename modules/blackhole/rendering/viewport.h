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
        ViewPort(Camera* camera);

        glm::dvec3 sphericalPosition() const;
        void uploadViewGrid(const glm::ivec2& screenSize);
        void updateViewGridTexture(std::vector<float>& data, const glm::vec2& screenSize);
        std::unique_ptr<ghoul::opengl::Texture> viewGrid;
    private:
        Camera* _camera = nullptr;
        float fovHorizontel = glm::radians(120.f);
    };
} // namespace openspace

#endif // __OPENSPACE_MODULE_BLACKHOLE___BLACKHOLECAMERA___H__
