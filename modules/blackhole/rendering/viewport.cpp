#include <modules/blackhole/rendering/viewport.h>

#include <ghoul/opengl/texture.h>
#include <ghoul/logging/logmanager.h>

#ifndef PI
#define PI 3.1415926535897932384626433832795f
#endif
namespace {
    constexpr std::string_view _loggerCat = "BlackHoleViewPort";
}

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

    void ViewPort::uploadViewGrid(const glm::ivec2& screenSize) {
        float halfWidth = tan(fovHorizontel / 2.f);
        float halfHeight = halfWidth * screenSize.y / screenSize.x;

        float stepSize = (2.f * halfWidth) / screenSize.x;

        std::vector<float> data(screenSize.x * screenSize.y * 2, 0.0f);

        float z = -1.0f; 
        for (int i = 0; i < screenSize.y; i++) {
            float y = (i - screenSize.y / 2) * stepSize;
            for (int j = 0; j < screenSize.x; j++) {
                float x = (j - screenSize.x / 2) * stepSize;

                float theta = atan2(sqrt(x * x + y * y) , z);
                float phi = atan2(y, x);

                int index = (screenSize.x * i + j) * 2;
                data[index] = phi;
                data[index + 1] = theta;
            }
        }
        updateViewGridTexture(data, screenSize);
    }

    void ViewPort::updateViewGridTexture(std::vector<float>& data, const glm::vec2& screenSize) {
        viewGrid = std::make_unique<ghoul::opengl::Texture>(
            data.data(),
            glm::uvec3(screenSize.x, screenSize.y, 1),
            GL_TEXTURE_2D,
            ghoul::opengl::Texture::Format::RG,
            GL_RG32F,
            GL_FLOAT
        );

        if (viewGrid) {
            viewGrid->uploadTexture();
        }
        else {
            LWARNING("Failed to load view grid on update");
        }
    }


} // openspace namespace
