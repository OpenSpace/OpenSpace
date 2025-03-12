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
    ViewPort::~ViewPort()
    {
        viewGrid = nullptr;
    }
    void ViewPort::uploadViewGrid(const glm::ivec2& screenSize) {
        float halfWidth = tan(fovHorizontel / 2.f);
        float halfHeight = halfWidth * screenSize.y / screenSize.x;

        float stepSize = (2.f * halfWidth) / screenSize.x;

        size_t dataSize = screenSize.x * screenSize.y * 2 * sizeof(float);
        float* data = static_cast<float*>(malloc(dataSize));
        for (int i = 0; i < screenSize.y; i++) {
            float y = (i - screenSize.y / 2) * stepSize;
            for (int j = 0; j < screenSize.x; j++) {
                float x = (j - screenSize.x / 2) * stepSize;
                int index = (screenSize.x * i + j) * 2;

                data[index] = x;
                data[index + 1] = y;
            }
        }
        updateViewGridTexture(data, screenSize);
    }

    void ViewPort::updateViewGridTexture(void* data, const glm::vec2& screenSize) {
        viewGrid = std::make_unique<ghoul::opengl::Texture>(
            data,
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
