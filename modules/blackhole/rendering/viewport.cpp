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
    ViewPort::~ViewPort() {
        viewGrid = nullptr;
    }

    void ViewPort::updateViewGrid(const glm::ivec2& screenSize) {
        ghoul_assert(screenSize.x > 0 && screenSize.y > 0,
            "Screen size dimensions must be positive!");
        if (screenSize == _lastScreenSize) return;
        _lastScreenSize = screenSize;

        float const halfWidth = tan(_fovHorizontel / 2.f);
        float stepSize = (2.f * halfWidth) / screenSize.x;

        size_t width = static_cast<size_t>(screenSize.x);
        size_t height = static_cast<size_t>(screenSize.y);
        size_t numFloats = width * height * 2;
        auto data = std::make_unique<float[]>(numFloats);

        for (size_t i = 0; i < height; i++) {
            float y = (static_cast<int>(i) - screenSize.y / 2) * stepSize;
            for (size_t j = 0; j < width; j++) {
                float x = (static_cast<int>(j) - screenSize.x / 2) * stepSize;
                size_t index = (width * i + j) * 2;
                data[index] = x;
                data[index + 1] = y;
            }
        }
        updateViewGridTexture(data.release(), screenSize);
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
