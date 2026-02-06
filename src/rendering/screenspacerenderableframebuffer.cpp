/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
 *                                                                                       *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
 * software and associated documentation files (the "Software"), to deal in the Software *
 * without restriction, including without limitation the rights to use, copy, modify,    *
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
 * permit persons to whom the Software is furnished to do so, subject to the following   *
 * conditions:                                                                           *
 *                                                                                       *
 * The above copyright notice and this permission notice shall be included in all copies *
 * or substantial portions of the Software.                                              *
 *                                                                                       *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
 ****************************************************************************************/

#include <openspace/rendering/screenspacerenderableframebuffer.h>

#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <ghoul/format.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/opengl/textureunit.h>
#include <array>

namespace {
    constexpr openspace::properties::Property::PropertyInfo SizeInfo = {
        "Size",
        "Size",
        "This value explicitly specifies the size of the screen space plane.",
        openspace::properties::Property::Visibility::AdvancedUser
    };
} // namespace

namespace openspace {

documentation::Documentation ScreenSpaceRenderableFramebuffer::Documentation() {
    using namespace documentation;
    return {
        "ScreenSpaceRenderableFramebuffer",
        "screenspace_framebuffer"
    };
}

ScreenSpaceRenderableFramebuffer::ScreenSpaceRenderableFramebuffer(
                                                      const ghoul::Dictionary& dictionary)
    : ScreenSpaceRenderable(dictionary)
    , _size(SizeInfo, glm::vec2(16), glm::vec2(16), glm::vec2(16384))
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "ScreenSpaceFramebuffer"
    );

    if (_identifier.empty()) {
        int idCounter = id();

        if (idCounter == 0) {
            setIdentifier("ScreenSpaceRenderableFramebuffer");
        }
        else {
            setIdentifier(std::format("ScreenSpaceRenderableFramebuffer{}", idCounter));
        }
    }

    _size = global::windowDelegate->currentDrawBufferResolution();
    addProperty(_size);
}

ScreenSpaceRenderableFramebuffer::~ScreenSpaceRenderableFramebuffer() {}

void ScreenSpaceRenderableFramebuffer::initializeGL() {
    ScreenSpaceRenderable::initializeGL();

    createFramebuffer();
}

void ScreenSpaceRenderableFramebuffer::deinitializeGL() {
    _framebuffer->activate();
    _framebuffer->detachAll();
    ghoul::opengl::FramebufferObject::deactivate();
    removeAllRenderFunctions();

    ScreenSpaceRenderable::deinitializeGL();
}

void ScreenSpaceRenderableFramebuffer::render(const RenderData& renderData) {
    const glm::vec2& resolution = global::windowDelegate->currentDrawBufferResolution();
    const glm::vec2& size = _size.value();
    const glm::vec2 ratio = resolution / size;

    if (!_renderFunctions.empty()) {
        std::array<GLint, 4> viewport;
        glGetIntegerv(GL_VIEWPORT, viewport.data());
        glViewport(0, 0, static_cast<GLint>(size.x), static_cast<GLint>(size.y));

        const GLint defaultFBO = ghoul::opengl::FramebufferObject::getActiveObject();
        _framebuffer->activate();

        glClearColor(0.f, 0.f, 0.f, 0.f);
        glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        for (const RenderFunction& renderFunction : _renderFunctions) {
            renderFunction();
        }
        ghoul::opengl::FramebufferObject::deactivate();

        glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);
        glViewport(viewport[0], viewport[1], viewport[2], viewport[3]);

        const glm::mat4 globalRotation = globalRotationMatrix();
        const glm::mat4 translation = translationMatrix();
        const glm::mat4 localRotation = localRotationMatrix();
        const glm::mat4 scale = glm::scale(
            scaleMatrix(),
            glm::vec3((1.f / ratio.x), (1.f / ratio.y), 1.f)
        );
        const glm::mat4 modelTransform = globalRotation*translation*localRotation*scale;
        draw(modelTransform, renderData);
    }
}

bool ScreenSpaceRenderableFramebuffer::isReady() const {
    return _shader && _texture;
}

void ScreenSpaceRenderableFramebuffer::addRenderFunction(RenderFunction renderFunction) {
    _renderFunctions.push_back(std::move(renderFunction));
}

void ScreenSpaceRenderableFramebuffer::removeAllRenderFunctions() {
    _renderFunctions.clear();
}

void ScreenSpaceRenderableFramebuffer::createFramebuffer() {
    const glm::vec2 resolution = global::windowDelegate->currentDrawBufferResolution();

    _framebuffer = std::make_unique<ghoul::opengl::FramebufferObject>();
    _framebuffer->activate();
    _texture = std::make_unique<ghoul::opengl::Texture>(
        glm::uvec3(resolution.x, resolution.y, 1),
        GL_TEXTURE_2D
    );
    _objectSize = glm::ivec2(resolution);

    _texture->uploadTexture();
    _texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
    _texture->purgeFromRAM();
    _framebuffer->attachTexture(_texture.get(), GL_COLOR_ATTACHMENT0);
    ghoul::opengl::FramebufferObject::deactivate();
}

int ScreenSpaceRenderableFramebuffer::id() {
    static int id = 0;
    return id++;
}

void ScreenSpaceRenderableFramebuffer::bindTexture(ghoul::opengl::TextureUnit& unit) {
    unit.bind(*_texture);
}

} //namespace openspace
