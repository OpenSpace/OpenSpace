/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <modules/base/rendering/screenspaceframebuffer.h>

#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/opengl/framebufferobject.h>
#include <ghoul/opengl/textureunit.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo SizeInfo = {
        "Size",
        "Size",
        "This value explicitly specifies the size of the screen space plane."
    };
} // namespace

namespace openspace {

documentation::Documentation ScreenSpaceFramebuffer::Documentation() {
    using namespace documentation;
    return {
        "ScreenSpace Framebuffer",
        "base_screenspace_framebuffer",
        {}
    };
}

ScreenSpaceFramebuffer::ScreenSpaceFramebuffer(const ghoul::Dictionary& dictionary)
    : ScreenSpaceRenderable(dictionary)
    , _size(SizeInfo, glm::vec4(0), glm::vec4(0), glm::vec4(16384))
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "ScreenSpaceFramebuffer"
    );

    int iIdentifier = 0;
    if (_identifier.empty()) {
        iIdentifier = id();

        if (iIdentifier == 0) {
            setIdentifier("ScreenSpaceFramebuffer");
        }
        else {
            setIdentifier("ScreenSpaceFramebuffer" + std::to_string(iIdentifier));
        }
    }

    if (_guiName.empty()) {
        // Adding an extra space to the user-facing name as it looks nicer
        setGuiName("ScreenSpaceFramebuffer " + std::to_string(iIdentifier));
    }

    glm::vec2 resolution = global::windowDelegate.currentWindowResolution();
    addProperty(_size);
    _size.set(glm::vec4(0, 0, resolution.x,resolution.y));
}

ScreenSpaceFramebuffer::~ScreenSpaceFramebuffer() {} // NOLINT

bool ScreenSpaceFramebuffer::initializeGL() {
    ScreenSpaceRenderable::initializeGL();
    createFramebuffer();

    return isReady();
}

bool ScreenSpaceFramebuffer::deinitializeGL() {
    ScreenSpaceRenderable::deinitializeGL();

    _framebuffer->activate();
    _framebuffer->detachAll();
    _framebuffer->deactivate();
    removeAllRenderFunctions();

    return true;
}

void ScreenSpaceFramebuffer::render() {
    const glm::vec2& resolution = global::windowDelegate.currentWindowResolution();
    const glm::vec4& size = _size.value();

    const float xratio = _originalViewportSize.x / (size.z - size.x);
    const float yratio = _originalViewportSize.y / (size.w - size.y);;

    if (!_renderFunctions.empty()) {
        glViewport(
            static_cast<GLint>(-size.x * xratio),
            static_cast<GLint>(-size.y * yratio),
            static_cast<GLsizei>(_originalViewportSize.x * xratio),
            static_cast<GLsizei>(_originalViewportSize.y * yratio)
        );
        GLint defaultFBO = _framebuffer->getActiveObject();
        _framebuffer->activate();

        glClearColor(0.f, 0.f, 0.f, 0.f);
        glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        for (const RenderFunction& renderFunction : _renderFunctions) {
            renderFunction();
        }
        _framebuffer->deactivate();

        glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);
        glViewport(
            0,
            0,
            static_cast<GLsizei>(resolution.x),
            static_cast<GLsizei>(resolution.y)
        );

        const glm::mat4 rotation = rotationMatrix();
        const glm::mat4 translation = translationMatrix();
        const glm::mat4 scale = glm::scale(
            scaleMatrix(),
            glm::vec3((1.f / xratio), (1.f / yratio), 1.f)
        );
        const glm::mat4 modelTransform = rotation*translation*scale;
        draw(modelTransform);
    }
}

bool ScreenSpaceFramebuffer::isReady() const {
    bool ready = true;
    if (!_shader) {
        ready &= false;
    }
    if (!_texture) {
        ready &= false;
    }
    return ready;
}

void ScreenSpaceFramebuffer::setSize(glm::vec4 size) {
    _size = std::move(size);
}

void ScreenSpaceFramebuffer::addRenderFunction(std::function<void()> renderFunction) {
    _renderFunctions.push_back(std::move(renderFunction));
}

void ScreenSpaceFramebuffer::removeAllRenderFunctions() {
    _renderFunctions.clear();
}

void ScreenSpaceFramebuffer::createFramebuffer() {
    _framebuffer = std::make_unique<ghoul::opengl::FramebufferObject>();
    _framebuffer->activate();
    _texture = std::make_unique<ghoul::opengl::Texture>(glm::uvec3(
        _originalViewportSize.x,
        _originalViewportSize.y,
        1
    ));
    _objectSize = glm::ivec2(_originalViewportSize);

    _texture->uploadTexture();
    _texture->setFilter(ghoul::opengl::Texture::FilterMode::LinearMipMap);
    _framebuffer->attachTexture(_texture.get(), GL_COLOR_ATTACHMENT0);
    _framebuffer->deactivate();
}

int ScreenSpaceFramebuffer::id() {
    static int id = 0;
    return id++;
}

void ScreenSpaceFramebuffer::bindTexture() {
    _texture->bind();
}

} //namespace openspace
