/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/textureunit.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo SizeInfo = {
        "Size",
        "Size",
        "This value explicitly specifies the size of the screen space plane.",
        openspace::properties::Property::Visibility::AdvancedUser
    };
} // namespace

namespace openspace {

documentation::Documentation ScreenSpaceFramebuffer::Documentation() {
    using namespace documentation;
    return {
        "ScreenSpaceFramebuffer",
        "base_screenspace_framebuffer",
        "",
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

    const glm::vec2 resolution = global::windowDelegate->currentDrawBufferResolution();
    addProperty(_size);
    _size = glm::vec4(0.f, 0.f, resolution.x, resolution.y);
}

ScreenSpaceFramebuffer::~ScreenSpaceFramebuffer() {}

bool ScreenSpaceFramebuffer::initializeGL() {
    ScreenSpaceRenderable::initializeGL();
    createFramebuffer();

    return isReady();
}

bool ScreenSpaceFramebuffer::deinitializeGL() {
    ScreenSpaceRenderable::deinitializeGL();

    _framebuffer->activate();
    _framebuffer->detachAll();
    ghoul::opengl::FramebufferObject::deactivate();
    removeAllRenderFunctions();

    return true;
}

void ScreenSpaceFramebuffer::render(const RenderData& renderData) {
    const glm::vec2& resolution = global::windowDelegate->currentDrawBufferResolution();
    const glm::vec4& size = _size.value();

    const float xratio = resolution.x / (size.z - size.x);
    const float yratio = resolution.y / (size.w - size.y);

    if (!_renderFunctions.empty()) {
        std::array<GLint, 4> viewport;
        //glGetIntegerv(GL_VIEWPORT, viewport);
        global::renderEngine->openglStateCache().viewport(viewport.data());
        glViewport(
            static_cast<GLint>(-size.x * xratio),
            static_cast<GLint>(-size.y * yratio),
            static_cast<GLsizei>(resolution.x * xratio),
            static_cast<GLsizei>(resolution.y * yratio)
        );
        global::renderEngine->openglStateCache().setViewportState(viewport.data());

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
            glm::vec3((1.f / xratio), (1.f / yratio), 1.f)
        );
        const glm::mat4 modelTransform = globalRotation*translation*localRotation*scale;
        draw(modelTransform, renderData);
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

int ScreenSpaceFramebuffer::id() {
    static int id = 0;
    return id++;
}

void ScreenSpaceFramebuffer::bindTexture() {
    _texture->bind();
}

} //namespace openspace
