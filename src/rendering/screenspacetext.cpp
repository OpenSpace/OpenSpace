/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <openspace/rendering/screenspacetext.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <ghoul/font/font.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/opengl/framebufferobject.h>
#include <optional>

namespace {
    constexpr openspace::properties::Property::PropertyInfo FontNameInfo = {
        "FontName",
        "Font name",
        "This value is the name of the font that is used. It can either refer to an "
        "internal name registered previously, or it can refer to a path that is used.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo FontSizeInfo = {
        "FontSize",
        "Font size",
        "This value determines the size of the font that is used to render the distance.",
        openspace::properties::Property::Visibility::User
    };

    struct [[codegen::Dictionary(DashboardTextItem)]] Parameters {
        // [[codegen::verbatim(FontNameInfo.description)]]
        std::optional<std::string> fontName;

        // [[codegen::verbatim(FontSizeInfo.description)]]
        std::optional<float> fontSize;
    };
#include "screenspacetext_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation ScreenSpaceText::Documentation() {
    return codegen::doc<Parameters>("screenspaceframebuffer");
}

ScreenSpaceText::ScreenSpaceText(const ghoul::Dictionary& dictionary)
    : ScreenSpaceRenderable(dictionary)
    , _fontName(FontNameInfo, "Mono")
    , _fontSize(FontSizeInfo, 15.f, 6.f, 144.f, 1.f)
    , _fontRenderer(ghoul::fontrendering::FontRenderer::createDefault())
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _fontName = p.fontName.value_or(_fontName);
    _fontName.onChange([this]() {
        _font = global::fontManager->font(_fontName, _fontSize);
    });
    addProperty(_fontName);

    _fontSize = p.fontSize.value_or(_fontSize);
    _fontSize.onChange([this]() {
        _font = global::fontManager->font(_fontName, _fontSize);
    });
    addProperty(_fontSize);

    _font = global::fontManager->font(_fontName, _fontSize);
}

void ScreenSpaceText::initializeGL() {
    ScreenSpaceRenderable::initializeGL();

    _framebuffer = std::make_unique<ghoul::opengl::FramebufferObject>();
}

void ScreenSpaceText::deinitializeGL() {
    _framebuffer->activate();
    _framebuffer->detachAll();
    ghoul::opengl::FramebufferObject::deactivate();

    _texture = nullptr;

    ScreenSpaceRenderable::deinitializeGL();
}

bool ScreenSpaceText::isReady() const {
    return _shader && _font && _texture;
}

void ScreenSpaceText::update() {
    updateFramebuffer();
}

void ScreenSpaceText::render(const RenderData& renderData) {
    const glm::vec2& resolution = global::windowDelegate->currentDrawBufferResolution();
    glm::vec2 size = _texture->dimensions();
    const glm::vec2 ratio = resolution / size;

    std::array<GLint, 4> viewport;
    glGetIntegerv(GL_VIEWPORT, viewport.data());
    glViewport(
        0,
        0,
        static_cast<GLint>(size.x),
        static_cast<GLint>(size.y)
    );

    const GLint defaultFBO = ghoul::opengl::FramebufferObject::getActiveObject();
    _framebuffer->activate();

    glClearColor(0.f, 0.f, 0.f, 0.f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glm::vec2 pos = glm::vec2(0.f, size.y / 4);
    _fontRenderer->render(*_font, pos, _buffer);
    ghoul::opengl::FramebufferObject::deactivate();

    glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);
    glViewport(viewport[0], viewport[1], viewport[2], viewport[3]);

    const glm::mat4 globalRotation = globalRotationMatrix();
    const glm::mat4 translation = translationMatrix();
    const glm::mat4 localRotation = localRotationMatrix();
    //const glm::mat4 scale = glm::scale(
    //    scaleMatrix(),
    //    glm::vec3((1.f / ratio.x), (1.f / ratio.y), 1.f)
    //);
    const glm::mat4 scale = scaleMatrix();
    const glm::mat4 modelTransform = globalRotation * translation * localRotation * scale;
    draw(modelTransform, renderData);
}

void ScreenSpaceText::updateFramebuffer() {
    const glm::vec2 bbox = _font->boundingBox(_buffer);
    const glm::uvec3 box = glm::uvec3(bbox.x, bbox.y, 1);
    const glm::uvec3 dim =  _texture ? _texture->dimensions() : glm::uvec3(0);
    if (box.x <= dim.x || box.y <= dim.y) {
        // The size has not changed
        return;
    }

    _objectSize = glm::ivec2(bbox.x, bbox.y);
    _fontRenderer->setFramebufferSize(_objectSize);

    _framebuffer->activate();
    // Create a texture that has 2 times the size to create a buffer
    _texture = std::make_unique<ghoul::opengl::Texture>(
        glm::uvec3(bbox.x, bbox.y, 1),
        GL_TEXTURE_2D
    );

    _texture->uploadTexture();
    _texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
    _texture->purgeFromRAM();
    _framebuffer->attachTexture(_texture.get(), GL_COLOR_ATTACHMENT0);
    ghoul::opengl::FramebufferObject::deactivate();
}

void ScreenSpaceText::bindTexture() {
    _texture->bind();
}

} //namespace openspace
