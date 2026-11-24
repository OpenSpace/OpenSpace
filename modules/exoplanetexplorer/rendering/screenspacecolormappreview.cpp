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

#include <modules/exoplanetexplorer/rendering/screenspacecolormappreview.h>

#include <modules/exoplanetexplorer/exoplanetexplorermodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/font/font.h>
#include <openspace/font/fontmanager.h>
#include <openspace/font/fontrenderer.h>
#include <openspace/opengl/framebufferobject.h>
#include <openspace/opengl/openglstatecache.h>
#include <openspace/opengl/texture.h>
#include <openspace/opengl/textureunit.h>
#include <openspace/rendering/helper.h>
#include <openspace/rendering/renderengine.h>
#include <algorithm>
#include <array>
#include <cmath>
#include <format>

namespace {
    using namespace openspace;

    constexpr float Padding = 12.f;
    constexpr float LineHeight = 22.f;
    constexpr float BarWidth = 20.f;
    constexpr float BarHeight = 150.f;
    constexpr float SwatchSize = 14.f;
    constexpr int NTicks = 5;
    constexpr size_t MaxCategorySwatches = 15;

    using Variable = ExoplanetExplorerModule::ColormapPreviewData::Variable;
    using Category = ExoplanetExplorerModule::ColormapPreviewData::Category;

    struct VariableLayout {
        float width = 0.f;
        float height = LineHeight;
    };

    bool hasInvalidLogRange(const Variable& variable) {
        return variable.useLogScale &&
            (variable.min <= 0.f || variable.max <= 0.f || variable.min == variable.max);
    }

    double tickValue(const Variable& variable, double position) {
        if (variable.useLogScale) {
            return std::pow(10.0, std::lerp(
                std::log10(variable.min), std::log10(variable.max), position
            ));
        }
        return std::lerp(variable.min, variable.max, position);
    }

    VariableLayout measureVariable(const Variable& variable, fontrendering::Font& font) {
        VariableLayout layout;
        layout.width = font.boundingBox(variable.name).x;
        if (variable.isNumeric && hasInvalidLogRange(variable)) {
            layout.width = std::max(
                layout.width,
                font.boundingBox("Invalid log range").x
            );
            layout.height += LineHeight;
        }
        else if (variable.isNumeric) {
            float maxLabelWidth = 0.f;
            for (int tick = 0; tick < NTicks; ++tick) {
                const double position = static_cast<double>(tick) / (NTicks - 1);
                const std::string label = std::format(
                    "{:.4g}",
                    tickValue(variable, position)
                );
                maxLabelWidth = std::max(maxLabelWidth, font.boundingBox(label).x);
            }
            layout.width = std::max(layout.width, BarWidth + 8.f + maxLabelWidth);
            layout.height += BarHeight;
        }
        else {
            const size_t count = std::min(variable.categories.size(), MaxCategorySwatches);
            layout.height += LineHeight * static_cast<float>(count);
            for (size_t i = 0; i < count; ++i) {
                const Category& category = variable.categories[i];
                layout.width = std::max(
                    layout.width,
                    SwatchSize + 8.f + font.boundingBox(category.name).x
                );
            }
            if (count < variable.categories.size()) {
                const std::string remaining = std::format(
                    "+{} more",
                    variable.categories.size() - count
                );
                layout.width = std::max(layout.width, font.boundingBox(remaining).x);
                layout.height += LineHeight;
            }
        }
        return layout;
    }

    void drawBox(const glm::vec2& position, const glm::vec2& dimensions,
                 const glm::vec4& color, const glm::vec2& canvasSize)
    {
        glDisable(GL_DEPTH_TEST);
        glDisable(GL_BLEND);
        rendering::renderBox(position / canvasSize, dimensions / canvasSize, color);
    }

    void drawText(const glm::vec2& position, std::string_view text,
                  const glm::vec2& canvasSize, fontrendering::Font& font,
                  fontrendering::FontRenderer& renderer)
    {
        renderer.render(
            font,
            glm::vec2(position.x, canvasSize.y - position.y - 15.f),
            text
        );
    }

    void drawVariable(const Variable& variable, glm::vec2 position,
                      const glm::vec2& canvasSize, fontrendering::Font& font,
                      fontrendering::FontRenderer& renderer)
    {
        drawText(position, variable.name, canvasSize, font, renderer);
        position.y += LineHeight;
        if (variable.isNumeric && hasInvalidLogRange(variable)) {
            drawText(position, "Invalid log range", canvasSize, font, renderer);
            return;
        }
        if (!variable.isNumeric) {
            const size_t count = std::min(variable.categories.size(), MaxCategorySwatches);
            for (size_t i = 0; i < count; ++i) {
                const Category& category = variable.categories[i];
                drawBox(
                    position + glm::vec2(0.f, 2.f),
                    glm::vec2(SwatchSize),
                    category.color,
                    canvasSize
                );
                drawText(
                    position + glm::vec2(SwatchSize + 8.f, 0.f),
                    category.name,
                    canvasSize,
                    font,
                    renderer
                );
                position.y += LineHeight;
            }
            if (count < variable.categories.size()) {
                drawText(
                    position,
                    std::format("+{} more", variable.categories.size() - count),
                    canvasSize,
                    font,
                    renderer
                );
            }
            return;
        }

        const float count = static_cast<float>(variable.colors.size());
        for (size_t i = 0; i < variable.colors.size(); ++i) {
            const float colorPosition = static_cast<float>(i) / count;
            drawBox(
                position + glm::vec2(
                    0.f,
                    BarHeight * (1.f - colorPosition - 1.f / count)
                ),
                glm::vec2(BarWidth, BarHeight / count),
                variable.colors[i],
                canvasSize
            );
        }
        for (int tick = 0; tick < NTicks; ++tick) {
            const double tickPosition = static_cast<double>(tick) / (NTicks - 1);
            const std::string label = std::format(
                "{:.4g}",
                tickValue(variable, tickPosition)
            );
            drawText(
                position + glm::vec2(
                    BarWidth + 8.f,
                    (BarHeight - 15.f) * (1.f - static_cast<float>(tickPosition))
                ),
                label,
                canvasSize,
                font,
                renderer
            );
        }
    }

    // Displays the active Exoplanet Explorer color mappings in screen space.
    struct [[codegen::Dictionary(ScreenSpaceColormapPreview)]] Parameters {};
} // namespace
#include "screenspacecolormappreview_codegen.cpp"

namespace openspace {

Documentation ScreenSpaceColormapPreview::Documentation() {
    return codegen::doc<Parameters>(
        "exoplanetexplorer_screenspace_colormap_preview",
        ScreenSpaceRenderable::Documentation()
    );
}

ScreenSpaceColormapPreview::ScreenSpaceColormapPreview(const Dictionary& dictionary)
    : ScreenSpaceRenderable(dictionary)
{
    if (identifier().empty()) {
        setIdentifier(makeUniqueIdentifier("ScreenSpaceColormapPreview"));
    }
    testSpecificationAndThrow(Documentation(), dictionary, "ScreenSpaceColormapPreview");
}

ScreenSpaceColormapPreview::~ScreenSpaceColormapPreview() = default;

void ScreenSpaceColormapPreview::initializeGL() {
    ScreenSpaceRenderable::initializeGL();
    _font = global::fontManager->font("Mono", 15.f);
    _fontRenderer = fontrendering::FontRenderer::createDefault();
    _framebuffer = std::make_unique<opengl::FramebufferObject>();
    _revision = 0;
}

void ScreenSpaceColormapPreview::deinitializeGL() {
    _framebuffer = nullptr;
    _texture = nullptr;
    _fontRenderer = nullptr;
    _font = nullptr;
    ScreenSpaceRenderable::deinitializeGL();
}

void ScreenSpaceColormapPreview::render(const RenderData& renderData) {
    updateTexture();
    if (_texture) {
        ScreenSpaceRenderable::render(renderData);
    }
}

void ScreenSpaceColormapPreview::bindTexture(opengl::TextureUnit& unit) {
    unit.bind(*_texture);
}

glm::mat4 ScreenSpaceColormapPreview::scaleMatrix() {
    const float factor = static_cast<float>(_objectSize.x) / _referenceWidth;
    return glm::scale(
        ScreenSpaceRenderable::scaleMatrix(),
        glm::vec3(factor, factor, 1.f)
    );
}

void ScreenSpaceColormapPreview::updateTexture() {
    using PreviewData = ExoplanetExplorerModule::ColormapPreviewData;
    const auto* module = global::moduleEngine->module<ExoplanetExplorerModule>();
    const PreviewData preview = module->colormapPreviewData();

    if (_revision == preview.revision) {
        return;
    }

    _revision = preview.revision;

    if (preview.variables.empty()) {
        _texture = nullptr;
        return;
    }

    std::vector<VariableLayout> layouts;
    layouts.reserve(preview.variables.size());
    float width = std::max(
        140.f,
        2.f * Padding + SwatchSize + 8.f + _font->boundingBox("No value").x
    );
    float height = Padding + LineHeight + Padding;
    float mappingsWidth = 2.f * Padding;
    for (const PreviewData::Variable& variable : preview.variables) {
        const VariableLayout layout = measureVariable(variable, *_font);
        if (!layouts.empty()) {
            mappingsWidth += Padding;
        }
        mappingsWidth += layout.width;
        width = std::max(width, mappingsWidth);
        if (layouts.empty()) {
            _referenceWidth = std::ceil(width);
        }
        height = std::max(height, 3.f * Padding + LineHeight + layout.height);
        layouts.push_back(layout);
    }
    _objectSize = glm::ivec2(
        static_cast<int>(std::ceil(width)),
        static_cast<int>(std::ceil(height))
    );
    const glm::vec2 size = glm::vec2(_objectSize);

    const GLboolean depthTestEnabled = glIsEnabled(GL_DEPTH_TEST);
    const GLboolean cullFaceEnabled = glIsEnabled(GL_CULL_FACE);
    constexpr std::array BlendParameters = {
        GL_BLEND_EQUATION_RGB, GL_BLEND_EQUATION_ALPHA,
        GL_BLEND_SRC_RGB, GL_BLEND_DST_RGB, GL_BLEND_SRC_ALPHA, GL_BLEND_DST_ALPHA
    };
    std::array<GLint, BlendParameters.size()> blendValues;
    for (size_t i = 0; i < BlendParameters.size(); ++i) {
        glGetIntegerv(BlendParameters[i], &blendValues[i]);
    }
    GLint maxDrawBuffers = 0;
    glGetIntegerv(GL_MAX_DRAW_BUFFERS, &maxDrawBuffers);
    std::vector<GLboolean> blendEnabled(static_cast<size_t>(maxDrawBuffers));
    for (GLuint i = 0; i < static_cast<GLuint>(maxDrawBuffers); ++i) {
        blendEnabled[i] = glIsEnabledi(GL_BLEND, i);
    }
    std::array<GLfloat, 4> clearColor;
    glGetFloatv(GL_COLOR_CLEAR_VALUE, clearColor.data());
    std::array<GLint, 4> viewport;
    glGetIntegerv(GL_VIEWPORT, viewport.data());
    GLint previousDrawFramebuffer = 0;
    GLint previousReadFramebuffer = 0;
    GLint previousProgram = 0;
    GLint previousVertexArray = 0;
    glGetIntegerv(GL_DRAW_FRAMEBUFFER_BINDING, &previousDrawFramebuffer);
    glGetIntegerv(GL_READ_FRAMEBUFFER_BINDING, &previousReadFramebuffer);
    glGetIntegerv(GL_CURRENT_PROGRAM, &previousProgram);
    glGetIntegerv(GL_VERTEX_ARRAY_BINDING, &previousVertexArray);
    std::array<GLboolean, 4> colorMask;
    glGetBooleanv(GL_COLOR_WRITEMASK, colorMask.data());
    const GLboolean scissorEnabled = glIsEnabled(GL_SCISSOR_TEST);
    glDisable(GL_SCISSOR_TEST);

    _framebuffer->activate();
    _texture = std::make_unique<opengl::Texture>(
        opengl::Texture::FormatInit {
            .dimensions = glm::uvec3(_objectSize.x, _objectSize.y, 1),
            .type = GL_TEXTURE_2D,
            .format = opengl::Texture::Format::RGBA,
            .dataType = GL_UNSIGNED_BYTE
        },
        opengl::Texture::SamplerInit {}
    );
    _framebuffer->attachTexture(_texture.get());
    glViewport(0, 0, _objectSize.x, _objectSize.y);
    glDisable(GL_DEPTH_TEST);
    glDisable(GL_CULL_FACE);
    glDisable(GL_BLEND);
    glBlendEquation(GL_FUNC_ADD);
    glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
    glClearColor(0.08f, 0.08f, 0.08f, 1.f);
    glClear(GL_COLOR_BUFFER_BIT);
    _fontRenderer->setFramebufferSize(size);

    drawBox(
        glm::vec2(Padding, Padding + 2.f),
        glm::vec2(SwatchSize),
        preview.noValueColor,
        size
    );
    drawText(
        glm::vec2(Padding + SwatchSize + 8.f, Padding),
        "No value",
        size,
        *_font,
        *_fontRenderer
    );
    float x = Padding;
    const float y = Padding + LineHeight + Padding;
    for (size_t index = 0; index < preview.variables.size(); ++index) {
        const VariableLayout& layout = layouts[index];
        drawVariable(
            preview.variables[index],
            glm::vec2(x, y),
            size,
            *_font,
            *_fontRenderer
        );
        x += layout.width + Padding;
    }

    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, previousDrawFramebuffer);
    glBindFramebuffer(GL_READ_FRAMEBUFFER, previousReadFramebuffer);
    glViewport(viewport[0], viewport[1], viewport[2], viewport[3]);
    glUseProgram(previousProgram);
    glBindVertexArray(previousVertexArray);
    glColorMask(colorMask[0], colorMask[1], colorMask[2], colorMask[3]);
    if (scissorEnabled) {
        glEnable(GL_SCISSOR_TEST);
    }
    if (depthTestEnabled) {
        glEnable(GL_DEPTH_TEST);
    }
    else {
        glDisable(GL_DEPTH_TEST);
    }
    if (cullFaceEnabled) {
        glEnable(GL_CULL_FACE);
    }
    else {
        glDisable(GL_CULL_FACE);
    }
    glBlendEquationSeparate(
        static_cast<GLenum>(blendValues[0]),
        static_cast<GLenum>(blendValues[1])
    );
    glBlendFuncSeparate(
        static_cast<GLenum>(blendValues[2]),
        static_cast<GLenum>(blendValues[3]),
        static_cast<GLenum>(blendValues[4]),
        static_cast<GLenum>(blendValues[5])
    );
    for (GLuint i = 0; i < static_cast<GLuint>(maxDrawBuffers); ++i) {
        if (blendEnabled[i]) {
            glEnablei(GL_BLEND, i);
        }
        else {
            glDisablei(GL_BLEND, i);
        }
    }
    glClearColor(clearColor[0], clearColor[1], clearColor[2], clearColor[3]);
}

} // namespace openspace
