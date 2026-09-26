/*****************************************************************************************
 *                                                                                       *
 * GHOUL                                                                                 *
 * General Helpful Open Utility Library                                                  *
 *                                                                                       *
 * Copyright (c) 2012-2026                                                               *
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

#include <ghoul/font/fontrenderer.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/font/font.h>
#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/misc/stringhelper.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/shaderobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <glm/gtc/matrix_transform.hpp>
#include <algorithm>
#include <array>
#include <filesystem>
#include <fstream>
#include <numeric>
#include <utility>

namespace {
    constexpr std::string_view _loggerCat = "FontRenderer";

    constexpr std::string_view DefaultVertexShaderPath =
        "${TEMPORARY}/defaultfontrenderer_vs.glsl";
    constexpr std::string_view DefaultFragmentShaderPath =
        "${TEMPORARY}/defaultfontrenderer_fs.glsl";
    constexpr std::string_view ProjectionVertexShaderPath =
        "${TEMPORARY}/projectionfontrenderer_vs.glsl";
    constexpr std::string_view ProjectionFragmentShaderPath =
        "${TEMPORARY}/projectionfontrenderer_fs.glsl";

    constexpr std::string_view DefaultVertexShaderSource = R"(
    #version __CONTEXT__

    layout (location = 0) in vec2 in_position;
    layout (location = 1) in vec2 in_texCoords;
    layout (location = 2) in vec2 in_outlineTexCoords;

    out Data {
      vec2 texCoords;
      vec2 outlineTexCoords;
    } out_data;

    uniform mat4 projection;


    void main() {
      out_data.texCoords = in_texCoords;
      out_data.outlineTexCoords = in_outlineTexCoords;
      gl_Position = projection * vec4(in_position, 0.0, 1.0);
    })";

    constexpr std::string_view DefaultFragmentShaderSource = R"(
    #version __CONTEXT__

    in Data {
      vec2 texCoords;
      vec2 outlineTexCoords;
    } in_data;

    out vec4 out_color;

    uniform sampler2D tex;
    uniform vec4 baseColor;
    uniform vec4 outlineColor;
    uniform bool hasOutline;


    void main() {
      if (hasOutline) {
        float inside = texture(tex, in_data.texCoords).r;
        float outline = texture(tex, in_data.outlineTexCoords).r;
        vec4 blend = mix(outlineColor, baseColor, inside);
        out_color = blend * vec4(1.0, 1.0, 1.0, max(inside, outline));
      }
      else {
        out_color = vec4(baseColor.rgb, baseColor.a * texture(tex, in_data.texCoords).r);
      }
    })";

    constexpr std::string_view ProjectionVertexShaderSource = R"(
    #version __CONTEXT__

    layout (location = 0) in vec3 in_position;
    layout (location = 1) in vec2 in_texCoords;
    layout (location = 2) in vec2 in_outlineTexCoords;

    out Data {
      vec2 texCoords;
      vec2 outlineTexCoords;
      float depth;
      vec3 vsPosition;
    } out_data;

    uniform dmat4 mvpMatrix;
    uniform dmat4 modelViewTransform;


    void main() {
      out_data.texCoords = in_texCoords;
      out_data.outlineTexCoords = in_outlineTexCoords;
      vec4 finalPos = vec4(mvpMatrix * dvec4(in_position.xyz, 1.0));
      out_data.depth = finalPos.w;
      finalPos.z = 0.0;
      out_data.vsPosition = vec3(modelViewTransform * dvec4(in_position.xyz, 1.0));
      gl_Position = finalPos;
    })";

    constexpr std::string_view ProjectionFragmentShaderSource = R"(
    #version __CONTEXT__

    in Data {
      vec2 texCoords;
      vec2 outlineTexCoords;
      float depth;
      vec3 vsPosition;
    } in_data;

    out vec4 out_color;
    out vec4 out_position;
    out vec4 out_normal;

    uniform sampler2D tex;
    uniform vec4 baseColor;
    uniform vec4 outlineColor;
    uniform bool hasOutline;
    uniform bool enableFalseDepth;
    uniform bool disableTransmittance;

    void main() {
      if (hasOutline) {
        float inside = texture(tex, in_data.texCoords).r;
        float outline = texture(tex, in_data.outlineTexCoords).r;
        vec4 blend = mix(outlineColor, baseColor, inside);
        out_color = blend * vec4(1.0, 1.0, 1.0, max(inside, outline));
      }
      else {
        out_color = vec4(baseColor.rgb, baseColor.a * texture(tex, in_data.texCoords).r);
      }
      if (out_color.a < 0.1) {
        discard;
      }
      if (enableFalseDepth) {
        gl_FragDepth = 0.0;
      }
      else {
        if (in_data.depth > 1.0) {
          gl_FragDepth = in_data.depth / pow(10, 30);
        }
        else {
          gl_FragDepth = in_data.depth - 1.0;
        }
      }
      if (disableTransmittance) {
        out_position = vec4(0.0, 0.0, -1.0, 1.0);
      }
      else {
        out_position = vec4(in_data.vsPosition, 1.0);
      }
      // 4th coord of the gNormal is the water reflectance
      out_normal = vec4(0.0, 0.0, 1.0, 0.0);
    })";

    /**
     * Extracts the next line from the string view and returns it, the passed string_view
     * is modified to remove the new line *and* the \n character.
     */
    std::string_view extractLine(std::string_view& view) {
        const std::string_view::size_type p = view.find('\n');
        if (p == std::string_view::npos) {
            // No new line found
            const std::string_view res = view;
            view = std::string_view();
            return res;
        }

        const std::string_view res = view.substr(0, p);
        view = view.substr(p + 1);
        return res;
    }
} // namespace

namespace ghoul::fontrendering {

std::unique_ptr<FontRenderer> FontRenderer::_defaultRenderer = nullptr;
std::unique_ptr<FontRenderer> FontRenderer::_defaultProjectionRenderer = nullptr;

FontRenderer::FontRenderer(std::unique_ptr<opengl::ProgramObject> program,
                           glm::vec2 framebufferSize)
    : _framebufferSize(std::move(framebufferSize))
    , _program(std::move(program))
{
    ghoul_assert(_program, "No program provided");

    //
    // Configure the OpenGL objects for the orthogonal font rendering
    struct OrthogonalVertex {
        glm::vec2 pos;
        glm::vec2 texCoords;
        glm::vec2 texCoordsOutline;
    };

    glCreateBuffers(1, &_orthogonal.vbo);
    glCreateBuffers(1, &_orthogonal.ibo);

    glCreateVertexArrays(1, &_orthogonal.vao);
    glVertexArrayVertexBuffer(
        _orthogonal.vao,
        0,
        _orthogonal.vbo,
        0,
        sizeof(OrthogonalVertex)
    );
    glVertexArrayElementBuffer(_orthogonal.vao, _orthogonal.ibo);

    glEnableVertexArrayAttrib(_orthogonal.vao, 0);
    glVertexArrayAttribFormat(_orthogonal.vao, 0, 2, GL_FLOAT, GL_FALSE, 0);
    glVertexArrayAttribBinding(_orthogonal.vao, 0, 0);

    glEnableVertexArrayAttrib(_orthogonal.vao, 1);
    glVertexArrayAttribFormat(
        _orthogonal.vao,
        1,
        2,
        GL_FLOAT,
        GL_FALSE,
        offsetof(OrthogonalVertex, texCoords)
    );
    glVertexArrayAttribBinding(_orthogonal.vao, 1, 0);

    glEnableVertexArrayAttrib(_orthogonal.vao, 2);
    glVertexArrayAttribFormat(
        _orthogonal.vao,
        2,
        2,
        GL_FLOAT,
        GL_FALSE,
        offsetof(OrthogonalVertex, texCoordsOutline)
    );
    glVertexArrayAttribBinding(_orthogonal.vao, 2, 0);


    //
    // Configure the OpenGL objects for the projective font rendering
    struct PerspectiveVertex {
        glm::vec3 position;
        glm::vec2 texCoords;
        glm::vec2 texCoordsOutline;
    };

    glCreateBuffers(1, &_perspective.vbo);
    glCreateBuffers(1, &_perspective.ibo);
    glCreateVertexArrays(1, &_perspective.vao);
    glVertexArrayVertexBuffer(
        _perspective.vao,
        0,
        _perspective.vbo,
        0,
        sizeof(PerspectiveVertex)
    );
    glVertexArrayElementBuffer(_perspective.vao, _perspective.ibo);

    glEnableVertexArrayAttrib(_perspective.vao, 0);
    glVertexArrayAttribFormat(_perspective.vao, 0, 3, GL_FLOAT, GL_FALSE, 0);
    glVertexArrayAttribBinding(_perspective.vao, 0, 0);

    glEnableVertexArrayAttrib(_perspective.vao, 1);
    glVertexArrayAttribFormat(
        _perspective.vao,
        1,
        2,
        GL_FLOAT,
        GL_FALSE,
        offsetof(PerspectiveVertex, texCoords)
    );
    glVertexArrayAttribBinding(_perspective.vao, 1, 0);

    glEnableVertexArrayAttrib(_perspective.vao, 2);
    glVertexArrayAttribFormat(
        _perspective.vao,
        2,
        2,
        GL_FLOAT,
        GL_FALSE,
        offsetof(PerspectiveVertex, texCoordsOutline)
    );
    glVertexArrayAttribBinding(_perspective.vao, 2, 0);
}

FontRenderer::~FontRenderer() {
    glDeleteVertexArrays(1, &_orthogonal.vao);
    glDeleteBuffers(1, &_orthogonal.vbo);
    glDeleteBuffers(1, &_orthogonal.ibo);

    glDeleteVertexArrays(1, &_perspective.vao);
    glDeleteBuffers(1, &_perspective.vbo);
    glDeleteBuffers(1, &_perspective.ibo);
}

std::unique_ptr<FontRenderer> FontRenderer::createDefault() {
    using namespace opengl;

    std::filesystem::path vsPath = absPath(DefaultVertexShaderPath);
    if (std::filesystem::is_regular_file(vsPath)) {
        LDEBUG(std::format("Skipping creation of existing vertex shader '{}'", vsPath));
    }
    else {
        LDEBUG(std::format("Writing default vertex shader to '{}'", vsPath));
        std::ofstream file(vsPath);
        file << DefaultVertexShaderSource;
    }

    std::filesystem::path fsPath = absPath(DefaultFragmentShaderPath);
    if (std::filesystem::is_regular_file(fsPath)) {
        LDEBUG(std::format("Skipping creation of existing fragment shader '{}'", fsPath));
    }
    else {
        LDEBUG(std::format("Writing default fragment shader to '{}'", fsPath));
        std::ofstream file(fsPath);
        file << DefaultFragmentShaderSource;
    }
    auto program = std::make_unique<ProgramObject>("Font");
    program->attachObject(
        std::make_unique<ShaderObject>(ShaderObject::ShaderType::Vertex, vsPath)
    );
    program->attachObject(
        std::make_unique<ShaderObject>(ShaderObject::ShaderType::Fragment, fsPath)
    );

    LDEBUG("Compile default font shader");
    program->compileShaderObjects();

    LDEBUG("Link default font shader");
    program->linkProgramObject();

    auto fr = std::make_unique<FontRenderer>(std::move(program), glm::vec2(0.f));
    updateUniformLocations(*fr->_program, fr->_uniformCache);
    return fr;
}

std::unique_ptr<FontRenderer> FontRenderer::createProjectionSubjectText() {
    using namespace opengl;

    std::filesystem::path vsPath = absPath(ProjectionVertexShaderPath);
    if (std::filesystem::is_regular_file(vsPath)) {
        LDEBUG(std::format("Skipping creation of existing vertex shader '{}'", vsPath));
    }
    else {
        LDEBUG(std::format("Writing default vertex shader to '{}'", vsPath));
        std::ofstream file(vsPath);
        file << ProjectionVertexShaderSource;
    }

    std::filesystem::path fsPath = absPath(ProjectionFragmentShaderPath);
    if (std::filesystem::is_regular_file(fsPath)) {
        LDEBUG(std::format("Skipping creation of existing fragment shader '{}'", vsPath));
    }
    else {
        LDEBUG(std::format("Writing default fragment shader to '{}'", fsPath));
        std::ofstream file(fsPath);
        file << ProjectionFragmentShaderSource;
    }
    auto pg = std::make_unique<ProgramObject>("ProjectionFont");
    pg->attachObject(
        std::make_unique<ShaderObject>(ShaderObject::ShaderType::Vertex, vsPath)
    );
    pg->attachObject(
        std::make_unique<ShaderObject>(ShaderObject::ShaderType::Fragment, fsPath)
    );

    LDEBUG("Compile projection font shader");
    pg->compileShaderObjects();

    LDEBUG("Link projection font shader");
    pg->linkProgramObject();

    // Can't create a unique_ptr directly here as the FontRenderer is not private
    auto fr = std::make_unique<FontRenderer>(std::move(pg), glm::vec2(0.f));
    updateUniformLocations(*fr->_program, fr->_uniformCacheProjection);
    fr->_uniformMvp = fr->_program->uniformLocation("mvpMatrix");
    return fr;
}

void FontRenderer::initialize() {
    LDEBUG("Creating default FontRenderer");
    ghoul_assert(!_defaultRenderer, "Default FontRenderer was already initialized");
    _defaultRenderer = createDefault();

    LDEBUG("Creating default projection FontRenderer");
    ghoul_assert(
        !_defaultProjectionRenderer,
        "Default projection Fontrenderer was already initialized"
    );
    _defaultProjectionRenderer = createProjectionSubjectText();
}

void FontRenderer::deinitialize() {
    _defaultRenderer = nullptr;
    _defaultProjectionRenderer = nullptr;
}

bool FontRenderer::isInitialized() {
    return _defaultRenderer != nullptr;
}

FontRenderer& FontRenderer::defaultRenderer() {
    ghoul_assert(_defaultRenderer != nullptr, "FontRenderer was not initialized");
    return *_defaultRenderer;
}

FontRenderer& FontRenderer::defaultProjectionRenderer() {
    ghoul_assert(
        _defaultProjectionRenderer != nullptr,
        "Projection FontRenderer was not initialized"
    );
    return *_defaultProjectionRenderer;
}

FontRenderer::BoundingBoxInformation FontRenderer::render(Font& font,
                                                          const glm::vec2& pos,
                                                          std::string_view text,
                                                          const glm::vec4& color) const
{
    return render(font, pos, text, color, { 0.f, 0.f, 0.f, color.a });
}

FontRenderer::BoundingBoxInformation FontRenderer::render(Font& font,
                                                          const glm::vec2& pos,
                                                          std::string_view text,
                                                          const glm::vec4& color,
                                                      const glm::vec4& outlineColor) const
{
    const size_t lines = std::count(text.begin(), text.end(), '\n') + 1;

    glDisable(GL_DEPTH_TEST);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    _program->activate();

    std::vector<float> vertexBuffer;
    vertexBuffer.reserve(128 * 10);
    std::vector<GLushort> indexBuffer;
    indexBuffer.reserve(128 * 10);

    GLushort vertexIndex = 0;
    glm::vec2 size = glm::vec2(0.f);
    glm::vec2 movingPos = pos;
    do {
        const std::string_view line = extractLine(text);
        movingPos.x = pos.x;
        float width = 0.f;
        float height = 0.f;
        for (size_t j = 0; j < line.size(); j++) {
            wchar_t character = line[j];
            if (character == wchar_t('\t')) {
                character = wchar_t(' ');
            }
            if (character == wchar_t('\0')) {
                continue;
            }

            const Font::Glyph* glyph = font.glyph(character);
            if (j > 0) {
                movingPos.x += glyph->kerning(line[j - 1]);
            }

            const float x0 = movingPos.x + glyph->leftBearing;
            const float y0 = movingPos.y + glyph->topBearing;
            const float s0 = glyph->topLeft.x;
            const float t0 = glyph->topLeft.y;
            const float outlineS0 = glyph->outlineTopLeft.x;
            const float outlineT0 = glyph->outlineTopLeft.y;

            const float x1 = x0 + glyph->width;
            const float y1 = y0 - glyph->height;
            const float s1 = glyph->bottomRight.x;
            const float t1 = glyph->bottomRight.y;
            const float outlineS1 = glyph->outlineBottomRight.x;
            const float outlineT1 = glyph->outlineBottomRight.y;

            // These variables are necessary as the insertion would otherwise produce a
            // narrowing error on clang
            const GLushort idx = vertexIndex;
            const GLushort idx1 = vertexIndex + 1;
            const GLushort idx2 = vertexIndex + 2;
            const GLushort idx3 = vertexIndex + 3;
            indexBuffer.insert(indexBuffer.end(), { idx, idx1, idx2, idx, idx2, idx3 });
            vertexIndex += 4;
            vertexBuffer.insert(
                vertexBuffer.end(),
                {
                    x0, y0, s0, t0, outlineS0, outlineT0,
                    x0, y1, s0, t1, outlineS0, outlineT1,
                    x1, y1, s1, t1, outlineS1, outlineT1,
                    x1, y0, s1, t0, outlineS1, outlineT0
                }
            );
            movingPos.x += glyph->horizontalAdvance;

            width += glyph->horizontalAdvance;
            height = std::max(height, static_cast<float>(glyph->height));
        }
        size.x = std::max(size.x, width);
        movingPos.y -= font.height();
    } while (!text.empty());
    size.y = lines * font.height();

    opengl::TextureUnit atlasUnit;
    atlasUnit.bind(font.atlasTexture());

    _program->setUniform(_uniformCache.baseColor, color);
    _program->setUniform(_uniformCache.outlineColor, outlineColor);
    _program->setUniform(_uniformCache.tex, atlasUnit);
    _program->setUniform(_uniformCache.hasOutline, font.hasOutline());
    _program->setUniform(
        _uniformCache.projection,
        glm::ortho(0.f, _framebufferSize.x, 0.f, _framebufferSize.y)
    );

    glNamedBufferData(
        _orthogonal.vbo,
        vertexBuffer.size() * sizeof(float),
        vertexBuffer.data(),
        GL_DYNAMIC_DRAW
    );

    glNamedBufferData(
        _orthogonal.ibo,
        indexBuffer.size() * sizeof(GLushort),
        indexBuffer.data(),
        GL_DYNAMIC_DRAW
    );

    glBindVertexArray(_orthogonal.vao);
    glDrawElements(
        GL_TRIANGLES,
        static_cast<GLsizei>(indexBuffer.size()),
        GL_UNSIGNED_SHORT,
        nullptr
    );
    glBindVertexArray(0);

    glEnable(GL_DEPTH_TEST);

    return { size, static_cast<int>(lines) };
}

FontRenderer::BoundingBoxInformation FontRenderer::render(Font& font,
                                                          const glm::vec3& pos,
                                                          std::string_view text,
                                                          const glm::vec4& color,
                                                          const glm::vec4& outlineColor,
                                              const ProjectedLabelsInformation& labelInfo,
                                                            const glm::vec2& offset) const
{
    const float h = font.height();

    std::vector<float> vertexBuffer;
    vertexBuffer.reserve(128 * 10);
    std::vector<GLushort> indexBuffer;
    indexBuffer.reserve(128 * 10);

    const size_t lines = std::count(text.begin(), text.end(), '\n') + 1;

    unsigned short vertexIndex = 0;
    glm::vec2 movingPos(offset);

    glm::vec2 size = glm::vec2(0.f);
    float heightInPixels = 0.f;
    do {
        const std::string_view line = extractLine(text);
        float width = 0.f;
        float height = 0.f;
        for (size_t j = 0; j < line.size(); j++) {
            wchar_t character = line[j];
            if (character == wchar_t('\t')) {
                character = wchar_t(' ');
            }

            const Font::Glyph* glyph = font.glyph(character);
            if (j > 0) {
                movingPos.x += glyph->kerning(line[j - 1]);
            }

            const float x0 = movingPos.x + glyph->leftBearing;
            const float y0 = movingPos.y + glyph->topBearing;
            const float s0 = glyph->topLeft.x;
            const float t0 = glyph->topLeft.y;
            const float outlineS0 = glyph->outlineTopLeft.x;
            const float outlineT0 = glyph->outlineTopLeft.y;

            const float x1 = x0 + glyph->width;
            const float y1 = y0 - glyph->height;
            const float s1 = glyph->bottomRight.x;
            const float t1 = glyph->bottomRight.y;
            const float outlineS1 = glyph->outlineBottomRight.x;
            const float outlineT1 = glyph->outlineBottomRight.y;

            glm::vec3 p0;
            glm::vec3 p1;
            glm::vec3 p2;
            glm::vec3 p3;

            if (labelInfo.renderType == 0) {
                p0 = (x0 * labelInfo.orthoRight + y0 * labelInfo.orthoUp) *
                    labelInfo.scale + pos;
                p1 = (x0 * labelInfo.orthoRight + y1 * labelInfo.orthoUp) *
                    labelInfo.scale + pos;
                p2 = (x1 * labelInfo.orthoRight + y1 * labelInfo.orthoUp) *
                    labelInfo.scale + pos;
                p3 = (x1 * labelInfo.orthoRight + y0 * labelInfo.orthoUp) *
                    labelInfo.scale + pos;
            }
            else {
                const glm::dvec3 normal = glm::normalize(
                    labelInfo.cameraPos - glm::dvec3(pos)
                );
                const glm::vec3 right = glm::vec3(
                    glm::cross(labelInfo.cameraLookUp, normal)
                );
                const glm::vec3 up = glm::vec3(glm::cross(normal, glm::dvec3(right)));

                p0 = (x0 * right + y0 * up) * labelInfo.scale + pos;
                p1 = (x0 * right + y1 * up) * labelInfo.scale + pos;
                p2 = (x1 * right + y1 * up) * labelInfo.scale + pos;
                p3 = (x1 * right + y0 * up) * labelInfo.scale + pos;
            }


            std::array<glm::vec4, 2> projPos = {
                glm::vec4(labelInfo.mvpMatrix * glm::dvec4(p0, 1.0)),
                glm::vec4(labelInfo.mvpMatrix * glm::dvec4(p1, 1.0))
            };
            glm::vec4 topLeft =
                (((projPos[0] / projPos[0].w) + glm::vec4(1.f)) / glm::vec4(2.f)) *
                glm::vec4(_framebufferSize.x, _framebufferSize.y, 1.f, 1.f);
            glm::vec4 bottomLeft =
                (((projPos[1] / projPos[1].w) + glm::vec4(1.f)) / glm::vec4(2.f)) *
                glm::vec4(_framebufferSize.x, _framebufferSize.y, 1.f, 1.f);

            // The billboard is bigger than the maximum size allowed
            heightInPixels =
                heightInPixels == 0.0 ?
                glm::length(topLeft - bottomLeft) :
                heightInPixels;

            // Size-based culling
            if (heightInPixels < static_cast<float>(labelInfo.minSize) ||
                heightInPixels > _framebufferSize.x ||
                heightInPixels > _framebufferSize.y)
            {
                return { size, static_cast<int>(lines) };
            }

            if (heightInPixels > labelInfo.maxSize) {
                const float scaleFix =
                    static_cast<float>(labelInfo.maxSize) / heightInPixels;
                if (labelInfo.renderType == 0) {
                    p0 = (x0 * labelInfo.orthoRight + y0 * labelInfo.orthoUp) *
                        labelInfo.scale * scaleFix + pos;
                    p1 = (x0 * labelInfo.orthoRight + y1 * labelInfo.orthoUp) *
                        labelInfo.scale * scaleFix + pos;
                    p2 = (x1 * labelInfo.orthoRight + y1 * labelInfo.orthoUp) *
                        labelInfo.scale * scaleFix + pos;
                    p3 = (x1 * labelInfo.orthoRight + y0 * labelInfo.orthoUp) *
                        labelInfo.scale * scaleFix + pos;
                }
                else {
                    glm::dvec3 normal =
                        glm::normalize(labelInfo.cameraPos - glm::dvec3(pos));
                    glm::vec3 newRight =
                        glm::vec3(glm::cross(labelInfo.cameraLookUp, normal));
                    glm::vec3 newUp = glm::vec3(glm::cross(normal, glm::dvec3(newRight)));

                    p0 = (x0 * newRight + y0 * newUp) * labelInfo.scale * scaleFix + pos;
                    p1 = (x0 * newRight + y1 * newUp) * labelInfo.scale * scaleFix + pos;
                    p2 = (x1 * newRight + y1 * newUp) * labelInfo.scale * scaleFix + pos;
                    p3 = (x1 * newRight + y0 * newUp) * labelInfo.scale * scaleFix + pos;
                }
            }

            vertexBuffer.insert(
                vertexBuffer.end(),
                {
                    p0.x, p0.y, p0.z, s0, t0, outlineS0, outlineT0,
                    p1.x, p1.y, p1.z, s0, t1, outlineS0, outlineT1,
                    p2.x, p2.y, p2.z, s1, t1, outlineS1, outlineT1,
                    p3.x, p3.y, p3.z, s1, t0, outlineS1, outlineT0
                }
            );

            const unsigned short vi = vertexIndex;
            const unsigned short vi1 = vertexIndex + 1;
            const unsigned short vi2 = vertexIndex + 2;
            const unsigned short vi3 = vertexIndex + 3;
            indexBuffer.insert(indexBuffer.end(), { vi, vi1, vi2, vi, vi2, vi3 });
            vertexIndex += 4;

            movingPos.x += glyph->horizontalAdvance;

            width += glyph->horizontalAdvance;
            height = std::max(height, static_cast<float>(glyph->height));
        }

        size.x = std::max(size.x, width);
        movingPos.y -= h;
    } while (!text.empty());
    size.y = lines * font.height();

    if (!labelInfo.enableDepth) {
        glDisable(GL_DEPTH_TEST);
    }
    glEnablei(GL_BLEND, 0);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    _program->activate();

    opengl::TextureUnit atlasUnit;
    atlasUnit.bind(font.atlasTexture());

    _program->setUniform(_uniformCacheProjection.baseColor, color);
    _program->setUniform(_uniformCacheProjection.outlineColor, outlineColor);
    _program->setUniform(_uniformCacheProjection.tex, atlasUnit);
    _program->setUniform(_uniformCacheProjection.hasOutline, font.hasOutline());
    _program->setUniform(_uniformMvp, labelInfo.mvpMatrix);
    _program->setUniform(
        _uniformCacheProjection.modelViewTransform,
        labelInfo.modelViewMatrix
    );
    _program->setUniform(
        _uniformCacheProjection.enableFalseDepth,
        labelInfo.enableFalseDepth
    );
    _program->setUniform(
        _uniformCacheProjection.disableTransmittance,
        labelInfo.disableTransmittance
    );

    glNamedBufferData(
        _perspective.vbo,
        vertexBuffer.size() * sizeof(float),
        vertexBuffer.data(),
        GL_DYNAMIC_DRAW
    );

    glNamedBufferData(
        _perspective.ibo,
        indexBuffer.size() * sizeof(GLushort),
        indexBuffer.data(),
        GL_DYNAMIC_DRAW
    );

    glBindVertexArray(_perspective.vao);
    glDrawElements(
        GL_TRIANGLES,
        static_cast<GLsizei>(indexBuffer.size()),
        GL_UNSIGNED_SHORT,
        nullptr
    );
    glBindVertexArray(0);

    if (!labelInfo.enableDepth) {
        glEnable(GL_DEPTH_TEST);
    }

    return { .boundingBox = size, .numberOfLines = static_cast<int>(lines) };
}

FontRenderer::BoundingBoxInformation FontRenderer::render(Font& font,
                                                          const glm::vec3& pos,
                                                          std::string_view text,
                                                          const glm::vec4& color,
                                              const ProjectedLabelsInformation& labelInfo,
                                                            const glm::vec2& offset) const
{
    const glm::vec4 outlineColor = glm::vec4(0.f, 0.f, 0.f, color.a);
    return render(font, pos, text, color, outlineColor, labelInfo, offset);
}

FontRenderer::BoundingBoxInformation FontRenderer::render(Font& font,
                                                          const glm::vec3& pos,
                                                          std::string_view text,
                                        const ProjectedLabelsInformation& labelInfo) const
{
    constexpr glm::vec4 Color = glm::vec4(1.f, 1.f, 1.f, 1.f);
    constexpr glm::vec4 OutlineColor = glm::vec4(0.f, 0.f, 0.f, 1.f);
    return render(font, pos, text, Color, OutlineColor, labelInfo);
}

void FontRenderer::setFramebufferSize(glm::vec2 framebufferSize) {
    _framebufferSize = std::move(framebufferSize);
}

glm::vec2 RenderFont(ghoul::fontrendering::Font& font, glm::vec2& pos,
                     std::string_view text, const glm::vec4& color, CrDirection direction,
                     const glm::vec4& outlineColor)
{
    const FontRenderer::BoundingBoxInformation r = FontRenderer::defaultRenderer().render(
        font,
        pos,
        text,
        color,
        outlineColor
    );

    switch (direction) {
        case CrDirection::Up:
            pos.y += r.numberOfLines * font.height();
            break;
        case CrDirection::None:
            break;
        case CrDirection::Down:
            pos.y -= r.numberOfLines * font.height();
            break;
    }
    return r.boundingBox;
}

glm::vec2 RenderFont(Font& font, const glm::vec2& pos, std::string_view text,
                     const glm::vec4& color, const glm::vec4& outlineColor)
{
    const FontRenderer::BoundingBoxInformation r = FontRenderer::defaultRenderer().render(
        font,
        pos,
        text,
        color,
        outlineColor
    );
    return r.boundingBox;
}

glm::vec2 RenderFont(Font& font, glm::vec2& pos, std::string_view text,
                     const glm::vec4& color, CrDirection direction)
{
    const glm::vec4 outlineColor = glm::vec4(0.f, 0.f, 0.f, color.a);
    return RenderFont(font, pos, text, color, direction, outlineColor);
}

glm::vec2 RenderFont(Font& font, const glm::vec2& pos, std::string_view text,
                     const glm::vec4& color)
{
    const glm::vec4 outlineColor = glm::vec4(0.f, 0.f, 0.f, color.a);
    return RenderFont(font, pos, text, color, outlineColor);
}

glm::vec2 RenderFont(Font& font, glm::vec2& pos, std::string_view text,
                     CrDirection direction)
{
    constexpr glm::vec4 Color = glm::vec4(1.f, 1.f, 1.f, 1.f);
    constexpr glm::vec4 OutlineColor = glm::vec4(0.f, 0.f, 0.f, 1.f);
    return RenderFont(font, pos, text, Color, direction, OutlineColor);
}

glm::vec2 RenderFont(Font& font, const glm::vec2& pos, std::string_view text) {
    constexpr glm::vec4 Color = glm::vec4(1.f, 1.f, 1.f, 1.f);
    constexpr glm::vec4 OutlineColor = glm::vec4(0.f, 0.f, 0.f, 1.f);
    return RenderFont(font, pos, text, Color, OutlineColor);
}

} // namespace ghoul::fontrendering
