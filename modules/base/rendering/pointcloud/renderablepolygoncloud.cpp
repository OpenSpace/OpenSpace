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

#include <modules/base/rendering/pointcloud/renderablepolygoncloud.h>

#include <openspace/documentation/documentation.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/glm.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <optional>

namespace {
    constexpr std::string_view _loggerCat = "RenderablePolygonCloud";

    // A RenderablePolygonCloud is a RenderablePointCloud where the shape of the points
    // is a uniform polygon with a given number of sides instead of a texture. For
    // instance, PolygonSides = 5 results in the points being rendered as pentagons.
    //
    // Note that while this renderable inherits the texture component from
    // RenderablePointCloud, any added texture information will be ignored in favor of the
    // polygon shape.
    //
    // See documentation of RenderablePointCloud for details on the other parts of the
    // point cloud rendering.
    struct [[codegen::Dictionary(RenderablePolygonCloud)]] Parameters {
        // The number of sides for the polygon used to represent each point. Default is
        // 3, i.e. to use triangles.
        std::optional<int> polygonSides [[codegen::greaterequal(3)]];
    };

#include "renderablepolygoncloud_codegen.cpp"
}  // namespace

namespace openspace {

documentation::Documentation RenderablePolygonCloud::Documentation() {
    return codegen::doc<Parameters>(
        "base_renderablepolygoncloud",
        RenderablePointCloud::Documentation()
    );
}

RenderablePolygonCloud::RenderablePolygonCloud(const ghoul::Dictionary& dictionary)
    : RenderablePointCloud(dictionary)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _nPolygonSides = p.polygonSides.value_or(_nPolygonSides);

    // The texture to use for the rendering will be generated in initializeGl. Make sure
    // we use it in the rendering
    _hasSpriteTexture = true;

    _textureMode = TextureInputMode::Other;
    removePropertySubOwner(_texture);
}

void RenderablePolygonCloud::deinitializeGL() {
    glDeleteBuffers(1, &_polygonVbo);
    _polygonVbo = 0;
    glDeleteVertexArrays(1, &_polygonVao);
    _polygonVao = 0;

    glDeleteTextures(1, &_pTexture);

    RenderablePointCloud::deinitializeGL();
}

void RenderablePolygonCloud::initializeCustomTexture() {
    ZoneScoped;

    if (_textureIsInitialized) {
        LWARNING("RenderablePolygonCloud texture cannot be updated during runtime");
        return;
    }

    LDEBUG("Creating Polygon Texture");
    constexpr gl::GLsizei TexSize = 512;

    // We don't use the helper function for the format and internal format here,
    // as we don't want the compression to be used for the polygon texture and we
    // always want alpha. This is also why we do not need to update the texture
    bool useAlpha = true;
    gl::GLenum format = gl::GLenum(glFormat(useAlpha));
    gl::GLenum internalFormat = GL_RGBA8;

    glGenTextures(1, &_pTexture);
    glBindTexture(GL_TEXTURE_2D, _pTexture);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    // Stopped using a buffer object for GL_PIXEL_UNPACK_BUFFER
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        internalFormat,
        TexSize,
        TexSize,
        0,
        format,
        GL_UNSIGNED_BYTE,
        nullptr
    );

    renderToTexture(_pTexture, TexSize, TexSize);

    // Download the data and use it to intialize the data we need to rendering.
    // Allocate memory: N channels, with one byte each
    constexpr unsigned int nChannels = 4;
    unsigned int arraySize = TexSize * TexSize * nChannels;
    std::vector<GLubyte> pixelData;
    pixelData.resize(arraySize);
    glBindTexture(GL_TEXTURE_2D, _pTexture);
    glGetTexImage(GL_TEXTURE_2D, 0, format, GL_UNSIGNED_BYTE, pixelData.data());

    // Create array from data, size and format
    unsigned int id = 0;
    glGenTextures(1, &id);
    glBindTexture(GL_TEXTURE_2D_ARRAY, id);
    initAndAllocateTextureArray(id, glm::uvec2(TexSize), 1, useAlpha);
    fillAndUploadTextureLayer(0, 0, 0, glm::uvec2(TexSize), useAlpha, pixelData.data());
    glBindTexture(GL_TEXTURE_2D_ARRAY, 0);

    _textureIsInitialized = true;
}

void RenderablePolygonCloud::renderToTexture(GLuint textureToRenderTo,
                                             GLuint textureWidth, GLuint textureHeight)
{
    LDEBUG("Rendering to Texture");

    // Saves initial Application's OpenGL State
    GLint defaultFBO = 0;
    std::array<GLint, 4> viewport;
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFBO);
    glGetIntegerv(GL_VIEWPORT, viewport.data());

    GLuint textureFBO;
    glGenFramebuffers(1, &textureFBO);
    glBindFramebuffer(GL_FRAMEBUFFER, textureFBO);
    const GLenum drawBuffers = GL_COLOR_ATTACHMENT0;
    glDrawBuffers(1, &drawBuffers);

    glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, textureToRenderTo, 0);

    glViewport(viewport[0], viewport[1], textureWidth, textureHeight);

    if (_polygonVao == 0) {
        glGenVertexArrays(1, &_polygonVao);
    }
    if (_polygonVbo == 0) {
        glGenBuffers(1, &_polygonVbo);
    }

    glBindVertexArray(_polygonVao);
    glBindBuffer(GL_ARRAY_BUFFER, _polygonVbo);

    constexpr std::array<GLfloat, 4> VertexData = {
        // x  y  z  w
        0.f, 0.f, 0.f, 1.f,
    };

    glBufferData(GL_ARRAY_BUFFER, sizeof(VertexData), VertexData.data(), GL_STATIC_DRAW);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, 4 * sizeof(GLfloat), nullptr);
    glEnableVertexAttribArray(0);
    glBindVertexArray(0);

    renderPolygonGeometry(_polygonVao);

    // Restores Applications' OpenGL State
    glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);
    glViewport(viewport[0], viewport[1], viewport[2], viewport[3]);

    glDeleteBuffers(1, &_polygonVbo);
    glDeleteVertexArrays(1, &_polygonVao);
    glDeleteFramebuffers(1, &textureFBO);
}

void RenderablePolygonCloud::renderPolygonGeometry(GLuint vao) {
    std::unique_ptr<ghoul::opengl::ProgramObject> program =
        ghoul::opengl::ProgramObject::Build(
            "RenderablePointCloud_Polygon",
            absPath("${MODULE_BASE}/shaders/polygon_vs.glsl"),
            absPath("${MODULE_BASE}/shaders/polygon_fs.glsl"),
            absPath("${MODULE_BASE}/shaders/polygon_gs.glsl")
        );

    program->activate();
    constexpr glm::vec4 Black = glm::vec4(0.f, 0.f, 0.f, 0.f);
    glClearBufferfv(GL_COLOR, 0, glm::value_ptr(Black));

    program->setUniform("sides", _nPolygonSides);

    glBindVertexArray(vao);
    glDrawArrays(GL_POINTS, 0, 1);
    glBindVertexArray(0);

    program->deactivate();
}

} // namespace openspace
