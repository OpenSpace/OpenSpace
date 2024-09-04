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

#include <modules/globebrowsing/src/tileprovider/texttileprovider.h>

#include <modules/globebrowsing/globebrowsingmodule.h>
#include <modules/globebrowsing/src/memoryawaretilecache.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/opengl/openglstatecache.h>

namespace openspace::globebrowsing {

TextTileProvider::TextTileProvider(TileTextureInitData initData_, size_t fontSize_)
    : initData(std::move(initData_))
    , fontSize(fontSize_)
    , tileCache(global::moduleEngine->module<GlobeBrowsingModule>()->tileCache())
{}

TextTileProvider::~TextTileProvider() {}

void TextTileProvider::internalInitialize() {
    ZoneScoped;

    font = global::fontManager->font("Mono", static_cast<float>(fontSize));
    fontRenderer = ghoul::fontrendering::FontRenderer::createDefault();
    fontRenderer->setFramebufferSize(glm::vec2(initData.dimensions));
    glGenFramebuffers(1, &fbo);
}

void TextTileProvider::internalDeinitialize() {
    glDeleteFramebuffers(1, &fbo);
}

Tile TextTileProvider::renderTile(const TileIndex& tileIndex, const std::string& text,
                                  const glm::vec2& position, const glm::vec4& color,
                                  const glm::vec4& backgroundColor)
{
    ZoneScoped;
    TracyGpuZone("tile");

    const cache::ProviderTileKey key = { tileIndex, uniqueIdentifier };
    Tile tile = tileCache->get(key);
    if (!tile.texture) {
        ghoul::opengl::Texture* texture = tileCache->texture(initData);

        GLint prevProgram = 0;
        GLint prevFBO = 0;
        glGetIntegerv(GL_CURRENT_PROGRAM, &prevProgram);
        glGetIntegerv(GL_FRAMEBUFFER_BINDING, &prevFBO);

        // Render to texture
        glBindFramebuffer(GL_FRAMEBUFFER, fbo);
        glFramebufferTexture2D(
            GL_FRAMEBUFFER,
            GL_COLOR_ATTACHMENT0,
            GL_TEXTURE_2D,
            *texture,
            0
        );

        const GLsizei w = static_cast<GLsizei>(texture->width());
        const GLsizei h = static_cast<GLsizei>(texture->height());
        global::renderEngine->openglStateCache().loadCurrentGLState();
        glViewport(0, 0, w, h);
        glClearColor(
            backgroundColor.r,
            backgroundColor.g,
            backgroundColor.b,
            backgroundColor.a
        );
        glClear(GL_COLOR_BUFFER_BIT);

        fontRenderer->render(*font, position, text, color);

        texture->setFilter(ghoul::opengl::Texture::FilterMode::LinearMipMap);

        tile = Tile{ texture, std::nullopt, Tile::Status::OK };
        tileCache->put(key, initData.hashKey, tile);

        // Reset FBO, shader program and viewport
        glUseProgram(prevProgram);
        glBindFramebuffer(GL_FRAMEBUFFER, prevFBO);
        global::renderEngine->openglStateCache().resetCachedStates();
    }
    return tile;
}

void TextTileProvider::reset() {
    ZoneScoped;

    tileCache->clear();
}

} // namespace openspace::globebrowsing
