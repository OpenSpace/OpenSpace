/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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
{
    ZoneScoped;

    tileCache = global::moduleEngine->module<GlobeBrowsingModule>()->tileCache();
}

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
                                  const glm::vec2& position, const glm::vec4& color)
{
    ZoneScoped;
    TracyGpuZone("tile");

    cache::ProviderTileKey key = { tileIndex, uniqueIdentifier };
    Tile tile = tileCache->get(key);
    if (!tile.texture) {
        ghoul::opengl::Texture* texture = tileCache->texture(initData);

        // Keep track of defaultFBO and viewport to be able to reset state when done
        GLint defaultFBO = global::renderEngine->openglStateCache().defaultFramebuffer();

        // Render to texture
        glBindFramebuffer(GL_FRAMEBUFFER, fbo);
        glFramebufferTexture2D(
            GL_FRAMEBUFFER,
            GL_COLOR_ATTACHMENT0,
            GL_TEXTURE_2D,
            *texture,
            0
        );

        GLsizei w = static_cast<GLsizei>(texture->width());
        GLsizei h = static_cast<GLsizei>(texture->height());
        glViewport(0, 0, w, h);
        glClearColor(0.f, 0.f, 0.f, 0.f);
        glClear(GL_COLOR_BUFFER_BIT);

        fontRenderer->render(*font, position, text, color);

        glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);
        global::renderEngine->openglStateCache().resetViewportState();

        tile = Tile{ texture, std::nullopt, Tile::Status::OK };
        tileCache->put(key, initData.hashKey, tile);
    }
    return tile;
}

void TextTileProvider::reset() {
    ZoneScoped;

    tileCache->clear();
}

} // namespace openspace::globebrowsing
