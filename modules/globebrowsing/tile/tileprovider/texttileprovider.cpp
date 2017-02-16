/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <modules/globebrowsing/tile/tileprovider/texttileprovider.h>

#include <modules/globebrowsing/geometry/geodeticpatch.h>
#include <modules/globebrowsing/tile/tileindex.h>

#include <openspace/engine/openspaceengine.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/misc/dictionary.h>

using namespace ghoul::fontrendering;

namespace openspace {
namespace globebrowsing {
namespace tileprovider {
    
TextTileProvider::TextTileProvider(const glm::uvec2& textureSize, size_t fontSize)
    : _tileCache(500)
    , _textureSize(textureSize)
    , _fontSize(fontSize)
{
    _font = OsEng.fontManager().font("Mono", _fontSize);
        
    _fontRenderer = std::unique_ptr<FontRenderer>(FontRenderer::createDefault());
    _fontRenderer->setFramebufferSize(textureSize);

    glGenFramebuffers(1, &_fbo);
}

TextTileProvider::~TextTileProvider() {
    glDeleteFramebuffers(1, &_fbo);
}

Tile TextTileProvider::getTile(const TileIndex& tileIndex) {
    TileIndex::TileHashKey key = tileIndex.hashKey();
        
    if (!_tileCache.exist(key)) {
        _tileCache.put(key, createChunkIndexTile(tileIndex));
    }

    return _tileCache.get(key);
}

Tile TextTileProvider::getDefaultTile() {
    return Tile::TileUnavailable;
}

Tile::Status TextTileProvider::getTileStatus(const TileIndex& index) {
    return Tile::Status::OK;
}

TileDepthTransform TextTileProvider::depthTransform() {
    TileDepthTransform transform;
    transform.depthOffset = 0.0f;
    transform.depthScale = 1.0f;
    return transform;
}

void TextTileProvider::update() {}

void TextTileProvider::reset() {
    _tileCache.clear();
}

Tile TextTileProvider::createChunkIndexTile(const TileIndex& tileIndex) {
    Tile tile = backgroundTile(tileIndex);

    // Keep track of defaultFBO and viewport to be able to reset state when done
    GLint defaultFBO;
    GLint viewport[4];
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFBO);
    glGetIntegerv(GL_VIEWPORT, viewport);

    // Render to texture
    glBindFramebuffer(GL_FRAMEBUFFER, _fbo);
    glFramebufferTexture2D(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        GL_TEXTURE_2D,
        *(tile.texture),
        0
    );

    GLenum status = glCheckFramebufferStatus(GL_FRAMEBUFFER);

    glViewport(
        0, 0,
        static_cast<GLsizei>(tile.texture->width()),
        static_cast<GLsizei>(tile.texture->height())
    );
        
    ghoul_assert(_fontRenderer != nullptr, "_fontRenderer must not be null");
    renderText(*_fontRenderer, tileIndex);

    // Reset state: bind default FBO and set viewport to what it was
    glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);
    glViewport(viewport[0], viewport[1], viewport[2], viewport[3]);

    return tile;
}

int TextTileProvider::maxLevel() {
    return 1337; // unlimited
}

TileIndex::TileHashKey TextTileProvider::toHash(const TileIndex& tileIndex) const {
    return tileIndex.hashKey();
}

Tile TextTileProvider::backgroundTile(const TileIndex& tileIndex) const {
    glm::uvec4 color = { 0, 0, 0, 0 };
    return Tile::createPlainTile(_textureSize, color);
}

} // namespace tileprovider
} // namespace globebrowsing
} // namespace openspace
