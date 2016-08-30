/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2016                                                               *
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

#include <modules/globebrowsing/chunk/chunkindex.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>

#include <ghoul/font/fontrenderer.h>
#include <ghoul/font/fontmanager.h>

#include <openspace/engine/openspaceengine.h>

#include <sstream>




namespace {
    const std::string _loggerCat = "TextTileProvider";
}


namespace openspace {

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

    Tile TextTileProvider::getTile(const ChunkIndex& chunkIndex) {
        ChunkHashKey key = chunkIndex.hashKey();
        
        if (!_tileCache.exist(key)) {
            _tileCache.put(key, createChunkIndexTile(chunkIndex));
        }

        return _tileCache.get(key);
    }

    Tile TextTileProvider::getDefaultTile() {
        return Tile::TileUnavailable;
    }


    Tile::Status TextTileProvider::getTileStatus(const ChunkIndex& index) {
        return Tile::Status::OK;
    }

    TileDepthTransform TextTileProvider::depthTransform() {
        TileDepthTransform transform;
        transform.depthOffset = 0.0f;
        transform.depthScale = 1.0f;
        return transform;
    }

    void TextTileProvider::update() {
        // nothing to be done
    }

    void TextTileProvider::reset() {
        _tileCache.clear();
    }

    Tile TextTileProvider::createChunkIndexTile(const ChunkIndex& chunkIndex) {
        glm::uvec4 color = { 0, 0, 0, 0 };
        Tile tile = Tile::createPlainTile(_textureSize, color);

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
        //LDEBUG(status);

        glViewport(
            0, 0,
            static_cast<GLsizei>(tile.texture->width()),
            static_cast<GLsizei>(tile.texture->height())
            );
        
        ghoul_assert(_fontRenderer != nullptr, "_fontRenderer must not be null");
        renderText(*_fontRenderer, chunkIndex);

        // Reset state: bind default FBO and set viewport to what it was
        glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);
        glViewport(viewport[0], viewport[1], viewport[2], viewport[3]);

        return tile;
    }

    int TextTileProvider::maxLevel() {
        return 1337; // unlimited
    }




    void ChunkIndexTileProvider::renderText(const FontRenderer& fontRenderer, const ChunkIndex& chunkIndex) const {
        fontRenderer.render(
            *_font,
            glm::vec2(
                _textureSize.x / 4 - (_textureSize.x / 32) * log10(1 << chunkIndex.level),
                _textureSize.y / 2 + _fontSize),
            glm::vec4(1.0, 0.0, 0.0, 1.0),
            "level: %i \nx: %i \ny: %i",
            chunkIndex.level, chunkIndex.x, chunkIndex.y
            );
    }





}  // namespace openspace
