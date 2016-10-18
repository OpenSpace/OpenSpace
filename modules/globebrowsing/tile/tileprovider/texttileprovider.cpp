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

#include <modules/globebrowsing/tile/tileindex.h>

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
namespace globebrowsing {

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
        TileHashKey key = tileIndex.hashKey();
        
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

    void TextTileProvider::update() {
        // nothing to be done
    }

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
        //LDEBUG(status);

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

    TileHashKey TextTileProvider::toHash(const TileIndex& tileIndex) const {
        return tileIndex.hashKey();
    }


    Tile TextTileProvider::backgroundTile(const TileIndex& tileIndex) const {
        glm::uvec4 color = { 0, 0, 0, 0 };
        return Tile::createPlainTile(_textureSize, color);
    }

    //////////////////////////////////////////////////////////////////////////////////////
    //                             Chunk Index Tile Provider                            //
    //////////////////////////////////////////////////////////////////////////////////////

    void TileIndexTileProvider::renderText(const FontRenderer& fontRenderer, const TileIndex& tileIndex) const {
        fontRenderer.render(
            *_font,
            glm::vec2(
                _textureSize.x / 4 - (_textureSize.x / 32) * log10(1 << tileIndex.level),
                _textureSize.y / 2 + _fontSize),
            glm::vec4(1.0, 0.0, 0.0, 1.0),
            "level: %i \nx: %i \ny: %i",
            tileIndex.level, tileIndex.x, tileIndex.y
            );
    }

    //////////////////////////////////////////////////////////////////////////////////////
    //                         Tile Size Reference Tile Provider                        //
    //////////////////////////////////////////////////////////////////////////////////////

    namespace {
        const std::string KeyRadii = "Radii";
        const std::string KeyBackgroundImagePath = "BackgroundImagePath";
    }

    SizeReferenceTileProvider::SizeReferenceTileProvider(const ghoul::Dictionary& dictionary) {
        _fontSize = 50;
        ghoul::fontrendering::FontManager& fm = OsEng.fontManager();
        _font = fm.font("Mono", _fontSize);

        glm::dvec3 radii(1,1,1);
        if (!dictionary.getValue(KeyRadii, radii)) {
            throw std::runtime_error("Must define key '" + KeyRadii + "'");
        }
        _ellipsoid = Ellipsoid(radii);

        _backgroundTile.status = Tile::Status::Unavailable;
        std::string backgroundImagePath;
        if (dictionary.getValue(KeyBackgroundImagePath, backgroundImagePath)) {
            using namespace ghoul::io;
            std::string imgAbsPath = absPath(backgroundImagePath);
            _backgroundTile.texture = TextureReader::ref().loadTexture(imgAbsPath);
            _backgroundTile.texture->uploadTexture();
            _backgroundTile.texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
            _backgroundTile.status = Tile::Status::OK;
        }

    }

    void SizeReferenceTileProvider::renderText(const FontRenderer& fontRenderer, const TileIndex& tileIndex) const {
        GeodeticPatch patch(tileIndex);
        bool aboveEquator = patch.isNorthern();
        
        double tileLongitudalLength = roundedLongitudalLength(tileIndex);

        std::string unit = "m";
        if (tileLongitudalLength > 9999) {
            tileLongitudalLength *= 0.001;
            unit = "km";
        }

        glm::vec2 textPosition;
        textPosition.x = 0;
        textPosition.y = aboveEquator ? _fontSize / 2 : _textureSize.y - 3 * _fontSize / 2;
        glm::vec4 color(1.0, 1.0, 1.0, 1.0);

        fontRenderer.render(
            *_font,
            textPosition,
            color,
            " %.0f %s",
            tileLongitudalLength, unit.c_str()
            );
    }

    int SizeReferenceTileProvider::roundedLongitudalLength(const TileIndex& tileIndex) const {
        GeodeticPatch patch(tileIndex);
        bool aboveEquator = patch.isNorthern();
        double lat = aboveEquator ? patch.minLat() : patch.maxLat();
        double lon1 = patch.minLon();
        double lon2 = patch.maxLon();
        int l = static_cast<int>(_ellipsoid.longitudalDistance(lat, lon1, lon2));

        bool useKm = l > 9999;
        if (useKm) l /= 1000;
        l = std::round(l);
        if (useKm) l *= 1000;

        return l;
    }

    TileHashKey SizeReferenceTileProvider::toHash(const TileIndex& tileIndex) const {
        int l = roundedLongitudalLength(tileIndex);
        TileHashKey key = static_cast<TileHashKey>(l);
        return key;
    }

    Tile SizeReferenceTileProvider::backgroundTile(const TileIndex& tileIndex) const {
        if (_backgroundTile.status == Tile::Status::OK) {
            Tile tile;
            auto t = _backgroundTile.texture;
            void* pixelData = new char[t->expectedPixelDataSize()];
            memcpy(pixelData, t->pixelData(), t->expectedPixelDataSize());
            tile.texture = std::make_shared<Texture>(
                pixelData, t->dimensions(), t->format(), t->internalFormat(), t->dataType(), t->filter(), t->wrapping());
            tile.texture->uploadTexture();
            tile.texture->setDataOwnership(Texture::TakeOwnership::Yes);
            tile.status = Tile::Status::OK;
            return tile;
        }
        else {
            // use default background
            return TextTileProvider::backgroundTile(tileIndex);
        }
    }

} // namespace globebrowsing
} // namespace openspace
