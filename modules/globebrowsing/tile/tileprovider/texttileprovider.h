/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___TEXT_TILE_PROVIDER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___TEXT_TILE_PROVIDER___H__

#include <modules/globebrowsing/tile/tileprovider/tileprovider.h>

#include <modules/globebrowsing/globebrowsingmodule.h>
#include <modules/globebrowsing/cache/lrucache.h>
#include <modules/globebrowsing/tile/tileindex.h>
#include <modules/globebrowsing/tile/tiletextureinitdata.h>

#include <ghoul/opengl/ghoul_gl.h>

namespace ghoul::fontrendering {
    class Font;
    class FontRenderer;
} // namespace ghoul::fontrendering

namespace openspace::globebrowsing::tileprovider {

/**
 * Enables a simple way of providing tiles with any type of rendered text.
 * Internally it handles setting up a FBO for rendering the text, and defines a new
 * interface, consisting of only a single method for subclasses to implement:
 * renderText(const FontRenderer&, const TileIndex&) const;
 */
class TextTileProvider : public TileProvider {
public:
    /**
     * Default constructor with default values for texture and font size
     */
    TextTileProvider(const TileTextureInitData& initData, size_t fontSize = 48);
    virtual ~TextTileProvider() override;

    bool initialize() override;
    bool deinitialize() override;

    // The TileProvider interface below is implemented in this class
    virtual Tile tile(const TileIndex& tileIndex) override;
    virtual Tile::Status tileStatus(const TileIndex& index) override;
    virtual TileDepthTransform depthTransform() override;
    virtual void update() override;
    virtual void reset() override;
    virtual int maxLevel() override;

    /**
     * Allow overriding of hash function.
     * Default is <code>TileIndex::hashKey()</code>
     *
     * \param tileIndex tileIndex to hash
     * \returns hashkey used for in LRU cache for this tile
     */
    virtual TileIndex::TileHashKey toHash(const TileIndex& tileIndex) const;

    /**
     * Uses the fontRenderer to render some text onto the tile texture provided in
     * backgroundTile(const TileIndex& tileIndex).
     *
     * \param fontRenderer used for rendering text onto texture
     * \param tileIndex associated with the tile to be rendered onto
     */
    virtual void renderText(const ghoul::fontrendering::FontRenderer& fontRenderer,
        const TileIndex& tileIndex) const = 0;

protected:
    const TileTextureInitData _initData;
    std::shared_ptr<ghoul::fontrendering::Font> _font;
    size_t _fontSize;

private:
    Tile
        createChunkIndexTile(const TileIndex& tileIndex);
    std::unique_ptr<ghoul::fontrendering::FontRenderer> _fontRenderer;

    GLuint _fbo = 0;

    cache::MemoryAwareTileCache* _tileCache;
};

} // namespace openspace::globebrowsing::tileprovider

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___TEXT_TILE_PROVIDER___H__
