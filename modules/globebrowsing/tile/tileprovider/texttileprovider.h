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

#ifndef __TEXT_TILE_PROVIDER_H__
#define __TEXT_TILE_PROVIDER_H__

#include <memory>

#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h> // absPath
#include <ghoul/opengl/texture.h>

#include <ghoul/io/texture/texturereader.h>

#include <ghoul/font/fontrenderer.h>
#include <ghoul/font/fontmanager.h>

#include <modules/globebrowsing/tile/asynctilereader.h>
#include <modules/globebrowsing/tile/tileprovider/tileprovider.h>
#include <modules/globebrowsing/other/lrucache.h>
#include <modules/globebrowsing/geometry/ellipsoid.h>


//////////////////////////////////////////////////////////////////////////////////////////
//                                    TILE PROVIDER                                        //
//////////////////////////////////////////////////////////////////////////////////////////


namespace openspace {
    using namespace ghoul::fontrendering;

    /**
     * Enables a simple way of providing tiles with any type of rendered text. 
     * Internally it handles setting up a FBO for rendering the text, and defines a new 
     * interface, consisting of only a single method for subclasses to implement: 
     * renderText(const FontRenderer&, const ChunkIndex&) const;
     */
    class TextTileProvider : public TileProvider {
    public:

        /**
        * Default constructor with default values for texture and font size
        */
        TextTileProvider(const glm::uvec2& textureSize = {512, 512}, size_t fontSize = 48);
        virtual ~TextTileProvider();

        // The TileProvider interface below is implemented in this class

        virtual Tile getTile(const ChunkIndex& chunkIndex);
        virtual Tile getDefaultTile();
        virtual Tile::Status getTileStatus(const ChunkIndex& index);
        virtual TileDepthTransform depthTransform();
        virtual void update();
        virtual void reset();
        virtual int maxLevel();

        /**
        * Returns the tile which will be used to draw text onto. 
        * Default implementation returns a tile with a plain transparent texture.
        */
        virtual Tile backgroundTile(const ChunkIndex& chunkIndex) const;


        /**
        * Allow overriding of hash function. 
        * Default is <code>ChunkIndex::hashKey()</code>
        *
        * \param chunkIndex chunkIndex to hash
        * \returns hashkey used for in LRU cache for this tile
        */
        virtual ChunkHashKey toHash(const ChunkIndex& chunkIndex) const;
        
        /**
        * Uses the fontRenderer to render some text onto the tile texture provided in 
        * backgroundTile(const ChunkIndex& chunkIndex). 
        * 
        * \param fontRenderer used for rendering text onto texture
        * \param chunkIndex associated with the tile to be rendered onto
        */
        virtual void renderText(const FontRenderer& fontRenderer, const ChunkIndex& chunkIndex) const = 0;

    protected:
        std::shared_ptr<ghoul::fontrendering::Font> _font;
        glm::uvec2 _textureSize;
        size_t _fontSize;

    private:
        Tile createChunkIndexTile(const ChunkIndex& chunkIndex);
        std::unique_ptr<ghoul::fontrendering::FontRenderer> _fontRenderer;

        TileCache _tileCache;
        GLuint _fbo;
    };


    /**
     * Provides <code>Tile</code>s with the chunk index rendered as text onto its tiles.
     */
    class ChunkIndexTileProvider : public TextTileProvider {
    public:
        virtual void renderText(const FontRenderer& fontRenderer, const ChunkIndex& chunkIndex) const;
    };

    /**
    * Constructed with an ellipsoid and uses that to render the longitudal length of each
    * of each tile.
    */
    class SizeReferenceTileProvider : public TextTileProvider {
    public:
        SizeReferenceTileProvider(const ghoul::Dictionary& dictionary);

        virtual void renderText(const FontRenderer& fontRenderer, const ChunkIndex& chunkIndex) const;
        virtual Tile backgroundTile(const ChunkIndex& chunkIndex) const;

        virtual ChunkHashKey toHash(const ChunkIndex& chunkIndex) const;


    private:

        int roundedLongitudalLength(const ChunkIndex& chunkIndex) const;

        Ellipsoid _ellipsoid;
        Tile _backgroundTile;
    };

}  // namespace openspace




#endif  // __TEXT_TILE_PROVIDER_H__