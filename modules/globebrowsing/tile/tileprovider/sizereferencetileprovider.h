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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___SIZEREFERENCE_TILE_PROVIDER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___SIZEREFERENCE_TILE_PROVIDER___H__

#include <modules/globebrowsing/tile/tileprovider/texttileprovider.h>

#include <modules/globebrowsing/geometry/ellipsoid.h>

namespace openspace::globebrowsing::tileprovider {

/**
 * Constructed with an ellipsoid and uses that to render the longitudal length of each
 * of each tile.
 */
class SizeReferenceTileProvider : public TextTileProvider {
public:
    SizeReferenceTileProvider(const ghoul::Dictionary& dictionary);

    virtual void renderText(const ghoul::fontrendering::FontRenderer& fontRenderer,
        const TileIndex& tileIndex) const override;

    virtual TileIndex::TileHashKey toHash(const TileIndex& tileIndex) const override;

private:
    int roundedLongitudalLength(const TileIndex& tileIndex) const;

    Ellipsoid _ellipsoid;
    Tile _backgroundTile = Tile::TileUnavailable;
};

} // namespace openspace::globebrowsing::tileprovider

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___SIZEREFERENCE_TILE_PROVIDER___H__
