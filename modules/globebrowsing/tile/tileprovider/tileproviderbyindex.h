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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___TILE_PROVIDER_BY_INDEX___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___TILE_PROVIDER_BY_INDEX___H__

#include <modules/globebrowsing/tile/tileprovider/tileprovider.h>

namespace openspace {
namespace globebrowsing {

class TileProviderByIndex : public TileProvider {
public:
    TileProviderByIndex(const ghoul::Dictionary& dictionary);
    TileProviderByIndex(const std::string& imagePath);
    virtual ~TileProviderByIndex() { }

    virtual Tile getTile(const TileIndex& tileIndex);
    virtual Tile getDefaultTile();
    virtual Tile::Status getTileStatus(const TileIndex& index);
    virtual TileDepthTransform depthTransform();
    virtual void update();
    virtual void reset();
    virtual int maxLevel();

private:
    TileProvider* indexProvider(const TileIndex& tileIndex) const;

    std::unordered_map<TileHashKey, std::shared_ptr<TileProvider>> _tileProviderMap;
    std::shared_ptr<TileProvider> _defaultTileProvider;
};

} // namespace globebrowsing
} // namespace openspace

#endif  // __OPENSPACE_MODULE_GLOBEBROWSING___TILE_PROVIDER_BY_INDEX___H__
