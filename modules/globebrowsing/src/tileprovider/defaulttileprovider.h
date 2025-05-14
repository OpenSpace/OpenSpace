/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___TILEPROVIDER__DEFAULTTILEPROVIDER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___TILEPROVIDER__DEFAULTTILEPROVIDER___H__

#include <modules/globebrowsing/src/tileprovider/tileprovider.h>
#include <modules/globebrowsing/src/tilecacheproperties.h>
#include <modules/globebrowsing/src/asynctiledataprovider.h>
#include <memory>

namespace openspace::globebrowsing {

class DefaultTileProvider : public TileProvider {
public:
    explicit DefaultTileProvider(const ghoul::Dictionary& dictionary);

    Tile tile(const TileIndex& tileIndex) override final;
    Tile::Status tileStatus(const TileIndex& index) override final;
    TileDepthTransform depthTransform() override final;
    void update() override final;
    void reset() override final;
    int minLevel() override final;
    int maxLevel() override final;
    float noDataValueAsFloat() override final;

    static documentation::Documentation Documentation();

private:
    void initAsyncTileDataReader(TileTextureInitData initData,
        TileCacheProperties cacheProperties);

    properties::StringProperty _filePath;
    properties::IntProperty _tilePixelSize;

    std::unique_ptr<AsyncTileDataProvider> _asyncTextureDataProvider;
    layers::Group::ID _layerGroupID = layers::Group::ID::Unknown;
    bool _performPreProcessing = false;
    TileCacheProperties _cacheProperties;
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___TILEPROVIDER__DEFAULTTILEPROVIDER___H__
