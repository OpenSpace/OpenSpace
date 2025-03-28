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

#ifndef __OPENSPACE_MODULE_VIDEO___VIDEOTILEPROVIDER___H__
#define __OPENSPACE_MODULE_VIDEO___VIDEOTILEPROVIDER___H__

#include <modules/globebrowsing/src/tileprovider/tileprovider.h>

#include <modules/video/include/videoplayer.h>
#include <openspace/properties/misc/triggerproperty.h>
#include <openspace/properties/scalar/doubleproperty.h>
#include <openspace/properties/vector/ivec2property.h>
#include <ghoul/glm.h>

// libmpv
#include <client.h>
#include <render_gl.h>

namespace openspace { struct Documentation; }

namespace openspace::globebrowsing {

class VideoTileProvider : public TileProvider {
public:
    explicit VideoTileProvider(const ghoul::Dictionary& dictionary);
    ~VideoTileProvider() override;

    void update() override final;
    void reset() override final;
    int minLevel() override final;
    int maxLevel() override final;
    float noDataValueAsFloat() override final;
    ChunkTile chunkTile(TileIndex tileIndex, int parents, int maxParents = 1337) override;
    Tile tile(const TileIndex& tileIndex) override final;
    Tile::Status tileStatus(const TileIndex& tileIndex) override final;
    TileDepthTransform depthTransform() override final;

    static documentation::Documentation Documentation();

private:
    void internalInitialize() override final;
    void internalDeinitialize() override final;

    // Tile handling
    // Cache for rendering 1 frame
    std::map<TileIndex::TileHashKey, Tile> _tileCache;
    bool _tileIsReady = false;

    VideoPlayer _videoPlayer;
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_VIDEO___VIDEOTILEPROVIDER___H__
