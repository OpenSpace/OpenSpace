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

#include <modules/video/include/videotileprovider.h>

#include <modules/globebrowsing/globebrowsingmodule.h>
#include <modules/globebrowsing/src/memoryawaretilecache.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/util/time.h>
#include <openspace/util/timemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr std::string_view _loggerCat = "VideoTileProvider";
} // namespace

namespace openspace::globebrowsing {

documentation::Documentation VideoTileProvider::Documentation() {
    documentation::Documentation doc = VideoPlayer::Documentation();
    doc.name = "VideoTileProvider";
    doc.id = "video_videotileprovider";
    return doc;
}

VideoTileProvider::VideoTileProvider(const ghoul::Dictionary& dictionary)
    : _videoPlayer(dictionary)
{
    ZoneScoped;
    addPropertySubOwner(_videoPlayer);
}

VideoTileProvider::~VideoTileProvider() {}

globebrowsing::Tile VideoTileProvider::tile(const globebrowsing::TileIndex& tileIndex) {
    ZoneScoped;

    if (!_videoPlayer.isInitialized()) {
        return Tile();
    }

    // Always check that our framebuffer is ok
    if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE) {
        LINFO("Framebuffer is not complete");
    }

    // For now, don't use the cache as we're trying to debug the problem w playback
    const uint64_t hash = tileIndex.hashKey();
    auto foundTile = _tileCache.find(hash);
    const bool textureChanged = foundTile != _tileCache.end() &&
        foundTile->second.texture != _videoPlayer.frameTexture().get();

    if (foundTile == _tileCache.end() || textureChanged) {
        _tileCache[hash] = Tile {
            _videoPlayer.frameTexture().get(),
            std::nullopt,
            Tile::Status::OK
        };
    }
    return _tileCache[hash];
}

Tile::Status VideoTileProvider::tileStatus(const TileIndex& tileIndex) {
    if (tileIndex.level > maxLevel()) {
        return Tile::Status::OutOfRange;
    }
    else if (_tileIsReady) {
        return Tile::Status::OK;
    }
    else {
        return Tile::Status::Unavailable;
    }
}

TileDepthTransform VideoTileProvider::depthTransform() {
    return { 0.f, 1.f };
}

void VideoTileProvider::update() {
    _videoPlayer.update();
}

void VideoTileProvider::reset() {
    _videoPlayer.reload();
}

ChunkTile VideoTileProvider::chunkTile(TileIndex tileIndex, int parents, int maxParents) {
    constexpr auto ascendToParent = [](TileIndex& ti, TileUvTransform&) {
        ti.level--;
    };

    const glm::vec2 nTiles = {
        std::pow(2, tileIndex.level),
        std::pow(2, tileIndex.level - 1)
    };
    const glm::vec2 ratios = glm::vec2(1.f / nTiles.x, 1.f / nTiles.y);
    const float offsetX = ratios.x * static_cast<float>(tileIndex.x);
    // The tiles on the y-axis should be traversed backwards
    const float offsetY = ratios.y * (nTiles.y - static_cast<float>(tileIndex.y) - 1.f);

    TileUvTransform uvTransform = { glm::vec2(offsetX, offsetY), ratios };

    return traverseTree(tileIndex, parents, maxParents, ascendToParent, uvTransform);
}

int VideoTileProvider::minLevel() {
    return 1;
}

int VideoTileProvider::maxLevel() {
    // This is the level where above the tile is marked as unavailable and is no longer
    // displayed. Since we want to display the tiles at all times we set the max level
    return 1337;
}

float VideoTileProvider::noDataValueAsFloat() {
    return std::numeric_limits<float>::min();
}

void VideoTileProvider::internalInitialize() {
    _videoPlayer.initialize();
}

void VideoTileProvider::internalDeinitialize() {
    _videoPlayer.destroy();
}

} // namespace openspace::globebrowsing
