/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___TILEPROVIDER__VIDEOTILEPROVIDER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___TILEPROVIDER__VIDEOTILEPROVIDER___H__

#include <modules/globebrowsing/src/tileprovider/tileprovider.h>
#include <modules/video/include/videoplayer.h>

#include <openspace/properties/triggerproperty.h>
#include <openspace/properties/scalar/doubleproperty.h>
#include <openspace/properties/vector/ivec2property.h>
#include <ghoul/glm.h>

// libmpv
#include <client.h>
#include <render_gl.h>

namespace openspace { struct Documentation; }

namespace openspace {
using namespace globebrowsing;

class VideoTileProvider : public TileProvider {
public:
    VideoTileProvider(const ghoul::Dictionary& dictionary);
    ~VideoTileProvider();

    void update() override final;
    void reset() override final;
    int minLevel() override final;
    int maxLevel() override final;
    float noDataValueAsFloat() override final;
    ChunkTile chunkTile(TileIndex tileIndex, int parents, int maxParents = 1337) override;
    Tile tile(const TileIndex& tileIndex) override final;
    Tile::Status tileStatus(const TileIndex& tileIndex) override final;
    TileDepthTransform depthTransform() override final;
    void syncToSimulationTime(); 

    static documentation::Documentation Documentation();

private:
    properties::TriggerProperty _play;
    properties::TriggerProperty _pause;
    properties::TriggerProperty _goToStart;

    enum class PlaybackMode {
        MapToSimulationTime = 0,
        RealTimeLoop
    };
    
    // Map to simulation time functions
    double correctVideoPlaybackTime() const;
    bool isWithingStartEndTime() const;

    void internalInitialize() override final;
    void internalDeinitialize() override final;

    PlaybackMode _playbackMode = PlaybackMode::RealTimeLoop; // Default is to loop
    std::filesystem::path _videoFile;

    // Video stretching: map to simulation time animation mode
    double _startJ200Time = 0.0;
    double _endJ200Time = 0.0;
    double _timeAtLastRender = 0.0;
    double _frameDuration = 0.0;

    // Tile handling
    std::map<TileIndex::TileHashKey, Tile> _tileCache; // Cache for rendering 1 frame
    bool _tileIsReady = false;
    double _seekThreshold = 1.0; // Threshold to ensure we seek to a different time

    VideoPlayer _videoPlayer;
};

} // namespace video::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___TILEPROVIDER__VIDEOTILEPROVIDER___H__
