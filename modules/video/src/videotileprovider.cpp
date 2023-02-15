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

namespace {
    constexpr std::string_view _loggerCat = "VideoTileProvider";

    constexpr openspace::properties::Property::PropertyInfo FileInfo = {
        "File",
        "File",
        "The file path that is used for this video provider. The file must point to a "
        "video that is then loaded and used for all tiles"
    };

    constexpr openspace::properties::Property::PropertyInfo StartTimeInfo = {
        "StartTime",
        "Start Time",
        "The date and time that the video should start in the format "
        "'YYYY MM DD hh:mm:ss'."
    };

    constexpr openspace::properties::Property::PropertyInfo EndTimeInfo = {
        "EndTime",
        "End Time",
        "The date and time that the video should end in the format "
        "'YYYY MM DD hh:mm:ss'."
    };

    constexpr openspace::properties::Property::PropertyInfo PlaybackModeInfo = {
        "PlaybackMode",
        "Playback Mode",
        "Determines the way the video should be played. The start and end time of the "
        "video can be set, or the video can be played as a loop in real time."
    };

    constexpr openspace::properties::Property::PropertyInfo PlayInfo = {
        "Play",
        "Play",
        "Play video"
    };

    constexpr openspace::properties::Property::PropertyInfo PauseInfo = {
        "Pause",
        "Pause",
        "Pause video"
    };

    constexpr openspace::properties::Property::PropertyInfo GoToStartInfo = {
        "GoToStart",
        "Go To Start",
        "Go to start in video"
    };

    struct [[codegen::Dictionary(VideoTileProvider)]] Parameters {
        // [[codegen::verbatim(FileInfo.description)]]
        std::filesystem::path file;

        // [[codegen::verbatim(StartTimeInfo.description)]]
        std::optional<std::string> startTime [[codegen::datetime()]];

        // [[codegen::verbatim(EndTimeInfo.description)]]
        std::optional<std::string> endTime [[codegen::datetime()]];

        enum class PlaybackMode {
            MapToSimulationTime = 0,
            RealTimeLoop
        };

        // The mode of how the video should be played back.
        // Default is video is played back according to the set start and end times.
        std::optional<PlaybackMode> playbackMode;

    };
#include "videotileprovider_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation VideoTileProvider::Documentation() {
    return codegen::doc<Parameters>("video_videotileprovider");
}

bool isDifferent(double first, double second) {
    return abs(first - second) > glm::epsilon<double>();
}

VideoTileProvider::VideoTileProvider(const ghoul::Dictionary& dictionary)
    : _videoPlayer(dictionary)
    , _play(PlayInfo)
    , _pause(PauseInfo)
    , _goToStart(GoToStartInfo)
{
    ZoneScoped

    const Parameters p = codegen::bake<Parameters>(dictionary);

    _videoFile = p.file;

    if (p.playbackMode.has_value()) {
        switch (*p.playbackMode) {
        case Parameters::PlaybackMode::RealTimeLoop:
            _playbackMode = PlaybackMode::RealTimeLoop;
            break;
        case Parameters::PlaybackMode::MapToSimulationTime:
            _playbackMode = PlaybackMode::MapToSimulationTime;
            break;
        default:
            LERROR("Missing playback mode in VideoTileProvider");
            throw ghoul::MissingCaseException();
        }
    }
    if (_playbackMode == PlaybackMode::RealTimeLoop) {
        // Video interaction. Only valid for real time looping
        _play.onChange([this]() { _videoPlayer.play(); });
        addProperty(_play);
        _pause.onChange([this]() { _videoPlayer.pause(); });
        addProperty(_pause);
        _goToStart.onChange([this]() { _videoPlayer.goToStart(); });
        addProperty(_goToStart);
    }
    else if (_playbackMode == PlaybackMode::MapToSimulationTime) {
        if (!p.startTime.has_value() || !p.endTime.has_value()) {
            LERROR("Video tile layer tried to map to simulation time but lacked start or"
                " end time");
            return;
        }
        _startJ200Time = Time::convertTime(*p.startTime);
        _endJ200Time = Time::convertTime(*p.endTime);
        ghoul_assert(_endJ200Time > _startJ200Time, "Invalid times for video");

        // Change the video time if OpenSpace time changes
        global::timeManager->addTimeJumpCallback([this]() {
            _videoPlayer.seekToTime(correctVideoPlaybackTime());
        });
        // Ensure we are synchronized to OpenSpace time in presync step
        global::callback::preSync->emplace_back([this]() {
            // This mode should always be paused as we're stepping through the frames
            //_videoPlayer.pause(); 
            syncToSimulationTime();
        });
    }
}

Tile VideoTileProvider::tile(const TileIndex& tileIndex) {
    ZoneScoped

        if (!_videoPlayer.isInitialized()) {
            return Tile();
        }

    // Always check that our framebuffer is ok
    if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE) {
        LINFO("Framebuffer is not complete");
    }

    // For now, don't use the cache as we're trying to debug the problem w playback
    uint64_t hash = tileIndex.hashKey();
    auto foundTile = _tileCache.find(hash);
    bool textureChanged = foundTile != _tileCache.end() &&
        foundTile->second.texture != _videoPlayer.frameTexture().get();

    if (foundTile == _tileCache.end() || textureChanged) {
        _tileCache[hash] = Tile{
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

void VideoTileProvider::update() {}

void VideoTileProvider::reset() {
    _videoPlayer.reset();
}

ChunkTile VideoTileProvider::chunkTile(TileIndex tileIndex, int parents, int maxParents) {

    lambda ascendToParent = [](TileIndex& ti, TileUvTransform& uv) {
        ti.level--;
    };

    glm::vec2 noOfTiles = { pow(2, tileIndex.level), pow(2, tileIndex.level - 1) };
    glm::vec2 ratios = { 1.f / noOfTiles.x, 1.f / noOfTiles.y };
    float offsetX = ratios.x * static_cast<float>(tileIndex.x);
    // The tiles on the y-axis should be traversed backwards
    float offsetY = ratios.y * (noOfTiles.y - static_cast<float>(tileIndex.y) - 1.f);

    TileUvTransform uvTransform = { glm::vec2(offsetX, offsetY), ratios };

    return traverseTree(tileIndex, parents, maxParents, ascendToParent, uvTransform);
}

bool VideoTileProvider::isWithingStartEndTime() const {
    const double now = global::timeManager->time().j2000Seconds();
    return now <= _endJ200Time && now >= _startJ200Time;
}

void VideoTileProvider::updateFrameDuration() {
    double openspaceVideoLength = (_endJ200Time - _startJ200Time) / _videoDuration;
    _frameDuration = (1.0 / _fps) * openspaceVideoLength;
}

double VideoTileProvider::correctVideoPlaybackTime() const {
    const double now = global::timeManager->time().j2000Seconds();
    double percentage = 0.0;
    if (now > _endJ200Time) {
        percentage = 1.0;
    }
    else if (now < _startJ200Time) {
        percentage = 0.0;
    }
    else {
        percentage = (now - _startJ200Time) / (_endJ200Time - _startJ200Time);
    }
    return percentage * _videoPlayer.videoDuration();
}

void VideoTileProvider::syncToSimulationTime() {
    if (_playbackMode == PlaybackMode::MapToSimulationTime) {
        bool fpsChanged = isDifferent(_videoPlayer.fps(), _fps);
        bool durationChanged = isDifferent(_videoPlayer.videoDuration(), _videoDuration);
        if (fpsChanged) {
            _fps = _videoPlayer.fps();
        }
        if (durationChanged) {
            _videoDuration = _videoPlayer.videoDuration();
        }

        if (fpsChanged || durationChanged) {
            updateFrameDuration();
        }
        if (!_videoPlayer.isPaused()) {
            //_videoPlayer.pause();
        }
        // If we are in valid times, step frames accordingly
        if (isWithingStartEndTime()) {
            double now = global::timeManager->time().j2000Seconds();
            double deltaTime = now - _timeAtLastRender;
            if (deltaTime > _frameDuration) {
                // Stepping forwards
                _videoPlayer.stepFrameForward();
                _timeAtLastRender = now;
            }
            else if (deltaTime < -_frameDuration) {
                // Stepping backwards
                _videoPlayer.stepFrameBackward();
                _timeAtLastRender = now;
            }
        }
        else if (!_videoPlayer.isPaused()) {
            _videoPlayer.pause();
        }
        // Make sure we are at the correct time
        double time = correctVideoPlaybackTime();
        bool shouldSeek = abs(time - _videoPlayer.currentPlaybackTime()) > _seekThreshold;
        if (shouldSeek) {
            _videoPlayer.seekToTime(time);
        }
    }
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

void VideoTileProvider::internalInitialize() {}

VideoTileProvider::~VideoTileProvider() {}

void VideoTileProvider::internalDeinitialize() {
    _videoPlayer.destroy();
}

} // namespace openspace::video
