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

#include <openspace/properties/triggerproperty.h>
#include <ghoul/glm.h>

// libmpv
#include <client.h>
#include <render_gl.h>

namespace openspace { struct Documentation; }

namespace openspace::globebrowsing {

class VideoTileProvider : public TileProvider {
public:
    static constexpr glm::ivec2 TileSize = { 512, 512 };
    //static constexpr glm::ivec2 FinalResolution = { 2048, 1024 };
    static constexpr int NumTilePixels = 262144;

    VideoTileProvider(const ghoul::Dictionary& dictionary);
    ~VideoTileProvider();

    Tile tile(const TileIndex& tileIndex) override final;
    Tile::Status tileStatus(const TileIndex& tileIndex) override final;
    TileDepthTransform depthTransform() override final;
    void update() override final;
    void reset() override final;
    int minLevel() override final;
    int maxLevel() override final;
    float noDataValueAsFloat() override final;
    ChunkTile chunkTile(TileIndex tileIndex, int parents, int maxParents = 1337) override;

    void pause();
    void play();
    void goToStart();

    static documentation::Documentation Documentation();

private:
    properties::TriggerProperty _play;
    properties::TriggerProperty _pause;
    properties::TriggerProperty _goToStart;

    void createFBO(int width, int height);
    void resizeFBO(int width, int height);
    double correctVideoPlaybackTime() const;
    bool isWithingStartEndTime() const;
    void pauseVideoIfOutsideValidTime();
    void seekToTime(double time);
    void updateStretchingOfTime();

    // Libmpv
    void initializeMpv(); // Called first time in postSyncPreDraw
    void renderMpv(); // Called in postSyncPreDraw
    void handleMpvEvents();
    void handleMpvProperties(mpv_event* event);
    void swapBuffersMpv(); // Called in postDraw
    void cleanUpMpv(); // Called in internalDeinitialze
    static void on_mpv_render_update(void*);

    void internalInitialize() override final;
    void internalDeinitialize() override final;

    enum class AnimationMode {
        MapToSimulationTime = 0,
        RealTimeLoop
    };

    AnimationMode _animationMode = AnimationMode::RealTimeLoop; // Default is to loop
    std::filesystem::path _videoFile;
    double _startJ200Time = 0.0;
    double _endJ200Time = 0.0;
    double _videoDuration = -1.0;
    double _currentVideoTime = 0.0;
    double _prevVideoTime = 0.0;
    double _frameTime = 1.0 / 24.0;
    bool _hasReachedEnd = false;
    bool _tileIsReady = false;
    bool _isInitialized = false;
    bool _isWaiting = false;
    glm::ivec2 _resolution = { 2048, 1024 };

    // libmpv
    mpv_handle* _mpvHandle = nullptr;
    mpv_render_context* _mpvRenderContext = nullptr;
    std::unique_ptr<ghoul::opengl::Texture>_frameTexture = nullptr;
    mpv_opengl_fbo _mpvFbo;
    GLuint _fbo = 0;
    static int _wakeup;
    bool _didRender = false;
    bool _isPaused = false;

    // libmpv property keys
    enum class LibmpvPropertyKey : uint64_t {
        Duration = 1,
        Eof,
        Height,
        Meta,
        Params,
        Time,
        Command,
        Width,
        FrameCount,
        Pause
    };

    void observePropertyMpv(std::string name, mpv_format format, LibmpvPropertyKey key);
    void setPropertyStringMpv(std::string name, std::string value);
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___TILEPROVIDER__VIDEOTILEPROVIDER___H__
