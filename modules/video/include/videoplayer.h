/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#ifndef __OPENSPACE_MODULE_VIDEO___VIDEOPLAYER___H__
#define __OPENSPACE_MODULE_VIDEO___VIDEOPLAYER___H__

#include <openspace/properties/propertyowner.h>
#include <openspace/util/syncable.h>

#include <openspace/properties/misc/triggerproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <ghoul/glm.h>
#include <ghoul/misc/boolean.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/texture.h>
#include <client.h>
#include <render.h>
#include <chrono>
#include <cstdint>
#include <filesystem>
#include <memory>

namespace ghoul { class Dictionary; }

namespace openspace {

struct Documentation;

enum class PlaybackMode {
    MapToSimulationTime = 0,
    RealTimeLoop
};

class VideoPlayer : public PropertyOwner, public Syncable {
public:
    BooleanType(PauseAfterSeek);

    explicit VideoPlayer(const ghoul::Dictionary& dictionary);
    ~VideoPlayer() override;

    void initialize();

    // Video interaction
    void pause();
    void play();
    void goToStart();

    void seekToTime(double time, PauseAfterSeek pauseAfter = PauseAfterSeek::Yes);
    void toggleMute();

    const std::unique_ptr<ghoul::opengl::Texture>& frameTexture() const;
    bool isInitialized() const;

    void reload();
    void destroy();
    void update();

    void preSync(bool isMaster) override;
    void encode(SyncBuffer* syncBuffer) override;
    void decode(SyncBuffer* syncBuffer) override;
    void postSync(bool isMaster) override;

    static openspace::Documentation Documentation();

private:
    // Flags used to syncronize playback
    struct syncFlags {
        bool goToStart = false;
        bool play = false;
    };

    // State keys
    enum class PlaybackState : uint16_t {
        Unitialized = 0,
        Waiting,
        Playing,
        Paused
    };

    // Libmpv keys
    enum class MpvKey : uint64_t {
        Duration = 1, // 0 is the default key in libmpv so avoid that
        Height,
        Width,
        Meta,
        Time,
        Fps,
        Pause,
        IsSeeking,
        Mute,
        Command,
        Seek,
        Loop,
        EndOfFile
    };

    // Framebuffer
    void createTexture(glm::ivec2 size);
    void resizeTexture(glm::ivec2 size);

    /**
     * Called first time in update.
     */
    void initializeMpv();

    /**
     * Called in update.
     */
    void renderMpv();

    /**
     * Renders a frame; called in renderMpv.
     */
    void renderFrame();
    void commandAsyncMpv(const char* cmd[], MpvKey key = MpvKey::Command);
    void handleMpvEvents();
    // Libmpv properties
    void handleMpvProperties(mpv_event* event);
    void observePropertyMpv(MpvKey key);
    void setPropertyStringMpv(const char* name, const char* value);
    void setPropertyAsyncMpv(int value, MpvKey key);
    void setPropertyAsyncMpv(const char* value, MpvKey key);
    void setPropertyAsyncMpv(double value, MpvKey key);
    void getPropertyAsyncMpv(MpvKey key);

    // Map to simulation time functions
    double correctVideoPlaybackTime() const;
    bool isWithingStartEndTime() const;
    void updateFrameDuration();

    // Properties for user interaction
    TriggerProperty _play;
    TriggerProperty _pause;
    TriggerProperty _goToStart;
    TriggerProperty _reload;
    BoolProperty _playAudio;
    BoolProperty _loopVideo;

    // Variables used when syncronizing play, pause and looping behavior
    const bool _isMaster;
    syncFlags _syncflags;
    PlaybackState _playbackState = PlaybackState::Unitialized;
    std::chrono::duration<long long, std::milli> _goTime;

    // Video properties. Try to read all these values from the video
    std::filesystem::path _videoFile;
    double _currentVideoTime = 0.0;
    /// If when we read it it is 0, use 24 fps
    double _fps = 24.0;
    double _videoDuration = 0.0;
    /// Used for the fbos
    glm::ivec2 _videoResolution = glm::ivec2(2048, 1024);
    bool _isPaused = false;
    /// Default is to loop
    PlaybackMode _playbackMode = PlaybackMode::RealTimeLoop;

    // Maps for keeping track of libmpv commands and formats
    std::map<MpvKey, const char*> keys;
    std::map<MpvKey, mpv_format> formats;

    // Syncing with multiple nodes
    double _correctPlaybackTime = 0.0;

    // Video stretching: map to simulation time animation mode
    double _startJ200Time = 0.0;
    double _endJ200Time = 0.0;
    double _frameDuration = 0.0;

    // Libmpv
    mpv_handle* _mpvHandle = nullptr;
    mpv_render_context* _mpvRenderContext = nullptr;
    std::unique_ptr<ghoul::opengl::Texture> _frameTexture;
    /// Our OpenGL framebuffer where mpv renders to
    GLuint _fbo = 0;
    /// If libmpv has been inititalized
    bool _isInitialized = false;
    /// Prevent seeking while already seeking
    bool _isSeeking = false;
    bool _isDestroying = false;
    /// Threshold to ensure we seek to a different time
    double _seekThreshold = 1.0;
};
} // namespace video

#endif // __OPENSPACE_MODULE_VIDEO___VIDEOPLAYER___H__
