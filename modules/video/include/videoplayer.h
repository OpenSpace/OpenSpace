/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <openspace/documentation/documentation.h>
#include <openspace/properties/triggerproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <ghoul/glm.h>
#include <ghoul/misc/boolean.h>
#include <ghoul/opengl/texture.h>
#include <map>

// libmpv
#include <client.h>
#include <render_gl.h>

namespace openspace {

enum class PlaybackMode {
    MapToSimulationTime = 0,
    RealTimeLoop
};

class VideoPlayer : public properties::PropertyOwner, public Syncable {
BooleanType(PauseAfterSeek);

public:
    VideoPlayer(const ghoul::Dictionary& dictionary);
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

    virtual void preSync(bool isMaster) override;
    virtual void encode(SyncBuffer* syncBuffer) override;
    virtual void decode(SyncBuffer* syncBuffer) override;
    virtual void postSync(bool isMaster) override;

    static documentation::Documentation Documentation();

private:
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
        Loop
    };
    // Framebuffer
    void createTexture(glm::ivec2 size);
    void resizeTexture(glm::ivec2 size);

    // Libmpv
    static void onMpvRenderUpdate(void*); // Has to be static because of C api
    void initializeMpv(); // Called first time in update
    void renderMpv(); // Called in update
    void renderFrame(); // Renders a frame; called in renderMpv
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
    properties::TriggerProperty _play;
    properties::TriggerProperty _pause;
    properties::TriggerProperty _goToStart;
    properties::TriggerProperty _reload;
    properties::BoolProperty _playAudio;
    properties::BoolProperty _loopVideo;

    // Video properties. Try to read all these values from the video
    std::filesystem::path _videoFile;
    double _currentVideoTime = 0.0;
    double _fps = 24.0; // If when we read it it is 0, use 24 fps
    double _videoDuration = 0.0;
    glm::ivec2 _videoResolution = glm::ivec2(2048, 1024); // Used for the fbos
    bool _isPaused = false;
    PlaybackMode _playbackMode = PlaybackMode::RealTimeLoop; // Default is to loop

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
    GLuint _fbo = 0; // Our opengl framebuffer where mpv renders to
    int _wakeup = 0; // Signals when libmpv has a new frame ready
    bool _isInitialized = false; // If libmpv has been inititalized
    bool _isSeeking = false; // Prevent seeking while already seeking
    bool _isDestroying = false;
    double _seekThreshold = 1.0; // Threshold to ensure we seek to a different time
};
} // namespace video::globebrowsing

#endif // __OPENSPACE_MODULE_VIDEO___VIDEOPLAYER___H__
