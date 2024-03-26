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

#include <modules/video/include/videoplayer.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/syncengine.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/interaction/sessionrecording.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/time.h>
#include <openspace/util/timemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/framebufferobject.h>
#include <ghoul/opengl/openglstatecache.h>


namespace {
    constexpr std::string_view _loggerCat = "VideoPlayer";

    constexpr openspace::properties::Property::PropertyInfo VideoInfo = {
        "Video",
        "Video",
        "This should point to the video file that should be played."
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
        "Sets the time to the beginning of the video and pauses it."
    };

    constexpr openspace::properties::Property::PropertyInfo ReloadInfo = {
        "Reload",
        "Reload",
        "Reloads the video and creates a new texture. This might be useful in case there "
        "was an error loading the video."
    };

    constexpr openspace::properties::Property::PropertyInfo AudioInfo = {
        "PlayAudio",
        "Play Audio",
        "Play audio"
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

    constexpr openspace::properties::Property::PropertyInfo LoopVideoInfo = {
        "LoopVideo",
        "Loop Video",
        "If checked, the video is continues playing from the start when it reaches the "
        "end of the video."
    };

    struct [[codegen::Dictionary(VideoPlayer)]] Parameters {
        // [[codegen::verbatim(VideoInfo.description)]]
        std::string video;

        // [[codegen::verbatim(AudioInfo.description)]]
        std::optional<bool> playAudio;

        // [[codegen::verbatim(LoopVideoInfo.description)]]
        std::optional<bool> loopVideo;

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
#include "videoplayer_codegen.cpp"
} // namespace

namespace openspace {

namespace {

bool checkMpvError(int status) {
    if (status < 0) {
        LERROR(std::format("Libmpv API error: {}", mpv_error_string(status)));
        return false;
    }
    return true;
}

void* getOpenGLProcAddress(void*, const char* name) {
    return reinterpret_cast<void*>(
        global::windowDelegate->openGLProcedureAddress(name)
    );
}
} // namespace

void VideoPlayer::onMpvRenderUpdate(void* ctx) {
    // The wakeup flag is set here to enable the mpv_render_context_render
    // path in the main loop.
    // The pattern here with a static function and a void pointer to the the class
    // instance is a common pattern where C++ integrates a C library
    static_cast<VideoPlayer*>(ctx)->_wakeup = 1;
}

void VideoPlayer::observePropertyMpv(MpvKey key) {
    mpv_observe_property(
        _mpvHandle,
        static_cast<uint64_t>(key),
        keys[key],
        formats[key]
    );
}

void VideoPlayer::setPropertyStringMpv(const char* name, const char* value) {
    const int result = mpv_set_property_string(_mpvHandle, name, value);
    if (!checkMpvError(result)) {
        LWARNING(std::format("Error setting property '{}'", name));
    }
}

void VideoPlayer::setPropertyAsyncMpv(int value, MpvKey key) {
    if (!_isInitialized) {
        return;
    }
    const int result = mpv_set_property_async(
        _mpvHandle,
        static_cast<uint64_t>(key),
        keys[key],
        formats[key],
        &value
    );
    if (!checkMpvError(result)) {
        LWARNING("Error when playing video");
    }
}

void VideoPlayer::setPropertyAsyncMpv(const char* value, MpvKey key) {
    if (!_isInitialized) {
        return;
    }
    const int result = mpv_set_property_async(
        _mpvHandle,
        static_cast<uint64_t>(key),
        keys[key],
        formats[key],
        &value
    );
    if (!checkMpvError(result)) {
        LWARNING("Error when playing video");
    }
}

void VideoPlayer::setPropertyAsyncMpv(double value, MpvKey key) {
    if (!_isInitialized) {
        return;
    }
    const int result = mpv_set_property_async(
        _mpvHandle,
        static_cast<uint64_t>(key),
        keys[key],
        formats[key],
        &value
    );
    if (!checkMpvError(result)) {
        LWARNING("Error when playing video");
    }
}

void VideoPlayer::getPropertyAsyncMpv(MpvKey key) {
    const int result = mpv_get_property_async(
        _mpvHandle,
        static_cast<uint64_t>(key),
        keys[key],
        formats[key]
    );
    if (!checkMpvError(result)) {
        LWARNING(std::format("Could not find property '{}'", keys[key]));
        return;
    }
}

void VideoPlayer::commandAsyncMpv(const char* cmd[], MpvKey key) {
    const int result = mpv_command_async(_mpvHandle, static_cast<uint64_t>(key), cmd);
    if (!checkMpvError(result)) {
        LERROR(std::format("Could not execute command '{}'", keys[key]));
        return;
    }
}

documentation::Documentation VideoPlayer::Documentation() {
    return codegen::doc<Parameters>("video_videoplayer");
}

VideoPlayer::VideoPlayer(const ghoul::Dictionary& dictionary)
    : PropertyOwner({ "VideoPlayer" })
    , _play(PlayInfo)
    , _pause(PauseInfo)
    , _goToStart(GoToStartInfo)
    , _reload(ReloadInfo)
    , _playAudio(AudioInfo, false)
    , _loopVideo(LoopVideoInfo, true)
{
    ZoneScoped;

    const Parameters p = codegen::bake<Parameters>(dictionary);

    _videoFile = p.video;
    _loopVideo = p.loopVideo.value_or(_loopVideo);

    _reload.onChange([this]() { reload(); });
    addProperty(_reload);

    if (p.playbackMode.has_value()) {
        switch (*p.playbackMode) {
            case Parameters::PlaybackMode::RealTimeLoop:
                _playbackMode = PlaybackMode::RealTimeLoop;
                break;
            case Parameters::PlaybackMode::MapToSimulationTime:
                _playbackMode = PlaybackMode::MapToSimulationTime;
                break;
        }
    }

    if (_playbackMode == PlaybackMode::RealTimeLoop) {
        // Video interaction. Only valid for real time looping
        _play.onChange([this]() { play(); });
        addProperty(_play);
        _pause.onChange([this]() { pause(); });
        addProperty(_pause);
        _goToStart.onChange([this]() { goToStart(); });
        addProperty(_goToStart);
        _loopVideo.onChange([this]() {
            setPropertyAsyncMpv(_loopVideo ? "inf" : "no", MpvKey::Loop);
        });
        addProperty(_loopVideo);
        // Audio only makes sense when the video is playing in real time
        _playAudio.onChange([this]() { toggleMute(); });
        addProperty(_playAudio);
    }

    if (_playbackMode == PlaybackMode::MapToSimulationTime) {
        if (!p.startTime.has_value() || !p.endTime.has_value()) {
            LERROR("Video tile layer tried to map to simulation time but lacked start or"
                " end time"
            );
            return;
        }
        _startJ200Time = Time::convertTime(*p.startTime);
        _endJ200Time = Time::convertTime(*p.endTime);
        ghoul_assert(_endJ200Time > _startJ200Time, "Invalid times for video");
    }

    global::syncEngine->addSyncable(this);

    keys = {
        { MpvKey::Pause, "pause" },
        { MpvKey::Time, "time-pos" },
        { MpvKey::Duration, "duration" },
        { MpvKey::Height, "height" },
        { MpvKey::Width, "width" },
        { MpvKey::Meta, "metadata" },
        { MpvKey::Fps, "container-fps" },
        { MpvKey::IsSeeking, "seeking" },
        { MpvKey::Mute, "mute" },
        { MpvKey::Seek, "seek" },
        { MpvKey::Loop, "loop-file" }
    };

    formats = {
        { MpvKey::Pause, MPV_FORMAT_FLAG },
        { MpvKey::Time, MPV_FORMAT_DOUBLE },
        { MpvKey::Duration, MPV_FORMAT_DOUBLE },
        { MpvKey::Height, MPV_FORMAT_INT64 },
        { MpvKey::Width, MPV_FORMAT_INT64 },
        { MpvKey::Meta, MPV_FORMAT_NODE },
        { MpvKey::Fps, MPV_FORMAT_DOUBLE },
        { MpvKey::IsSeeking, MPV_FORMAT_FLAG },
        { MpvKey::Mute, MPV_FORMAT_STRING },
        { MpvKey::Loop, MPV_FORMAT_STRING }
    };
}

VideoPlayer::~VideoPlayer() {
    global::syncEngine->removeSyncable(this);
    destroy();
}

void VideoPlayer::pause() {
    constexpr int IsPaused = 1;
    setPropertyAsyncMpv(IsPaused, MpvKey::Pause);
}

void VideoPlayer::play() {
    constexpr int IsPaused = 0;
    setPropertyAsyncMpv(IsPaused, MpvKey::Pause);
}

void VideoPlayer::goToStart() {
    seekToTime(0.0);
}

void VideoPlayer::initialize() {
    initializeMpv();
}

void VideoPlayer::initializeMpv() {
    _mpvHandle = mpv_create();
    if (!_mpvHandle) {
        LINFO("LibMpv: mpv context init failed");
    }

    // Set libmpv flags before initializing
    // See order at https://github.com/mpv-player/mpv/blob/master/libmpv/client.h#L420
    // Avoiding async calls in uninitialized state

    // Loop video
    if (_loopVideo && _playbackMode == PlaybackMode::RealTimeLoop) {
        // https://mpv.io/manual/master/#options-loop
        setPropertyStringMpv("loop-file", "inf");
    }

    // Allow only OpenGL (requires OpenGL 2.1+ or GLES 2.0+)
    // https://mpv.io/manual/master/#options-gpu-api
    setPropertyStringMpv("gpu-api", "opengl");

    // Keep open the file. Even when we reach EOF we want to keep the video player
    // running, in case the user starts the video from the beginning again
    // https://mpv.io/manual/stable/#options-keep-open
    setPropertyStringMpv("keep-open", "yes");

    // Enable hardware decoding
    // https://mpv.io/manual/master/#options-hwdec
    setPropertyStringMpv("hwdec", "auto");

    // Enable direct rendering (default: auto). If this is set to yes, the video will be
    // decoded directly to GPU video memory (or staging buffers).
    // https://mpv.io/manual/master/#options-vd-lavc-dr
    setPropertyStringMpv("vd-lavc-dr", "yes");

    // Print libmpv couts to the terminal
    // https://mpv.io/manual/master/#options-terminal
    setPropertyStringMpv("terminal", "yes");

    // Control how long before video display target time the frame should be rendered
    // https://mpv.io/manual/master/#options-video-timing-offset
    setPropertyStringMpv("video-timing-offset", "0");

    // Turn off audio as default
    setPropertyStringMpv("mute", "yes");

    // Starting MPV in a paused state seems to reduce problems with initialization
    setPropertyStringMpv("pause", "");

    // Allow alpha channels
    setPropertyStringMpv("alpha", "yes");

    // Verbose mode for debug purposes
    // setPropertyStringMpv("msg-level", "all=v");
    //mpv_request_log_messages(_mpvHandle, "debug");

    if (mpv_initialize(_mpvHandle) < 0) {
        LINFO("mpv init failed");
    }

    mpv_opengl_init_params gl_init_params{ getOpenGLProcAddress, nullptr };
    int adv = 1; // Use libmpv advanced mode since we will use the update callback
    // Decouple mpv from waiting to get the correct fps. Use with flag video-timing-offset
    // set to 0
    int blockTime = 0;

    mpv_render_param params[]{
        { MPV_RENDER_PARAM_API_TYPE, const_cast<char*>(MPV_RENDER_API_TYPE_OPENGL) },
        { MPV_RENDER_PARAM_OPENGL_INIT_PARAMS, &gl_init_params },
        { MPV_RENDER_PARAM_ADVANCED_CONTROL, &adv },
        { MPV_RENDER_PARAM_BLOCK_FOR_TARGET_TIME, &blockTime },
        { MPV_RENDER_PARAM_INVALID, nullptr }
    };

    // This makes mpv use the currently set GL context. It will use the callback
    // (passed via params) to resolve GL builtin functions, as well as extensions.
    int result = mpv_render_context_create(&_mpvRenderContext, _mpvHandle, params);
    if (result < 0) {
        LINFO("Failed to initialize libmpv OpenGL context");
    }

    // When there is a need to call mpv_render_context_update(), which can
    // request a new frame to be rendered.
    // (Separate from the normal event handling mechanism for the sake of
    //  users which run OpenGL on a different thread.)
    mpv_render_context_set_update_callback(
        _mpvRenderContext,
        onMpvRenderUpdate,
        this
    );

    // Load file
    const std::string file = _videoFile;
    const char* cmd[] = { "loadfile", file.c_str(), nullptr };
    result = mpv_command(_mpvHandle, cmd);
    if (!checkMpvError(result)) {
        LERROR("Could not open video file");
        return;
    }

    glGenFramebuffers(1, &_fbo);
    //Create FBO to render video into
    createTexture(_videoResolution);

    //Observe video parameters
    observePropertyMpv(MpvKey::Duration);
    observePropertyMpv(MpvKey::Meta);
    observePropertyMpv(MpvKey::Height);
    observePropertyMpv(MpvKey::Width);
    observePropertyMpv(MpvKey::Pause);
    observePropertyMpv(MpvKey::Fps);
    observePropertyMpv(MpvKey::Time);
    observePropertyMpv(MpvKey::IsSeeking);

    // Render the first frame so we can see the video
    renderFrame();

    _isInitialized = true;
}

void VideoPlayer::seekToTime(double time, PauseAfterSeek pauseAfter) {
    if (_isSeeking || std::abs(_currentVideoTime - time) < glm::epsilon<double>()) {
        return;
    }
    pause();
    setPropertyAsyncMpv(time, MpvKey::Time);
    if (!pauseAfter) {
        play();
    }
}

void VideoPlayer::toggleMute() {
    const char* mute = _playAudio ? "no" : "yes";
    setPropertyAsyncMpv(mute, MpvKey::Mute);
}

void VideoPlayer::update() {
    if (_isDestroying) {
        return;
    }

    if (global::sessionRecording->isSavingFramesDuringPlayback()) {
        const double dt = global::sessionRecording->fixedDeltaTimeDuringFrameOutput();
        if (_playbackMode == PlaybackMode::MapToSimulationTime) {
            _currentVideoTime = correctVideoPlaybackTime();
        }
        else {
            _currentVideoTime = _currentVideoTime + dt;
        }

        const MpvKey key = MpvKey::Time;
        mpv_set_property(_mpvHandle, keys[key], formats[key], &_currentVideoTime);

        uint64_t result = mpv_render_context_update(_mpvRenderContext);
        while ((result & MPV_RENDER_UPDATE_FRAME) == 0) {
            renderFrame();

            result = mpv_render_context_update(_mpvRenderContext);
        }
        return;
    }

    if (_playbackMode == PlaybackMode::MapToSimulationTime) {
        seekToTime(correctVideoPlaybackTime());
    }
    if (_mpvRenderContext && _mpvHandle) {
        renderMpv();
    }
}

void VideoPlayer::renderMpv() {
    handleMpvEvents();

    // Renders a frame libmpv has been updated
    if (_wakeup) {
        const uint64_t result = mpv_render_context_update(_mpvRenderContext);
        if ((result & MPV_RENDER_UPDATE_FRAME)) {
            renderFrame();
            _wakeup = 0;
        }
    }
}

void VideoPlayer::renderFrame() {
    // Save the currently bound fbo
    const GLint defaultFBO = ghoul::opengl::FramebufferObject::getActiveObject();

    // See render_gl.h on what OpenGL environment mpv expects, and other API
    // details. This function fills the fbo and texture with data, after it
    // we can get the data on the GPU, not the CPU
    const int fboInt = static_cast<int>(_fbo);
    mpv_opengl_fbo mpfbo{
        fboInt,
        _videoResolution.x,
        _videoResolution.y,
        0
    };
    int flipY{ 1 };

    mpv_render_param params[] = {
        { MPV_RENDER_PARAM_OPENGL_FBO, &mpfbo },
        { MPV_RENDER_PARAM_FLIP_Y, &flipY },
        { MPV_RENDER_PARAM_INVALID, nullptr }
    };
    // This "renders" to the video_framebuffer "linked by ID" in the
    // params_fbo
    mpv_render_context_render(_mpvRenderContext, params);

    // We have to set the Viewport on every cycle because
    // mpv_render_context_render internally rescales the fb of the context(?!)...
    global::renderEngine->openglStateCache().resetViewportState();

    // We also need to reset the render target
    glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);
}

void VideoPlayer::handleMpvEvents() {
    while (_mpvHandle) {
        mpv_event* event = mpv_wait_event(_mpvHandle, 0.0);

        // Validate event
        if (event->event_id == MPV_EVENT_NONE) {
            break;
        }
        if (!checkMpvError(event->error)) {
            LWARNING(std::format(
                "Error at mpv event: {} {}",
                static_cast<int>(event->event_id), event->reply_userdata
            ));
            break;
        }

        switch (event->event_id) {
            case MPV_EVENT_VIDEO_RECONFIG: {
                // Retrieve the new video size
                getPropertyAsyncMpv(MpvKey::Width);
                getPropertyAsyncMpv(MpvKey::Height);
                break;
            }
            case MPV_EVENT_PROPERTY_CHANGE: {
                mpv_event_property* prop =
                    reinterpret_cast<mpv_event_property*>(event->data);
                // Validate reply
                if (prop->format == MPV_FORMAT_NONE) {
                    break;
                }
                // Validate reply with what we have stored
                const MpvKey key = static_cast<MpvKey>(event->reply_userdata);
                if (formats[key] != prop->format) {
                    LINFO(std::format("Wrong format for property '{}'", keys[key]));
                    break;
                }
                getPropertyAsyncMpv(key);
                break;
            }
            case MPV_EVENT_GET_PROPERTY_REPLY: {
                handleMpvProperties(event);
                break;
            }
            case MPV_EVENT_LOG_MESSAGE: {
                mpv_event_log_message* msg =
                    reinterpret_cast<mpv_event_log_message*>(event->data);
                LINFO(std::format("[{}] {}: {}", msg->prefix, msg->level, msg->text));
                break;
            }
            default: {
                // Ignore uninteresting or unknown events.
                break;
            }
        }
    }
}

void VideoPlayer::handleMpvProperties(mpv_event* event) {
    const MpvKey key = static_cast<MpvKey>(event->reply_userdata);

    if (!event->data) {
        LERROR(std::format("Could not find data for property: {}", keys[key]));
        return;
    }
    // Cast event to node or property depending on its format
    mpv_event_property* prop = nullptr;
    mpv_node node = {};
    if (formats[key] == MPV_FORMAT_NODE) {
        const int result = mpv_event_to_node(&node, event);
        if (!checkMpvError(result)) {
            LWARNING(
                std::format("Error getting data from libmpv property: {}", keys[key])
            );
        }
    }
    else {
        prop = reinterpret_cast<mpv_event_property*>(event->data);
    }

    // Handle new values
    switch (key) {
        case MpvKey::Duration: {
            if (!prop) {
                break;
            }
            double* duration = reinterpret_cast<double*>(prop->data);

            if (!duration) {
                LERROR("Could not find duration property");
                break;
            }

            _videoDuration = *duration;
            if (_playbackMode == PlaybackMode::MapToSimulationTime) {
                updateFrameDuration();
            }

            LINFO(std::format("Duration: {}", *duration));
            break;
        }
        case MpvKey::Height: {
            if (!prop) {
                break;
            }
            int* height = reinterpret_cast<int*>(prop->data);

            if (!height) {
                LERROR("Could not find height property");
                break;
            }

            resizeTexture(glm::ivec2(_videoResolution.x, *height));
            LINFO(std::format("New height: {}", *height));

            // Each time a size property is updated, it means libmpv is updating the video
            // so we have to re-render the first frame to show it
            renderFrame();

            break;
        }
        case MpvKey::Width: {
            if (!prop) {
                break;
            }
            int* width = reinterpret_cast<int*>(prop->data);

            if (!width) {
                LERROR("Could not find width property");
                break;
            }

            resizeTexture(glm::ivec2(* width, _videoResolution.y));
            LINFO(std::format("New width: {}", *width));

            // Each time a size property is updated, it means libmpv is updating the video
            // so we have to re-render the first frame to show it
            renderFrame();

            break;
        }
        case MpvKey::Time: {
            if (!prop) {
                break;
            }
            double* time = reinterpret_cast<double*>(prop->data);

            if (!time) {
                LERROR("Could not find playback time property");
                break;
            }
            _currentVideoTime = *time;
            break;
        }
        case MpvKey::IsSeeking: {
            if (!prop) {
                break;
            }
            bool* isSeekingBool = reinterpret_cast<bool*>(prop->data);
            _isSeeking = *isSeekingBool;
            break;
        }
        case MpvKey::Fps: {
            if (!prop) {
                break;
            }
            double* fps = reinterpret_cast<double*>(prop->data);
            if (*fps < glm::epsilon<double>()) {
                _fps = 24.0;
                LWARNING("Detected fps was 0. Falling back on 24 fps");
                break;
            }
            if (!fps) {
                LERROR("Could not find fps property");
                break;
            }
            _fps = *fps;
            if (_playbackMode == PlaybackMode::MapToSimulationTime) {
                updateFrameDuration();
            }

            LINFO(std::format("Detected fps: {}", *fps));
            _seekThreshold = 2.0 * (1.0 / _fps);
            break;
        }
        case MpvKey::Pause: {
            if (!prop) {
                break;
            }
            int* videoIsPaused = reinterpret_cast<int*>(prop->data);
            _isPaused = (* videoIsPaused == 1);
            break;
        }
        case MpvKey::Meta: {
            LINFO("Printing meta data reply");
            if (node.format == MPV_FORMAT_NODE_MAP) {
                for (int n = 0; n < node.u.list->num; n++) {
                    if (node.u.list->values[n].format == MPV_FORMAT_STRING) {
                        LINFO(node.u.list->values[n].u.string);
                    }
                }
            }
            else {
                LWARNING("No meta data could be read");
            }
            break;
        }
        default:
            throw ghoul::MissingCaseException();
    }
}

void VideoPlayer::destroy() {
    _isDestroying = true;
    // Destroy the GL renderer and all of the GL objects it allocated. If video
    // is still running, the video track will be deselected.
    mpv_render_context_free(_mpvRenderContext);
    _mpvRenderContext = nullptr;
    mpv_destroy(_mpvHandle);
    _mpvHandle = nullptr;
    glDeleteFramebuffers(1, &_fbo);
}

void VideoPlayer::preSync(bool isMaster) {
    _correctPlaybackTime = isMaster ? _currentVideoTime : -1.0;
}

void VideoPlayer::encode(SyncBuffer* syncBuffer) {
    syncBuffer->encode(_correctPlaybackTime);
}

void VideoPlayer::decode(SyncBuffer* syncBuffer) {
    syncBuffer->decode(_correctPlaybackTime);
}

void VideoPlayer::postSync(bool isMaster) {
    if (_correctPlaybackTime < 0.0) {
        return;
    }
    // Ensure the nodes have the same time as the master node
    const bool isMappingTime = _playbackMode == PlaybackMode::MapToSimulationTime;
    if (!isMaster) {
        if ((_correctPlaybackTime - _currentVideoTime) > glm::epsilon<double>()) {
            seekToTime(_correctPlaybackTime, PauseAfterSeek(isMappingTime));
        }
    }
}

const std::unique_ptr<ghoul::opengl::Texture>& VideoPlayer::frameTexture() const {
    return _frameTexture;
}

void VideoPlayer::reload() {
    if (_videoFile.empty()) {
        return;
    }
    destroy();
    _isDestroying = false;
    initializeMpv();
}

bool VideoPlayer::isInitialized() const {
    return _isInitialized;
}

bool VideoPlayer::isWithingStartEndTime() const {
    const double now = global::timeManager->time().j2000Seconds();
    return now <= _endJ200Time && now >= _startJ200Time;
}

void VideoPlayer::updateFrameDuration() {
    const double openspaceVideoLength = (_endJ200Time - _startJ200Time) / _videoDuration;
    _frameDuration = (1.0 / _fps) * openspaceVideoLength;
}

double VideoPlayer::correctVideoPlaybackTime() const {
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
    return percentage * _videoDuration;
}

void VideoPlayer::createTexture(glm::ivec2 size) {
    LINFO(std::format("Creating new FBO with width: {} and height: {}", size.x, size.y));

    if (size.x <= 0 || size.y <= 0) {
        LERROR("Cannot create empty fbo");
        return;
    }

    // Update resolution of video
    _videoResolution = size;

    glBindFramebuffer(GL_FRAMEBUFFER, _fbo);

    _frameTexture = std::make_unique<ghoul::opengl::Texture>(
        glm::uvec3(size, 1),
        GL_TEXTURE_2D
    );
    _frameTexture->uploadTexture();

    // Disable mipmaps
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_BASE_LEVEL, 0);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAX_LEVEL, 0);

    // Bind texture to framebuffer
    glFramebufferTexture2D(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        GL_TEXTURE_2D,
        *_frameTexture,
        0
    );

    // Unbind FBO
    glBindFramebuffer(GL_FRAMEBUFFER, 0);
}

void VideoPlayer::resizeTexture(glm::ivec2 size) {
    const bool isValid = size.x > 0 && size.y > 0 && _fbo > 0;
    const bool isNew = size != _videoResolution;
    if (isValid && isNew) {
        _videoResolution = size;
        LINFO(std::format("Resizing texture: width: {} height: {}", size.x, size.y));

        // Delete texture
        _frameTexture = nullptr;

        createTexture(size);
    }
}

} // namespace openspace
