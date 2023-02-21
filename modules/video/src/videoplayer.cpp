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

#include <modules/video/include/videoplayer.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/util/time.h>
#include <openspace/util/timemanager.h>
#include <ghoul/filesystem/filesystem.h>

namespace {
    constexpr std::string_view _loggerCat = "VideoPlayer";

    constexpr openspace::properties::Property::PropertyInfo VideoInfo = {
        "Video",
        "Video",
        "This should point to the video that should be played. It can "
        "be either a file path or a url to a youtube video."
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

    constexpr openspace::properties::Property::PropertyInfo ResetInfo = {
        "Reset",
        "Reset",
        "Reset video"
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

    constexpr openspace::properties::Property::PropertyInfo PlaybackModeInfo = {
        "PlaybackMode",
        "Playback Mode",
        "Determines the way the video should be played. The start and end time of the "
        "video can be set, or the video can be played as a loop in real time."
    };

    struct [[codegen::Dictionary(VideoPlayer)]] Parameters {
        // [[codegen::verbatim(VideoInfo.description)]]
        std::string video;

        // [[codegen::verbatim(AudioInfo.description)]]
        std::optional<bool> playAudio;

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

bool checkMpvError(int status) {
    if (status < 0) {
        LERROR(fmt::format("Libmpv API error: {}", mpv_error_string(status)));
        return false;
    }
    return true;
}

void* getOpenGLProcAddress(void*, const char* name) {
    return reinterpret_cast<void*>(
        global::windowDelegate->openGLProcedureAddress(name)
    );
}

void VideoPlayer::on_mpv_render_update(void* ctx) {
    // The wakeup flag is set here to enable the mpv_render_context_render 
    // path in the main loop.
    // The pattern here with a static function and a void pointer to the the class
    // instance is a common pattern where C++ integrates a C library
    static_cast<VideoPlayer*>(ctx)->_wakeup = 1;
}

void VideoPlayer::observePropertyMpv(std::string name, mpv_format format, 
                                           LibmpvPropertyKey key) {
    mpv_observe_property(
        _mpvHandle, 
        static_cast<uint64_t>(key), 
        name.c_str(), 
        format
    );
}

void VideoPlayer::setPropertyStringMpv(std::string name, std::string value) {
    int result = mpv_set_property_string(_mpvHandle, name.c_str(), value.c_str());
    if (!checkMpvError(result)) {
        LWARNING(fmt::format("Error setting property {}", name));
    }
}

void VideoPlayer::getPropertyAsyncMpv(std::string name, mpv_format format, 
                                            LibmpvPropertyKey key) {
    int result = mpv_get_property_async(
        _mpvHandle,
        static_cast<uint64_t>(key),
        name.c_str(),
        format
    );
    if (!checkMpvError(result)) {
        LWARNING("Could not find property " + name);
        return;
    }
}

void VideoPlayer::commandAsyncMpv(const char* cmd[], LibmpvPropertyKey key) {
    int result = mpv_command_async(
        _mpvHandle,
        static_cast<uint64_t>(key),
        cmd
    );
    if (!checkMpvError(result)) {
        LERROR(fmt::format("Could not execute command {}", cmd[0]));
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
    , _reset(ResetInfo)
    , _playAudio(AudioInfo, false)
{
    ZoneScoped

    const Parameters p = codegen::bake<Parameters>(dictionary);

    _videoFile = p.video;

    _reset.onChange([this]() { reset(); });
    addProperty(_reset);
    _playAudio.onChange([this]() { toggleMute(); });
    addProperty(_playAudio);

    if (_playbackMode == PlaybackMode::RealTimeLoop) {
        // Video interaction. Only valid for real time looping
        _play.onChange([this]() { play(); });
        addProperty(_play);
        _pause.onChange([this]() { pause(); });
        addProperty(_pause);
        _goToStart.onChange([this]() { goToStart(); });
        addProperty(_goToStart);
    }

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

    if (_playbackMode == PlaybackMode::MapToSimulationTime) {
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
            seekToTime(correctVideoPlaybackTime());
        });

        // Ensure we are synchronized to OpenSpace time in presync step
        global::callback::preSync->emplace_back([this]() {
            syncToSimulationTime();
        });
    }
    
    global::callback::postSyncPreDraw->emplace_back([this]() {
        if (_isDestroying) {
            return;
        }
        // Initialize mpv here to ensure that the opengl context is the same as in for 
        // the rendering
        if (!_isInitialized) {
            initializeMpv();
        }
        else if(_mpvRenderContext && _mpvHandle) {
            renderMpv();
        }
    });

    global::callback::postDraw->emplace_back([this]() {
        if (_isDestroying) {
            return;
        }
        swapBuffersMpv();
    });
}

void VideoPlayer::pause() {
    if (!_isInitialized) {
        return;
    }
    int isPaused = 1;
    int result = mpv_set_property_async(
        _mpvHandle,
        static_cast<uint64_t>(LibmpvPropertyKey::Pause),
        "pause",
        MPV_FORMAT_FLAG,
        &isPaused
    );
    if (!checkMpvError(result)) {
        LWARNING("Error when pausing video");
    }
}

void VideoPlayer::play() {
    if (!_isInitialized) {
        return;
    }
    int isPaused = 0;
    int result = mpv_set_property_async(
        _mpvHandle,
        static_cast<uint64_t>(LibmpvPropertyKey::Pause),
        "pause",
        MPV_FORMAT_FLAG,
        &isPaused
    );
    if (!checkMpvError(result)) {
        LWARNING("Error when playing video");
    }
}

void VideoPlayer::goToStart() {
    seekToTime(0.0);
}

void VideoPlayer::stepFrameForward() {
    if (!_isInitialized) {
        return;
    }
    const char* cmd[] = { "frame-step", nullptr };
    commandAsyncMpv(cmd);
}

void VideoPlayer::stepFrameBackward() {
    if (!_isInitialized) {
        return;
    }
    const char* cmd[] = { "frame-back-step", nullptr };
    commandAsyncMpv(cmd);
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
    // https://mpv.io/manual/master/#options-loop
    setPropertyStringMpv("loop", "");

    // Allow only OpenGL (requires OpenGL 2.1+ or GLES 2.0+)
    // https://mpv.io/manual/master/#options-gpu-api
    setPropertyStringMpv("gpu-api", "opengl");

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

    //setPropertyStringMpv("load-stats-overlay", "");

    //mpv_set_property_string(_mpvHandle, "script-opts", "autoload-disabled=yes");
    
    // Verbose mode
    mpv_set_property_string(_mpvHandle, "msg-level", "all=v");
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
        {MPV_RENDER_PARAM_API_TYPE, const_cast<char*>(MPV_RENDER_API_TYPE_OPENGL)},
        {MPV_RENDER_PARAM_OPENGL_INIT_PARAMS, &gl_init_params},
        {MPV_RENDER_PARAM_ADVANCED_CONTROL, &adv},
        {MPV_RENDER_PARAM_BLOCK_FOR_TARGET_TIME, &blockTime},
        {MPV_RENDER_PARAM_INVALID, nullptr}
    };

    // This makes mpv use the currently set GL context. It will use the callback
    // (passed via params) to resolve GL builtin functions, as well as extensions.
    if (mpv_render_context_create(&_mpvRenderContext, _mpvHandle, params) < 0) {
        LINFO("Failed to initialize libmpv OpenGL context");
    }

    // When there is a need to call mpv_render_context_update(), which can
    // request a new frame to be rendered.
    // (Separate from the normal event handling mechanism for the sake of
    //  users which run OpenGL on a different thread.)
    mpv_render_context_set_update_callback(
        _mpvRenderContext, 
        on_mpv_render_update, 
        this
    );

    // Load file
    std::string file = _videoFile;
    const char* cmd[] = { "loadfile", file.c_str(), nullptr };
    int result = mpv_command(_mpvHandle, cmd);
    if (!checkMpvError(result)) {
        LERROR("Could not open video file");
        return;
    }

    //Create FBO to render video into
    createFBO(_videoResolution.x, _videoResolution.y);

    //Observe video parameters
    observePropertyMpv("video-params", MPV_FORMAT_NODE, LibmpvPropertyKey::Params);
    observePropertyMpv("pause", MPV_FORMAT_FLAG, LibmpvPropertyKey::Pause);
    observePropertyMpv("time-pos", MPV_FORMAT_DOUBLE, LibmpvPropertyKey::Time);
    observePropertyMpv("duration", MPV_FORMAT_DOUBLE, LibmpvPropertyKey::Duration);
    observePropertyMpv("height", MPV_FORMAT_INT64, LibmpvPropertyKey::Height);
    observePropertyMpv("width", MPV_FORMAT_INT64, LibmpvPropertyKey::Width);
    observePropertyMpv("metadata", MPV_FORMAT_NODE, LibmpvPropertyKey::Meta);
    observePropertyMpv("container-fps", MPV_FORMAT_DOUBLE, LibmpvPropertyKey::Fps);
    observePropertyMpv("seeking", MPV_FORMAT_DOUBLE, LibmpvPropertyKey::IsSeeking);

    _isInitialized = true;
}

void VideoPlayer::seekToTime(double time) {
    if (!_isInitialized) {
        return;
    }
    // Prevent from seeking to the same time multiple times in a row
    bool seekIsDifferent = abs(time - _currentVideoTime) > _seekThreshold;
    if (seekIsDifferent && !_isSeeking) {
        // Pause while seeking
        pause();
        std::string timeString = std::to_string(time);
        const char* params = timeString.c_str();
        const char* cmd[] = { "seek", params, "absolute", NULL };
        commandAsyncMpv(cmd, LibmpvPropertyKey::Seek);
        _isSeeking = true;
    }
}

void VideoPlayer::toggleMute() {
    if (!_isInitialized) {
        return;
    }
    const char* mute = _playAudio ? "no" : "yes";
    int result = mpv_set_property_async(
        _mpvHandle,
        static_cast<uint64_t>(LibmpvPropertyKey::Mute),
        "mute",
        MPV_FORMAT_STRING,
        &mute
    );
    if (!checkMpvError(result)) {
        LWARNING("Error when pausing video");
    }
}

bool VideoPlayer::isPaused() const {
    return _isPaused;
}

void VideoPlayer::renderMpv() {
    handleMpvEvents();

    if (_wakeup) {
        if ((mpv_render_context_update(_mpvRenderContext) & MPV_RENDER_UPDATE_FRAME)) {
            // See render_gl.h on what OpenGL environment mpv expects, and other API 
            // details. This function fills the fbo and texture with data, after it 
            // we can get the data on the GPU, not the CPU
            int fboInt = static_cast<int>(_fbo);
            mpv_opengl_fbo mpfbo{ 
                fboInt , 
                _videoResolution.x, 
                _videoResolution.y, 0 
            };
            int flip_y{ 1 };

            mpv_render_param params[] = {
                {MPV_RENDER_PARAM_OPENGL_FBO, &mpfbo},
                {MPV_RENDER_PARAM_FLIP_Y, &flip_y},
                {MPV_RENDER_PARAM_INVALID, nullptr}
            };
            // This "renders" to the video_framebuffer "linked by ID" in the
            // params_fbo
            mpv_render_context_render(_mpvRenderContext, params);

            /* TODO: remove this comment in case we never encounter this issue again */
            // We have to set the Viewport on every cycle because 
            // mpv_render_context_render internally rescales the fb of the context(?!)...
            //glm::ivec2 window = global::windowDelegate->currentDrawBufferResolution();
            //glViewport(0, 0, window.x, window.y);
            _didRender = true;
        }
    }
}

void VideoPlayer::handleMpvEvents() {
    while (_mpvHandle) {
        mpv_event* event = mpv_wait_event(_mpvHandle, 0.0);
        if (event->event_id == MPV_EVENT_NONE) {
            return;
        }

        switch (event->event_id) {
            case MPV_EVENT_VIDEO_RECONFIG: {
                // Retrieve the new video size
                // Get width
                getPropertyAsyncMpv("width", MPV_FORMAT_INT64, LibmpvPropertyKey::Width);
                getPropertyAsyncMpv("height", MPV_FORMAT_INT64, LibmpvPropertyKey::Height);
                break;
            }
            case MPV_EVENT_PROPERTY_CHANGE: {
                mpv_event_property* prop = (mpv_event_property*)event->data;
                if (strcmp(prop->name, "video-params") == 0 &&
                    prop->format == MPV_FORMAT_NODE)
                {
                    getPropertyAsyncMpv("video-params", MPV_FORMAT_NODE, LibmpvPropertyKey::Params);
                }
                if (strcmp(prop->name, "time-pos") == 0 &&
                    prop->format == MPV_FORMAT_DOUBLE)
                {
                    getPropertyAsyncMpv("time-pos", MPV_FORMAT_DOUBLE, LibmpvPropertyKey::Time);
                }
                if (strcmp(prop->name, "duration") == 0 &&
                    prop->format == MPV_FORMAT_DOUBLE)
                {
                    getPropertyAsyncMpv("duration", MPV_FORMAT_DOUBLE, LibmpvPropertyKey::Duration);
                }
                if (strcmp(prop->name, "container-fps") == 0 &&
                    prop->format == MPV_FORMAT_DOUBLE)
                {
                    getPropertyAsyncMpv("container-fps", MPV_FORMAT_DOUBLE, LibmpvPropertyKey::Fps);
                }
                if (strcmp(prop->name, "pause") == 0 &&
                    prop->format == MPV_FORMAT_FLAG)
                {
                    getPropertyAsyncMpv("pause", MPV_FORMAT_FLAG, LibmpvPropertyKey::Pause);
                }
                if (strcmp(prop->name, "height") == 0 &&
                    prop->format == MPV_FORMAT_INT64)
                {
                    getPropertyAsyncMpv("height", MPV_FORMAT_INT64, LibmpvPropertyKey::Height);
                }
                if (strcmp(prop->name, "width") == 0 &&
                    prop->format == MPV_FORMAT_INT64)
                {
                    getPropertyAsyncMpv("width", MPV_FORMAT_INT64, LibmpvPropertyKey::Width);
                }
                if (strcmp(prop->name, "metadata") == 0 &&
                    prop->format == MPV_FORMAT_NODE)
                {
                    getPropertyAsyncMpv("metadata", MPV_FORMAT_NODE, LibmpvPropertyKey::Meta);
                }
                break;
            }
            case MPV_EVENT_LOG_MESSAGE: {
                struct mpv_event_log_message* msg =
                    (struct mpv_event_log_message*)event->data;
                std::stringstream ss;
                ss << "[" << msg->prefix << "] " << msg->level << ": " << msg->text;
                LINFO(ss.str());
                break;
            }
            case MPV_EVENT_COMMAND_REPLY: {
                switch (event->reply_userdata) {
                    case static_cast<uint64_t>(LibmpvPropertyKey::Command): {
                        int result = event->error;
                        if (!checkMpvError) {
                            LINFO("Command Error");
                        }
                        break;
                    }
                    case static_cast<uint64_t>(LibmpvPropertyKey::Seek): {
                        int result = event->error;
                        if (!checkMpvError) {
                            LINFO("Seek Error");
                        }
                        _isSeeking = false;
                        break;
                    }
                    default: {
                        break;
                    }
                }
                break;
            }
            case MPV_EVENT_GET_PROPERTY_REPLY: {
                int result = event->error;
                if (!checkMpvError(result)) {
                    LWARNING(fmt::format(
                        "Error while gettting property of type: {}", event->reply_userdata
                    ));
                    break;
                }
                handleMpvProperties(event);
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
    switch (static_cast<LibmpvPropertyKey>(event->reply_userdata)) {
    case LibmpvPropertyKey::Duration: {
        if (!event->data) {
            LERROR("Could not find duration property");
            break;
        }

        struct mpv_event_property* property = (struct mpv_event_property*)event->data;
        double* duration = static_cast<double*>(property->data);

        if (!duration) {
            LERROR("Could not find duration property");
            break;
        }

        _videoDuration = *duration;
        if (_playbackMode == PlaybackMode::MapToSimulationTime) {
            updateFrameDuration();
        }

        LINFO(fmt::format("Duration: {}", *duration));
        break;
    }
    case LibmpvPropertyKey::Height: {
        if (!event->data) {
            LERROR("Could not find height property");
            break;
        }

        struct mpv_event_property* property = (struct mpv_event_property*)event->data;
        int* height = static_cast<int*>(property->data);

        if (!height) {
            LERROR("Could not find height property");
            break;
        }

        if (*height == _videoResolution.y) {
            break;
        }

        LINFO(fmt::format("New height: {}", *height));

        if (*height > 0 && _videoResolution.x > 0 && _fbo > 0) {
            resizeFBO(_videoResolution.x, *height);
        }

        break;
    }
    case LibmpvPropertyKey::Width: {
        if (!event->data) {
            LERROR("Could not find height property");
            break;
        }

        struct mpv_event_property* property = (struct mpv_event_property*)event->data;
        int* width = static_cast<int*>(property->data);

        if (!width) {
            LERROR("Could not find width property");
            break;
        }

        if (*width == _videoResolution.y) {
            break;
        }

        LINFO(fmt::format("New width: {}", *width));

        if (*width > 0 && _videoResolution.y > 0 && _fbo > 0) {
            resizeFBO(*width, _videoResolution.y);
        }
        
        break;
    }
    case LibmpvPropertyKey::Meta: {
        if (!event->data) {
            LERROR("Could not find video parameters");
            break;
        }

        mpv_node node;
        int result = mpv_event_to_node(&node, event);
        if (!checkMpvError(result)) {
            LWARNING("Could not find video parameters of video");
        }

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
    case LibmpvPropertyKey::Params: {
        if (!event->data) {
            LINFO("Could not find video parameters");
            break;
        }

        mpv_node videoParams;
        int result = mpv_event_to_node(&videoParams, event);
        if (!checkMpvError(result)) {
            LWARNING("Could not find video parameters of video");
        }

        if (videoParams.format == MPV_FORMAT_NODE_ARRAY ||
            videoParams.format == MPV_FORMAT_NODE_MAP)
        {
            mpv_node_list* list = videoParams.u.list;

            mpv_node width, height;
            bool foundWidth = false;
            bool foundHeight = false;
            for (int i = 0; i < list->num; ++i) {
                if (foundWidth && foundHeight) {
                    break;
                }

                if (list->keys[i] == "w") {
                    width = list->values[i];
                    foundWidth = true;
                }
                else if (list->keys[i] == "h") {
                    height = list->values[i];
                    foundHeight = true;
                }
            }

            if (!foundWidth || !foundHeight) {
                LINFO("Could not find width or height params from parameters");
                return;
            }

            int w = -1;
            int h = -1;
            if (width.format == MPV_FORMAT_INT64) {
                w = width.u.int64;
            }
            if (height.format == MPV_FORMAT_INT64) {
                h = height.u.int64;
            }

            if (w == -1 || h == -1) {
                LERROR("Invalid width or height params");
                return;
            }
            resizeFBO(w, h);
        }
        break;
    }
    case LibmpvPropertyKey::Time: {
        if (!event->data) {
            LERROR("Could not find playback time property");
            break;
        }

        struct mpv_event_property* property = (struct mpv_event_property*)event->data;
        double* time = static_cast<double*>(property->data);

        if (!time) {
            LERROR("Could not find playback time property");
            break;
        }
        _currentVideoTime = *time;
        break;
    }
    case LibmpvPropertyKey::IsSeeking: {
        if (!event->data) {
            LERROR("Could not find playback time property");
            break;
        }

        struct mpv_event_property* property = (struct mpv_event_property*)event->data;
        bool* isSeekingBool = static_cast<bool*>(property->data);
        std::string isSeekingString = *isSeekingBool ? "is Seeking" : "is not seeking";
        LINFO(isSeekingString);
        break;
    }
    case LibmpvPropertyKey::Fps: {
        if (!event->data) {
            LERROR("Could not find fps property");
            break;
        }

        struct mpv_event_property* property = (struct mpv_event_property*)event->data;
        double* fps = static_cast<double*>(property->data);
        if (*fps < glm::epsilon<double>()) {
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

        LINFO(fmt::format("Detected fps: {}", *fps));
        _seekThreshold = 2.0 * (1.0 / _fps);
        break;
    }
    case LibmpvPropertyKey::Pause: {
        if (!event->data) {
            LERROR("Could not find pause property");
            break;
        }
        struct mpv_event_property* property = (struct mpv_event_property*)event->data;
        int* videoIsPaused = static_cast<int*>(property->data);
        _isPaused = *videoIsPaused == 0 ? false : true;
        break;
    }
    default: {
        throw ghoul::MissingCaseException();
        break;
    }
    }
}

void VideoPlayer::swapBuffersMpv() {
    // Only swap buffers if there was a frame rendered and there is a new frame waiting
    if (_wakeup && _didRender && _mpvRenderContext) {
        mpv_render_context_report_swap(_mpvRenderContext);
        _wakeup = 0;
        _didRender = 0;
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

const std::unique_ptr<ghoul::opengl::Texture>& VideoPlayer::frameTexture() const {
    return _frameTexture;
}

void VideoPlayer::reset() {
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

double VideoPlayer::videoDuration() const {
    return _videoDuration;
}

double VideoPlayer::fps() const {
    return _fps;
}

double VideoPlayer::currentPlaybackTime() const {
    return _currentVideoTime;
}

PlaybackMode VideoPlayer::playbackMode() const {
    return _playbackMode;
}


bool VideoPlayer::isWithingStartEndTime() const {
    const double now = global::timeManager->time().j2000Seconds();
    return now <= _endJ200Time && now >= _startJ200Time;
}

void VideoPlayer::updateFrameDuration() {
    double openspaceVideoLength = (_endJ200Time - _startJ200Time) / _videoDuration;
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
    return percentage * videoDuration();
}

void VideoPlayer::syncToSimulationTime() {
    if (_playbackMode == PlaybackMode::MapToSimulationTime) {
        // If we are in valid times, step frames accordingly
        if (isWithingStartEndTime()) {
            double now = global::timeManager->time().j2000Seconds();
            double deltaTime = now - _timeAtLastRender;
            if (deltaTime > _frameDuration) {
                // Stepping forwards
                stepFrameForward();
                _timeAtLastRender = now;
            }
            else if (deltaTime < -_frameDuration) {
                // Stepping backwards
                stepFrameBackward();
                _timeAtLastRender = now;
            }
        }
        else if (!_isPaused) {
            pause();
        }
        // Make sure we are at the correct time
        double time = correctVideoPlaybackTime();
        bool shouldSeek = abs(time - _currentVideoTime) > _seekThreshold;
        if (shouldSeek) {
            seekToTime(time);
        }
    }
}


void VideoPlayer::createFBO(int width, int height) {
    LINFO(fmt::format("Creating new FBO with width: {} and height: {}", width, height));

    if (width <= 0 || height <= 0) {
        LERROR("Cannot create empty fbo");
        return;
    }

    // Update resolution of video
    _videoResolution = glm::ivec2(width, height);

    glGenFramebuffers(1, &_fbo);
    glBindFramebuffer(GL_FRAMEBUFFER, _fbo);
    
    _frameTexture = std::make_unique<ghoul::opengl::Texture>(
        glm::uvec3(width, height, 1),
        GL_TEXTURE_2D
    );
    _frameTexture->uploadTexture();

    // Configure
    _frameTexture->bind();
    glPixelStorei(GL_PACK_ALIGNMENT, 1);
    glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

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

void VideoPlayer::resizeFBO(int width, int height) {
    if (width == _videoResolution.x && height == _videoResolution.y) {
        return;
    }
    LINFO(fmt::format("Resizing FBO with width: {} and height: {}", width, height));

    // Update resolution of video
    _videoResolution = glm::ivec2(width, height);

    // Delete old FBO and texture
    glDeleteFramebuffers(1, &_fbo);
    _frameTexture.reset(nullptr);

    createFBO(width, height);
}

VideoPlayer::~VideoPlayer() {}

} // namespace openspace::video
