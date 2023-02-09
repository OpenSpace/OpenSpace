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

#include <modules/globebrowsing/src/tileprovider/videotileprovider.h>

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

namespace openspace::globebrowsing {

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

void VideoTileProvider::on_mpv_render_update(void* ctx) {
    // The wakeup flag is set here to enable the mpv_render_context_render 
    // path in the main loop.
    // The pattern here with a static function and a void pointer to the the class
    // instance is a common pattern where C++ integrates a C library
    static_cast<VideoTileProvider*>(ctx)->_wakeup = 1;
}

void VideoTileProvider::observePropertyMpv(std::string name, mpv_format format, 
                                           LibmpvPropertyKey key) {
    mpv_observe_property(
        _mpvHandle, 
        static_cast<uint64_t>(key), 
        name.c_str(), 
        format
    );
}

void VideoTileProvider::setPropertyStringMpv(std::string name, std::string value) {
    int result = mpv_set_property_string(_mpvHandle, name.c_str(), value.c_str());
    if (!checkMpvError(result)) {
        LWARNING(fmt::format("Error setting property {}", name));
    }
}

void VideoTileProvider::getPropertyAsyncMpv(std::string name, mpv_format format, 
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

void VideoTileProvider::commandAsyncMpv(const char* cmd[], LibmpvPropertyKey key) {
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

documentation::Documentation VideoTileProvider::Documentation() {
    return codegen::doc<Parameters>("globebrowsing_videotileprovider");
}

VideoTileProvider::VideoTileProvider(const ghoul::Dictionary& dictionary) 
    : _play(PlayInfo)
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
            throw ghoul::MissingCaseException();
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
    }
    else if (_playbackMode == PlaybackMode::MapToSimulationTime) {
        if (!p.startTime.has_value() || !p.endTime.has_value()) {
            LERROR("Video tile layer tried to map to simulation time but lacked start or"
                " end time");
            return;
        }
        //_videoDuration = *p.duration;
        _startJ200Time = Time::convertTime(*p.startTime);
        _endJ200Time = Time::convertTime(*p.endTime);
        ghoul_assert(_endJ200Time > _startJ200Time, "Invalid times for video");

        global::timeManager->addTimeJumpCallback([this]() {
            seekToTime(correctVideoPlaybackTime());
        });
    }
    
    global::callback::postSyncPreDraw->emplace_back([this]() {
        // Initialize mpv here to ensure that the opengl context is the same as in for 
        // the rendering
        if (!_isInitialized) {
            initializeMpv();
        }
        else {
            renderMpv();
        }
    });

    global::callback::postDraw->emplace_back([this]() {
        swapBuffersMpv();
    });
}

Tile VideoTileProvider::tile(const TileIndex& tileIndex) {
    ZoneScoped

    if (!_isInitialized) {
        return Tile();
    }

    // Always check that our framebuffer is ok
    if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE) {
        LINFO("Framebuffer is not complete");
    }
    uint64_t hash = tileIndex.hashKey();
    if (_tileCache.find(hash) == _tileCache.end()) {
        _tileCache[hash] = Tile{
            _frameTexture.get(), 
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

ChunkTile VideoTileProvider::chunkTile(TileIndex tileIndex, int parents, int maxParents) {
    ZoneScoped

    ghoul_assert(_isInitialized, "VideoTileProvider was not initialized");

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

void VideoTileProvider::pause() {
    bool pause = true;
    int result = mpv_set_property_async(
        _mpvHandle,
        static_cast<uint64_t>(LibmpvPropertyKey::Pause),
        "pause",
        MPV_FORMAT_FLAG,
        &pause
    );
    if (!checkMpvError(result)) {
        LWARNING("Error when pausing video");
    }
}

void VideoTileProvider::play() {
    bool pause = false;
    int result = mpv_set_property_async(
        _mpvHandle,
        static_cast<uint64_t>(LibmpvPropertyKey::Pause),
        "pause",
        MPV_FORMAT_FLAG,
        &pause
    );
    if (!checkMpvError(result)) {
        LWARNING("Error when playing video");
    }
}

void VideoTileProvider::goToStart() {
    seekToTime(0.0);
}

void VideoTileProvider::stepFrameForward() {
    const char* cmd[] = { "frame-step", nullptr };
    commandAsyncMpv(cmd);
}

void VideoTileProvider::stepFrameBackward() {
    const char* cmd[] = { "frame-back-step", nullptr };
    commandAsyncMpv(cmd);
}


void VideoTileProvider::initializeMpv() {
    _mpvHandle = mpv_create();
    if (!_mpvHandle) {
        LINFO("LibMpv: mpv context init failed");
    }

    // Set libmpv flags before initializing 
    // See order at https://github.com/mpv-player/mpv/blob/master/libmpv/client.h#L420
    // Avoiding async calls in uninitialized state
    
    if (_playbackMode == PlaybackMode::RealTimeLoop) {
        // Loop video
        // https://mpv.io/manual/master/#options-loop
        setPropertyStringMpv("loop", "");
    }
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

    //setPropertyStringMpv("load-stats-overlay", "");

    //mpv_set_property_string(_mpvHandle, "script-opts", "autoload-disabled=yes");
    
    // Verbose mode
    //mpv_set_property_string(_mpvHandle, "msg-level", "all=v");
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
    const char* cmd[] = { "loadfile", _videoFile.string().c_str(), nullptr };
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

    if (_playbackMode == PlaybackMode::MapToSimulationTime) {
        pause();
    }

    _isInitialized = true;
}

bool VideoTileProvider::isWithingStartEndTime() const {
    const double now = global::timeManager->time().j2000Seconds();
    return now <= _endJ200Time && now >= _startJ200Time;
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
    return percentage * _videoDuration;
}

void VideoTileProvider::seekToTime(double time) {
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

void VideoTileProvider::renderMpv() {
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
        // Make sure we are at the correct time
        double time = correctVideoPlaybackTime();
        bool shouldSeek = abs(time - _currentVideoTime) > _seekThreshold;
        if (shouldSeek) {
            seekToTime(time);
        }
    }
   
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
            // Since all tiles use the same texture, all tiles are ready now
            _tileIsReady = true;
        }
    }
}

void VideoTileProvider::handleMpvEvents() {
    while (_mpvHandle) {
        mpv_event* event = mpv_wait_event(_mpvHandle, 0);
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
                if (!checkMpvError(event->error)) {
                    LWARNING(fmt::format("Error getting property {}", prop->name));
                }
                else {
                    // If the property has changed, request its value
                    uint64_t i = event->reply_userdata;
                    LibmpvPropertyKey key = static_cast<LibmpvPropertyKey>(i);
                    getPropertyAsyncMpv(prop->name, prop->format, key);
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

void VideoTileProvider::handleMpvProperties(mpv_event* event) {
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
        _frameDuration = ( 1.0 / _fps) * ((_endJ200Time - _startJ200Time) / _videoDuration);

        if (_playbackMode == PlaybackMode::MapToSimulationTime) {
            seekToTime(correctVideoPlaybackTime());
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
                LERROR("Could not find width or height params");
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
        // Time has changed - we don't have a texture yet
        _tileIsReady = false;
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

        LINFO(fmt::format("Detected fps: {}", *fps));
        _frameDuration = (1.0 / _fps) * ((_endJ200Time - _startJ200Time) / _videoDuration);
        _seekThreshold = 2.0 * (1.0 / _fps);
        break;
    }
    case LibmpvPropertyKey::Pause: {
        if (!event->data) {
            LERROR("Could not find pause property");
            break;
        }
        break;
    }
    default: {
        throw ghoul::MissingCaseException();
        break;
    }
    }
}

void VideoTileProvider::swapBuffersMpv() {
    // Only swap buffers if there was a frame rendered and there is a new frame waiting
    if (_wakeup && _didRender) {
        mpv_render_context_report_swap(_mpvRenderContext);
        _wakeup = 0;
        _didRender = 0;
    }
}

void VideoTileProvider::cleanUpMpv() {
    // Destroy the GL renderer and all of the GL objects it allocated. If video
    // is still running, the video track will be deselected.
    mpv_render_context_free(_mpvRenderContext);

    mpv_destroy(_mpvHandle);

    glDeleteFramebuffers(1, &_fbo);
}

int VideoTileProvider::minLevel() {
    return 1;
}

int VideoTileProvider::maxLevel() {
    // This is the level where above the tile is marked as unavailable and is no longer 
    // displayed. Since we want to display the tiles at all times we set the max level
    return 1337;
}

void VideoTileProvider::reset() {
    if (_videoFile.empty()) {
        return;
    }
    cleanUpMpv();
    initializeMpv();
}

float VideoTileProvider::noDataValueAsFloat() {
    return std::numeric_limits<float>::min();
}

void VideoTileProvider::internalInitialize() {}

void VideoTileProvider::createFBO(int width, int height) {
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

void VideoTileProvider::resizeFBO(int width, int height) {
    LINFO(fmt::format("Resizing FBO with width: {} and height: {}", width, height));

    if (width == _videoResolution.x && height == _videoResolution.y) {
        return;
    }

    // Update resolution of video
    _videoResolution = glm::ivec2(width, height);

    // Delete old FBO and texture
    glDeleteFramebuffers(1, &_fbo);
    _frameTexture.reset(nullptr);

    createFBO(width, height);
}

VideoTileProvider::~VideoTileProvider() {}

void VideoTileProvider::internalDeinitialize() {
    cleanUpMpv();
}

} // namespace openspace::globebrowsing
