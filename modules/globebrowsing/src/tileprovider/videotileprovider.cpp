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

    struct [[codegen::Dictionary(VideoTileProvider)]] Parameters {
        // [[codegen::verbatim(FileInfo.description)]]
        std::filesystem::path file;

        // [[codegen::verbatim(StartTimeInfo.description)]]
        std::string startTime [[codegen::datetime()]];

        // [[codegen::verbatim(EndTimeInfo.description)]]
        std::string endTime [[codegen::datetime()]];

        enum class AnimationMode {
            MapToSimulationTime = 0,
            RealTimeLoopFromStart,
            RealTimeLoopInfinitely
        };

        // The mode of how the animation should be played back.
        // Default is video is played back according to the set start and end times.
        std::optional<AnimationMode> animationMode;

    };
#include "videotileprovider_codegen.cpp"
} // namespace

namespace openspace::globebrowsing {

int VideoTileProvider::_wakeup = 0;

void save_gray_frame(unsigned char* buf, int wrap, int xsize, int ysize,
                     const char* filename)
{
    FILE* f;
    int i;
    f = fopen(filename, "w");
    // writing the minimal required header for a pgm file format
    // portable graymap format -> https://en.wikipedia.org/wiki/Netpbm_format#PGM_example
    fprintf(f, "P5\n%d %d\n%d\n", xsize, ysize, 255);

    // writing line by line
    for (i = 0; i < ysize; i++)
        fwrite(buf + i * wrap, 1, xsize, f);
    fclose(f);
}

bool checkMpvError(int status) {
    if (status < 0) {
        LERROR(fmt::format("Libmpv API error: {}", mpv_error_string(status)));
        return false;
    }
    return true;
}

void* getOpenGLProcAddress(void*, const char* name) {
    return reinterpret_cast<void*>(global::windowDelegate->openGLProcedureAddress(name));
}

void VideoTileProvider::on_mpv_render_update(void*) {
    // The wakeup flag is set here to enable the mpv_render_context_render 
    // path in the main loop.
    _wakeup = 1;
}

documentation::Documentation VideoTileProvider::Documentation() {
    return codegen::doc<Parameters>("globebrowsing_videotileprovider");
}

VideoTileProvider::VideoTileProvider(const ghoul::Dictionary& dictionary) {
    ZoneScoped

    const Parameters p = codegen::bake<Parameters>(dictionary);

    _videoFile = p.file;

    if (p.animationMode.has_value()) {
        switch (*p.animationMode) {
            case Parameters::AnimationMode::RealTimeLoopFromStart:
                _animationMode = AnimationMode::RealTimeLoopFromStart;
                break;
            case Parameters::AnimationMode::RealTimeLoopInfinitely:
                _animationMode = AnimationMode::RealTimeLoopInfinitely;
                break;
            case Parameters::AnimationMode::MapToSimulationTime:
                _animationMode = AnimationMode::MapToSimulationTime;
                break;
            default:
                throw ghoul::MissingCaseException();
        }
    }
    _startJ200Time = Time::convertTime(p.startTime);
    _endJ200Time = Time::convertTime(p.endTime);
    ghoul_assert(_endJ200Time > _startJ200Time, "Invalid times for video");

    global::callback::postSyncPreDraw->emplace_back([this]() {
        // Initialize mpv here to ensure that the opengl context is the same as in for 
        // the rendering
        if (!_isInitialized) {
            initializeMpv();
        }
        renderMpv();
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

    // Look for tile in cache
    cache::ProviderTileKey key = { tileIndex, _prevVideoTime }; // TODO: Improve caching with a better id
    cache::MemoryAwareTileCache* tileCache =
        global::moduleEngine->module<GlobeBrowsingModule>()->tileCache();

    if (tileCache->exist(key)) {
        return tileCache->get(key);
    }

    // Bind the texture to the tile
    Tile ourTile = Tile{ _frameTexture, std::nullopt, Tile::Status::OK };
    tileCache->put(key, _frameTextureHashKey, ourTile);

    return ourTile;
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
    ZoneScoped

    // Always check that our framebuffer is ok
    if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE) {
        LINFO("Framebuffer is not complete");
    }

    if (!_isInitialized) {
        return;
    }
    /*
    // Double check the video duration
    if (_videoDuration < 0.0) {
        int result = mpv_get_property_async(
            _mpvHandle,
            static_cast<uint64_t>(LibmpvPropertyKey::Duration),
            "duration",
            MPV_FORMAT_DOUBLE
        );
        if (!checkMpvError(result)) {
            LWARNING("Could not find video duration");
            return;
        }
    }

    const double now = global::timeManager->time().j2000Seconds();
    double OSvideoTime = 0.0;
    double percentage = 0.0;

    switch (_animationMode) {
        case AnimationMode::MapToSimulationTime:
            // Check so we are currently in interval where video is playing
            if (now > _endJ200Time || now < _startJ200Time) {
                LINFO(fmt::format(
                    "Time '{}' is not during video", now
                ));
                return;
            }
            percentage = (now - _startJ200Time) / (_endJ200Time - _startJ200Time);
            OSvideoTime = percentage * _videoDuration;
            break;
        case AnimationMode::RealTimeLoopFromStart:
            OSvideoTime = std::fmod(now - _startJ200Time, _videoDuration);
            if (OSvideoTime < 0.0) {
                OSvideoTime += _videoDuration;
            }
            break;
        case AnimationMode::RealTimeLoopInfinitely:
            OSvideoTime =
                _videoDuration - abs(
                    fmod(now - _startJ200Time, 2 * _videoDuration) - _videoDuration
                );
            break;
        default:
            throw ghoul::MissingCaseException();
    }
    if (OSvideoTime > _videoDuration || OSvideoTime < 0.0) {
        LWARNING("Video time is outside range of video");
        return;
    }

    // Check if we have reached the end of the file
    int result = mpv_get_property_async(
        _mpvHandle,
        static_cast<uint64_t>(LibmpvPropertyKey::Eof),
        "eof-reached",
        MPV_FORMAT_FLAG
    );
    if (!checkMpvError(result)) {
        LWARNING("Could not check if end of video reached");
    }

    if (_hasReachedEnd) {
        LINFO("Reached end of video");
        return;
    }

    result = mpv_get_property_async(
        _mpvHandle,
        static_cast<uint64_t>(LibmpvPropertyKey::Time),
        "playback-time",
        MPV_FORMAT_DOUBLE
    );
    if (!checkMpvError(result)) {
        LWARNING("Could not find current time in video");
    }

    if (_currentVideoTime > _videoDuration || _currentVideoTime < 0.0) {
        LWARNING("Current time is outside range of video");
        return;
    }

    LINFO(fmt::format(
        "OS video time: {}, mpv current time: {}",
        OSvideoTime, _currentVideoTime
    ));

    // TODO: deal with backwards
    // Are we going backwards?
    if (global::timeManager->deltaTime() < 0) {
        LINFO("Backwards");
    }

    // Check if we need to seek in the video
    if (OSvideoTime - _currentVideoTime > 2.0 * _frameTime) {
        LINFO(fmt::format(
            "Seek needed OS curr: {}, video curr: {}",
            OSvideoTime, _currentVideoTime
        ));
        result = mpv_set_property(
            _mpvHandle,
            "playback-time",
            MPV_FORMAT_DOUBLE,
            &OSvideoTime
        );
        if (!checkMpvError(result)) {
            LWARNING("Could not seek in video");
        }
    }

    // Check if it is time for a new frame
    if (OSvideoTime - _currentVideoTime < 2.0 * _frameTime) {
        if (_isWaiting) {
            return;
        }

        // Don't need a new frame just now
        LINFO("Waiting..");

        int setPauseTrue = 1;
        result = mpv_set_property(_mpvHandle, "pause", MPV_FORMAT_FLAG, &setPauseTrue);
        if (!checkMpvError(result)) {
            LWARNING("Could not pause video");
        }

        _isWaiting = true;
        return;
    }

    // If we get this far then we want a new frame
    if (_isWaiting) {
        LINFO("Continue");

        int setPauseFalse = 0;
        result = mpv_set_property(_mpvHandle, "pause", MPV_FORMAT_FLAG, &setPauseFalse);
        if (!checkMpvError(result)) {
            LWARNING("Could not un-pause video");
        }
        _isWaiting = false;
    }
    */
    // TEST save a grayscale frame into a .pgm file
    // This ends up in OpenSpace\build\apps\OpenSpace
    // https://github.com/leandromoreira/ffmpeg-libav-tutorial/blob/master/0_hello_world.c
    /*char frame_filename[1024];
    snprintf(frame_filename, sizeof(frame_filename), "%s-%d.pgm", "frame", _codecContext->frame_number);
    save_gray_frame(_avFrame->data[0], _avFrame->linesize[0], _avFrame->width, _avFrame->height, frame_filename);
    */
    _tileIsReady = true;
    _prevVideoTime = _currentVideoTime;
}

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

void VideoTileProvider::initializeMpv() {
    _mpvHandle = mpv_create();
    if (!_mpvHandle)
        LINFO("mpv context init failed");

    // Some minor options can only be set before mpv_initialize().
    if (mpv_initialize(_mpvHandle) < 0)
        LINFO("mpv init failed");

    mpv_opengl_init_params gl_init_params{ getOpenGLProcAddress, nullptr };
    int adv{ 1 }; // we will use the update callback

    mpv_render_param params[]{
        {MPV_RENDER_PARAM_API_TYPE, const_cast<char*>(MPV_RENDER_API_TYPE_OPENGL)},
        {MPV_RENDER_PARAM_OPENGL_INIT_PARAMS, &gl_init_params},
        {MPV_RENDER_PARAM_ADVANCED_CONTROL, &adv},
        {MPV_RENDER_PARAM_BLOCK_FOR_TARGET_TIME, (int*)0},
        {MPV_RENDER_PARAM_INVALID, nullptr},
    };

    // This makes mpv use the currently set GL context. It will use the callback
    // (passed via params) to resolve GL builtin functions, as well as extensions.
    if (mpv_render_context_create(&_mpvRenderContext, _mpvHandle, params) < 0)
        LINFO("failed to initialize mpv GL context");

    // When there is a need to call mpv_render_context_update(), which can
    // request a new frame to be rendered.
    // (Separate from the normal event handling mechanism for the sake of
    //  users which run OpenGL on a different thread.)
    mpv_render_context_set_update_callback(_mpvRenderContext, on_mpv_render_update, NULL);

    // Load file
    const char* cmd[] = { "loadfile", _videoFile.string().c_str(), NULL };
    int result = mpv_command(_mpvHandle, cmd);
    if (!checkMpvError(result)) {
        LERROR("Could not open video file");
        return;
    }
    mpv_set_option_string(_mpvHandle, "loop", "");
    mpv_set_option_string(_mpvHandle, "gpu-api", "opengl");
    mpv_set_option_string(_mpvHandle, "hwdec", "auto");
    mpv_set_option_string(_mpvHandle, "vd-lavc-dr", "yes");
    mpv_set_option_string(_mpvHandle, "terminal", "yes");

    //Create FBO to render video into
    createFBO(_resolution.x, _resolution.y);

    // Print meta data
    result = mpv_get_property_async(
        _mpvHandle,
        static_cast<uint64_t>(LibmpvPropertyKey::Meta),
        "metadata",
        MPV_FORMAT_NODE
    );
    if (!checkMpvError(result)) {
        LWARNING("Could not load meta data");
    }
    
    // Get resolution of video
    result = mpv_get_property_async(
        _mpvHandle,
        static_cast<uint64_t>(LibmpvPropertyKey::Width),
        "width", MPV_FORMAT_INT64
    );

    int result2 = mpv_get_property_async(
        _mpvHandle,
        static_cast<uint64_t>(LibmpvPropertyKey::Height),
        "height",
        MPV_FORMAT_INT64
    );
    
    if (!checkMpvError(result) || !checkMpvError(result2)) {
        LWARNING("Could not load video resolution");
    }
    // Find duration of video
    result = mpv_get_property_async(
        _mpvHandle,
        static_cast<uint64_t>(LibmpvPropertyKey::Duration),
       "duration",
        MPV_FORMAT_DOUBLE
    );
    if (!checkMpvError(result)) {
        LWARNING("Could not find video duration");
    }

    _videoDuration = 41.0;
    _isInitialized = true;
}

void VideoTileProvider::renderMpv() {
    handleMpvEvents();
    mpv_opengl_fbo mpfbo{ static_cast<int>(_fbo), _resolution.x, _resolution.y, 0 };
    int flip_y{ 1 };

    mpv_render_param params[] = {
        {MPV_RENDER_PARAM_OPENGL_FBO, &mpfbo},
        {MPV_RENDER_PARAM_FLIP_Y, &flip_y},
        {MPV_RENDER_PARAM_INVALID, nullptr}
    };
    // See render_gl.h on what OpenGL environment mpv expects, and
    // other API details.
    // See render_gl.h on what OpenGL environment mpv expects, and other API details
    // This function fills the fbo and texture with data, after it we can get the data on 
    // the GPU, not the CPU
    if (_wakeup)
    {
        if ((mpv_render_context_update(_mpvRenderContext) & MPV_RENDER_UPDATE_FRAME))
        {
            // This "renders" to the video_framebuffer "linked by ID" in the
            // params_fbo - BLOCKING
            mpv_render_context_render(_mpvRenderContext, params);

            /* TODO: remove this comment in case we never encounter this issue again */
            // We have to set the Viewport on every cycle because 
            // mpv_render_context_render internally rescales the fb of the context(?!)...
            //glm::ivec2 window = global::windowDelegate->currentDrawBufferResolution();
            //glViewport(0, 0, window.x, window.y);
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
                int result1 = mpv_get_property_async(
                    _mpvHandle,
                    static_cast<uint64_t>(LibmpvPropertyKey::Width),
                    "dwidth", MPV_FORMAT_INT64
                );
                if (!checkMpvError(result1)) {
                    LWARNING("Could not find new width of video");
                }

                // Get Height
                int result2 = mpv_get_property_async(
                    _mpvHandle,
                    static_cast<uint64_t>(LibmpvPropertyKey::Height),
                    "dheight",
                    MPV_FORMAT_INT64
                );
                if (!checkMpvError(result2)) {
                    LWARNING("Could not find new height of video");
                }
                break;
            }
            case MPV_EVENT_PROPERTY_CHANGE: {
                mpv_event_property* prop = (mpv_event_property*)event->data;
                if (strcmp(prop->name, "video-params") == 0 &&
                    prop->format == MPV_FORMAT_NODE)
                {
                    int result = mpv_get_property_async(
                        _mpvHandle,
                        static_cast<uint64_t>(LibmpvPropertyKey::Params),
                        "video-params",
                        MPV_FORMAT_NODE
                    );
                    if (!checkMpvError(result)) {
                        LWARNING("Could not find video parameters");
                        return;
                    }
                    else {
                        LERROR("Invalid video-params");
                        return;
                    }
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
        LINFO(fmt::format("Duration: {}", *duration));
        break;
    }
    case LibmpvPropertyKey::Eof: {
        if (!event->data) {
            LERROR("Could not find eof property");
            break;
        }

        struct mpv_event_property* property = (struct mpv_event_property*)event->data;
        int* eof = static_cast<int*>(property->data);

        if (!eof) {
            LERROR("Could not find eof property");
            break;
        }

        _hasReachedEnd = *eof > 0;
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

        if (*height == _resolution.y) {
            break;
        }

        LINFO(fmt::format("New height: {}", *height));

        if (*height > 0) {
            if (_resolution.x > 0 && _fbo > 0) {
                resizeFBO(_resolution.x, *height);
            }
            else {
                _resolution.y = *height;
            }
        }
        else {
            LERROR("Could not find height of video");
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
            LERROR("Could not find video parameters");
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

        if (*width == _resolution.y) {
            break;
        }

        LINFO(fmt::format("New width: {}", *width));

        if (*width > 0) {
            if (_resolution.y > 0 && _fbo > 0) {
                resizeFBO(*width, _resolution.y);
            }
            else {
                _resolution.x = *width;
            }
        }
        else {
            LERROR("Could not find width of video");
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
    if (_wakeup) {
        mpv_render_context_report_swap(_mpvRenderContext);
        _wakeup = 0;
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
    _tileIsReady = false;
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
    _resolution = glm::ivec2(width, height);

    glGenFramebuffers(1, &_fbo);
    glBindFramebuffer(GL_FRAMEBUFFER, _fbo);

    cache::MemoryAwareTileCache* tileCache =
        global::moduleEngine->module<GlobeBrowsingModule>()->tileCache();

    // Create or get a texture with this initialization data
    TileTextureInitData initData(
        width,
        height,
        GL_UNSIGNED_BYTE,                       // TODO: What format should we use?
        ghoul::opengl::Texture::Format::RGBA,
        TileTextureInitData::PadTiles::No,
        TileTextureInitData::ShouldAllocateDataOnCPU::No
    );
    _frameTexture = tileCache->texture(initData);
    _frameTextureHashKey = initData.hashKey;

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

    // Render video frame to texture
    _mpvFbo = mpv_opengl_fbo{ static_cast<int>(_fbo), _resolution.x, _resolution.y, 0 };
}

void VideoTileProvider::resizeFBO(int width, int height) {
    LINFO(fmt::format("Resizing FBO with width: {} and height: {}", width, height));

    if (width == _resolution.x && height == _resolution.y) {
        return;
    }

    // Update resolution of video
    _resolution = glm::ivec2(width, height);

    // Delete old FBO and texture
    glDeleteFramebuffers(1, &_fbo);
    delete _frameTexture;

    createFBO(width, height);
}

VideoTileProvider::~VideoTileProvider() {}

void VideoTileProvider::internalDeinitialize() {
    cleanUpMpv();
}

} // namespace openspace::globebrowsing
