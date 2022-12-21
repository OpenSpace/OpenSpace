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

#include <modules/globebrowsing/src/tileprovider/ffmpegtileprovider.h>

#include <modules/globebrowsing/globebrowsingmodule.h>
#include <modules/globebrowsing/src/memoryawaretilecache.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/util/time.h>
#include <openspace/util/timemanager.h>
#include <ghoul/filesystem/filesystem.h>

namespace {
    constexpr std::string_view _loggerCat = "FfmpegTileProvider";

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

    struct [[codegen::Dictionary(FfmpegTileProvider)]] Parameters {
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
#include "ffmpegtileprovider_codegen.cpp"
} // namespace


namespace openspace::globebrowsing {

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

void mpvRenderUpdate(void*) {
    LINFO("libmpv: new video frame");
}

documentation::Documentation FfmpegTileProvider::Documentation() {
    return codegen::doc<Parameters>("globebrowsing_ffmpegtileprovider");
}

FfmpegTileProvider::FfmpegTileProvider(const ghoul::Dictionary& dictionary) {
    ZoneScoped

    const Parameters p = codegen::bake<Parameters>(dictionary);

    _videoFile = p.file;
    _startTime = p.startTime;
    _endTime = p.endTime;

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
}

Tile FfmpegTileProvider::tile(const TileIndex& tileIndex) {
    ZoneScoped

    // Look for tile in cache
    cache::ProviderTileKey key = { tileIndex, _prevVideoTime }; // TODO: Improve cachign with a better id
    cache::MemoryAwareTileCache* tileCache =
        global::moduleEngine->module<GlobeBrowsingModule>()->tileCache();

    if (tileCache->exist(key)) {
        return tileCache->get(key);
    }

    // TODO: Move this to GPU
    /*
    // If tile not found in cache then create it
    const int wholeRowSize = _resolution.x * BytesPerPixel;
    const int tileRowSize = TileSize.x * BytesPerPixel;

    // The range of rows of the whole image that this tile needs
    const glm::ivec2 rowRange = glm::ivec2(
        TileSize.y * tileIndex.y,
        TileSize.y * (tileIndex.y + 1)
    );

     {
        ZoneScopedN("Copy Frame")
        TracyGpuZone("Copy Frame")

        // Copy every row inside the part of the texture we want for the tile
        GLubyte* destination = _tilePixels;
        GLubyte* source = &_glFrame->data[0][0];
        // Traverse backwards so texture is placed correctly
        for (int row = rowRange.y - 1; row >= rowRange.x; --row) {
            // Find index of first item, row & col
            int rowIndex = row * wholeRowSize;
            int columnIndex = tileRowSize * tileIndex.x;

            // Copy row
            memcpy(destination, source + rowIndex + columnIndex, tileRowSize);

            // Advance the destination pointer
            destination += tileRowSize;
        }
        glUnmapBuffer(GL_PIXEL_UNPACK_BUFFER);
        glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
    }*/

    // The data for initializing the texture
    TileTextureInitData initData(
        TileSize.x,
        TileSize.y,
        GL_UNSIGNED_BYTE,
        ghoul::opengl::Texture::Format::RGBA,
        TileTextureInitData::PadTiles::No,
        TileTextureInitData::ShouldAllocateDataOnCPU::No
    );

    // Create a texture with the initialization data
    ghoul::opengl::Texture* writeTexture = tileCache->texture(initData);

    // Bind the texture to the tile
    Tile ourTile = Tile{ _frameTexture, std::nullopt, Tile::Status::OK };
    tileCache->put(key, initData.hashKey, ourTile);

    return ourTile;
}

Tile::Status FfmpegTileProvider::tileStatus(const TileIndex& tileIndex) {
    //return _tileIsReady ? Tile::Status::OK : Tile::Status::Unavailable;

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

TileDepthTransform FfmpegTileProvider::depthTransform() {
    return { 0.f, 1.f };
}

void FfmpegTileProvider::update() {
    ZoneScoped

    // Always check that our framebuffer is ok
    if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE) {
        LINFO("Framebuffer is not complete");
    }

    if (!_isInitialized) {
        return;
    }

    //Check mpv events
    handleMpvEvents();

    // Double check the video duration
    if (_videoDuration < 0.0) {
        int result = mpv_get_property(_mpvHandle, "duration", MPV_FORMAT_DOUBLE, &_videoDuration);
        if (!checkMpvError(result)) {
            LWARNING("Could not find video duration");
            return;
        }
    }

    // TODO: bind OpenSpace tiem to video time
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
    int hasReachedEnd;
    int result = mpv_get_property(_mpvHandle, "eof-reached", MPV_FORMAT_FLAG, &hasReachedEnd);
    if (!checkMpvError(result)) {
        LWARNING("Could not check if end of video reached");
    }

    if (hasReachedEnd > 0) {
        LINFO("Reached end of video");
        return;
    }

    double currentVideoTime;
    result = mpv_get_property(_mpvHandle, "playback-time", MPV_FORMAT_DOUBLE, &currentVideoTime);
    if (!checkMpvError(result)) {
        LWARNING("Could not find current time in video");
    }

    if (currentVideoTime > _videoDuration || currentVideoTime < 0.0) {
        LWARNING("Current time is outside range of video");
        return;
    }

    LINFO(fmt::format(
        "OS video time: {}, mpv current time: {}",
        OSvideoTime, currentVideoTime
    ));

    // Are we going backwards?
    if (global::timeManager->deltaTime() < 0) {
        LINFO("Backwards!");
    }

    // Check if we need to seek in the video
    if (OSvideoTime - currentVideoTime > 2.0 * _frameTime) {
        LINFO(fmt::format(
            "Seek needed OS curr: {}, video curr: {}",
            OSvideoTime, currentVideoTime
        ));
        result = mpv_set_property(_mpvHandle, "playback-time", MPV_FORMAT_DOUBLE, &OSvideoTime);
        if (!checkMpvError(result)) {
            LWARNING("Could not seek in video");
        }

        result = mpv_get_property(_mpvHandle, "playback-time", MPV_FORMAT_DOUBLE, &currentVideoTime);
        if (!checkMpvError(result)) {
            LWARNING("Could not find current time in video");
        }
        LINFO(fmt::format("Seeked to time {}", currentVideoTime));
    }

    // Check if it is time for a new frame
    if (OSvideoTime - currentVideoTime < _frameTime) {
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
        LINFO("Un-waiting :)");

        int setPauseFalse = 0;
        result = mpv_set_property(_mpvHandle, "pause", MPV_FORMAT_FLAG, &setPauseFalse);
        if (!checkMpvError(result)) {
            LWARNING("Could not un-pause video");
        }
        _isWaiting = false;
    }

    _tileIsReady = false;

    // Render video frame to texture
    mpv_opengl_fbo mpvfbo{ static_cast<int>(_fbo), _resolution.x, _resolution.y, 0 };
    int flip_y{ 1 };

    mpv_render_param params[] = {
        {MPV_RENDER_PARAM_OPENGL_FBO, &mpvfbo},
        {MPV_RENDER_PARAM_FLIP_Y, &flip_y},
        {MPV_RENDER_PARAM_INVALID, nullptr}
    };

    // See render_gl.h on what OpenGL environment mpv expects, and other API details
    // This function fills the fbo and texture with data, after it we can get the data on the GPU, not the CPU
    mpv_render_context_render(_mpvRenderContext, params);

    // TEST save a grayscale frame into a .pgm file
    // This ends up in OpenSpace\build\apps\OpenSpace
    // https://github.com/leandromoreira/ffmpeg-libav-tutorial/blob/master/0_hello_world.c
    /*char frame_filename[1024];
    snprintf(frame_filename, sizeof(frame_filename), "%s-%d.pgm", "frame", _codecContext->frame_number);
    save_gray_frame(_avFrame->data[0], _avFrame->linesize[0], _avFrame->width, _avFrame->height, frame_filename);
    */

    _tileIsReady = true;
    _prevVideoTime = currentVideoTime;
}

void FfmpegTileProvider::handleMpvEvents() {
    while (_mpvHandle) {
        mpv_event* event = mpv_wait_event(_mpvHandle, 0);
        if (event->event_id == MPV_EVENT_NONE) {
            return;
        }

        switch (event->event_id) {
            case MPV_EVENT_VIDEO_RECONFIG: {
                // Retrieve the new video size.
                int64_t w, h;

                // Get width
                int result1 = mpv_get_property(_mpvHandle, "dwidth", MPV_FORMAT_INT64, &w);
                if (!checkMpvError(result1)) {
                    LWARNING("Could not find new width of video");
                }

                // Get Height
                int result2 = mpv_get_property(_mpvHandle, "dheight", MPV_FORMAT_INT64, &h);
                if (!checkMpvError(result2)) {
                    LWARNING("Could not find new height of video");
                }

                // Resize FBO
                if (result1 >= 0 && result2 >= 0 && w > 0 && h > 0) {
                    resizeFBO(static_cast<int>(w), static_cast<int>(h));
                }
                break;
            }
            case MPV_EVENT_PROPERTY_CHANGE: {
                // TODO: change getting of this property without qt
                mpv_event_property* prop = (mpv_event_property*)event->data;
                if (strcmp(prop->name, "video-params") == 0 &&
                    prop->format == MPV_FORMAT_NODE)
                {
                    mpv_node videoParams;
                    int result = mpv_get_property(_mpvHandle, "video-params", MPV_FORMAT_NODE, &videoParams);
                    if (!checkMpvError(result)) {
                        LWARNING("Could not find video parameters");
                        return;
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
                    else {
                        LERROR("Invalid video-params");
                        return;
                    }
                }
                // TODO: handle pause event
                break;
            }
            default: {
                // Ignore uninteresting or unknown events.
                break;
            }
        }
    }
}

int FfmpegTileProvider::minLevel() {
    return 1;
}

int FfmpegTileProvider::maxLevel() {
    // Every tile needs to be 512 by 512, how far can we subdivide this video
    // TODO: Check if it should be floor or ceil
    return std::floor(std::log2(_resolution.x) - std::log2(1024)) + 1;
}

void FfmpegTileProvider::reset() {
    if (_videoFile.empty()) {
        return;
    }
    _tileIsReady = false;
    internalDeinitialize();
    internalInitialize();
}


float FfmpegTileProvider::noDataValueAsFloat() {
    return std::numeric_limits<float>::min();
}

void FfmpegTileProvider::internalInitialize() {
    _startJ200Time = Time::convertTime(_startTime);
    _endJ200Time = Time::convertTime(_endTime);
    std::string path = absPath(_videoFile).string();


    // libmpv handle
    _mpvHandle = mpv_create();
    if (!_mpvHandle) {
        LERROR("Could not create mpv handle");
        return;
    }

    int result = mpv_initialize(_mpvHandle);
    if (!checkMpvError(result)) {
        LERROR("Could not initialize mpv");
        return;
    }

    // Set mpv parameters to render using openGL
    mpv_opengl_init_params glInitParams{ getOpenGLProcAddress, nullptr};
    mpv_render_param params[]{
        {MPV_RENDER_PARAM_API_TYPE, const_cast<char*>(MPV_RENDER_API_TYPE_OPENGL)},
        {MPV_RENDER_PARAM_OPENGL_INIT_PARAMS, &glInitParams},
        {MPV_RENDER_PARAM_INVALID, nullptr}
    };

    // This makes libmpv use the currently set OpenGL context. It will use the callback
    // (passed via params) to resolve built in OpenGL functions, as well as extensions
    result = mpv_render_context_create(&_mpvRenderContext, _mpvHandle, params);
    if (!checkMpvError(result)) {
        LERROR("Could not create mpv render context");
        return;
    }

    // When there is a need to call mpv_render_context_update(), which can
    // request a new frame to be rendered.
    // (Separate from the normal event handling mechanism for the sake of
    //  users which run OpenGL on a different thread.)
    mpv_render_context_set_update_callback(_mpvRenderContext, mpvRenderUpdate, NULL);

    // TODO: Load mpv configuration?
    // TODO: Set default settings?

    // Load file
    const char* cmd[] = { "loadfile", _videoFile.string().c_str(), NULL };
    result = mpv_command(_mpvHandle, cmd);
    if (!checkMpvError(result)) {
        LERROR("Could not open video file");
        return;
    }

    // TODO: Make sure to handle all of them in handleMpvEvents() function
    //Observe video parameters
    mpv_observe_property(_mpvHandle, 0, "video-params", MPV_FORMAT_NODE);
    mpv_observe_property(_mpvHandle, 0, "pause", MPV_FORMAT_FLAG);
    mpv_observe_property(_mpvHandle, 0, "time-pos", MPV_FORMAT_DOUBLE);

    //Create FBO to render video into
    createFBO(_resolution.x, _resolution.y);

    // Find duration of video
    result = mpv_get_property(_mpvHandle, "duration", MPV_FORMAT_DOUBLE, &_videoDuration);
    if (!checkMpvError(result)) {
        LWARNING("Could not find video duration");
    }

    // TODO: Find the estimated frame time of the video

    _isInitialized = true;
}

void FfmpegTileProvider::createFBO(int width, int height) {
    LINFO(fmt::format("Creating new FBO with width: {} and height: {}", width, height));

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

void FfmpegTileProvider::resizeFBO(int width, int height) {
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

FfmpegTileProvider::~FfmpegTileProvider() {

}

void FfmpegTileProvider::internalDeinitialize() {
    // lib mpv
    // Destroy the GL renderer and all of the GL objects it allocated. If video
    // is still running, the video track will be deselected.
    mpv_render_context_free(_mpvRenderContext);

    mpv_destroy(_mpvHandle);

    glDeleteFramebuffers(1, &_fbo);

    delete _frameTexture;
}

} // namespace openspace::globebrowsing
