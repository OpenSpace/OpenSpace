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

void check_error(int status) {
    if (status < 0) {
        LERROR("mpv API error: %s\n", mpv_error_string(status));
    }
}

void* get_proc_address_mpv(void*, const char* name) {
    return reinterpret_cast<void*>(global::windowDelegate->openGLProcedureAddress(name));
}

void on_mpv_render_update(void*) {}

std::string getFffmpegErrorString(const int errorCode) {
    const int size = 100;
    const char initChar = '@';
    std::string result;

    std::vector<char> buf;
    buf.resize(size, initChar);
    char* newBuf = av_make_error_string(buf.data(), size, errorCode);

    for (int i = 0; i < buf.size(); ++i) {
        if (buf[i] != initChar) {
            result.append(1, buf[i]);
        }
        else {
            result.append(1, '\n');
            break;
        }
    }
    return result;
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

    if (!_glFrame || !_codecContext ) {
        return Tile{nullptr, std::nullopt, Tile::Status::Unavailable };
    }

    // Look for tile in cache
    cache::ProviderTileKey key = { tileIndex, _prevFrameIndex }; // TODO: Improve cachign with a better id
    cache::MemoryAwareTileCache* tileCache =
        global::moduleEngine->module<GlobeBrowsingModule>()->tileCache();

    if (tileCache->exist(key)) {
        return tileCache->get(key);
    }

    // If tile not found in cache then create it
    const int wholeRowSize = _resolution.x * BytesPerPixel;
    const int tileRowSize = TileSize.x * BytesPerPixel;

    // The range of rows of the whole image that this tile needs
    const glm::ivec2 rowRange = glm::ivec2(
        TileSize.y * tileIndex.y,
        TileSize.y * (tileIndex.y + 1)
    );

    {
        ZoneScopedN("Map PBO")
        TracyGpuZone("Map PBO")

        // Map PBO
        glBindBuffer(GL_PIXEL_UNPACK_BUFFER, _pbo);
        glBufferData(GL_PIXEL_UNPACK_BUFFER, BytesPerTile, nullptr, GL_STREAM_DRAW);
        _tilePixels = reinterpret_cast<GLubyte*>(
            glMapBuffer(GL_PIXEL_UNPACK_BUFFER, GL_WRITE_ONLY)
        );
        if (!_tilePixels) {
            LERROR("Failed to map PBO");
            return Tile{ nullptr, std::nullopt, Tile::Status::IOError };
        }
    }
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
    }

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
    Tile ourTile = Tile{ _mpvTexture, std::nullopt, Tile::Status::OK };
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

    mpv_opengl_fbo mpfbo{ static_cast<int>(_mpvFBO), _resolution.x, _resolution.y, 0 };
    int flip_y{ 1 };

    mpv_render_param params[] = {
        {MPV_RENDER_PARAM_OPENGL_FBO, &mpfbo},
        {MPV_RENDER_PARAM_FLIP_Y, &flip_y},
        {MPV_RENDER_PARAM_INVALID, nullptr}
    };

    // See render_gl.h on what OpenGL environment mpv expects, and other API details
    // This function fills the fbo and texture with data, after it we can get the data on the GPU, not the CPU
    mpv_render_context_render(_mpvRenderContext, params);


    const double now = global::timeManager->time().j2000Seconds();
    double videoTime = 0.0;
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
            videoTime = percentage * _videoDuration;
            break;
        case AnimationMode::RealTimeLoopFromStart:
            videoTime = std::fmod(now - _startJ200Time, _videoDuration);
            if (videoTime < 0.0) {
                videoTime += _videoDuration;
            }
            break;
        case AnimationMode::RealTimeLoopInfinitely:
            videoTime =
                _videoDuration - abs(
                    fmod(now - _startJ200Time, 2 * _videoDuration) - _videoDuration
                );
            break;
        default:
            throw ghoul::MissingCaseException();
    }
    if (videoTime > _videoDuration || videoTime < 0.0) {
        LINFO(std::to_string(videoTime));
    }

    // Find the frame number that corresponds to the current video time
    int64_t internalFrameIndex = static_cast<int64_t>(
        videoTime / av_q2d(_formatContext->streams[_streamIndex]->time_base)
    );
    int64_t currentFrameIndex = internalFrameIndex / 1000;

    // Check if we found a new frame
    if (_prevFrameIndex == currentFrameIndex) {
        return;
    }
    _tileIsReady = false;

    {
        ZoneScopedN("Seek Frame")
        TracyGpuZone("Seek Frame")

        if ((currentFrameIndex - 1) != _prevFrameIndex) {
            // Jump more than one frame in the video
            int result = av_seek_frame(_formatContext, _streamIndex, internalFrameIndex, 0);
            if (result < 0) {
                std::string message = getFffmpegErrorString(result);
                LERROR(fmt::format(
                    "Seeking frame {} failed with error code {}, message: '{}'",
                    currentFrameIndex, result, message
                ));
                return;
            }
            //LINFO(fmt::format(
            //    "Seek frame {} matches video duration {} and frame index {}",
            //    internalFrameIndex, videoTime, currentFrameIndex
            //));
        }
    }
    {
        ZoneScopedN("Read Frame")
        TracyGpuZone("Read Frame")

        // Read frame
        while (true) {
            int result;
            {
                ZoneScopedN("av_read_frame")
                result = av_read_frame(_formatContext, _packet);
            }
            if (result < 0) {
                LERROR(fmt::format("Reading frame failed with code {}", result));
                av_packet_unref(_packet);
                return;
            }

            // Does this packet belong to this video stream?
            if (_packet->stream_index != _streamIndex) {
                continue;
            }

            // Send packet to the decoder
            {
                ZoneScopedN("avcodec_send_packet")
                result = avcodec_send_packet(_codecContext, _packet);
            }
            if (result < 0 || result == AVERROR(EAGAIN) || result == AVERROR(AVERROR_EOF)) {
                LERROR(fmt::format("Sending packet failed with code {}", result));
                av_packet_unref(_packet);
                return;
            }

            // Get result from decoder
            {
                ZoneScopedN("avcodec_receive_frame")
                result = avcodec_receive_frame(
                    _codecContext,
                    _avFrame
                );
            }

            // Is the frame finished? If not then we need to wait for more packets
            // to finish the frame
            if (result == AVERROR(EAGAIN)) {
                continue;
            }
            if (result < 0) {
                LERROR(fmt::format("Receiving packet failed with code {}", result));
                av_packet_unref(_packet);
                return;
            }
            // Successfully collected a frame
            break;
        }
    }
    //LINFO(fmt::format(
    //    "Successfully decoded frame {}", currentFrameIndex
    //));

    // WIP
    /*int64_t pts = _packet->pts;
    pts = pts == AV_NOPTS_VALUE ? 0 : pts;
    LINFO(fmt::format("Pts: {}", pts));
    double ptsSec = pts * av_q2d(_formatContext->streams[_streamIndex]->time_base);
    LINFO(fmt::format("Pts Sec: {}", ptsSec));
    LINFO(fmt::format("repeat_pict: {}", _avFrame->repeat_pict));*/

    // Debug checks
    int64_t pts = _packet->pts;
    int64_t dts = _packet->dts;
    pts = pts == AV_NOPTS_VALUE ? 0 : pts;
    dts = dts == AV_NOPTS_VALUE ? 0 : dts;
    if (pts != dts) {
        LWARNING(fmt::format(
            "Format error, frames might be out of order, diff {}", abs(pts - dts)
        ));
    }
    if (_avFrame->repeat_pict > 0) {
        LWARNING(fmt::format(
            "Frame {} requested to be repeated {} times", currentFrameIndex,
            _avFrame->repeat_pict
        ));
    }

    // TODO: Need to check the format of the video and decide what formats we want to
    // support and how they relate to the GL formats

    {
        ZoneScopedN("Convert Frame")
        TracyGpuZone("Convert Frame")

        // Convert the color format to AV_PIX_FMT_RGB24
        // Only create the conversion context once
        // Scale all videos to 2048 * 1024 pixels
        // TODO: support higher resolutions
        if (!_conversionContext) {
            _conversionContext = sws_getContext(
                _codecContext->width,
                _codecContext->height,
                _codecContext->pix_fmt,
                _resolution.x,
                _resolution.y,
                AV_PIX_FMT_RGB24,
                SWS_BICUBIC,
                nullptr,
                nullptr,
                nullptr
            );
        }

        sws_scale(
            _conversionContext,
            _avFrame->data,
            _avFrame->linesize,
            0,
            _codecContext->height,
            _glFrame->data,
            _glFrame->linesize
        );
    }

    // TEST save a grayscale frame into a .pgm file
    // This ends up in OpenSpace\build\apps\OpenSpace
    // https://github.com/leandromoreira/ffmpeg-libav-tutorial/blob/master/0_hello_world.c
    /*char frame_filename[1024];
    snprintf(frame_filename, sizeof(frame_filename), "%s-%d.pgm", "frame", _codecContext->frame_number);
    save_gray_frame(_avFrame->data[0], _avFrame->linesize[0], _avFrame->width, _avFrame->height, frame_filename);
    */

    _prevFrameIndex = currentFrameIndex;
    av_packet_unref(_packet);
    _tileIsReady = true;
}

void FfmpegTileProvider::handleMpvEvents() {
    while (_mpvHandle) {
        mpv_event* event = mpv_wait_event(_mpvHandle, 0);
        if (event->event_id == MPV_EVENT_NONE) {
            break;
        }

        switch (event->event_id) {
            case MPV_EVENT_VIDEO_RECONFIG: {
                // Retrieve the new video size.
                int64_t w, h;
                if (mpv_get_property(_mpvHandle, "dwidth", MPV_FORMAT_INT64, &w) >= 0 &&
                    mpv_get_property(_mpvHandle, "dheight", MPV_FORMAT_INT64, &h) >= 0 &&
                    w > 0 && h > 0)
                {
                    resizeMpvFBO(static_cast<int>(w), static_cast<int>(h));
                    _mpvVideoReconfigs++;
                }
                break;
            }
            case MPV_EVENT_PROPERTY_CHANGE: {
                // TODO: change getting of this property without qt
                /*mpv_event_property* prop = (mpv_event_property*)event->data;
                if (strcmp(prop->name, "video-params") == 0) {
                    if (prop->format == MPV_FORMAT_NODE) {
                        const QVariant videoParams = mpv::qt::get_property(_mpvHandle, "video-params");
                        auto vm = videoParams.toMap();
                        int w = vm["w"].toInt();
                        int h = vm["h"].toInt();
                        resizeMpvFBO((int)w, (int)h);
                    }
                }*/
                break;
                // TODO: handle pause event
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

    // Open video
    int openRes = avformat_open_input(
        &_formatContext,
        path.c_str(),
        nullptr,
        nullptr
    );
    if (openRes < 0) {
        throw ghoul::RuntimeError(fmt::format("Failed to open input for file {}", path));
    }

    // Find stream info
    if (avformat_find_stream_info(_formatContext, nullptr) < 0) {
        throw ghoul::RuntimeError(fmt::format("Failed to get stream info for {}", path));
    }
    // DEBUG dump info
    av_dump_format(_formatContext, 0, path.c_str(), false);
    LINFO(fmt::format("Duration: ", _formatContext->duration));

    // Find the video stream
    for (unsigned int i = 0; i < _formatContext->nb_streams; ++i) {
        AVMediaType codec = _formatContext->streams[i]->codecpar->codec_type;
        if (codec == AVMEDIA_TYPE_VIDEO) {
            _streamIndex = i;
            _videoStream = _formatContext->streams[_streamIndex];
            break;
        }
    }
    if (_streamIndex == -1 || _videoStream == nullptr) {
        throw ghoul::RuntimeError(fmt::format("Failed to find video stream for {}", path));
    }

    // Find decoder
    _decoder = avcodec_find_decoder(_videoStream->codecpar->codec_id);
    if (!_decoder) {
        throw ghoul::RuntimeError(fmt::format("Failed to find decoder for {}", path));
    }

    // Find codec
    _codecContext = avcodec_alloc_context3(nullptr);
    int contextSuccess = avcodec_parameters_to_context(
        _codecContext,
        _videoStream->codecpar
    );
    if (contextSuccess < 0) {
        throw ghoul::RuntimeError(
            fmt::format("Failed to create codec context for {}", path)
        );
    }

    // Use multithreaded decoding if suitable
    _codecContext->thread_count = 0;
    if (_decoder->capabilities | AV_CODEC_CAP_FRAME_THREADS) {
        _codecContext->thread_type = FF_THREAD_FRAME;
    }
    else if (_decoder->capabilities | AV_CODEC_CAP_SLICE_THREADS) {
        _codecContext->thread_type = FF_THREAD_SLICE;
    }
    else {
        // Multithreading not suitable
        _codecContext->thread_count = 1;
    }

    // Open the decoder
    if (avcodec_open2(_codecContext, _decoder, nullptr) < 0) {
        throw ghoul::RuntimeError(fmt::format("Failed to open codec for {}", path));
    }

    // Find the duration of the video
    _videoDuration = _formatContext->duration * av_q2d(_avTimeBaseQ);

    // Allocate the video frames
    _packet = av_packet_alloc();
    _avFrame = av_frame_alloc();    // Raw frame
    _glFrame = av_frame_alloc();    // Color-converted frame

    // Fill the destination frame for the convertion
    int glFrameSize = av_image_get_buffer_size(
        AV_PIX_FMT_RGB24,
        _resolution.x,
        _resolution.y,
        1
    );
    uint8_t* internalBuffer =
        reinterpret_cast<uint8_t*>(av_malloc(glFrameSize * sizeof(uint8_t)));
    av_image_fill_arrays(
        _glFrame->data,
        _glFrame->linesize,
        internalBuffer,
        AV_PIX_FMT_RGB24,
        _resolution.x,
        _resolution.y,
        1
    );
    _buffer = av_buffer_alloc(glFrameSize);
    _packet->buf = _buffer;

    // Create PBO for async texture upload
    glGenBuffers(1, &_pbo);

    // libmpv
    _mpvHandle = mpv_create();
    if (!_mpvHandle) {
        LERROR("Could not create mpv context");
    }

    // Some minor options can only be set before mpv_initialize()
    if (mpv_initialize(_mpvHandle) < 0) {
        LERROR("mpv context failed to initialize");
    }

    // Set mpv to render using openGL
    mpv_opengl_init_params gl_init_params{ get_proc_address_mpv, nullptr};
    mpv_render_param params[]{
        {MPV_RENDER_PARAM_API_TYPE, const_cast<char*>(MPV_RENDER_API_TYPE_OPENGL)},
        {MPV_RENDER_PARAM_OPENGL_INIT_PARAMS, &gl_init_params},
        {MPV_RENDER_PARAM_INVALID, nullptr}
    };

    // This makes mpv use the currently set GL context. It will use the callback
    // (passed via params) to resolve GL built in functions, as well as extensions
    if (mpv_render_context_create(&_mpvRenderContext, _mpvHandle, params) < 0) {
        LERROR("Failed to initialize mpv OpenGL context");
    }

    // When there is a need to call mpv_render_context_update(), which can
    // request a new frame to be rendered.
    // (Separate from the normal event handling mechanism for the sake of
    //  users which run OpenGL on a different thread.)
    mpv_render_context_set_update_callback(_mpvRenderContext, on_mpv_render_update, NULL);

    //Observe video parameters
    mpv_observe_property(_mpvHandle, 0, "video-params", MPV_FORMAT_NODE);
    mpv_observe_property(_mpvHandle, 0, "pause", MPV_FORMAT_FLAG);
    mpv_observe_property(_mpvHandle, 0, "time-pos", MPV_FORMAT_DOUBLE);

    //Creating new FBO to render mpv into
    createMpvFBO(_resolution.x, _resolution.y);

    // Play this file
    const char* cmd[] = { "loadfile", _videoFile.string().c_str(), NULL };
    check_error(mpv_command(_mpvHandle, cmd));

    _isInitialized = true;
}

void FfmpegTileProvider::createMpvFBO(int width, int height) {
    // Update resolution of video
    _resolution = glm::ivec2(width, height);

    glGenFramebuffers(1, &_mpvFBO);
    glBindFramebuffer(GL_FRAMEBUFFER, _mpvFBO);

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
    _mpvTexture = tileCache->texture(initData);

    // Configure
    _mpvTexture->bind();
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
        *_mpvTexture,
        0
    );

    // Unbind FBO
    glBindFramebuffer(GL_FRAMEBUFFER, 0);
}

void FfmpegTileProvider::resizeMpvFBO(int width, int height) {
    if (width == _resolution.x && height == _resolution.y) {
        return;
    }

    LINFO(fmt::format("New MPV FBO width:{} and height:{}", width, height));

    glDeleteFramebuffers(1, &_mpvFBO);
    delete _mpvTexture;
    createMpvFBO(width, height);
}

FfmpegTileProvider::~FfmpegTileProvider() {

}

void FfmpegTileProvider::internalDeinitialize() {
    avformat_close_input(&_formatContext);
    av_free(_avFrame);
    av_free(_glFrame);
    av_free(_packet);
    avformat_free_context(_formatContext);
    avcodec_close(_codecContext);

    // lib mpv
    // Destroy the GL renderer and all of the GL objects it allocated. If video
    // is still running, the video track will be deselected.
    mpv_render_context_free(_mpvRenderContext);

    mpv_destroy(_mpvHandle);

    glDeleteFramebuffers(1, &_mpvFBO);

    delete _mpvTexture;
}

} // namespace openspace::globebrowsing
