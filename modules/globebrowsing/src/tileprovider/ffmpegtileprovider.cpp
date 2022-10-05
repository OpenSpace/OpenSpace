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
#include <openspace/util/time.h>
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

    struct [[codegen::Dictionary(FfmpegTileProvider)]] Parameters {
        // [[codegen::verbatim(FileInfo.description)]]
        std::filesystem::path file;

        // [[codegen::verbatim(StartTimeInfo.description)]]
        std::string startTime [[codegen::datetime()]];
    };
#include "ffmpegtileprovider_codegen.cpp"
} // namespace


namespace openspace::globebrowsing {

void save_gray_frame(unsigned char* buf, int wrap, int xsize, int ysize, const char* filename)
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

documentation::Documentation FfmpegTileProvider::Documentation() {
    return codegen::doc<Parameters>("globebrowsing_ffmpegtileprovider");
}

FfmpegTileProvider::FfmpegTileProvider(const ghoul::Dictionary& dictionary) {
    ZoneScoped

    const Parameters p = codegen::bake<Parameters>(dictionary);

    _videoFile = p.file;
    _startTime = p.startTime;
}

Tile FfmpegTileProvider::tile(const TileIndex& tileIndex) {
    ZoneScoped

    if (!_glFrame || !_codecContext ) {
        return Tile{nullptr, std::nullopt, Tile::Status::Unavailable };
    }

    const int wholeRowSize = FinalResolution.x * BytesPerPixel;
    const int tileRowSize = TileSize.x * BytesPerPixel;

    // The range of rows of the whole image that this tile needs
    const glm::ivec2 rowRange = glm::ivec2(
        TileSize.y * tileIndex.y,
        TileSize.y * (tileIndex.y + 1)
    );

    // Copy every row inside the part of the texture we want for the tile
    GLubyte* destination = &_tilePixels[0];
    GLubyte* source = &_glFrame->data[0][0];
    // Traverse backwards so texture is placed correctly
    for (int row = rowRange.y - 1; row > rowRange.x; --row) {
        // Find index of first item, row & col
        int rowIndex = row * wholeRowSize;
        int columnIndex = tileRowSize * tileIndex.x;

        // Copy row
        memcpy(destination, source + rowIndex + columnIndex, tileRowSize);

        // Advance the destination pointer
        destination += tileRowSize;
    }
    // Look for tile in cache. If not found, create it
    cache::ProviderTileKey key = { tileIndex, _codecContext->frame_number };
    cache::MemoryAwareTileCache* tileCache =
        global::moduleEngine->module<GlobeBrowsingModule>()->tileCache();

    if (tileCache->exist(key)) {
        return tileCache->get(key);
    }

    // The data for initializing the texture
    TileTextureInitData initData(
        TileSize.x,
        TileSize.y,
        GL_UNSIGNED_BYTE,
        ghoul::opengl::Texture::Format::RGB,
        TileTextureInitData::PadTiles::No,
        TileTextureInitData::ShouldAllocateDataOnCPU::No
    );

    // Create a texture with the initialization data
    ghoul::opengl::Texture* writeTexture = tileCache->texture(initData);

    // Update the pixel data for this texture
    writeTexture->setPixelData(
        _tilePixels,
        ghoul::opengl::Texture::TakeOwnership::No
    );
    writeTexture->uploadTexture();

    // Bind the texture to the tile
    Tile ourTile = Tile{ writeTexture, std::nullopt, Tile::Status::OK };
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

    if (!_isInitialized) {
        return;
    }
    // Check if it is time for a new frame
    std::chrono::system_clock::time_point now = std::chrono::system_clock::now();
    std::chrono::system_clock::duration diff = now - _lastFrameTime;
    const double j2000Time = Time::now().j2000Seconds();

    const bool hasNewFrame = (j2000Time > Time::convertTime(_startTime)) &&
        diff > _frameTime;

    if(!hasNewFrame) {
        return;
    }
    _tileIsReady = false;

    // Read frame
    while(true) {
        int result = av_read_frame(_formatContext, _packet);
        if (result < 0) {
            av_packet_unref(_packet);
            return;
        }

        // Does this packet belong to this video stream?
        if (_packet->stream_index != _streamIndex) {
            continue;
        }

        // Send packet to the decoder
        result = avcodec_send_packet(_codecContext, _packet);
        if (result < 0 || result == AVERROR(EAGAIN) || result == AVERROR_EOF) {
            LERROR(fmt::format("Sending packet failed with {}", result));
            av_packet_unref(_packet);
            return;
        }

        // Get result from decoder
        result = avcodec_receive_frame(
            _codecContext,
            _avFrame
        );

        // Is the frame finished? If not then we need to wait for more packets
        // to finish the frame
        if (result == AVERROR(EAGAIN)) {
            continue;
        }
        if (result < 0) {
            LERROR(fmt::format("Receiving packet failed with {}", result));
            av_packet_unref(_packet);
            return;
        }
        // Successfully collected a frame
        LINFO(fmt::format(
            "Successfully decoded frame {}", _codecContext->frame_number
        ));
        break;
    }

    // Update times
    if (_frameTime.count() <= 0) {
        // Calculate frame time
        double sPerFrame = av_q2d(_codecContext->time_base) * _codecContext->ticks_per_frame;
        int msPerFrame = static_cast<int>(sPerFrame * 1000);
        _frameTime = std::chrono::milliseconds(msPerFrame);
    }
    _lastFrameTime = now;

    // TODO: Need to check the format of the video and decide what formats we want to
    // support and how they relate to the GL formats

    // Convert the color format to AV_PIX_FMT_RGB24
    // Only create the conversion context once
    // Scale all videos to 2048 * 1024 pixels
    // TODO: support higher resolutions
    if (!_conversionContext) {
        _conversionContext = sws_getContext(
            _codecContext->width,
            _codecContext->height,
            _codecContext->pix_fmt,
            FinalResolution.x,
            FinalResolution.y,
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

    // TEST save a grayscale frame into a .pgm file
    // This ends up in OpenSpace\build\apps\OpenSpace
    // https://github.com/leandromoreira/ffmpeg-libav-tutorial/blob/master/0_hello_world.c
    /*char frame_filename[1024];
    snprintf(frame_filename, sizeof(frame_filename), "%s-%d.pgm", "frame", _codecContext->frame_number);
    save_gray_frame(_avFrame->data[0], _avFrame->linesize[0], _avFrame->width, _avFrame->height, frame_filename);
    */

    av_packet_unref(_packet);
    _tileIsReady = true;
}

int FfmpegTileProvider::minLevel() {
    return 1;
}

int FfmpegTileProvider::maxLevel() {
    // Every tile needs to be 512 by 512, how far can we subdivide this video
    // TODO: Maybe set this vlaue (512) as a constant somehere?
    // TODO: Check if it should be floor or ceil
    return std::floor(std::log2(FinalResolution.x) - std::log2(1024)) + 1;
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

    // Open the decoder
    if (avcodec_open2(_codecContext, _decoder, nullptr) < 0) {
        throw ghoul::RuntimeError(fmt::format("Failed to open codec for {}", path));
    }

    // Allocate the video frames
    _packet = av_packet_alloc();
    _avFrame = av_frame_alloc();    // Raw frame
    _glFrame = av_frame_alloc();    // Color-converted frame

    // Fill the destination frame for the convertion
    int glFrameSize = av_image_get_buffer_size(
        AV_PIX_FMT_RGB24,
        FinalResolution.x,
        FinalResolution.y,
        1
    );
    uint8_t* internalBuffer =
        reinterpret_cast<uint8_t*>(av_malloc(glFrameSize * sizeof(uint8_t)));
    av_image_fill_arrays(
        _glFrame->data,
        _glFrame->linesize,
        internalBuffer,
        AV_PIX_FMT_RGB24,
        FinalResolution.x,
        FinalResolution.y,
        1
    );

    // Update times
    _lastFrameTime = std::chrono::system_clock::now();
    _isInitialized = true;
}

FfmpegTileProvider::~FfmpegTileProvider() {
    // TODO: Check so internalDeinitialize is called after the last
    // update function and move code there

}

void FfmpegTileProvider::internalDeinitialize() {
    avformat_close_input(&_formatContext);
    av_free(_avFrame);
    av_free(_glFrame);
    av_free(_packet);
    avformat_free_context(_formatContext);
}

} // namespace openspace::globebrowsing
