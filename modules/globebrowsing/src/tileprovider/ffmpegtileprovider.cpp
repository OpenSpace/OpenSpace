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

#include <openspace/documentation/documentation.h>
#include <openspace/util/time.h>
#include <ghoul/filesystem/filesystem.h>
#include <iostream>

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

    struct [[codegen::Dictionary(SingleImageProvider)]] Parameters {
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

FfmpegTileProvider::FfmpegTileProvider(const ghoul::Dictionary& dictionary)
    : TileProvider()
{
    ZoneScoped

    const Parameters p = codegen::bake<Parameters>(dictionary);

    _videoFile = p.file;
    _startTime = p.startTime;

    reset();

    std::string path = absPath(_videoFile).string();

    // Open video
    int openRes = avformat_open_input(
        &_formatContext,
        path.c_str(),
        nullptr,
        nullptr
    );
    if (openRes < 0) {
        LERRORC("FfmpegTileProvider", "Failed to open input for file " + path);
        return;
    }

    // Find stream info
    if (avformat_find_stream_info(_formatContext, nullptr) < 0) {
        LERRORC("FfmpegTileProvider", "Failed to get stream info for " + path);
        return;
    }
    // Dump debug info
    av_dump_format(_formatContext, 0, path.c_str(), false);

    for (unsigned int i = 0; i < _formatContext->nb_streams; ++i) {
        AVMediaType codec = _formatContext->streams[i]->codecpar->codec_type;
        if (codec == AVMEDIA_TYPE_VIDEO) {
            _streamIndex = i;
            _videoStream = _formatContext->streams[_streamIndex];
            break;
        }
    }

    if (_streamIndex == -1 || _videoStream == nullptr) {
        LERRORC("FfmpegTileProvider", "Failed to find video stream for " + path);
        return;
    }

    // Find decoder
    _decoder = avcodec_find_decoder(_videoStream->codecpar->codec_id);
    if (!_decoder) {
        LERRORC("FfmpegTileProvider", "Failed to find decoder for " + path);
        return;
    }

    _codecContext = avcodec_alloc_context3(nullptr);

    int contextSuccess = avcodec_parameters_to_context(
        _codecContext,
        _videoStream->codecpar
    );
    if (contextSuccess < 0) {
        LERRORC(
            "FfmpegTileProvider",
            "Failed to create codec context for " + path
        );
        return;
    }
    _nativeSize = { _codecContext->width, _codecContext->height };

    // Open the decoder
    if (avcodec_open2(_codecContext, _decoder, nullptr) < 0) {
        LERRORC("FfmpegTileProvider", "Failed to open codec for " + path);
        return;
    }

    // Allocate the video frames
    _packet = av_packet_alloc();
    _avFrame = av_frame_alloc();
    _glFrame = av_frame_alloc();

    // Fill the destination frame for the convertion
    int glFrameSize = av_image_get_buffer_size(
        AV_PIX_FMT_RGB24,
        _codecContext->width,
        _codecContext->height,
        1
    );
    uint8_t* internalBuffer =
        reinterpret_cast<uint8_t*>(av_malloc(glFrameSize * sizeof(uint8_t)));
    av_image_fill_arrays(
        _glFrame->data,
        _glFrame->linesize,
        internalBuffer,
        AV_PIX_FMT_RGB24,
        _codecContext->width,
        _codecContext->height,
        1
    );

    _lastFrameTime = std::max(Time::convertTime(_startTime), Time::now().j2000Seconds());
}

Tile FfmpegTileProvider::tile(const TileIndex& tileIndex) {
    ZoneScoped
    return _tile;
}

Tile::Status FfmpegTileProvider::tileStatus(const TileIndex&) {
    return _tile.status;
}

TileDepthTransform FfmpegTileProvider::depthTransform() {
    return { 0.f, 1.f };
}

void FfmpegTileProvider::update() {
    ZoneScoped

    if (_tileTexture) {
        return;
    }

    // New frame, new texture make sure it gets reset properly
    reset();

    // Check if it is time for a new frame
    double now = Time::now().j2000Seconds();
    double diff = now - _lastFrameTime;
    const bool hasNewFrame = (now > Time::convertTime(_startTime)) &&
        (now - _lastFrameTime) > _frameTime;

    if(!hasNewFrame) {
        //return; wait with this for now
    }

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

    // TODO: Need to check the format of the video and decide what formats we want to
    // support and how they relate to the GL formats

    // Convert the color format to AV_PIX_FMT_RGB24
    // Only create the conversion context once
    if (!_conversionContext) {
        _conversionContext = sws_getContext(
            _codecContext->width,
            _codecContext->height,
            _codecContext->pix_fmt,
            _codecContext->width,
            _codecContext->height,
            AV_PIX_FMT_RGB24,
            SWS_BICUBIC,
            nullptr,
            nullptr,
            nullptr
        );
    }

    // NOTE: This crashes at the moment somewhere inside the sws_scale function
    sws_scale(
        _conversionContext,
        _avFrame->data,
        _avFrame->linesize,
        0,
        _codecContext->height,
        _glFrame->data,
        _glFrame->linesize
    );

    // Create the texture
    // TODO: We should probably create a deep copy of the data that the Texture object
    // can have ownership of, otherwise the destructor crashes and can have other bugs too
    _tileTexture = std::make_unique<ghoul::opengl::Texture>(
        reinterpret_cast<char*>(_glFrame->data[0]),
        glm::uvec3(_nativeSize, 1),
        GL_TEXTURE_2D,
        ghoul::opengl::Texture::Format::RGB,
        GL_RGB
    );
    if (!_tileTexture) {
        throw ghoul::RuntimeError(fmt::format(
            "Unable to load texture for frame '{}' in video {}",
            _codecContext->frame_number, _videoFile
        ));
    }

    // Binds the texture to the tile
    _tileTexture->uploadTexture();
    _tileTexture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
    _tile = Tile{ _tileTexture.get(), std::nullopt, Tile::Status::OK };


    // TEST save a grayscale frame into a .pgm file
    // This ends up in OpenSpace\build\apps\OpenSpace
    // https://github.com/leandromoreira/ffmpeg-libav-tutorial/blob/master/0_hello_world.c
    /*char frame_filename[1024];
    snprintf(frame_filename, sizeof(frame_filename), "%s-%d.pgm", "frame", _codecContext->frame_number);
    save_gray_frame(_avFrame->data[0], _avFrame->linesize[0], _avFrame->width, _avFrame->height, frame_filename);
    */

    av_packet_unref(_packet);
}

int FfmpegTileProvider::minLevel() {
    return 1;
}

int FfmpegTileProvider::maxLevel() {
    return 1337; // unlimited
}

void FfmpegTileProvider::reset() {
    if (_videoFile.empty() || !_tileTexture) {
        return;
    }

    _tileTexture.reset();
}


float FfmpegTileProvider::noDataValueAsFloat() {
    return std::numeric_limits<float>::min();
}

void FfmpegTileProvider::internalInitialize() {
    // TODO: Currently the update function is called before this
    // function - fix that and then move constructor code here
}

FfmpegTileProvider::~FfmpegTileProvider() {
    // TODO: Check so internalDeinitialize is called after the last
    // update function and move code there
    avformat_close_input(&_formatContext);
    av_free(_avFrame);
    av_free(_glFrame);
    av_free(_packet);
    avformat_free_context(_formatContext);
}

void FfmpegTileProvider::internalDeinitialize() {

}

} // namespace openspace::globebrowsing
