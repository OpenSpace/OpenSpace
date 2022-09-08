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
}

Tile FfmpegTileProvider::tile(const TileIndex& tileIndex) {
    ZoneScoped
    return Tile();
}

Tile::Status FfmpegTileProvider::tileStatus(const TileIndex&) {
    return Tile::Status::OK;
}

TileDepthTransform FfmpegTileProvider::depthTransform() {
    return { 0.f, 1.f };
}

void FfmpegTileProvider::update() {
    ZoneScoped

    // Check if it is time for a new frame
    double now = Time::now().j2000Seconds();
    double diff = now - _lastFrameTime;
    const bool hasNewFrame = (now > Time::convertTime(_startTime)) &&
        (now - _lastFrameTime) > _frameTime;

    if (!hasNewFrame) {
        return;
    }

    // Read frame
    do {
        if (!_formatContext || !_packet) {
            break;
        }
        int result = av_read_frame(_formatContext, _packet);
        if (result < 0) {
            av_packet_unref(_packet);
            break;
        }

        // Does this packet belong to this video stream?
        if (_packet->stream_index != _streamIndex) {
            continue;
        }

        // Send packet to the decoder
        result = avcodec_send_packet(_codecContext, _packet);
        if (result < 0) {
            LERROR(fmt::format("Sending packet failed with {}", result));
            av_packet_unref(_packet);
            break;
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
            break;
        }

        // We have a new full frame!
        _lastFrameTime = now;

       // @TODO Do something with it!
        _glFrame = av_frame_alloc();
        int sz = av_image_get_buffer_size(
            AV_PIX_FMT_RGB24,
            _codecContext->width,
            _codecContext->height,
            1
        );
        uint8_t* internalBuffer = reinterpret_cast<uint8_t*>(av_malloc(sz * sizeof(uint8_t)));
        av_image_fill_arrays(
            _glFrame->data,
            _glFrame->linesize,
            internalBuffer,
            AV_PIX_FMT_RGB24,
            _codecContext->width,
            _codecContext->height,
            1
        );
        _packet = av_packet_alloc();

        glGenTextures(1, &_frameTexture);
        glBindTexture(GL_TEXTURE_2D, _frameTexture);
        glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        glTexImage2D(
            GL_TEXTURE_2D,
            0,
            GL_RGB,
            _codecContext->width,
            _codecContext->height,
            0,
            GL_RGB,
            GL_UNSIGNED_BYTE,
            nullptr
        );

        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        glPushAttrib(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        glEnable(GL_TEXTURE_2D);
        glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL);

        glBindTexture(GL_TEXTURE_2D, _frameTexture);

        glFlush();
        glDisable(GL_TEXTURE_2D);

        ///FBO
        glGenFramebuffers(1, &_FBO);
        glBindFramebuffer(GL_FRAMEBUFFER, _FBO);
        //Attach 2D texture to this FBO
        glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, _frameTexture, 0);

        // Print to file
        FILE* output_image;
        int     output_width, output_height;

        output_width = _codecContext->width,
        output_height = _codecContext->height;

        /// READ THE PIXELS VALUES from FBO AND SAVE TO A .PPM FILE
        int             i, j, k;
        unsigned char* pixels = (unsigned char*)malloc(output_width * output_height * 3);

        /// READ THE CONTENT FROM THE FBO
        glReadBuffer(GL_COLOR_ATTACHMENT0);
        glReadPixels(0, 0, output_width, output_height, GL_RGB, GL_UNSIGNED_BYTE, pixels);

        output_image = fopen("C:\\Users\\ylvaselling\\Documents\\Work\\Dataset\\output.ppm", "wt");
        fprintf(output_image, "P3\n");
        fprintf(output_image, "# Created by Ricao\n");
        fprintf(output_image, "%d %d\n", output_width, output_height);
        fprintf(output_image, "255\n");
        k = 0;
        for (i = 0; i < output_width; i++)
        {
            for (j = 0; j < output_height; j++)
            {
                fprintf(output_image, "%u %u %u ", (unsigned int)pixels[k], (unsigned int)pixels[k + 1],
                    (unsigned int)pixels[k + 2]);
                k = k + 3;
            }
            fprintf(output_image, "\n");
        }
        free(pixels);


        break;

        av_packet_unref(_packet);
    } while (true);
}

int FfmpegTileProvider::minLevel() {
    return 1;
}

int FfmpegTileProvider::maxLevel() {
    return 1337; // unlimited
}

void FfmpegTileProvider::reset() {}


float FfmpegTileProvider::noDataValueAsFloat() {
    return std::numeric_limits<float>::min();
}

void FfmpegTileProvider::internalInitialize() {
    std::string path = absPath(_videoFile).string();
    int result;

    // Open the video
    result = avformat_open_input(
        &_formatContext,
        path.c_str(),
        nullptr,
        nullptr
    );
    if (result < 0) {
        LERROR(fmt::format("Failed to open input for video file {}", _videoFile));
        return;
    }

    // Get stream info
    if (avformat_find_stream_info(_formatContext, nullptr) < 0) {
        LERROR(fmt::format("Failed to get stream info for {}", _videoFile));
        return;
    }

    // Debug
    //av_dump_format(_formatContext, 0, path.c_str(), 0);

    // Find the stream with the video (there anc also be audio and data streams)
    for (unsigned int i = 0; i < _formatContext->nb_streams; ++i) {
        AVMediaType codec = _formatContext->streams[i]->codecpar->codec_type;
        if (codec == AVMEDIA_TYPE_VIDEO) {
            _streamIndex = i;
            break;
        }
    }
    if (_streamIndex == -1) {
        LERROR(fmt::format("Failed to find video stream for {}", _videoFile));
        return;
    }

    _videoStream = _formatContext->streams[_streamIndex];
    _codecContext = avcodec_alloc_context3(nullptr);
    result = avcodec_parameters_to_context(
        _codecContext,
        _videoStream->codecpar
    );
    if (result) {
        LERROR(fmt::format("Failed to create codec context for {}", _videoFile));
        return;
    }

    // Get the size of the video
    _nativeSize = glm::ivec2(_codecContext->width, _codecContext->height);

    // Get the decoder
    _decoder = avcodec_find_decoder(_codecContext->codec_id);
    if (!_decoder) {
        LERROR(fmt::format("Failed to find decoder for {}", _videoFile));
        return;
    }

    // Open the decoder
    result = avcodec_open2(_codecContext, _decoder, nullptr);
    if (result < 0) {
        LERROR(fmt::format("Failed to open codec for {}", _videoFile));
        return;
    }

    // Allocate the video frames
    _avFrame = av_frame_alloc();
    _glFrame = av_frame_alloc();
    int bufferSize = av_image_get_buffer_size(
        AV_PIX_FMT_RGB24,
        _codecContext->width,
        _codecContext->height,
        1
    );
    uint8_t* internalBuffer =
        reinterpret_cast<uint8_t*>(av_malloc(bufferSize * sizeof(uint8_t)));
    result = av_image_fill_arrays(
        _glFrame->data,
        _glFrame->linesize,
        internalBuffer,
        AV_PIX_FMT_RGB24,
        _codecContext->width,
        _codecContext->height,
        1
    );
    if (result < 0) {
        LERROR(fmt::format("Failed to fill buffer data for video {}", _videoFile));
        return;
    }
    // Allocate packet
    _packet = av_packet_alloc();

    // Read the first frame to get the framerate of the video
    if (_formatContext && _codecContext && _packet) {
        av_read_frame(_formatContext, _packet);
        avcodec_send_packet(_codecContext, _packet);

        _frameTime = av_q2d(_codecContext->time_base) * _codecContext->ticks_per_frame;
    }
    else {
        LERROR(fmt::format("Error loading video {}", path));
    }

    _lastFrameTime = std::max(Time::convertTime(_startTime), Time::now().j2000Seconds());

}

void FfmpegTileProvider::internalDeinitialize() {
    avformat_close_input(&_formatContext);
    av_free(_avFrame);
    av_free(_glFrame);
    av_free(_packet);
    avformat_free_context(_formatContext);
}

} // namespace openspace::globebrowsing
