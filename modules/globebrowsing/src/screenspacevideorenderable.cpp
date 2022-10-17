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

#include <modules/globebrowsing/src/screenspacevideorenderable.h>

#include <ghoul/filesystem/filesystem.h>

namespace {
    constexpr std::string_view _loggerCat = "ScreenSpaceVideoRenderable";

    constexpr openspace::properties::Property::PropertyInfo FileInfo = {
        "File",
        "File",
        "The file path that is used for this video provider. The file must point to a "
        "video that is then loaded and used for all tiles"
    };

    constexpr openspace::properties::Property::PropertyInfo ResetInfo = {
        "Reset",
        "Reset",
        "Reset the video."
    };

    struct [[codegen::Dictionary(ScreenSpaceVideoRenderable)]] Parameters {
        // [[codegen::verbatim(FileInfo.description)]]
        std::filesystem::path file;
    };

#include "screenspacevideorenderable_codegen.cpp"

} // namespace

namespace openspace {

documentation::Documentation ScreenSpaceVideoRenderable::Documentation() {
    return codegen::doc<Parameters>("skybrowser_screenspacevideorenderable");
}

ScreenSpaceVideoRenderable::ScreenSpaceVideoRenderable(const ghoul::Dictionary& dictionary)
    : ScreenSpaceRenderable(dictionary)
    , _videoFile(FileInfo, "")
    , _reset(ResetInfo)

{
    _identifier = makeUniqueIdentifier(_identifier);

    // Handle target dimension property
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _videoFile = absPath(p.file).string();

    _reset.onChange([this]() {
        this->reset();
        });

    addProperty(_videoFile);
    addProperty(_reset);
}

ScreenSpaceVideoRenderable::~ScreenSpaceVideoRenderable() {

}

bool ScreenSpaceVideoRenderable::initialize() {
    return true;
}

bool ScreenSpaceVideoRenderable::deinitialize() {
    return true;
}

void ScreenSpaceVideoRenderable::reset() {
    deinitializeGL();
    initializeGL();
}

bool ScreenSpaceVideoRenderable::initializeGL() {
    ScreenSpaceRenderable::initializeGL();

    std::string path = _videoFile;

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

    // Update times
    _lastFrameTime = std::chrono::system_clock::now();

    // Create the texture
    _texture = std::make_unique<ghoul::opengl::Texture>(
        glm::uvec3(_codecContext->width, _codecContext->height, 1),
        GL_TEXTURE_2D,
        ghoul::opengl::Texture::Format::RGB,
        GL_RGB,
        GL_UNSIGNED_BYTE,
        ghoul::opengl::Texture::FilterMode::Linear,
        ghoul::opengl::Texture::WrappingMode::Repeat,
        ghoul::opengl::Texture::AllocateData::No,
        ghoul::opengl::Texture::TakeOwnership::No
    );

    _isInitialized = true;
    _objectSize = { _codecContext->width, _codecContext->height };
    return true;
}

bool ScreenSpaceVideoRenderable::deinitializeGL() {
    avformat_close_input(&_formatContext);
    av_free(_avFrame);
    av_free(_glFrame);
    av_free(_packet);
    avformat_free_context(_formatContext);

    ScreenSpaceRenderable::deinitializeGL();
    return true;
}

void ScreenSpaceVideoRenderable::render() {
    ScreenSpaceRenderable::render();
}

void ScreenSpaceVideoRenderable::update() {

    ZoneScoped

    if (!_isInitialized) {
        return;
    }
    // Check if it is time for a new frame
    std::chrono::system_clock::time_point now = std::chrono::system_clock::now();
    std::chrono::system_clock::duration diff = now - _lastFrameTime;

    const bool hasNewFrame = diff > _frameTime;

    if (!hasNewFrame) {
        return;
    }
    
    // Read frame
    while (true) {
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
    // TODO: support higher resolutions
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
    // Successfully collected a frame
    av_packet_unref(_packet);

    _texture->setPixelData(
        reinterpret_cast<char*>(_glFrame->data[0]),
        ghoul::opengl::Texture::TakeOwnership::No
    );
    _texture->uploadTexture();

    ScreenSpaceRenderable::update();
}

void ScreenSpaceVideoRenderable::bindTexture() {
    _texture->bind();
}
}
