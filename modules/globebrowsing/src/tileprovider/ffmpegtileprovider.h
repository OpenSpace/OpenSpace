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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___TILEPROVIDER__FFMPEGTILEPROVIDER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___TILEPROVIDER__FFMPEGTILEPROVIDER___H__

#include <modules/globebrowsing/src/tileprovider/tileprovider.h>

#include <ghoul/glm.h>

// FFMPEG
#include <libavcodec/avcodec.h> // avcodec_alloc_context3
#include <libavformat/avformat.h> // avformat_open_input, AVFormatContext
#include <libavutil/imgutils.h> // av_image_get_buffer_size

namespace openspace { struct Documentation; }

namespace openspace::globebrowsing {

class FfmpegTileProvider : public TileProvider {
public:
    FfmpegTileProvider(const ghoul::Dictionary& dictionary);

    Tile tile(const TileIndex& tileIndex) override final;
    Tile::Status tileStatus(const TileIndex& index) override final;
    TileDepthTransform depthTransform() override final;
    void update() override final;
    void reset() override final;
    int minLevel() override final;
    int maxLevel() override final;
    float noDataValueAsFloat() override final;

    static documentation::Documentation Documentation();

private:
    std::filesystem::path _videoFile;
    glm::ivec2 _nativeSize;
    std::chrono::microseconds _frameTime;
    std::chrono::steady_clock::time_point _lastFrameTime;

    AVFormatContext* _formatContext = nullptr;
    int _streamIndex = -1;
    AVStream* _videoStream = nullptr;
    AVCodecContext* _codecContext = nullptr;
    const AVCodec* _decoder = nullptr;
    AVFrame* _avFrame = nullptr;
    AVFrame* _glFrame = nullptr;
    AVPacket* _packet = nullptr;

    void internalInitialize() override final;
    void internalDeinitialize() override final;
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___TILEPROVIDER__FFMPEGTILEPROVIDER___H__
