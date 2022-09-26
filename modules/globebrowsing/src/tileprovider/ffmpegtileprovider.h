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
extern "C" {
#include <libavcodec/avcodec.h> // avcodec_alloc_context3
#include <libavformat/avformat.h> // avformat_open_input, AVFormatContext
#include <libavutil/imgutils.h> // av_image_get_buffer_size
#include <libswscale/swscale.h> // SwsContext
}

namespace openspace { struct Documentation; }

namespace openspace::globebrowsing {

class FfmpegTileProvider : public TileProvider {
public:
    FfmpegTileProvider(const ghoul::Dictionary& dictionary);
    ~FfmpegTileProvider();

    Tile tile(const TileIndex& tileIndex) override final;
    Tile::Status tileStatus(const TileIndex& tileIndex) override final;
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
    double _frameTime = -1.0;   // Seconds per frame
    double _lastFrameTime;      // The in game time of the last frame in J2000 seconds
    std::string _startTime;
    bool _tileIsReady = false;

    AVFormatContext* _formatContext = nullptr;
    AVCodecContext* _codecContext = nullptr;
    struct SwsContext* _conversionContext = nullptr;
    const AVCodec* _decoder = nullptr;
    AVFrame* _avFrame = nullptr;
    AVFrame* _glFrame = nullptr;
    int _streamIndex = -1;
    AVStream* _videoStream = nullptr;
    AVPacket* _packet = nullptr;

    std::unique_ptr<ghoul::opengl::Texture> _tileTexture = nullptr;
    GLubyte* _tilePixels = nullptr;

    void internalInitialize() override final;
    void internalDeinitialize() override final;
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___TILEPROVIDER__FFMPEGTILEPROVIDER___H__
