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

#ifndef __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACEVIDEORENDERABLE___H__
#define __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACEVIDEORENDERABLE___H__

#include <openspace/rendering/screenspacerenderable.h>

#include <openspace/documentation/documentation.h>

 // FFMPEG
extern "C" {
#include <libavcodec/avcodec.h> // avcodec_alloc_context3
#include <libavformat/avformat.h> // avformat_open_input, AVFormatContext
#include <libavutil/imgutils.h> // av_image_get_buffer_size
#include <libswscale/swscale.h> // SwsContext
}

namespace openspace {

class ScreenSpaceVideoRenderable : public ScreenSpaceRenderable {
public:
    explicit ScreenSpaceVideoRenderable(const ghoul::Dictionary& dictionary);
    ~ScreenSpaceVideoRenderable() override;

    bool initialize() override;
    bool deinitialize() override;
    bool initializeGL() override;
    bool deinitializeGL() override;
    void render() override;
    void update() override;
    void reset();

    static documentation::Documentation Documentation();

private:
    glm::mat4 localRotationMatrix() override;
    void bindTexture() override;
    void readFrame();
    properties::TriggerProperty _reset;
    properties::TriggerProperty _play;
    properties::TriggerProperty _pause;
    properties::StringProperty _videoFile;

    std::unique_ptr<ghoul::opengl::Texture> _texture;
    bool _isPlaying = false;

    AVFormatContext* _formatContext = nullptr;
    AVCodecContext* _codecContext = nullptr;
    struct SwsContext* _conversionContext = nullptr;
    const AVCodec* _decoder = nullptr;
    AVFrame* _avFrame = nullptr;
    AVFrame* _glFrame = nullptr;
    int _streamIndex = -1;
    AVStream* _videoStream = nullptr;
    AVPacket* _packet = nullptr;

    bool _isInitialized = false;

    std::chrono::milliseconds _frameTime = std::chrono::milliseconds(0);
    std::chrono::system_clock::time_point _lastFrameTime;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SKYBROWSER___SCREENSPACEVIDEORENDERABLE___H__
