/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#ifndef __OPENSPACE_MODULE_SOLARBROWSING___VIDEOPLAYER___H__
#define __OPENSPACE_MODULE_SOLARBROWSING___VIDEOPLAYER___H__

#include <modules/solarbrowsing/util/streambuffer.h>
#include <modules/solarbrowsing/rendering/renderablesolarimagery.h>

#include <mutex>

#define BUFFER_SIZE 1

namespace openspace {

// class VideoJob : public Job<SolarImageData> {
// public:
//     VideoJob(H265Decoder* decoder) {
//         _decoder(decoder), _more(0)
//     }

//     virtual void execute() final {
//         _more =_decoder->decode();
//     }
//     // Just used for decoding
//     virtual std::shared_ptr<int> product() final {
//         return std::make_shared<int>(_more);
//     }

// private:
//     int _more;
//     H265Decoder* _decoder;
// };

class VideoPlayer {
public:

    void decode() {
        while (_decoder->hasMoreFrames()) {
            int more = _decoder->decode();
            if (more == 0) {
                _decoder->giveDataToDecoder();
            }
        }
    }
    VideoPlayer(const std::string& path, VideoMetadata& vim) {
        _decoder = std::make_unique<H265Decoder>(path);
        _metadata = vim;
        _decodeThread = std::thread([this] { decode(); });
        _decodeThread.detach();
    }

    const unsigned char* popFrame() {
        // Do some decoding
        // _streamBuffer.popFinishedJob();
        // auto job = VideoJob(_decoder.get());
        // _streamBuffer.enqueueJob(job);
        // return _decoder.popFrame();
        mtx.lock();
        return _decoder->popFrame();
        mtx.unlock();
    }

    // void seek(const double& osTime) {
    // }
private:

    std::unique_ptr<H265Decoder> _decoder;
    VideoMetadata _metadata;
    std::thread _decodeThread;
    //StreamBuffer<SolarImageData> _streamBuffer;
    std::mutex mtx;
};

}
#endif // __OPENSPACE_MODULE_SOLARBROWSING___H265DECODER___H__
