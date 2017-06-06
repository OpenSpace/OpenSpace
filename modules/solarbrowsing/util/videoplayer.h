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
#include <modules/solarbrowsing/util/h265decoder.h>

#include <mutex>

namespace openspace {

#define CHUNK_SIZE 500000

class VideoPlayer {
public:
    // Keep decoding thread running
    void decode() {
        while (_decoder->hasMoreFrames() && isRunning) {
            int more = _decoder->decode();
            if (more == 0) {
                //_decoder->giveDataToDecoder();
                pushDataToDecoder();
            }
        }
    }

    void pushDataToDecoder() {
        uint8_t buf[CHUNK_SIZE];

        int elementsRead = fread(buf, 1, CHUNK_SIZE, _fh);
        if (elementsRead) {
            bool ok = _decoder->pushData(buf, elementsRead);
            if (!ok) {
                std::cerr << "Couldn't give data to decoder";
                isRunning = false;
            }
        }
        if (feof(_fh)) {
            std::cerr << "End of stream..";
            _decoder->flush();
        }
    }

    void start(const double& osTime) {
        //startByte = findByteFromosTime();
    }

    void stop() {
        isRunning = false;
        _decodeThread.join();
        _decoder->reset();
    }

    void clear() {
        fclose(_fh);
    }

    VideoPlayer() {
        _decoder = std::make_unique<H265Decoder>();
    }

    void playSign(int sign) {
        clockwisePush = sign;
        backwardsCount = 1;
       // isRunning = false;
       // _decoder->reset();
       // run();
       // _decoder->reset();
    }

    void run() {
        isRunning = true;
        _decodeThread = std::thread([=] { decode(); });
    }

    void initialize(VideoMetadata& metadata) {
        _metadata = metadata;
        const std::string& path = _metadata.path;

        _fh = fopen(path.c_str(), "rb");
        if (_fh == NULL) {
            std::cerr << "Cannot open file: " << path << std::endl;
        }
        pushDataToDecoder();
        //_decodeThread.detach();
    }

    const unsigned char* popFrame() {
        return _decoder->popFrame();
    }

private:

    int clockwisePush = 1;
    std::unique_ptr<H265Decoder> _decoder;
    VideoMetadata _metadata;
    std::thread _decodeThread;
    std::atomic<bool> isRunning;
    int backwardsCount;
    //StreamBuffer<SolarImageData> _streamBuffer;
    //std::mutex mtx;
    FILE* _fh;
};

}
#endif // __OPENSPACE_MODULE_SOLARBROWSING___H265DECODER___H__
