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

#ifndef __OPENSPACE_MODULE_SOLARBROWSING___H265DECODER___H__
#define __OPENSPACE_MODULE_SOLARBROWSING___H265DECODER___H__

#include <modules/solarbrowsing/util/decodejob.h>
#include <iostream>
#include "de265.h"
#include <stdio.h>

#include <chrono>

typedef std::chrono::high_resolution_clock Clock;

#define CHUNK_SIZE 500000

namespace openspace {

// class VideoJob : public StreamJob<SolarImageData> {
//     virtual void execute() final {
//     }
//     virtual std::shared_ptr<SolarImageData> product() final {
//         return std::move(_solarImageData);
//     }
// }

class H265Decoder {
public:
    H265Decoder(const std::string& path) {
      initialize(path);
    };

    ~H265Decoder() {
        fclose(_fh);
        de265_free_decoder(_ctx);
    }

    bool hasMoreFrames() {
        return _isRunning;
    }

    const uint8_t* popFrame() {
        //auto t1 = Clock::now();
        //bool hasMoreWork = decode();
        //auto t2 = Clock::now();
        //std:: cerr << "Bottleneck function ? "
        //<< std::chrono::duration_cast<std::chrono::milliseconds>(t2 - t1).count()
        //<< " ms" << std::endl;
        const de265_image* img = de265_get_next_picture(_ctx);
        if (img) {
            int width  = de265_get_image_width(img,0);
            int height = de265_get_image_height(img,0);
            //std::cerr << " We got an image!!!" << width << " " << height;
            int stride; // bytes per row
            const uint8_t* yptr = de265_get_image_plane(img, 0, &stride);
            // Empty buffer, push more data
            return yptr;
        }
        return nullptr;
    }

    bool decode() {
        int more = 0;
        if (_isRunning) {
            de265_error err = de265_decode(_ctx, &more);
            // ERROR
            if (err != DE265_OK) {
                if (err == DE265_ERROR_CHECKSUM_MISMATCH) {
                    std::cerr << "Some error mismatch";
                    _isRunning = false;
                    more = 0;
                }
                // Must collect some more
                else if (err == DE265_ERROR_IMAGE_BUFFER_FULL) {
                    std::cerr << "Warning, buffer is full needs collect";
                    more = 1;
                }

                else if (err == DE265_ERROR_WAITING_FOR_INPUT_DATA) {
                    std::cerr << "Warning, decoder is empty"; 
                    more = 0;
                }
            }
            // if (!more && _isRunning) {
            //     giveDataToDecoder();
            // }
        }
        return more;
    }

    void giveDataToDecoder() {
        if (!_isRunning) {
            std::cerr << "Is not running " << std::endl;
             return;
        }
        std::cerr << "Pushing some data to decoder " << std::endl;

        de265_error err;
        uint8_t buf[CHUNK_SIZE];
        int n = fread(buf, 1, CHUNK_SIZE, _fh);
        if (n) {
            err = de265_push_data(_ctx, buf, n, 0, (void*)2);
            if (err != DE265_OK) {
                std::cerr << "Error in giving data to buffer" << std::endl;
                _isRunning = false;
                return;
            }
        }

        if (feof(_fh)) {
            std::cerr << "Breaking..";
            err = de265_flush_data(_ctx);
            _isRunning = false;
        }
    }


private:
    void initialize(const std::string path) {
        std::cerr << "Initializing decoder ..." << std::endl;
        _isRunning = true;
        de265_error err = DE265_OK;
        _ctx = de265_new_decoder();
        de265_set_parameter_bool(_ctx, DE265_DECODER_PARAM_BOOL_SEI_CHECK_HASH, false);
        de265_set_parameter_bool(_ctx, DE265_DECODER_PARAM_SUPPRESS_FAULTY_PICTURES, false);
        de265_set_parameter_bool(_ctx, DE265_DECODER_PARAM_DISABLE_DEBLOCKING, 1);
        de265_set_parameter_bool(_ctx, DE265_DECODER_PARAM_DISABLE_SAO, 1);

        de265_disable_logging();
        de265_set_verbosity(false);

        // Set highest temporal substream to decode
        de265_set_limit_TID(_ctx, 100);
        _fh = fopen(path.c_str(), "rb");
        if (_fh == NULL) {
            std::cerr << "Cannot open file: " << path << std::endl;
        }

        giveDataToDecoder();
        // int hasMoreWork = decode();
        // if (!hasMoreWork) {
        //     giveDataToDecoder();
        // }
    };

    FILE* _fh;
    bool _isRunning;
    de265_decoder_context* _ctx;

};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SOLARBROWSING___H265DECODER___H__
