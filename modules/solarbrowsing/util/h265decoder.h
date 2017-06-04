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

#define BUFFER_SIZE 40000

namespace openspace {

class H265Decoder {
public:
    H265Decoder(const std::string& path)
    {
      initialize(path);
    };

    ~H265Decoder() {
        fclose(_fh);
        de265_free_decoder(_ctx);
    }

    bool hasMoreFrames() {
        return _isRunning;
    };
    void setBufferSize(const unsigned int size);
    void popFrame() {
        const de265_image* img = de265_get_next_picture(_ctx);
        if (img) {
            std::cout << "And we got an image!!!";
        }
        // Do some decoding
        bool hasMoreWork = decode();
        // Empty buffer, push more data
        if (!hasMoreWork && _isRunning) {
            giveDataToDecoder();
        }
        return img;
    };
private:
    void giveDataToDecoder() {
        if (!_isRunning) {
            return;
        }

        de265_error err;
        uint8_t buf[BUFFER_SIZE];
        int n = fread(buf, 1, BUFFER_SIZE, _fh);
        if (n) {
            err = de265_push_data(_ctx, buf, n, 0   , (void*)2);
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

    int decode() {
        int more = 1;
        de265_error err = de265_decode(_ctx, &more);
        // ERROR
        if (err != DE265_OK) {
            if (err == DE265_ERROR_CHECKSUM_MISMATCH) {
                _isRunning = false;
            }
        }
        return more;
    }

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
        decode();
    };
    FILE* _fh;
    bool _isRunning;
    de265_decoder_context* _ctx;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SOLARBROWSING___H265DECODER___H__
