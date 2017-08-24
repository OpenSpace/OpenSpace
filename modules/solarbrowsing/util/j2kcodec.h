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

#ifndef J2KCODEC_H
#define J2KCODEC_H

#include "openjpeg.h"
#include <string>
#include <memory>

#define ALL_THREADS 0

namespace openspace {

/**
 * TODO(mnoven): A simple wrapper around openjpeg. As of 08/24/17 openjpeg has 3 major bottlenecks which are
 * listed below:
 *
 * 1. We can't perform decoding on multiple threads, this feature is not stable and results
 *    in a segmentation fault.
 * 2. We want to be able to decode directly into our buffer without having to go through the opj_image_t object.
 *    See: https://github.com/uclouvain/openjpeg/issues/837
 * 3. Decoding precison is always 32-bits integers, meaning conversion has to be done if 8-bytes are preferred.
 *    See: https://github.com/uclouvain/openjpeg/issues/836
 */

struct ImageData {
    int32_t* data;
    uint32_t w;
    uint32_t h;
};

class J2kCodec {
public:
    J2kCodec(bool verboseMode = false);
    ~J2kCodec();
    // Decode and return image object
    std::shared_ptr<ImageData> decode(const std::string& path, const int resolutionLevel,
                                      const int numQualityLayers = 1, const int x0 = -1,
                                      const int y0 = -1, const int x1 = -1, const int y1 = -1,
                                      const int numThreads = ALL_THREADS);

    // Decode into a client allocated buffer
    void decodeIntoBuffer(const std::string& path, unsigned char* buffer,
                          const int resolutionLevel, const int numQualityLayers = 1,
                          const int x0 = -1, const int y0 = -1, const int x1 = -1,
                          const int y1 = -1, const int numThreads = ALL_THREADS);

    // Experimental and not used at the moment
    void encodeAsTiles(const char* outfile,
                       const int32_t* data,
                       const unsigned int imageWidth,
                       const unsigned int imageHeight,
                       const unsigned int tileWidth,
                       const unsigned int tileHeight,
                       const unsigned int numComps,
                       const unsigned int compPrec);
private:
    void destroy();
    void createInfileStream(const std::string& filename);
    void setupDecoder(const int resolutionLevel, const int numQualityLayers, const int x0,
                      const int x1, const int y0, const int y1, const int numThreads);

    opj_codestream_info_v2_t* _codestreamInfo;
    opj_codec_t* _decoder;
    opj_dparameters_t _decoderParams;
    opj_image_t* _image;

    std::string _infileName;
    opj_stream_t* _infileStream;
    bool _verboseMode;
};

}

#endif // J2KCODEC_H
