/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#ifndef __OPENSPACE_MODULE_SOLARBROWSING___J2KCODEC___H__
#define __OPENSPACE_MODULE_SOLARBROWSING___J2KCODEC___H__

#include <memory>
#include <openjpeg.h>
#include <string>

namespace openspace {

/**
 * TODO(mnoven): A simple wrapper around openjpeg. As of 08/24/17 openjpeg has 3 major
 * bottlenecks which are listed below:
 *
 * 1. We can't perform decoding on multiple threads, this feature is not stable and
 *    results in a segmentation fault.
 * 2. We want to be able to decode directly into our buffer without having to go through
 *    the opj_image_t object.
 *    See: https://github.com/uclouvain/openjpeg/issues/837
 * 3. Decoding precison is always 32-bits integers, meaning conversion has to be done if
 *    8-bytes are preferred.
 *    See: https://github.com/uclouvain/openjpeg/issues/836
 */

struct ImageData {
    int32_t* data;
    uint32_t w;
    uint32_t h;
};
    
class J2kCodec {
public:
    static constexpr const int ALL_THREADS = 0;

    J2kCodec(bool verboseMode = false);
    ~J2kCodec();

    // Decode into a client allocated buffer
    void decodeIntoBuffer(const std::string& path, unsigned char* buffer, 
        int resolutionLevel, int numQualityLayers = 1, int x0 = -1, int y0 = -1,
        int x1 = -1, int y1 = -1, int numThreads = ALL_THREADS);

    // Experimental and not used at the moment
    void encodeAsTiles(const char* outfile, const int32_t* data, unsigned int imageWidth,
        unsigned int imageHeight, unsigned int tileWidth, unsigned int tileHeight,
        unsigned int numComps, unsigned int compPrec);

private:
    void destroy();
    void createInfileStream(std::string filename);
    void setupDecoder(int resolutionLevel, int numQualityLayers, int x0, int x1, int y0,
        int y1, int nThreads);

    opj_codec_t* _decoder = nullptr;
    opj_dparameters_t _decoderParams;
    opj_image_t* _image = nullptr;

    std::string _infileName;
    opj_stream_t* _infileStream = nullptr;
    bool _verboseMode;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SOLARBROWSING___J2KCODEC___H__
