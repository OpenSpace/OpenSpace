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
#ifndef __OPENSPACE_MODULE_SOLARBROWSING___DECODEJOB___H__
#define __OPENSPACE_MODULE_SOLARBROWSING___DECODEJOB___H__

#include <modules/solarbrowsing/util/streambuffer.h>

namespace openspace {

struct SolarImageData {
    unsigned char* data;
    std::shared_ptr<ImageMetadata> im;
    double timeObserved;
};

struct DecodeData {
    std::shared_ptr<ImageMetadata> im;
    unsigned int resolutionLevel;
    double timeObserved;
    bool verboseMode;
};

class DecodeJob : public StreamJob<SolarImageData> {
public:
    DecodeJob(unsigned char* data, const DecodeData& decodeData, const std::string& id)
        : StreamJob(id), _decodeData(decodeData), _data(data)
    { }

    virtual void execute() final {
        SolarImageData imd {_data,
                            _decodeData.im,
                            _decodeData.timeObserved};
        SimpleJ2kCodec j2c(_decodeData.verboseMode);
        j2c.DecodeIntoBuffer(imd.im->filename, imd.data, _decodeData.resolutionLevel);
        _solarImageData = std::make_shared<SolarImageData>(imd);
    }

    virtual std::shared_ptr<SolarImageData> product() final {
        return std::move(_solarImageData);
    }

private:
    std::shared_ptr<SolarImageData> _solarImageData;
    DecodeData _decodeData;
    unsigned char* _data;
};
} // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___DECODEJOB___H__
