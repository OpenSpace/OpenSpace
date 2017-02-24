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

#ifndef __OPENSPACE_APP_DATACONVERTER___MILKYWAYCONVERSIONTASK___H__
#define __OPENSPACE_APP_DATACONVERTER___MILKYWAYCONVERSIONTASK___H__

#include <apps/DataConverter/conversiontask.h>
#include <string>
#include <ghoul/glm.h>
#include <functional>
#include <modules/volume/textureslicevolumereader.h>
#include <modules/volume/rawvolumewriter.h>


namespace openspace {
namespace dataconverter {

/**
 * Converts a set of exr image slices to a raw volume
 * with floating point RGBA data (32 bit per channel).
 */
class MilkyWayConversionTask : public ConversionTask {
public:
    MilkyWayConversionTask(const std::string& inFilenamePrefix,
                           const std::string& inFilenameSuffix,
                           size_t inFirstIndex,
                           size_t inNSlices, 
                           const std::string& outFilename,
                           const glm::ivec3& outDimensions);
    
    void perform(const std::function<void(float)>& onProgress) override;
private:
    std::string _inFilenamePrefix;
    std::string _inFilenameSuffix;
    size_t _inFirstIndex;
    size_t _inNSlices;
    std::string _outFilename;
    glm::ivec3 _outDimensions;
};

} // namespace dataconverter
} // namespace openspace

#endif // __OPENSPACE_APP_DATACONVERTER___MILKYWAYCONVERSIONTASK___H__
