/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#ifndef __OPENSPACE_MODULE_FIELDLINESSEQUENCE___CDFTOOSFLSTASK___H__
#define __OPENSPACE_MODULE_FIELDLINESSEQUENCE___CDFTOOSFLSTASK___H__

#include <openspace/util/task.h>

#include <ghoul/glm.h>
#include <string>

namespace openspace {

class CDFtoOSFLStask : public Task {
public:
    CDFtoOSFLStask(const ghoul::Dictionary& dictionary);

    std::string description() override;
    void perform(const Task::ProgressCallback& progressCallback) override;
    static documentation::Documentation Documentation();

private:
    std::string _tracingVar;
    std::vector<std::string> _extraScalarVars;
    std::vector<std::string> _extraMagnitudeVars;
    std::string _inputPath;
    std::string _timeKernelPath = "";
    std::string _seedpointsPath;
    std::string _outputFolder;
    // Manual time offset
    float _manualTimeOffset = 0.0;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_FIELDLINESSEQUENCE___CDFTOOSFLSTASK___H__
