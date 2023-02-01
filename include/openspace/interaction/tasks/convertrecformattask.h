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

#ifndef __OPENSPACE_CORE___CONVERTRECFORMATTASK___H__
#define __OPENSPACE_CORE___CONVERTRECFORMATTASK___H__

#include <openspace/util/task.h>
#include <openspace/interaction/sessionrecording.h>

#include <ghoul/glm.h>
#include <filesystem>
#include <string>

namespace openspace::interaction {

class ConvertRecFormatTask : public Task {
public:
    enum class ConversionDirection {
        ToAscii = 0,
        ToBinary
    };
    ConvertRecFormatTask(const ghoul::Dictionary& dictionary);
    ~ConvertRecFormatTask() override;
    std::string description() override;
    void perform(const Task::ProgressCallback& progressCallback) override;
    static documentation::Documentation documentation();
    void convert();

private:
    void convertToAscii();
    void convertToBinary();
    void determineFormatType();
    std::string addFileSuffix(const std::string& filePath, const std::string& suffix);
    std::filesystem::path _inFilePath;
    std::filesystem::path _outFilePath;
    std::ifstream _iFile;
    std::ofstream _oFile;
    SessionRecording::DataMode _fileFormatType;
    std::string _version;

    std::string _valueFunctionLua;
    SessionRecording* sessRec;
};

} // namespace openspace::interaction

#endif // __OPENSPACE_CORE___CONVERTRECFORMATTASK___H__
