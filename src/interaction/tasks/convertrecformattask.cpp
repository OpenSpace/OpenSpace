/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#include <openspace/interaction/tasks/convertrecformattask.h>
#include <openspace/interaction/sessionrecording.h>
#include <openspace/documentation/verifier.h>

#include <openspace/engine/globals.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <iomanip>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const char* _loggerCat = "ConvertRecFormatTask";
} // namespace

namespace openspace {

ConvertRecFormatTask::ConvertRecFormatTask(const ghoul::Dictionary& dictionary) {
    openspace::documentation::testSpecificationAndThrow(
        documentation(),
        dictionary,
        "ConvertRecFormatTask"
    );

    _inFilePath = absPath(dictionary.value<std::string>(KeyInFilePath));
    _outFilePath = absPath(dictionary.value<std::string>(KeyOutFilePath));

    ghoul_assert(FileSys.fileExists(_inFilePath), "The filename must exist");
    if (!FileSys.fileExists(_inFilePath)) {
        LERROR(fmt::format("Failed to load session recording file: {}", _inFilePath));
        //throw ghoul::FileNotFoundError(_inFilePath);
    }

    _iFile.exceptions(std::ofstream::failbit | std::ofstream::badbit);
    _iFile.open(_inFilePath);
}

ConvertRecFormatTask::~ConvertRecFormatTask() {
    _iFile.close();
    _oFile.close();
}

void ConvertRecFormatTask::perform(const Task::ProgressCallback& progressCallback) {

}

void ConvertRecFormatTask::convert() {
    interaction::RecordedDataMode type = formatType();

    if (type == interaction::RecordedDataMode::Ascii) {
        convertToBinary();
    }
    else if (type == interaction::RecordedDataMode::Binary) {
        convertToAscii();
    }
    else {
        //Add error output for file type not recognized
        LERROR("Session recording file unrecognized format type.");
    }
}

RecordedDataMode ConvertRecFormatTask::formatType() {
    const std::string expectedHeader = "OpenSpace_record/playback";
    std::string line;
    //Get first line, which is ASCII regardless of format
    std::getline(_iFile, line);

    if (line.substr(0, interaction::FileHeaderTitle.length())
        != interaction::FileHeaderTitle)
    {
        LERROR(fmt::format("Session recording file {} does not have expected header.", _inFilePath));
        return RecordedDataMode::Binary + 1;
    }
    else {
        if (line.back() == DataFormatAsciiTag) {
            return RecordedDataMode::Ascii;
        }
        else if (line.back() == DataFormatBinaryTag) {
            return RecordedDataMode::Binary;
        }
        else {
            return RecordedDataMode::Binary + 1;
        }
    }
}

void ConvertRecFormatTask::convertToAscii() {
    _outFilePath = addFileSuffix(_inFilePath, "_ascii");
}

void ConvertRecFormatTask::convertToBinary() {
    _outFilePath = addFileSuffix(_inFilePath, "_binary");
}

std::string ConvertRecFormatTask::addFileSuffix(const std::string& filePath,
                                                const std::string& suffix)
{
    size_t lastdot = filename.find_last_of(".");
    std::string extension = filePath.substr(0, lastdot);
    if (lastdot == std::string::npos) {
        return filePath + suffix;
    }
    else {
        return filePath.substr(0, lastdot) + suffix + extension;
    }
}

documentation::Documentation ConvertRecFormatTask::documentation() {
    using namespace documentation;
    return {
        "ConvertRecFormatTask",
        "convert_format_task",
        {
            {
                "Type",
                new StringEqualVerifier("ConvertRecFormatTask"),
                Optional::No,
                "The type of this task",
            },
            {
                KeyInFilePath,
                new StringAnnotationVerifier("A valid filename to convert"),
                Optional::No,
                "The filename to convert to the opposite format.",
            },
            {
                KeyOutFilePath,
                new StringAnnotationVerifier("A valid output filename"),
                Optional::No,
                "The filename containing the converted result.",
            },
        },
    };
}
