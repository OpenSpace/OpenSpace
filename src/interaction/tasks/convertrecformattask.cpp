/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
#include <ghoul/misc/stringhelper.h>
#include <filesystem>
#include <iomanip>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr std::string_view _loggerCat = "ConvertRecFormatTask";

    constexpr std::string_view KeyInFilePath = "InputFilePath";
    constexpr std::string_view KeyOutFilePath = "OutputFilePath";
} // namespace

namespace openspace::interaction {

ConvertRecFormatTask::ConvertRecFormatTask(const ghoul::Dictionary& dictionary) {
    openspace::documentation::testSpecificationAndThrow(
        documentation(),
        dictionary,
        "ConvertRecFormatTask"
    );

    _inFilePath = absPath(dictionary.value<std::string>(KeyInFilePath));
    _outFilePath = absPath(dictionary.value<std::string>(KeyOutFilePath));

    ghoul_assert(std::filesystem::is_regular_file(_inFilePath), "The file must exist");
    if (!std::filesystem::is_regular_file(_inFilePath)) {
        LERROR(std::format("Failed to load session recording file: {}", _inFilePath));
    }
    else {
        //_iFile.open(_inFilePath, std::ifstream::in | std::ifstream::binary);
        auto [_fileFormatType, _version] = determineFormatTypeAndVersion(_inFilePath);
        //sessRec = new SessionRecording(false);
    }
}

ConvertRecFormatTask::~ConvertRecFormatTask() {
    //_iFile.close();
    //_oFile.close();
    //delete sessRec;
}

std::string ConvertRecFormatTask::description() {
    std::string description =
        std::format("Convert session recording file '{}'", _inFilePath);
    if (_fileFormatType == SessionRecording::DataMode::Ascii) {
        description += "(ascii format) ";
    }
    else if (_fileFormatType == SessionRecording::DataMode::Binary) {
        description += "(binary format) ";
    }
    else {
        description += "(UNKNOWN format) ";
    }
    description += std::format("conversion to file '{}'", _outFilePath);
    return description;
}

void ConvertRecFormatTask::perform(const Task::ProgressCallback&) {
    convertTypes(_fileFormatType, _inFilePath, _outFilePath, _version);
}

documentation::Documentation ConvertRecFormatTask::documentation() {
    using namespace documentation;
    return {
        "ConvertRecFormatTask",
        "convert_format_task",
        "",
        {
            {
                "InputFilePath",
                new StringAnnotationVerifier("A valid filename to convert"),
                Optional::No,
                "The filename to convert to the opposite format",
            },
            {
                "OutputFilePath",
                new StringAnnotationVerifier("A valid output filename"),
                Optional::No,
                "The filename containing the converted result",
            },
        },
    };
}

} // namespace openspace::interaction
