/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

    struct [[codegen::Dictionary(ConvertRecFormatTask)]] Parameters {
        std::filesystem::path inputFilePath;
        std::filesystem::path outputFilePath;

        enum class DataMode {
            Ascii,
            Binary
        };
        DataMode outputMode;
    };

#include "convertrecformattask_codegen.cpp"
} // namespace

namespace openspace::interaction {

documentation::Documentation ConvertRecFormatTask::documentation() {
    return codegen::doc<Parameters>("convert_format_task");
}

ConvertRecFormatTask::ConvertRecFormatTask(const ghoul::Dictionary& dictionary) {
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _inFilePath = p.inputFilePath;
    _outFilePath = p.outputFilePath;

    switch (p.outputMode) {
        case Parameters::DataMode::Ascii:
            _dataMode = DataMode::Ascii;
            break;
        case Parameters::DataMode::Binary:
            _dataMode = DataMode::Binary;
            break;
    }

    if (!std::filesystem::is_regular_file(_inFilePath)) {
        LERROR(std::format("Failed to load session recording file: {}", _inFilePath));
    }
}

std::string ConvertRecFormatTask::description() {
    return "Convert session recording files between ASCII and Binary formats";
}

void ConvertRecFormatTask::perform(const Task::ProgressCallback&) {
    SessionRecording sessionRecording = loadSessionRecording(_inFilePath);
    saveSessionRecording(_outFilePath, sessionRecording, _dataMode);
}

} // namespace openspace::interaction
