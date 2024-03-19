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

#include <openspace/interaction/tasks/convertrecfileversiontask.h>
#include <openspace/interaction/sessionrecording.h>
#include <openspace/documentation/verifier.h>

#include <openspace/engine/globals.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <filesystem>
#include <iomanip>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr std::string_view _loggerCat = "ConvertRecFileVersionTask";

    constexpr std::string_view KeyInFilePath = "InputFilePath";
} // namespace

namespace openspace::interaction {

ConvertRecFileVersionTask::ConvertRecFileVersionTask(const ghoul::Dictionary& dictionary)
{
    openspace::documentation::testSpecificationAndThrow(
        documentation(),
        dictionary,
        "ConvertRecFileVersionTask"
    );

    _inFilename = dictionary.value<std::string>(KeyInFilePath);
    _inFilePath = absPath(_inFilename);

    ghoul_assert(std::filesystem::is_regular_file(_inFilePath), "The file must exist");
    if (!std::filesystem::is_regular_file(_inFilePath)) {
        LERROR(fmt::format("Failed to load session recording file: {}", _inFilePath));
    }
    else {
        sessRec = new SessionRecording(false);
    }
}

ConvertRecFileVersionTask::~ConvertRecFileVersionTask() {
    delete sessRec;
}

std::string ConvertRecFileVersionTask::description() {
    std::string description = fmt::format(
        "Convert file format of session recording file '{}' to current version",
        _inFilePath
    );
    return description;
}

void ConvertRecFileVersionTask::perform(const Task::ProgressCallback&) {
    convert();
}

void ConvertRecFileVersionTask::convert() {
    const bool hasBinaryFileExtension = SessionRecording::hasFileExtension(
        _inFilename,
        SessionRecording::FileExtensionBinary
    );
    const bool hasAsciiFileExtension = SessionRecording::hasFileExtension(
        _inFilename,
        SessionRecording::FileExtensionAscii
    );
    if (!hasBinaryFileExtension && !hasAsciiFileExtension) {
        LERROR(fmt::format(
            "Input filename does not have expected '{}' or '{}' extension",
            SessionRecording::FileExtensionBinary, SessionRecording::FileExtensionAscii
        ));
        return;
    }
    sessRec->convertFileRelativePath(_inFilename);
}

documentation::Documentation ConvertRecFileVersionTask::documentation() {
    using namespace documentation;
    return {
        "ConvertRecFileVersionTask",
        "convert_file_version_task",
        "",
        {
            {
                "InputFilePath",
                new StringAnnotationVerifier("A valid filename to convert"),
                Optional::No,
                "The filename to update to the current file format",
            },
        },
    };
}

} // namespace openspace::interaction
