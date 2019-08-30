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

namespace {
    constexpr const char* KeyConvertToAscii = "ConvertToAscii";
    constexpr const char* KeyConvertToBinary = "ConvertToBinary";
    constexpr const char* KeyInFilePath = "InputFilePath";
    constexpr const char* KeyOutFilePath = "OutputFilePath";
}

namespace openspace {

ConvertRecFormatTask::ConvertRecFormatTask(const ghoul::Dictionary& dictionary) {
    openspace::documentation::testSpecificationAndThrow(
        documentation(),
        dictionary,
        "ConvertRecFormatTask"
    );

    _inFilePath = absPath(dictionary.value<std::string>(KeyInFilePath));
    _outFilePath = absPath(dictionary.value<std::string>(KeyOutFilePath));
}

void ConvertRecFormatTask::perform(const Task::ProgressCallback& progressCallback) {

}

void ConvertRecFormatTask::convert() {
    RecordedDataMode type = formatType();

    if (type == RecordedDataMode::Ascii) {
        convertToBinary();
    }
    else if (type == RecordedDataMode::Binary) {
        convertToAscii();
    }
    else {
        //Add error output for file type not recognized
    }
}

RecordedDataMode ConvertRecFormatTask::formatType() {
    //Read file at _inFilePath

    //Read first line

    //First verify that the line starts with the valid string

    //Get last character which should be either 'A' or 'B', and return Ascii or Binary based on this.
}

void ConvertRecFormatTask::convertToAscii() {

}

void ConvertRecFormatTask::convertToBinary() {

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
