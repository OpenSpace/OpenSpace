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

namespace openspace::interaction {

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
    }
    else {
        _iFile.open(_inFilePath, std::ifstream::in);
        determineFormatType();
    }
}

ConvertRecFormatTask::~ConvertRecFormatTask() {
    _iFile.close();
    _oFile.close();
}

std::string ConvertRecFormatTask::description() {
    std::string description = "Convert session recording file '" + _inFilePath + "' ";
    if (_fileFormatType == SessionRecordingDataMode::Ascii) {
        description += "(ascii format) ";
    }
    else if (_fileFormatType == SessionRecordingDataMode::Binary) {
        description += "(binary format) ";
    }
    else {
        description += "(UNKNOWN format) ";
    }
    description += "conversion to file '" + _outFilePath + "'.";
    return description;
}

void ConvertRecFormatTask::perform(const Task::ProgressCallback& progressCallback) {
    convert();
}

void ConvertRecFormatTask::convert() {
    std::string expectedFileExtension_in, expectedFileExtension_out;
    std::string currentFormat;
    if (_fileFormatType == SessionRecordingDataMode::Binary) {
        currentFormat = "binary";
        expectedFileExtension_in = SessionRecordingFileExtensionBinary;
        expectedFileExtension_out = SessionRecordingFileExtensionAscii;
    }
    else if (_fileFormatType == SessionRecordingDataMode::Ascii) {
        currentFormat = "ascii";
        expectedFileExtension_in = SessionRecordingFileExtensionAscii;
        expectedFileExtension_out = SessionRecordingFileExtensionBinary;
    }

    if (!SessionRecording::hasFileExtension(_inFilePath, expectedFileExtension_in)) {
        LWARNING(fmt::format(
            "Input filename doesn't have expected {} "
            "format file extension",
            currentFormat)
        );
    }
    if (SessionRecording::hasFileExtension(_outFilePath, expectedFileExtension_in)) {
        LERROR(fmt::format(
            "Output filename has {} file extension, but is conversion from {}",
            currentFormat,
            currentFormat)
        );
        return;
    }
    else if (!SessionRecording::hasFileExtension(_outFilePath, expectedFileExtension_out)) {
        _outFilePath += expectedFileExtension_out;
    }

    if (_fileFormatType == SessionRecordingDataMode::Ascii) {
        _oFile.open(_outFilePath);
    }
    else if (_fileFormatType == SessionRecordingDataMode::Binary) {
        _oFile.open(_outFilePath, std::ios::binary);
    }
    _oFile.write(SessionRecordingFileHeaderTitle.c_str(),
        SessionRecordingFileHeaderTitle.length());
    _oFile.write(SessionRecording::FileHeaderVersion,
        SessionRecording::FileHeaderVersionLength);
    _oFile.close();

    if (_fileFormatType == SessionRecordingDataMode::Ascii) {
        convertToBinary();
    }
    else if (_fileFormatType == SessionRecordingDataMode::Binary) {
        convertToAscii();
    }
    else {
        //Add error output for file type not recognized
        LERROR("Session recording file unrecognized format type.");
    }
}

void ConvertRecFormatTask::determineFormatType() {
    _fileFormatType = SessionRecordingDataMode::Unknown;
    std::string line;

    line = SessionRecording::readHeaderElement(_iFile,
        SessionRecordingFileHeaderTitle.length());

    if (line.substr(0, SessionRecordingFileHeaderTitle.length())
        != SessionRecordingFileHeaderTitle)
    {
        LERROR(fmt::format("Session recording file {} does not have expected header.",
            _inFilePath));
    }
    else {
        //Read version string and throw it away (and also line feed character at end)
        SessionRecording::readHeaderElement(_iFile,
            SessionRecording::FileHeaderVersionLength);
        line = SessionRecording::readHeaderElement(_iFile, 1);
        SessionRecording::readHeaderElement(_iFile, 1);

        if (line.at(0) == SessionRecording::DataFormatAsciiTag) {
            _fileFormatType = SessionRecordingDataMode::Ascii;
        }
        else if (line.at(0) == SessionRecording::DataFormatBinaryTag) {
            _fileFormatType = SessionRecordingDataMode::Binary;
        }
    }
}

void ConvertRecFormatTask::convertToAscii() {
    SessionRecording::timestamps times;
    datamessagestructures::CameraKeyframe ckf;
    datamessagestructures::TimeKeyframe   tkf;
    datamessagestructures::ScriptMessage  skf;
    int lineNum = 1;
    unsigned char frameType;
    _oFile.open(_outFilePath, std::ifstream::app);
    char tmpType = SessionRecording::DataFormatAsciiTag;
    _oFile.write(&tmpType, 1);
    _oFile.write("\n", 1);

    bool fileReadOk = true;
    while (fileReadOk) {
        frameType = readFromPlayback<unsigned char>(_iFile);
        // Check if have reached EOF
        if (!_iFile) {
            LINFO(fmt::format(
                "Finished converting {} entries from file {}",
                lineNum - 1, _inFilePath
            ));
            fileReadOk = false;
            break;
        }

        std::stringstream keyframeLine = std::stringstream();
        keyframeLine.str(std::string());
        if (frameType == SessionRecordingHeaderCameraBinary) {
            SessionRecording::readCameraKeyframeBinary(times, ckf, _iFile, lineNum);
            SessionRecording::saveHeaderAscii(times, SessionRecordingHeaderCameraAscii,
                keyframeLine);
            ckf.write(keyframeLine);
        }
        else if (frameType == SessionRecordingHeaderTimeBinary) {
            SessionRecording::readTimeKeyframeBinary(times, tkf, _iFile, lineNum);
            SessionRecording::saveHeaderAscii(times, SessionRecordingHeaderTimeAscii,
                keyframeLine);
            tkf.write(keyframeLine);
        }
        else if (frameType == SessionRecordingHeaderScriptBinary) {
            SessionRecording::readScriptKeyframeBinary(times, skf, _iFile, lineNum);
            SessionRecording::saveHeaderAscii(times, SessionRecordingHeaderScriptAscii,
                keyframeLine);
            skf.write(keyframeLine);
        }
        else {
            LERROR(fmt::format(
                "Unknown frame type @ index {} of playback file {}",
                lineNum - 1, _inFilePath
            ));
            break;
        }
        SessionRecording::saveKeyframeToFile(keyframeLine.str(), _oFile);
        lineNum++;
    }
    _oFile.close();
}

void ConvertRecFormatTask::convertToBinary() {
    SessionRecording::timestamps times;
    datamessagestructures::CameraKeyframe ckf;
    datamessagestructures::TimeKeyframe   tkf;
    datamessagestructures::ScriptMessage  skf;
    int lineNum = 1;
    std::string lineContents;
    unsigned char keyframeBuffer[SessionRecording::_saveBufferMaxSize_bytes];
    _oFile.open(_outFilePath, std::ifstream::app | std::ios::binary);
    char tmpType = SessionRecording::DataFormatBinaryTag;
    _oFile.write(&tmpType, 1);
    _oFile.write("\n", 1);
    size_t idx = 0;

    while (std::getline(_iFile, lineContents)) {
        lineNum++;

        std::istringstream iss(lineContents);
        std::string entryType;
        if (!(iss >> entryType)) {
            LERROR(fmt::format(
                "Error reading entry type @ line {} of file {}",
                lineNum, _inFilePath
            ));
            break;
        }

        if (entryType == SessionRecordingHeaderCameraAscii) {
            SessionRecording::readCameraKeyframeAscii(times, ckf, lineContents, lineNum);
            SessionRecording::saveCameraKeyframeBinary(times, ckf, keyframeBuffer,
            _oFile);
        }
        else if (entryType == SessionRecordingHeaderTimeAscii) {
            SessionRecording::readTimeKeyframeAscii(times, tkf, lineContents, lineNum);
            SessionRecording::saveTimeKeyframeBinary(times, tkf, keyframeBuffer,
            _oFile);
        }
        else if (entryType == SessionRecordingHeaderScriptAscii) {
            SessionRecording::readScriptKeyframeAscii(times, skf, lineContents, lineNum);
            SessionRecording::saveScriptKeyframeBinary(times, skf, keyframeBuffer,
            _oFile);
        }
        else {
            LERROR(fmt::format(
                "Unknown frame type {} @ line {} of file {}",
                entryType, lineContents, _inFilePath
            ));
            break;
        }
    }
    _oFile.close();
    LINFO(fmt::format(
        "Finished converting {} entries from file {}",
        lineNum, _inFilePath
    ));
}

std::string ConvertRecFormatTask::addFileSuffix(const std::string& filePath,
                                                const std::string& suffix)
{
    size_t lastdot = filePath.find_last_of(".");
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

}
