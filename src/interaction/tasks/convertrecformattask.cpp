/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

    constexpr const char* KeyInFilePath = "InputFilePath";
    constexpr const char* KeyOutFilePath = "OutputFilePath";
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
        _iFile.open(_inFilePath, std::ifstream::in | std::ifstream::binary);
        determineFormatType();
        sessRec = new SessionRecording(false);
    }
}

ConvertRecFormatTask::~ConvertRecFormatTask() {
    _iFile.close();
    _oFile.close();
    if (sessRec != nullptr) {
        delete sessRec;
    }
}

std::string ConvertRecFormatTask::description() {
    std::string description = "Convert session recording file '" + _inFilePath + "' ";
    if (_fileFormatType == SessionRecording::DataMode::Ascii) {
        description += "(ascii format) ";
    }
    else if (_fileFormatType == SessionRecording::DataMode::Binary) {
        description += "(binary format) ";
    }
    else {
        description += "(UNKNOWN format) ";
    }
    description += "conversion to file '" + _outFilePath + "'.";
    return description;
}

void ConvertRecFormatTask::perform(const Task::ProgressCallback&) {
    convert();
}

void ConvertRecFormatTask::convert() {
    std::string expectedFileExtension_in, expectedFileExtension_out;
    std::string currentFormat;
    if (_fileFormatType == SessionRecording::DataMode::Binary) {
        currentFormat = "binary";
        expectedFileExtension_in = SessionRecording::FileExtensionBinary;
        expectedFileExtension_out = SessionRecording::FileExtensionAscii;
    }
    else if (_fileFormatType == SessionRecording::DataMode::Ascii) {
        currentFormat = "ascii";
        expectedFileExtension_in = SessionRecording::FileExtensionAscii;
        expectedFileExtension_out = SessionRecording::FileExtensionBinary;
    }

    if (!SessionRecording::hasFileExtension(_inFilePath, expectedFileExtension_in)) {
        LWARNING(fmt::format(
            "Input filename doesn't have expected {} format file extension",
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
    else if (!SessionRecording::hasFileExtension(_outFilePath, expectedFileExtension_out))
    {
        _outFilePath += expectedFileExtension_out;
    }

    if (_fileFormatType == SessionRecording::DataMode::Ascii) {
        _oFile.open(_outFilePath);
    }
    else if (_fileFormatType == SessionRecording::DataMode::Binary) {
        _oFile.open(_outFilePath, std::ios::binary);
    }
    _oFile.write(
        SessionRecording::FileHeaderTitle.c_str(),
        SessionRecording::FileHeaderTitle.length()
    );
    _oFile.write(_version.c_str(), SessionRecording::FileHeaderVersionLength);
    _oFile.close();

    if (_fileFormatType == SessionRecording::DataMode::Ascii) {
        convertToBinary();
    }
    else if (_fileFormatType == SessionRecording::DataMode::Binary) {
        convertToAscii();
    }
    else {
        //Add error output for file type not recognized
        LERROR("Session recording file unrecognized format type.");
    }
}

void ConvertRecFormatTask::determineFormatType() {
    _fileFormatType = SessionRecording::DataMode::Unknown;
    std::string line;

    line = SessionRecording::readHeaderElement(_iFile,
        SessionRecording::FileHeaderTitle.length());

    if (line.substr(0, SessionRecording::FileHeaderTitle.length())
        != SessionRecording::FileHeaderTitle)
    {
        LERROR(fmt::format("Session recording file {} does not have expected header.",
            _inFilePath));
    }
    else {
        //Read version string and throw it away (and also line feed character at end)
        _version = SessionRecording::readHeaderElement(_iFile,
            SessionRecording::FileHeaderVersionLength);
        line = SessionRecording::readHeaderElement(_iFile, 1);
        SessionRecording::readHeaderElement(_iFile, 1);

        if (line.at(0) == SessionRecording::DataFormatAsciiTag) {
            _fileFormatType = SessionRecording::DataMode::Ascii;
        }
        else if (line.at(0) == SessionRecording::DataFormatBinaryTag) {
            _fileFormatType = SessionRecording::DataMode::Binary;
        }
    }
}

void ConvertRecFormatTask::convertToAscii() {
    SessionRecording::Timestamps times;
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

        if (frameType == SessionRecording::HeaderCameraBinary) {
            sessRec->readCameraKeyframeBinary(times, ckf, _iFile, lineNum);
            sessRec->saveHeaderAscii(times, SessionRecording::HeaderCameraAscii,
                keyframeLine);
            ckf.write(keyframeLine);
        }
        else if (frameType == SessionRecording::HeaderTimeBinary) {
            sessRec->readTimeKeyframeBinary(times, tkf, _iFile, lineNum);
            sessRec->saveHeaderAscii(times, SessionRecording::HeaderTimeAscii,
                keyframeLine);
            tkf.write(keyframeLine);
        }
        else if (frameType == SessionRecording::HeaderScriptBinary) {
            sessRec->readScriptKeyframeBinary(times, skf, _iFile, lineNum);
            sessRec->saveHeaderAscii(times, SessionRecording::HeaderScriptAscii,
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
    SessionRecording::Timestamps times;
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

        if (entryType == SessionRecording::HeaderCameraAscii) {
            sessRec->readCameraKeyframeAscii(times, ckf, lineContents, lineNum);
            sessRec->saveCameraKeyframeBinary(times, ckf, keyframeBuffer,
            _oFile);
        }
        else if (entryType == SessionRecording::HeaderTimeAscii) {
            sessRec->readTimeKeyframeAscii(times, tkf, lineContents, lineNum);
            sessRec->saveTimeKeyframeBinary(times, tkf, keyframeBuffer,
            _oFile);
        }
        else if (entryType == SessionRecording::HeaderScriptAscii) {
            sessRec->readScriptKeyframeAscii(times, skf, lineContents, lineNum);
            sessRec->saveScriptKeyframeBinary(times, skf, keyframeBuffer,
            _oFile);
        }
        else if (entryType.substr(0, 1) == SessionRecording::HeaderCommentAscii) {
            continue;
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
