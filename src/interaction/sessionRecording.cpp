/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <openspace/interaction/sessionRecording.h>
#include <openspace/interaction/externInteraction.h>

#include <openspace/openspace.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/interaction/orbitalnavigator.h>
#include <openspace/interaction/keyframenavigator.h>
#include <openspace/scripting/scriptscheduler.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scene.h>
#include <openspace/util/camera.h>
#include <openspace/util/time.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/spicemanager.h>

#include <ghoul/logging/logmanager.h>

#include<openspace/rendering/renderengine.h>

#include <iterator>

namespace {
    const char* _loggerCat = "SessionRecording";
}

#include "sessionRecording_lua.inl"


namespace openspace::interaction {

SessionRecording::SessionRecording()
    : properties::PropertyOwner({ "SessionRecording" }) {
}

SessionRecording::~SessionRecording() {} // NOLINT

void SessionRecording::deinitialize() {
    stopRecording();
    stopPlayback();
}

void SessionRecording::setRecordDataFormat(recordDataMode dataMode) {
    _recordingDataMode = dataMode;
}

bool SessionRecording::startRecording(std::string filename) {
    if (_state == sessionState::playback) {
        _playbackFile.close();
    }
    _state = sessionState::recording;
    _playbackActive_camera = false;
    _playbackActive_time = false;
    _playbackActive_script = false;
    if (isDataModeBinary())
        _recordFile.open(filename, std::ios::binary);
    else
        _recordFile.open(filename);

    if (!_recordFile.is_open() || !_recordFile.good()) {
        LERROR(fmt::format("Unable to open file {} for keyframe recording", filename.c_str()));
        return false;
    }
    _recordFile << _fileHeaderTitle;
    _recordFile.write(_fileHeaderVersion, _fileHeaderVersionLength);
    if (isDataModeBinary())
        _recordFile << dataFormatBinaryTag;
    else
        _recordFile << dataFormatAsciiTag;
    _recordFile << '\n';

    LINFO("Session recording started");
    _timestampRecordStarted = global::windowDelegate.applicationTime();
    return true;
}

void SessionRecording::stopRecording() {
    if (_state == sessionState::recording) {
        _state = sessionState::idle;
        LINFO("Session recording stopped");
    }
    //Close the recording file
    _recordFile.close();
}

bool SessionRecording::startPlayback(std::string filename, KeyframeTimeRef timeMode) {
    if (_state == sessionState::recording) {
        LERROR("Unable to start playback while in session recording mode");
        return false;
    }
    else if (_state == sessionState::playback) {
        if (_playbackFilename.compare(filename) == 0) {
            LERROR(fmt::format(
                "Unable to start playback on file {} since it is already in playback",
                filename)
            );
            return false;
        }
    }

    _playbackLineNum = 1;
    _playbackFilename = filename;

    //Open in ASCII first
    _playbackFile.open(_playbackFilename, std::ifstream::in);
    //Read header
    std::string readBackHeaderString = readHeaderElement(_fileHeaderTitle.length());
    if (readBackHeaderString.compare(_fileHeaderTitle) != 0) {
        LERROR("Specified playback file does not contain expected header.");
        return false;
    }
    readHeaderElement(_fileHeaderVersionLength);
    std::string readDataMode = readHeaderElement(1);
    if (readDataMode[0] == dataFormatAsciiTag)
        _recordingDataMode = recordDataMode::ascii;
    else if (readDataMode[0] == dataFormatBinaryTag)
        _recordingDataMode = recordDataMode::binary;
    else
        LERROR("Unknown data type in header (should be Ascii or Binary)");
    std::string throwawayNewlineChar = readHeaderElement(1);
    
    if (isDataModeBinary()) {
        //Close & re-open the file, starting from the beginning, and do dummy read
        // past the header, version, and data type
        _playbackFile.close();
        _playbackFile.open(_playbackFilename, std::ifstream::in | std::ios::binary);
        unsigned int throwAwayHeaderReadSize = _fileHeaderTitle.length() +
                                               _fileHeaderVersionLength +
                                               sizeof(dataFormatBinaryTag) +
                                               sizeof('\n');
        _playbackFile.read(reinterpret_cast<char*>(&_keyframeBuffer),
                           throwAwayHeaderReadSize);
    }

    if (!_playbackFile.is_open() || !_playbackFile.good())
    {
        LERROR(fmt::format("Unable to open file {} for keyframe playback",
               filename.c_str()));
        stopPlayback();
        return false;
    }
    //Set time reference mode
    double now = global::windowDelegate.applicationTime();
    _timestampPlaybackStarted_application = now;
    _timestampPlaybackStarted_simulation = global::timeManager.time().j2000Seconds();
    _timestampApplicationStarted_simulation = _timestampPlaybackStarted_simulation - now;
    _playbackTimeReferenceMode = timeMode;

    //Set playback flags to true for all modes
    _playbackActive_camera = true;
    _playbackActive_script = true;
    if (_usingTimeKeyframes)
        _playbackActive_time = true;

    global::navigationHandler.keyframeNavigator().setTimeReferenceMode(timeMode, now);
    global::scriptScheduler.setTimeReferenceMode(timeMode);
    if (!playbackAddEntriesToTimeline())
        return false;

    LINFO(fmt::format("Playback session started @ ({:8.3f},0.0,{:13.3f}) with {}/{}/{} entries",
        now, _timestampPlaybackStarted_simulation, _keyframesCamera.size(),
        _keyframesTime.size(), _keyframesScript.size()));

    global::navigationHandler.triggerPlaybackStart();
    global::scriptScheduler.triggerPlaybackStart();
    global::timeManager.triggerPlaybackStart();
    _state = sessionState::playback;

    return true;
}

std::string SessionRecording::readHeaderElement(size_t readLen_chars) {
    std::vector<char> readTemp(readLen_chars);
    _playbackFile.read(&readTemp[0], readLen_chars);
    return std::string(readTemp.begin(), readTemp.end());
    //return std::string(&readTemp[0], &readTemp[readLen_chars - 1]);
}

void SessionRecording::signalPlaybackFinishedForComponent(recordedType type) {
    if (type == recordedType::camera) {
        _playbackActive_camera = false;
        LINFO("Playback finished signal: camera");
    }
    else if (type == recordedType::time) {
        _playbackActive_time = false;
        LINFO("Playback finished signal: time");
    }
    else if (type == recordedType::script) {
        _playbackActive_script = false;
        LINFO("Playback finished signal: script");
    }

    if (!_playbackActive_camera && !_playbackActive_time && !_playbackActive_script ) {
        _state = sessionState::idle;
        _cleanupNeeded = true;
        LINFO("Playback session finished");
    }
}

void SessionRecording::stopPlayback() {
    if (_state == sessionState::playback) {
        _state = sessionState::idle;
        _cleanupNeeded = true;
        LINFO("Session playback stopped");
    }
}

void SessionRecording::cleanUpPlayback() {
    global::navigationHandler.setDisableKeyFrameInteraction();
    global::navigationHandler.stopPlayback();
    global::scriptScheduler.stopPlayback();

    _playbackFile.close();

    //Clear all timelines and keyframes
    _timeline.clear();
    _keyframesCamera.clear();
    _keyframesTime.clear();
    _keyframesScript.clear();
    _idxTimeline_nonCamera = 0;
    _idxTime = 0;
    _idxScript = 0;
    _idxTimeline_cameraPtr = 0;
    _idxTimeline_cameraPrevUpperBound = 0;
}

bool SessionRecording::isDataModeBinary() {
    if (_recordingDataMode == recordDataMode::binary)
        return true;
    else
        return false;
}

void SessionRecording::writeToFileBuffer(const double& src) {
    const size_t writeSize_bytes = sizeof(double);
    unsigned char const *p = reinterpret_cast<unsigned char const*>(&src);
    memcpy((_keyframeBuffer + _bufferIndex), p, writeSize_bytes);
    _bufferIndex += writeSize_bytes;
}

void SessionRecording::writeToFileBuffer(const unsigned char c) {
    const size_t writeSize_bytes = sizeof(char);
    _keyframeBuffer[_bufferIndex] = c;
    _bufferIndex += writeSize_bytes;
}

void SessionRecording::writeToFileBuffer(const bool b) {
    const size_t writeSize_bytes = sizeof(char);
    if( b )
        _keyframeBuffer[_bufferIndex] = 1;
    else
        _keyframeBuffer[_bufferIndex] = 0;
    _bufferIndex += writeSize_bytes;
}

void SessionRecording::saveStringToFile(const std::string s) {
    size_t strLen = s.size();
    size_t writeSize_bytes = sizeof(size_t);

    _bufferIndex = 0;
    unsigned char const *p = reinterpret_cast<unsigned char const*>(&strLen);
    memcpy((_keyframeBuffer + _bufferIndex), p, writeSize_bytes);
    _bufferIndex += (unsigned int)writeSize_bytes;
    saveKeyframeToFileBinary(_keyframeBuffer, _bufferIndex);

    _recordFile.write(s.c_str(), s.size());
}

void SessionRecording::readFromPlayback(unsigned char& result) {
    _playbackFile.read(reinterpret_cast<char*>(&result), sizeof(unsigned char));
}

void SessionRecording::readFromPlayback(double& result) {
    _playbackFile.read(reinterpret_cast<char*>(&result), sizeof(double));
}

void SessionRecording::readFromPlayback(float& result) {
    _playbackFile.read(reinterpret_cast<char*>(&result), sizeof(float));
}

void SessionRecording::readFromPlayback(size_t& result) {
    _playbackFile.read(reinterpret_cast<char*>(&result), sizeof(size_t));
}

void SessionRecording::readFromPlayback(bool& result) {
    unsigned char b;
    _playbackFile.read(reinterpret_cast<char*>(&b), sizeof(unsigned char));
    if (b == 0)
        result = false;
    else if (b == 1)
        result = true;
    else
        LERROR(fmt::format("Invalid bool read value at {}", _playbackLineNum - 1));
}

void SessionRecording::readFromPlayback(std::string& result) {
    result.erase();
    size_t strLen;
    //Read string length from file
    _playbackFile.read(reinterpret_cast<char*>(&strLen), sizeof(strLen));
    //Read back full string
    char* temp = new char[strLen + 1];
    _playbackFile.read(temp, strLen);
    temp[strLen] = '\0';
    result = temp;
    delete[] temp;
}

bool SessionRecording::hasCameraChangedFromPrev(datamessagestructures::CameraKeyframe kfNew) {
    const double threshold = 1e-2;
    bool hasChanged = false;

    glm::dvec3 positionDiff = kfNew._position - _prevRecordedCameraKeyframe._position;
    if (glm::length(positionDiff) > threshold)
        hasChanged = true;

    float rotationDiff = dot(kfNew._rotation, _prevRecordedCameraKeyframe._rotation);
    if (std::abs(rotationDiff - 1.0) > threshold)
        hasChanged = true;

    _prevRecordedCameraKeyframe = kfNew;
    return hasChanged;
}

void SessionRecording::saveCameraKeyframe() {
    if (_state != sessionState::recording)
        return;

    SceneGraphNode* focusNode = global::navigationHandler.focusNode();
    if (!focusNode) {
        return;
    }

    //Create a camera keyframe, then call to populate it with current position
    // & orientation of camera
    datamessagestructures::CameraKeyframe kf;
    _externInteract.generateCameraKeyframe(kf);

    if (/*hasCameraChangedFromPrev(kf)*/true) {
        if (isDataModeBinary()) {
            _bufferIndex = 0;
            _keyframeBuffer[_bufferIndex++] = 'c';
            writeToFileBuffer(kf._timestamp);
            writeToFileBuffer(kf._timestamp - _timestampRecordStarted);
            writeToFileBuffer(global::timeManager.time().j2000Seconds());
            writeToFileBuffer(kf._position.x);
            writeToFileBuffer(kf._position.y);
            writeToFileBuffer(kf._position.z);
            writeToFileBuffer(kf._rotation.x);
            writeToFileBuffer(kf._rotation.y);
            writeToFileBuffer(kf._rotation.z);
            writeToFileBuffer(kf._rotation.w);
            writeToFileBuffer(kf._followNodeRotation);
            saveKeyframeToFileBinary(_keyframeBuffer, _bufferIndex);

            saveStringToFile(kf._focusNode);
        } else {
            std::stringstream keyframeLine = std::stringstream();
            //Add simulation timestamp, timestamp relative, simulation time to recording start
            keyframeLine << "camera ";
            keyframeLine << kf._timestamp << " ";
            keyframeLine << (kf._timestamp - _timestampRecordStarted) << " ";
            keyframeLine << std::fixed << std::setprecision(3) << global::timeManager.time().j2000Seconds();
            keyframeLine << " ";
            //Add camera position
            keyframeLine << std::fixed << std::setprecision(7) << kf._position.x << " "
                << std::fixed << std::setprecision(7) << kf._position.y << " "
                << std::fixed << std::setprecision(7) << kf._position.z << " ";
            //Add camera rotation
            keyframeLine << std::fixed << std::setprecision(7) << kf._rotation.x << " "
                << std::fixed << std::setprecision(7) << kf._rotation.y << " "
                << std::fixed << std::setprecision(7) << kf._rotation.z << " "
                << std::fixed << std::setprecision(7) << kf._rotation.w << " ";
            if (kf._followNodeRotation)
                keyframeLine << "F ";
            else
                keyframeLine << "- ";
            keyframeLine << kf._focusNode;

            saveKeyframeToFile(keyframeLine.str());
        }
    }
}

void SessionRecording::saveTimeKeyframe() {
    if (_state != sessionState::recording)
        return;

    //Create a time keyframe, then call to populate it with current time props
    datamessagestructures::TimeKeyframe kf;
    _externInteract.generateTimeKeyframe(kf);

    if (isDataModeBinary()) {
        _bufferIndex = 0;
        _keyframeBuffer[_bufferIndex++] = 't';
        writeToFileBuffer(kf._timestamp);
        writeToFileBuffer(kf._timestamp - _timestampRecordStarted);
        writeToFileBuffer(kf._time);
        writeToFileBuffer(kf._dt);
        writeToFileBuffer(kf._paused);
        writeToFileBuffer(kf._requiresTimeJump);

        saveKeyframeToFileBinary(_keyframeBuffer, _bufferIndex);
    } else {
        std::stringstream keyframeLine = std::stringstream();
        //Add simulation timestamp, timestamp relative, simulation time to recording start
        keyframeLine << "time ";
        keyframeLine << kf._timestamp << " ";
        keyframeLine << (kf._timestamp - _timestampRecordStarted) << " ";

        //keyframeLine << std::fixed << std::setprecision(3) << global::timeManager.time().j2000Seconds();
        keyframeLine << std::fixed << std::setprecision(3) << kf._time;

        keyframeLine << " " << kf._dt;
        if (kf._paused)
            keyframeLine << " P";
        else
            keyframeLine << " R";

        if (kf._requiresTimeJump)
            keyframeLine << " J";
        else
            keyframeLine << " -";

        saveKeyframeToFile(keyframeLine.str());
    }
}

void SessionRecording::saveScriptKeyframe(std::string scriptToSave) {
    if (_state != sessionState::recording)
        return;

    datamessagestructures::ScriptMessage sm;
    _externInteract.generateScriptMessage(sm, scriptToSave);
    if (isDataModeBinary()) {
        _bufferIndex = 0;
        _keyframeBuffer[_bufferIndex++] = 's';
        writeToFileBuffer(sm._timestamp);
        writeToFileBuffer(sm._timestamp - _timestampRecordStarted);
        writeToFileBuffer(global::timeManager.time().j2000Seconds());
        //Write header to file
        saveKeyframeToFileBinary(_keyframeBuffer, _bufferIndex);

        saveStringToFile(scriptToSave);
    } else {
        unsigned int numLinesInScript = std::count(scriptToSave.begin(),
            scriptToSave.end(), '\n');
        std::stringstream keyframeLine = std::stringstream();
        //Add simulation timestamp, timestamp relative, simulation time to recording start
        keyframeLine << "script ";
        keyframeLine << sm._timestamp << " ";
        keyframeLine << (sm._timestamp - _timestampRecordStarted) << " ";
        keyframeLine << std::fixed << std::setprecision(3) << global::timeManager.time().j2000Seconds();
        keyframeLine << " ";
        keyframeLine << (numLinesInScript + 1) << " ";
        keyframeLine << scriptToSave;

        saveKeyframeToFile(keyframeLine.str());
    }
}

void SessionRecording::preSynchronization(double deltaTime) {
    if( _state == sessionState::recording ) {
        saveCameraKeyframe();
        if( _usingTimeKeyframes )
            saveTimeKeyframe();
    }
    else if (_state == sessionState::playback) {
        moveAheadInTime(deltaTime);
    } else if (_cleanupNeeded) {
        cleanUpPlayback();
    }
}

bool SessionRecording::isRecording() {
    return (_state == sessionState::recording);
}

bool SessionRecording::isPlayingBack() {
    return (_state == sessionState::playback);
}

bool SessionRecording::playbackAddEntriesToTimeline() {
    bool parsingErrorsFound = false;

    if (isDataModeBinary()) {
        unsigned char frameType;
        bool fileReadOk = true;

        while (fileReadOk) {
            readFromPlayback(frameType);
            //Check if have reached EOF
            if (!_playbackFile) {
                LINFO(fmt::format("Finished parsing {} entries from playback file {}",
                    _playbackLineNum - 1, _playbackFilename.c_str()));
                fileReadOk = false;
                break;
            }
            if (frameType == 'c') {
                playbackCamera();
            }
            else if (frameType == 't') {
                playbackTimeChange();
            }
            else if (frameType == 's') {
                playbackScript();
            }
            else {
                LERROR(fmt::format("Unknown frame type {} @ index {} of playback file {}",
                    frameType, _playbackLineNum - 1, _playbackFilename.c_str()));
                parsingErrorsFound = true;
                break;
            }

            _playbackLineNum++;
        }
    } else {
        while (std::getline(_playbackFile, _playbackLineParsing)) {
            _playbackLineNum++;

            std::istringstream iss(_playbackLineParsing);
            std::string entryType;
            if (!(iss >> entryType)) {
                LERROR(fmt::format("Error reading entry type @ line {} of playback file {}",
                    _playbackLineNum, _playbackFilename.c_str()));
                break;
            }

            if (entryType.compare("camera") == 0) {
                playbackCamera();
            }
            else if (entryType.compare("time") == 0) {
                playbackTimeChange();
            }
            else if (entryType.compare("script") == 0) {
                playbackScript();
            }
            else {
                LERROR(fmt::format("Unknown frame type {} @ line {} of playback file {}",
                    entryType, _playbackLineNum, _playbackFilename.c_str()));
                parsingErrorsFound = true;
                break;
            }
        }
        LINFO(fmt::format("Finished parsing {} entries from playback file {}",
            _playbackLineNum, _playbackFilename.c_str()));
    }

    ghoul_assert(
        std::is_sorted(
            _timeline.begin(),
            _timeline.end(),
            [](const timelineEntry& lhs, const timelineEntry& rhs) { return lhs.timestamp < rhs.timestamp; }),
        ""
        );

    return !parsingErrorsFound;
}

double SessionRecording::getAppropriateTimestamp(double timeOs,
                                                 double timeRec,
                                                 double timeSim)
{
    if (_playbackTimeReferenceMode == KeyframeTimeRef::relative_recordedStart)
        return timeRec;
    else if (_playbackTimeReferenceMode == KeyframeTimeRef::absolute_simTimeJ2000)
        return timeSim;
    else
        return timeOs;
}

double SessionRecording::getEquivalentSimulationTime(double timeOs,
                                                     double timeRec,
                                                     double timeSim)
{
    if (_playbackTimeReferenceMode == KeyframeTimeRef::relative_recordedStart)
        return _timestampPlaybackStarted_simulation + timeRec;
    else if (_playbackTimeReferenceMode == KeyframeTimeRef::relative_applicationStart)
        return _timestampApplicationStarted_simulation + timeOs;
    else
        return timeSim;
}

double SessionRecording::getEquivalentApplicationTime(double timeOs,
                                                      double timeRec,
                                                      double timeSim)
{
    if (_playbackTimeReferenceMode == KeyframeTimeRef::relative_recordedStart)
        return _timestampPlaybackStarted_application + timeRec;
    else if (_playbackTimeReferenceMode == KeyframeTimeRef::absolute_simTimeJ2000)
        return timeSim - _timestampApplicationStarted_simulation;
    else
        return timeOs;
}

double SessionRecording::currentTime() const {
    if (_playbackTimeReferenceMode == KeyframeTimeRef::relative_recordedStart)
        return (global::windowDelegate.applicationTime() - _timestampPlaybackStarted_application);
    else if (_playbackTimeReferenceMode == KeyframeTimeRef::absolute_simTimeJ2000)
        return global::timeManager.time().j2000Seconds();
    else
        return global::windowDelegate.applicationTime();
}
    
void SessionRecording::playbackCamera() {
    double timeRef;
    double timeOs, timeRec, timeSim;
    std::string rotationFollowing;
    interaction::KeyframeNavigator::CameraPose pbFrame;
    if (isDataModeBinary()) {
        double tmpRotation;
        readFromPlayback(timeOs);
        readFromPlayback(timeRec);
        readFromPlayback(timeSim);
        readFromPlayback(pbFrame.position.x);
        readFromPlayback(pbFrame.position.y);
        readFromPlayback(pbFrame.position.z);
        readFromPlayback(tmpRotation);
        pbFrame.rotation.x = static_cast<float>(tmpRotation);
        readFromPlayback(tmpRotation);
        pbFrame.rotation.y = static_cast<float>(tmpRotation);
        readFromPlayback(tmpRotation);
        pbFrame.rotation.z = static_cast<float>(tmpRotation);
        readFromPlayback(tmpRotation);
        pbFrame.rotation.w = static_cast<float>(tmpRotation);

        readFromPlayback(pbFrame.followFocusNodeRotation);
        readFromPlayback(pbFrame.focusNode);
        if (!_playbackFile) {
            LINFO(fmt::format("Error reading camera playback from keyframe entry {}",
                _playbackLineNum - 1));
            return;
        }
    } else {
        std::istringstream iss(_playbackLineParsing);
        std::string entryType;
        iss >> entryType;
        iss >> timeOs >> timeRec >> timeSim;
        iss >> pbFrame.position.x
            >> pbFrame.position.y
            >> pbFrame.position.z
            >> pbFrame.rotation.x
            >> pbFrame.rotation.y
            >> pbFrame.rotation.z
            >> pbFrame.rotation.w
            >> rotationFollowing
            >> pbFrame.focusNode;
        if (iss.fail() || !iss.eof()) {
            LERROR(fmt::format("Error parsing camera line {} of playback file", _playbackLineNum));
            return;
        }
        if (rotationFollowing.compare("F") == 0)
            pbFrame.followFocusNodeRotation = true;
        else
            pbFrame.followFocusNodeRotation = false;
    }
    timeRef = getAppropriateTimestamp(timeOs, timeRec, timeSim);

    //global::navigationHandler.keyframeNavigator().addKeyframe(timeRef, pbFrame);
    addKeyframe(timeRef, pbFrame);
}

void SessionRecording::playbackTimeChange() {
    double timeOs, timeRec, timeSim;
    datamessagestructures::TimeKeyframe pbFrame;
    if (isDataModeBinary()) {
        readFromPlayback(timeOs);
        readFromPlayback(timeRec);
        readFromPlayback(timeSim);
        readFromPlayback(pbFrame._dt);
        readFromPlayback(pbFrame._paused);
        readFromPlayback(pbFrame._requiresTimeJump);
        if (!_playbackFile) {
            LERROR(fmt::format("Error reading time playback from keyframe entry {}",
                _playbackLineNum - 1));
            return;
        }
    } else {
        std::istringstream iss(_playbackLineParsing);
        std::string entryType;
        //double timeRef;
        std::string paused, jump;
        iss >> entryType;
        iss >> timeOs >> timeRec >> timeSim;
        iss >> pbFrame._dt
            >> paused
            >> jump;
        if (iss.fail() || !iss.eof()) {
            LERROR(fmt::format("Error parsing time line {} of playback file", _playbackLineNum));
            return;
        }
        if (paused.compare("P") == 0)
            pbFrame._paused = true;
        else
            pbFrame._paused = false;
        if (jump.compare("J") == 0)
            pbFrame._requiresTimeJump = true;
        else
            pbFrame._requiresTimeJump = false;
    }
    pbFrame._timestamp = getEquivalentApplicationTime(timeOs, timeRec, timeSim);

    pbFrame._time = pbFrame._timestamp + _timestampApplicationStarted_simulation;
    //global::timeManager.addKeyframe(timeRef, pbFrame._timestamp);
    //_externInteract.timeInteraction(pbFrame);
    addKeyframe(pbFrame._timestamp, pbFrame);
}

void SessionRecording::playbackScript() {
    double timeRef;
    double timeOs, timeRec, timeSim;
    unsigned int numScriptLines;
    datamessagestructures::ScriptMessage pbFrame;

    if (isDataModeBinary()) {
        readFromPlayback(timeOs);
        readFromPlayback(timeRec);
        readFromPlayback(timeSim);
        readFromPlayback(pbFrame._script);
        if (!_playbackFile) {
            LERROR(fmt::format("Error reading script playback from keyframe entry {}",
                _playbackLineNum - 1));
            return;
        }
    } else {
        std::istringstream iss(_playbackLineParsing);
        std::string entryType;
        std::string tmpReadbackScript;

        iss >> entryType;
        iss >> timeOs >> timeRec >> timeSim;
        iss >> numScriptLines;
        std::getline(iss, tmpReadbackScript); //iss >> tmpReadbackScript;
        pbFrame._script.append(tmpReadbackScript);
        if (iss.fail()) {
            LERROR(fmt::format("Error parsing script line {} of playback file", _playbackLineNum));
            return;
        } else if (!iss.eof()) {
            LERROR(fmt::format("Did not find an EOL at line {} of playback file", _playbackLineNum));
            return;
        }
        if (numScriptLines > 1) {
            //Now loop to read any subsequent lines if is a multi-line script
            for (unsigned int i = 1; i < numScriptLines; ++i) {
                pbFrame._script.append("\n");
                std::getline(_playbackFile, tmpReadbackScript);
                pbFrame._script.append(tmpReadbackScript);
            }
        }
    }
    timeRef = getAppropriateTimestamp(timeOs, timeRec, timeSim);
    //timeRef = getEquivalentSimulationTime(timeOs, timeRec, timeSim);

    //Call script scheduler with this new script entry
    //std::string timeDescription = SpiceManager::ref().dateFromEphemerisTime(timeRef, "YYYY MON DD HR:MN:SC.###");
    //ghoul::Dictionary scriptDict(ghoul::Dictionary{ {KeyTime, timeDescription },
    //                                                {KeyForwardScript, pbFrame._script} }
    //                            );
    //global::scriptScheduler.loadScripts({ { "1", scriptDict } });
    addKeyframe(timeRef, pbFrame._script);
}

void SessionRecording::addKeyframe(double timestamp, interaction::KeyframeNavigator::CameraPose keyframe)
{
    unsigned int indexIntoCameraKeyframesFromMainTimeline = _keyframesCamera.size();
    _keyframesCamera.push_back(keyframe);
    _timeline.push_back({ recordedType::camera,
                          indexIntoCameraKeyframesFromMainTimeline,
                          timestamp });
}

void SessionRecording::addKeyframe(double timestamp, datamessagestructures::TimeKeyframe keyframe)
{
    unsigned int indexIntoTimeKeyframesFromMainTimeline = _keyframesTime.size();
    _keyframesTime.push_back(keyframe);
    _timeline.push_back({ recordedType::time,
                          indexIntoTimeKeyframesFromMainTimeline,
                          timestamp });
}

void SessionRecording::addKeyframe(double timestamp, std::string scriptToQueue)
{
    unsigned int indexIntoScriptKeyframesFromMainTimeline = _keyframesScript.size();
    _keyframesScript.push_back(scriptToQueue);
    _timeline.push_back({ recordedType::script,
                          indexIntoScriptKeyframesFromMainTimeline,
                          timestamp });
}

void SessionRecording::moveAheadInTime(double deltaTime) {
    if (_state == sessionState::playback) {
        LINFOC("frame", std::to_string(global::renderEngine._frameNumber));

        double currTime = currentTime();
        lookForNonCameraKeyframesThatHaveComeDue(currTime, deltaTime);
        updateCameraWithOrWithoutNewKeyframes(currTime);
    }
}

void SessionRecording::lookForNonCameraKeyframesThatHaveComeDue(double currTime, double deltaTime) {
    while (isTimeToHandleNextNonCameraKeyframe(currTime, deltaTime)) {
        if (!processNextNonCameraKeyframeAheadInTime(currTime, deltaTime))
            break;

        if (++_idxTimeline_nonCamera >= _timeline.size()) {
            _idxTimeline_nonCamera--;
            break;
        }
    }
}

void SessionRecording::updateCameraWithOrWithoutNewKeyframes(double currTime) {
    if (_playbackActive_camera)
    {
        findNextFutureCameraIndex(currTime);
        processCameraKeyframe(currTime);
    }
}

bool SessionRecording::isTimeToHandleNextNonCameraKeyframe(double currTime, double deltaTime) {
    if (currTime > getNextTimestamp() && (_playbackActive_time || _playbackActive_script))
        return true;
    else
        return false;
}

void SessionRecording::findNextFutureCameraIndex(double currTime) {
    unsigned int seekAheadIndex = _idxTimeline_cameraPtr;

    while  (++seekAheadIndex < _timeline.size()) {
        recordedType keyframeType = _timeline[seekAheadIndex].keyframeType;
        double timestamp = _timeline[seekAheadIndex].timestamp;
        if (keyframeType == recordedType::camera && timestamp > currTime) {
            break;
        }
    }
    //Set next keyframe in camera timeline to the next keyframe in the timeline that is
    // one less than the first future camera keyframe found.
    _idxTimeline_cameraPtr = (seekAheadIndex > 0) ? seekAheadIndex : 0;
    //Signal finished with camera keyframes if end of timeline reached
    if (seekAheadIndex >= _timeline.size())
        signalPlaybackFinishedForComponent(recordedType::camera);
}

bool SessionRecording::processNextNonCameraKeyframeAheadInTime(double now, double deltaTime) {
    bool returnValue = false;

    //LINFO(fmt::format("Keyframe at {} frame={} timelineIndex={}", now, global::renderEngine._frameNumber, _idxTimeline));

    switch (getNextKeyframeType()) {
    case recordedType::camera:
        //Just return true since this function no longer handles camera keyframes
        returnValue = true;
        break;

    case recordedType::time:
        _idxTime = _timeline[_idxTimeline_nonCamera].idxIntoKeyframeTypeArray;
        if (_keyframesTime.size() == 0) {
            return false;
        }
        LINFO("Time keyframe type");
        //TBD: the TimeManager restricts setting time directly
        break;

    case recordedType::script:
        _idxScript = _timeline[_idxTimeline_nonCamera].idxIntoKeyframeTypeArray;
        returnValue = processScriptKeyframe(now, deltaTime);
        break;

    default:
        LERROR(fmt::format("Bad keyframe type encountered during playback at index {}.",
            _idxTimeline_nonCamera));
        break;
    }
    return returnValue;
}

//void SessionRecording::moveBackInTime() { } //for future use

bool SessionRecording::processCameraKeyframe(double now) {
    interaction::KeyframeNavigator::CameraPose nextPose;
    interaction::KeyframeNavigator::CameraPose prevPose;
    Camera * camera = global::navigationHandler.camera();
    ghoul_assert(camera != nullptr, "Camera must not be nullptr");
    Scene * scene = camera->parent()->scene();

    if (!_playbackActive_camera) {
        return false;
    } else if (_keyframesCamera.size() == 0) {
        return false;
    } else {
        unsigned int nextIdx = _timeline[_idxTimeline_cameraPtr].idxIntoKeyframeTypeArray;
        nextPose = _keyframesCamera[nextIdx];
        unsigned int prevIdx = _timeline[_idxTimeline_cameraPrevUpperBound].idxIntoKeyframeTypeArray;
        prevPose = _keyframesCamera[prevIdx];
    }

    SceneGraphNode* prevFocusNode = scene->sceneGraphNode(prevPose.focusNode);
    SceneGraphNode* nextFocusNode = scene->sceneGraphNode(nextPose.focusNode);

    LINFOC("now", std::to_string(now));

    if (_idxTimeline_cameraPrevUpperBound == 0)
        _idxTimeline_cameraPrevUpperBound = _idxTimeline_cameraPtr;

    double nextTime = _timeline[_idxTimeline_cameraPtr].timestamp;  //getNextTimestamp();
    double prevTime = _timeline[_idxTimeline_cameraPrevUpperBound].timestamp;  //getPrevTimestamp();

    LINFOC("next", std::to_string(nextTime));
    LINFOC("prev", std::to_string(prevTime));
    double t;
    if ((nextTime - prevTime) < 1e-7)
        t = 0;
    else
        t = (now - prevTime) / (nextTime - prevTime);
    t = (t < 0.0) ? 0.0 : t;

    if (!prevFocusNode || !nextFocusNode) {
        return false;
    }

    glm::dvec3 prevKeyframeCameraPosition = prevPose.position;
    glm::dvec3 nextKeyframeCameraPosition = nextPose.position;
    glm::dquat prevKeyframeCameraRotation = prevPose.rotation;
    glm::dquat nextKeyframeCameraRotation = nextPose.rotation;

    // Transform position and rotation based on focus node rotation
    // (if following rotation)
    if (prevPose.followFocusNodeRotation) {
        prevKeyframeCameraRotation = glm::dquat(
            prevFocusNode->worldRotationMatrix() *
            glm::dmat3(glm::dquat(prevPose.rotation))
        );
        prevKeyframeCameraPosition = prevFocusNode->worldRotationMatrix() *
            prevPose.position;
    }
    if (nextPose.followFocusNodeRotation) {
        nextKeyframeCameraRotation = glm::dquat(
            nextFocusNode->worldRotationMatrix() *
            glm::dmat3(glm::dquat(nextPose.rotation))
        );
        nextKeyframeCameraPosition = nextFocusNode->worldRotationMatrix() *
            nextPose.position;
    }

    // Transform position based on focus node position
    prevKeyframeCameraPosition += prevFocusNode->worldPosition();
    nextKeyframeCameraPosition += nextFocusNode->worldPosition();

    // Linear interpolation
    camera->setPositionVec3(
        prevKeyframeCameraPosition * (1 - t) + nextKeyframeCameraPosition * t
    );
    if (prevKeyframeCameraRotation == nextKeyframeCameraRotation) {
        camera->setRotation(prevKeyframeCameraRotation);
        LINFO("Skipped slerp call with equal prev/next frames.");
    }
    else {
        camera->setRotation(
            glm::slerp(prevKeyframeCameraRotation, nextKeyframeCameraRotation, t)
        );
    }
LINFO(fmt::format("Cam pos prev={}, next={}", std::to_string(prevKeyframeCameraPosition),
    std::to_string(nextKeyframeCameraPosition)));
LINFO(fmt::format("Cam rot prev={} {} {} {}  next={} {} {} {}", prevKeyframeCameraRotation.x,
    prevKeyframeCameraRotation.y, prevKeyframeCameraRotation.z, prevKeyframeCameraRotation.w,
    nextKeyframeCameraRotation.x, nextKeyframeCameraRotation.y, nextKeyframeCameraRotation.z,
    nextKeyframeCameraRotation.w));
LINFO(fmt::format("Cam interp = {}", t));

    _idxTimeline_cameraPrevUpperBound = _idxTimeline_cameraPtr;

    return true;
}

bool SessionRecording::processScriptKeyframe(double now, double deltaTime) {
    std::string nextScript;

    if (!_playbackActive_script) {
        return false;
    } else if (_keyframesScript.size() == 0) {
        return false;
    } else {
        nextScript = getNextKeyframeObj(_idxScript, _keyframesScript,
            ([&]() { signalPlaybackFinishedForComponent(recordedType::script); }));
        global::scriptEngine.queueScript(nextScript,
            scripting::ScriptEngine::RemoteScripting::Yes);
    }
    LINFO("Script");

    return true;
}

double SessionRecording::getNextTimestamp() {
    if (_timeline.size() == 0) {
        return 0.0;
    } else if (_idxTimeline_nonCamera < _timeline.size()) {
        return _timeline[_idxTimeline_nonCamera].timestamp;
    } else {
        return _timeline.back().timestamp;
    }
}

double SessionRecording::getPrevTimestamp() {
    if (_timeline.size() == 0)
        return 0.0;
    else if (_idxTimeline_nonCamera == 0 )
        return _timeline.front().timestamp;
    else if (_idxTimeline_nonCamera < _timeline.size())
        return _timeline[_idxTimeline_nonCamera - 1].timestamp;
    else
        return _timeline.back().timestamp;
}

SessionRecording::recordedType SessionRecording::getNextKeyframeType() {
    if (_timeline.size() == 0) {
        return recordedType::invalid;
    } else if (_idxTimeline_nonCamera < _timeline.size()) {
        return _timeline[_idxTimeline_nonCamera].keyframeType;
    } else {
        return _timeline.back().keyframeType;
    }
}

SessionRecording::recordedType SessionRecording::getPrevKeyframeType() {
    if (_timeline.size() == 0) {
        return recordedType::invalid;
    } else if (_idxTimeline_nonCamera < _timeline.size()) {
        if (_idxTimeline_nonCamera > 0)
            return _timeline[_idxTimeline_nonCamera - 1].keyframeType;
        else
            return _timeline.front().keyframeType;
    }
    else {
        return _timeline.back().keyframeType;
    }
}

void SessionRecording::saveKeyframeToFileBinary(unsigned char* bufferSource,
                                                unsigned int size)
{
    _recordFile.write((char*)bufferSource, size);
}

void SessionRecording::saveKeyframeToFile(std::string entry) {
    _recordFile << entry << std::endl;
}

scripting::LuaLibrary SessionRecording::luaLibrary() {
    return {
        "sessionRecording",
        {
            {
                "startRecording",
                &luascriptfunctions::startRecording,
                {},
                "string",
                "Starts a recording session. The string argument is the filename used "
                "for the file where the recorded keyframes are saved. "
                "The file data format is binary."
            },
            {
                "startRecordingAscii",
                &luascriptfunctions::startRecordingAscii,
                {},
                "string",
                "Starts a recording session. The string argument is the filename used "
                "for the file where the recorded keyframes are saved. "
                "The file data format is ASCII."
            },
            {
                "stopRecording",
                &luascriptfunctions::stopRecording,
                {},
                "void",
                "Stops a recording session"
            },
            {
                "startPlaybackApplicationTime",
                &luascriptfunctions::startPlaybackApplicationTime,
                {},
                "string",
                "Starts a playback session with keyframe times that are relative to "
                "application time (seconds since OpenSpace application started). "
                "The string argument is the filename to pull playback keyframes from."
            },
            {
                "startPlaybackRecordedTime",
                &luascriptfunctions::startPlaybackRecordedTime,
                {},
                "string",
                "Starts a playback session with keyframe times that are relative to "
                "the time since the recording was started (the same relative time "
                "applies to the playback). The string argument is the filename to pull "
                "playback keyframes from."
            },
            {
                "startPlaybackSimulationTime",
                &luascriptfunctions::startPlaybackSimulationTime,
                {},
                "string",
                "Starts a playback session with keyframe times that are relative to "
                "the simulated date & time. The string argument is the filename to pull "
                "playback keyframes from."
            },
            {
                "stopPlayback",
                &luascriptfunctions::stopPlayback,
                {},
                "void",
                "Stops a playback session before playback of all keyframes is complete"
            }
        }
    };
}

} // namespace openspace::interaction
