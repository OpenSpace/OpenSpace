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
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scripting/scriptscheduler.h>
#include <openspace/util/time.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/spicemanager.h>

#include <ghoul/logging/logmanager.h>

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

bool SessionRecording::startRecording(std::string filename) {
    if( _state == sessionState::playback ) {
        _playbackFile.close();
    }
    _state = sessionState::recording;
    _playbackActive_camera = false;
#ifdef SESSION_RECORDING_TIME
    _playbackActive_time = false;
#endif
    _playbackActive_script = false;
#ifdef RECORD_BINARY
    _recordFile.open(filename, std::ios::binary);
#else
    _recordFile.open(filename);
#endif //#ifdef RECORD_BINARY
    if( ! _recordFile.is_open() || ! _recordFile.good() ) {
        LERROR(fmt::format("Unable to open file {} for keyframe recording", filename.c_str()));
        return false;
    }
    LINFO("Session recording started");
    _timestampRecordStarted = global::windowDelegate.applicationTime();
    return true;
}

void SessionRecording::stopRecording() {
    if( _state == sessionState::recording )
        _state = sessionState::idle;
    //Close the recording file
    _recordFile.close();
    LINFO("Session recording stopped");
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
#ifdef RECORD_BINARY
    _playbackFile.open(_playbackFilename, std::ifstream::in|std::ios::binary);
#else
    _playbackFile.open(_playbackFilename, std::ifstream::in);
#endif //#ifdef RECORD_BINARY
    if( ! _playbackFile.is_open() || ! _playbackFile.good() )
    {
        LERROR(fmt::format("Unable to open file {} for keyframe playback",
               filename.c_str()));
        return false;
    }
    //Set time reference mode
    double now = global::windowDelegate.applicationTime();
    _timestampPlaybackStarted_application = now;
    _timestampPlaybackStarted_simulation = global::timeManager.time().j2000Seconds();
    _timestampApplicationStarted_simulation = _timestampPlaybackStarted_simulation - now;
    _playbackTimeReferenceMode = timeMode;

    LINFO(fmt::format("Playback session started @ ({:8.3f},0.0,{:13.3f})",
                      now, _timestampPlaybackStarted_simulation));
    //Set playback flags to true for all modes
    _playbackActive_camera = true;
#ifdef SESSION_RECORDING_TIME
    _playbackActive_time = true;
#endif
    _playbackActive_script = true;

    global::navigationHandler.keyframeNavigator().setTimeReferenceMode(timeMode, now);
    global::scriptScheduler.setTimeReferenceMode(timeMode);
    playbackAddEntriesToTimeline();

    global::navigationHandler.triggerPlaybackStart([&]() {
        signalPlaybackFinishedForComponent(recordedType::camera);
    });
    global::scriptScheduler.triggerPlaybackStart([&]() {
        signalPlaybackFinishedForComponent(recordedType::script);
    });
#ifdef SESSION_RECORDING_TIME
    global::timeManager.triggerPlaybackStart([&]() {
        signalPlaybackFinishedForComponent(recordedType::time);
    });
#endif
    return true;
}

void SessionRecording::signalPlaybackFinishedForComponent(recordedType type) {
    if (type == recordedType::camera) {
        _playbackActive_camera = false;
        LINFO("Playback finished signal: camera");
    }
#ifdef SESSION_RECORDING_TIME
    else if (type == recordedType::time) {
        _playbackActive_time = false;
        LINFO("Playback finished signal: time");
    }
#endif
    else if (type == recordedType::script) {
        _playbackActive_script = false;
        LINFO("Playback finished signal: script");
    }

    if (   !_playbackActive_camera
#ifdef SESSION_RECORDING_TIME
        && !_playbackActive_time
#endif
        && !_playbackActive_script )
    {
        _playbackFile.close();
        //Reset the script scheduler's time reference to simulation time, which is the
        // default mode for non-playback uses of the scheduler
        global::scriptScheduler.setTimeReferenceMode(
            KeyframeTimeRef::absolute_simTimeJ2000);
        LINFO("Playback session finished");
    }
}

void SessionRecording::stopPlayback() {
    global::navigationHandler.setDisableKeyFrameInteraction();
    //Clear the timelines of all keyframes
    global::scriptScheduler.clearSchedule();
    global::navigationHandler.keyframeNavigator().clearKeyframes();
    //Close the playback file
    _playbackFile.close();
    LINFO("Playback session was stopped");
}

void SessionRecording::saveCameraKeyframe() {
    SceneGraphNode* focusNode = global::navigationHandler.focusNode();
    if (!focusNode) {
        return;
    }

    //Create a camera keyframe, then call to populate it with current position
    // & orientation of camera
    datamessagestructures::CameraKeyframe kf;
    _externInteract.generateCameraKeyframe(kf);

    if (hasCameraChangedFromPrev(kf)) {
#ifdef RECORD_BINARY
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
        writeToFileBuffer(kf._focusNode);

        saveKeyframeToFileBinary();
#else
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
#endif //#ifdef RECORD_BINARY
    }
}

#ifdef RECORD_BINARY
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

void SessionRecording::writeToFileBuffer(const std::string s) {
    size_t strLen = s.size();
    size_t writeSize_bytes = sizeof(size_t);

    //Write the length of the string in bytes first
    unsigned char const *p = reinterpret_cast<unsigned char const*>(&strLen);
    memcpy((_keyframeBuffer + _bufferIndex), p, writeSize_bytes);
    _bufferIndex += (unsigned int)writeSize_bytes;

    //Now write the string contents
    writeSize_bytes = strLen;
    strcpy((char*)_keyframeBuffer + _bufferIndex, s.c_str());
    _bufferIndex += (unsigned int)writeSize_bytes;
}

void SessionRecording::readFromPlayback(unsigned char& result) {
    _playbackFile.read(reinterpret_cast<char*>(&result), sizeof(unsigned char));
    //LINFO(fmt::format("Read char {}.", result));
}

void SessionRecording::readFromPlayback(double& result) {
    _playbackFile.read(reinterpret_cast<char*>(&result), sizeof(double));
    //LINFO(fmt::format("Read double {}.", result));
}

void SessionRecording::readFromPlayback(float& result) {
    _playbackFile.read(reinterpret_cast<char*>(&result), sizeof(float));
    //LINFO(fmt::format("Read float {}.", result));
}

void SessionRecording::readFromPlayback(size_t& result) {
    _playbackFile.read(reinterpret_cast<char*>(&result), sizeof(size_t));
    //LINFO(fmt::format("Read size_t {}.", result));
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
    //Read the string from file to keyframe buffer, then into string arg
    _playbackFile.read(reinterpret_cast<char*>(&_keyframeBuffer), strLen);
    result.append((const char*)_keyframeBuffer, strLen);
}
#endif //#ifdef RECORD_BINARY

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

#ifdef SESSION_RECORDING_TIME
void SessionRecording::saveTimeKeyframe() {
    //Create a time keyframe, then call to populate it with current time props
    datamessagestructures::TimeKeyframe kf;
    _externInteract.generateTimeKeyframe(kf);

#ifdef RECORD_BINARY
    _bufferIndex = 0;
    _keyframeBuffer[_bufferIndex++] = 't';
    writeToFileBuffer(kf._timestamp);
    writeToFileBuffer(kf._timestamp - _timestampRecordStarted);
    writeToFileBuffer(kf._time);
    writeToFileBuffer(kf._dt);
    writeToFileBuffer(kf._paused);
    writeToFileBuffer(kf._requiresTimeJump);

    saveKeyframeToFileBinary();
#else
    std::stringstream keyframeLine = std::stringstream();
    //Add simulation timestamp, timestamp relative, simulation time to recording start
    keyframeLine << "time ";
    keyframeLine << kf._timestamp << " ";
    keyframeLine << (kf._timestamp - _timestampRecordStarted) << " ";

    //keyframeLine << std::fixed << std::setprecision(3) << global::timeManager.time().j2000Seconds();
    keyframeLine << std::fixed << std::setprecision(3) << kf._time;

    keyframeLine << " " << kf._dt;
    if( kf._paused )
        keyframeLine << " P";
    else
        keyframeLine << " R";
    
    if( kf._requiresTimeJump )
        keyframeLine << " J";
    else
        keyframeLine << " -";
    
    saveKeyframeToFile(keyframeLine.str());
#endif //#ifdef RECORD_BINARY
}
#endif

void SessionRecording::saveScript(std::string scriptToSave) {
    datamessagestructures::ScriptMessage sm;
    _externInteract.generateScriptMessage(sm, scriptToSave);
#ifdef RECORD_BINARY
    _bufferIndex = 0;
    _keyframeBuffer[_bufferIndex++] = 's';
    writeToFileBuffer(sm._timestamp);
    writeToFileBuffer(sm._timestamp - _timestampRecordStarted);
    writeToFileBuffer(global::timeManager.time().j2000Seconds());
    writeToFileBuffer(scriptToSave);

    saveKeyframeToFileBinary();
#else
    std::stringstream keyframeLine = std::stringstream();
    //Add simulation timestamp, timestamp relative, simulation time to recording start
    keyframeLine << "script ";
    keyframeLine << sm._timestamp << " ";
    keyframeLine << (sm._timestamp - _timestampRecordStarted) << " ";
    keyframeLine << std::fixed << std::setprecision(3) << global::timeManager.time().j2000Seconds();
    keyframeLine << " ";
    keyframeLine << scriptToSave;

    saveKeyframeToFile(keyframeLine.str());
#endif //#ifdef RECORD_BINARY
}

void SessionRecording::preSynchronization() {
    if( _state == sessionState::recording ) {
        saveCameraKeyframe();
#ifdef SESSION_RECORDING_TIME
        saveTimeKeyframe();
#endif //SESSION_RECORDING_TIME
    }
}

bool SessionRecording::isRecording() {
    return (_state == sessionState::recording);
}

void SessionRecording::playbackAddEntriesToTimeline() {
#ifdef RECORD_BINARY
    unsigned char frameType;
    bool fileReadOk = true;

    while (fileReadOk) {
        readFromPlayback(frameType);
        //Check if have reached EOF
        if (!_playbackFile) {
            LINFO(fmt::format("Finished parsing {} entries from playback file {}",
                _playbackLineNum, _playbackFilename.c_str()));
            fileReadOk = false;
            break;
        }
        if (frameType == 'c') {
            playbackCamera();
#ifdef SESSION_RECORDING_TIME
        }
        else if (frameType == 't') {
            playbackTimeChange();
#endif //#ifdef SESSION_RECORDING_TIME
        }
        else if (frameType == 's') {
            playbackScript();
        }
        else {
            LERROR(fmt::format("Unknown frame type {} @ index {} of playback file {}",
                frameType, _playbackIndex, _playbackFilename.c_str()));
            break;
        }

        _playbackLineNum++;
    }
#else
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
#ifdef SESSION_RECORDING_TIME
        }
        else if (entryType.compare("time") == 0) {
            playbackTimeChange();
#endif
        }
        else if (entryType.compare("script") == 0) {
            playbackScript();
        }
        else {
            LERROR(fmt::format("Unknown frame type {} @ line {} of playback file {}",
                entryType, _playbackLineNum, _playbackFilename.c_str()));
            break;
        }
    }
    LINFO(fmt::format("Finished parsing {} entries from playback file {}",
        _playbackLineNum, _playbackFilename.c_str()));
#endif //#ifdef RECORD_BINARY
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
    
void SessionRecording::playbackCamera() {
    double timeRef;
    double timeOs, timeRec, timeSim;
    std::string rotationFollowing;
    interaction::KeyframeNavigator::CameraPose pbFrame;
#ifdef RECORD_BINARY
    double tmpRotation;
    //LINFO(fmt::format("Reading frame #{} for camera", _playbackLineNum - 1));
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
#else
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
    if( iss.fail() || !iss.eof() ) {
        LERROR(fmt::format("Error parsing camera line {} of playback file", _playbackLineNum));
        return;
    }
    if (rotationFollowing.compare("F") == 0)
        pbFrame.followFocusNodeRotation = true;
    else
        pbFrame.followFocusNodeRotation = false;
#endif //#ifdef RECORD_BINARY
    timeRef = getAppropriateTimestamp(timeOs, timeRec, timeSim);

    global::navigationHandler.keyframeNavigator().addKeyframe(timeRef, pbFrame);
}

#ifdef SESSION_RECORDING_TIME
void SessionRecording::playbackTimeChange() {
    double timeOs, timeRec, timeSim;
    datamessagestructures::TimeKeyframe pbFrame;
#ifdef RECORD_BINARY
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
#else
    std::istringstream iss(_playbackLineParsing);
    std::string entryType;
    //double timeRef;
    std::string paused, jump;
    iss >> entryType;
    iss >> timeOs >> timeRec >> timeSim;
    iss >> pbFrame._dt
        >> paused
        >> jump;
    if( iss.fail() || !iss.eof() ) {
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
#endif //#ifdef RECORD_BINARY
    pbFrame._timestamp = getEquivalentApplicationTime(timeOs, timeRec, timeSim);

    pbFrame._time = pbFrame._timestamp + _timestampApplicationStarted_simulation;
    //global::timeManager.addKeyframe(timeRef, pbFrame._timestamp);
    _externInteract.timeInteraction(pbFrame);
}
#endif

void SessionRecording::playbackScript() {
    double timeRef;
    double timeOs, timeRec, timeSim;
    datamessagestructures::ScriptMessage pbFrame;
#ifdef RECORD_BINARY
    readFromPlayback(timeOs);
    readFromPlayback(timeRec);
    readFromPlayback(timeSim);
    readFromPlayback(pbFrame._script);
    if (!_playbackFile) {
        LERROR(fmt::format("Error reading script playback from keyframe entry {}",
            _playbackLineNum - 1));
        return;
    }
#else
    std::istringstream iss(_playbackLineParsing);
    std::string entryType;
    iss >> entryType;
    iss >> timeOs >> timeRec >> timeSim;
    getline(iss, pbFrame._script);
    if( iss.fail() || !iss.eof() ) {
        LERROR(fmt::format("Error parsing script line {} of playback file", _playbackLineNum));
        return;
    }
#endif //#ifdef RECORD_BINARY
    timeRef = getEquivalentSimulationTime(timeOs, timeRec, timeSim);

    //Call script scheduler with this new script entry
    std::string timeDescription = SpiceManager::ref().dateFromEphemerisTime(timeRef, "YYYY MON DD HR:MN:SC.###");

    ghoul::Dictionary scriptDict(ghoul::Dictionary{ {KeyTime, timeDescription },
                                                    {KeyForwardScript, pbFrame._script} }
                                );
    global::scriptScheduler.loadScripts({ { "1", scriptDict } });
}

#ifdef RECORD_BINARY
void SessionRecording::saveKeyframeToFileBinary() {
    _recordFile.write((char*)_keyframeBuffer, _bufferIndex);
}
#else
void SessionRecording::saveKeyframeToFile(std::string entry) {
    _recordFile << entry << std::endl;
}
#endif //#ifdef RECORD_BINARY

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
                "for the file where the recorded keyframes are saved"
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
