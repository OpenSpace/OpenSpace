/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#include <openspace/interaction/sessionrecording.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/interaction/keyframenavigator.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/interaction/orbitalnavigator.h>
#include <openspace/rendering/luaconsole.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/scripting/scriptscheduler.h>
#include <openspace/util/camera.h>
#include <openspace/util/timemanager.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>
#include <iomanip>

namespace {
    constexpr const char* _loggerCat = "SessionRecording";

    constexpr const bool UsingTimeKeyframes = false;
    const std::string FileHeaderTitle = "OpenSpace_record/playback";
    constexpr const size_t FileHeaderVersionLength = 5;
    constexpr const char FileHeaderVersion[FileHeaderVersionLength] = {
        '0', '0', '.', '8', '5'
    };
    constexpr const char DataFormatAsciiTag = 'A';
    constexpr const char DataFormatBinaryTag = 'B';


    template <typename T>
    T readFromPlayback(std::ifstream& stream) {
        T res;
        stream.read(reinterpret_cast<char*>(&res), sizeof(T));
        return res;
    }

    template <>
    bool readFromPlayback(std::ifstream& stream) {
        unsigned char b;
        stream.read(reinterpret_cast<char*>(&b), sizeof(unsigned char));
        if (b == 0) {
            return false;
        }
        else {
            return true;
        }
    }

    template <>
    std::string readFromPlayback(std::ifstream& stream) {
        size_t strLen;
        // Read string length from file
        stream.read(reinterpret_cast<char*>(&strLen), sizeof(strLen));
        // Read back full string
        std::vector<char> temp(strLen + 1);
        stream.read(temp.data(), strLen);
        temp[strLen] = '\0';
        return temp.data();
    }

    std::string readHeaderElement(std::ifstream& stream, size_t readLen_chars) {
        std::vector<char> readTemp(readLen_chars);
        stream.read(&readTemp[0], readLen_chars);
        return std::string(readTemp.begin(), readTemp.end());
    }

} // namespace

#include "sessionrecording_lua.inl"

namespace openspace::interaction {

SessionRecording::SessionRecording()
    : properties::PropertyOwner({ "SessionRecording", "Session Recording" })
{}

SessionRecording::~SessionRecording() {} // NOLINT

void SessionRecording::deinitialize() {
    stopRecording();
    stopPlayback();
}

void SessionRecording::setRecordDataFormat(RecordedDataMode dataMode) {
    _recordingDataMode = dataMode;
}

bool SessionRecording::startRecording(const std::string& filename) {
    if (filename.find("/") != std::string::npos) {
        LERROR("Recording filename must not contain path (/) elements");
        return false;
    }
    if (!FileSys.directoryExists(absPath("${RECORDINGS}"))) {
        FileSys.createDirectory(
            absPath("${RECORDINGS}"),
            ghoul::filesystem::FileSystem::Recursive::Yes
        );
    }
    const std::string absFilename = absPath("${RECORDINGS}/" + filename);

    if (FileSys.fileExists(absFilename)) {
        LERROR(fmt::format(
            "Unable to start recording; file {} already exists.", absFilename.c_str()
        ));
        return false;
    }
    if (_state == SessionState::Recording) {
        LERROR("Unable to start recording while already in recording mode");
        return false;
    }
    else if (_state == SessionState::Playback) {
        LERROR("Unable to start recording while in session playback mode");
        return false;
    }

    _state = SessionState::Recording;
    _playbackActive_camera = false;
    _playbackActive_time = false;
    _playbackActive_script = false;
    if (_recordingDataMode == RecordedDataMode::Binary) {
        _recordFile.open(absFilename, std::ios::binary);
    }
    else {
        _recordFile.open(absFilename);
    }

    if (!_recordFile.is_open() || !_recordFile.good()) {
        LERROR(fmt::format(
            "Unable to open file {} for keyframe recording", absFilename.c_str()
        ));
        return false;
    }
    _recordFile << FileHeaderTitle;
    _recordFile.write(FileHeaderVersion, FileHeaderVersionLength);
    if (_recordingDataMode == RecordedDataMode::Binary) {
        _recordFile << DataFormatBinaryTag;
    }
    else {
        _recordFile << DataFormatAsciiTag;
    }
    _recordFile << '\n';

    LINFO("Session recording started");
    _timestampRecordStarted = global::windowDelegate.applicationTime();
    return true;
}

void SessionRecording::stopRecording() {
    if (_state == SessionState::Recording) {
        _state = SessionState::Idle;
        LINFO("Session recording stopped");
    }
    // Close the recording file
    _recordFile.close();
}

bool SessionRecording::startPlayback(const std::string& filename,
                                     KeyframeTimeRef timeMode, bool forceSimTimeAtStart)
{
    if (filename.find("/") != std::string::npos) {
        LERROR("Playback filename must not contain path (/) elements");
        return false;
    }
    const std::string absFilename = absPath("${RECORDINGS}/" + filename);

    if (_state == SessionState::Recording) {
        LERROR("Unable to start playback while in session recording mode");
        return false;
    }
    else if (_state == SessionState::Playback) {
        LERROR("Unable to start new playback while in session playback mode");
        return false;
    }

    if (!FileSys.fileExists(absFilename)) {
        LERROR("Cannot find the specified playback file.");
        cleanUpPlayback();
        return false;
    }

    _playbackLineNum = 1;
    _playbackFilename = absFilename;

    // Open in ASCII first
    _playbackFile.open(_playbackFilename, std::ifstream::in);
    // Read header
    std::string readBackHeaderString = readHeaderElement(
        _playbackFile,
        FileHeaderTitle.length()
    );
    if (readBackHeaderString != FileHeaderTitle) {
        LERROR("Specified playback file does not contain expected header.");
        cleanUpPlayback();
        return false;
    }
    readHeaderElement(_playbackFile, FileHeaderVersionLength);
    std::string readDataMode = readHeaderElement(_playbackFile, 1);
    if (readDataMode[0] == DataFormatAsciiTag) {
        _recordingDataMode = RecordedDataMode::Ascii;
    }
    else if (readDataMode[0] == DataFormatBinaryTag) {
        _recordingDataMode = RecordedDataMode::Binary;
    }
    else {
        LERROR("Unknown data type in header (should be Ascii or Binary)");
        cleanUpPlayback();
    }
    std::string throwawayNewlineChar = readHeaderElement(_playbackFile, 1);

    if (_recordingDataMode == RecordedDataMode::Binary) {
        //Close & re-open the file, starting from the beginning, and do dummy read
        // past the header, version, and data type
        _playbackFile.close();
        _playbackFile.open(_playbackFilename, std::ifstream::in | std::ios::binary);
        size_t headerSize = FileHeaderTitle.length() + FileHeaderVersionLength +
                            sizeof(DataFormatBinaryTag) + sizeof('\n');
        _playbackFile.read(reinterpret_cast<char*>(&_keyframeBuffer), headerSize);
    }

    if (!_playbackFile.is_open() || !_playbackFile.good()) {
        LERROR(fmt::format(
            "Unable to open file {} for keyframe playback", absFilename.c_str()
        ));
        stopPlayback();
        cleanUpPlayback();
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
    if (UsingTimeKeyframes) {
        _playbackActive_time = true;
    }

    global::navigationHandler.keyframeNavigator().setTimeReferenceMode(timeMode, now);
    global::scriptScheduler.setTimeReferenceMode(timeMode);

    _setSimulationTimeWithNextCameraKeyframe = forceSimTimeAtStart;
    if (!playbackAddEntriesToTimeline()) {
        cleanUpPlayback();
        return false;
    }

    _hasHitEndOfCameraKeyframes = false;
    findFirstCameraKeyframeInTimeline();

    LINFO(fmt::format(
        "Playback session started: ({:8.3f},0.0,{:13.3f}) with {}/{}/{} entries, "
        "forceTime={}",
        now, _timestampPlaybackStarted_simulation, _keyframesCamera.size(),
        _keyframesTime.size(), _keyframesScript.size(), (forceSimTimeAtStart ? 1 : 0)
    ));

    global::navigationHandler.triggerPlaybackStart();
    global::scriptScheduler.triggerPlaybackStart();
    global::timeManager.triggerPlaybackStart();
    _state = SessionState::Playback;

    return true;
}

void SessionRecording::findFirstCameraKeyframeInTimeline() {
    bool foundCameraKeyframe = false;
    for (unsigned int i = 0; i < _timeline.size(); i++) {
        if (doesTimelineEntryContainCamera(i)) {
            _idxTimeline_cameraFirstInTimeline = i;
            _idxTimeline_cameraPtrPrev = _idxTimeline_cameraFirstInTimeline;
            _idxTimeline_cameraPtrNext = _idxTimeline_cameraFirstInTimeline;
            _cameraFirstInTimeline_timestamp
                = _timeline[_idxTimeline_cameraFirstInTimeline].timestamp;
            foundCameraKeyframe = true;
            break;
        }
    }

    if (!foundCameraKeyframe) {
        signalPlaybackFinishedForComponent(RecordedType::Camera);
    }
}

void SessionRecording::signalPlaybackFinishedForComponent(RecordedType type) {
    if (type == RecordedType::Camera) {
        _playbackActive_camera = false;
        LINFO("Playback finished signal: camera");
    }
    else if (type == RecordedType::Time) {
        _playbackActive_time = false;
        LINFO("Playback finished signal: time");
    }
    else if (type == RecordedType::Script) {
        _playbackActive_script = false;
        LINFO("Playback finished signal: script");
    }

    if (!_playbackActive_camera && !_playbackActive_time && !_playbackActive_script) {
        _state = SessionState::Idle;
        _cleanupNeeded = true;
        LINFO("Playback session finished");
    }
}

void SessionRecording::enableTakeScreenShotDuringPlayback(int fps) {
    _saveRenderingDuringPlayback = true;
    _saveRenderingDeltaTime = 1.0 / fps;
}

void SessionRecording::disableTakeScreenShotDuringPlayback() {
    _saveRenderingDuringPlayback = false;
}

void SessionRecording::stopPlayback() {
    if (_state == SessionState::Playback) {
        _state = SessionState::Idle;
        _cleanupNeeded = true;
        LINFO("Session playback stopped");
    }
}

void SessionRecording::cleanUpPlayback() {
    global::navigationHandler.stopPlayback();

    Camera* camera = global::navigationHandler.camera();
    ghoul_assert(camera != nullptr, "Camera must not be nullptr");
    Scene* scene = camera->parent()->scene();
    if (!_timeline.empty()) {
        unsigned int p = _timeline[_idxTimeline_cameraPtrPrev].idxIntoKeyframeTypeArray;
        const SceneGraphNode* node = scene->sceneGraphNode(_keyframesCamera[p].focusNode);
        if (node) {
            global::navigationHandler.orbitalNavigator().setFocusNode(node->identifier());
        }
    }
    global::scriptScheduler.stopPlayback();

    _playbackFile.close();

    // Clear all timelines and keyframes
    _timeline.clear();
    _keyframesCamera.clear();
    _keyframesTime.clear();
    _keyframesScript.clear();
    _idxTimeline_nonCamera = 0;
    _idxTime = 0;
    _idxScript = 0;
    _idxTimeline_cameraPtrNext = 0;
    _idxTimeline_cameraPtrPrev = 0;
    _hasHitEndOfCameraKeyframes = false;
    _saveRenderingDuringPlayback = false;

    _cleanupNeeded = false;
}

void SessionRecording::writeToFileBuffer(double src) {
    const size_t writeSize_bytes = sizeof(double);
    unsigned char const *p = reinterpret_cast<unsigned char const*>(&src);
    memcpy((_keyframeBuffer + _bufferIndex), p, writeSize_bytes);
    _bufferIndex += writeSize_bytes;
}

void SessionRecording::writeToFileBuffer(std::vector<char>& cvec) {
    const size_t writeSize_bytes = cvec.size() * sizeof(char);
    memcpy((_keyframeBuffer + _bufferIndex), cvec.data(), writeSize_bytes);
    _bufferIndex += writeSize_bytes;
}

void SessionRecording::writeToFileBuffer(unsigned char c) {
    const size_t writeSize_bytes = sizeof(char);
    _keyframeBuffer[_bufferIndex] = c;
    _bufferIndex += writeSize_bytes;
}

void SessionRecording::writeToFileBuffer(bool b) {
    _keyframeBuffer[_bufferIndex] = b ? 1 : 0;
    _bufferIndex += sizeof(char);
}

void SessionRecording::saveStringToFile(const std::string& s) {
    size_t strLen = s.size();
    size_t writeSize_bytes = sizeof(size_t);

    _bufferIndex = 0;
    unsigned char const *p = reinterpret_cast<unsigned char const*>(&strLen);
    memcpy((_keyframeBuffer + _bufferIndex), p, writeSize_bytes);
    _bufferIndex += static_cast<unsigned int>(writeSize_bytes);
    saveKeyframeToFileBinary(_keyframeBuffer, _bufferIndex);

    _recordFile.write(s.c_str(), s.size());
}

bool SessionRecording::hasCameraChangedFromPrev(
                                              datamessagestructures::CameraKeyframe kfNew)
{
    constexpr const  double threshold = 1e-2;
    bool hasChanged = false;

    glm::dvec3 positionDiff = kfNew._position - _prevRecordedCameraKeyframe._position;
    if (glm::length(positionDiff) > threshold) {
        hasChanged = true;
    }

    double rotationDiff = dot(kfNew._rotation, _prevRecordedCameraKeyframe._rotation);
    if (std::abs(rotationDiff - 1.0) > threshold) {
        hasChanged = true;
    }

    _prevRecordedCameraKeyframe = kfNew;
    return hasChanged;
}

void SessionRecording::saveCameraKeyframe() {
    if (_state != SessionState::Recording) {
        return;
    }

    const SceneGraphNode* an = global::navigationHandler.orbitalNavigator().anchorNode();
    if (!an) {
        return;
    }

    // Create a camera keyframe, then call to populate it with current position
    // & orientation of camera
    datamessagestructures::CameraKeyframe kf = _externInteract.generateCameraKeyframe();

    if (_recordingDataMode == RecordedDataMode::Binary) {
        // Writing to a binary session recording file
        _bufferIndex = 0;
        _keyframeBuffer[_bufferIndex++] = 'c';

        // Writing to internal buffer, and then to file, for performance reasons
        writeToFileBuffer(kf._timestamp);
        writeToFileBuffer(kf._timestamp - _timestampRecordStarted);
        writeToFileBuffer(global::timeManager.time().j2000Seconds());
        std::vector<char> kfBuffer;
        kf.serialize(kfBuffer);
        writeToFileBuffer(kfBuffer);

        saveKeyframeToFileBinary(_keyframeBuffer, _bufferIndex);
    }
    else {
        // Writing to an ASCII session recording file
        std::stringstream keyframeLine = std::stringstream();
        // Add simulation timestamp, timestamp relative, simulation time to recording
        // start
        keyframeLine << "camera ";
        keyframeLine << kf._timestamp << ' ';
        keyframeLine << (kf._timestamp - _timestampRecordStarted) << ' ';
        keyframeLine << std::fixed << std::setprecision(3) <<
                        global::timeManager.time().j2000Seconds();
        keyframeLine << ' ';
        // Add camera position
        keyframeLine << std::fixed << std::setprecision(7) << kf._position.x << ' '
            << std::fixed << std::setprecision(7) << kf._position.y << ' '
            << std::fixed << std::setprecision(7) << kf._position.z << ' ';
        // Add camera rotation
        keyframeLine << std::fixed << std::setprecision(7) << kf._rotation.x << ' '
            << std::fixed << std::setprecision(7) << kf._rotation.y << ' '
            << std::fixed << std::setprecision(7) << kf._rotation.z << ' '
            << std::fixed << std::setprecision(7) << kf._rotation.w << ' ';
        keyframeLine << std::scientific << kf._scale << ' ';
        if (kf._followNodeRotation) {
            keyframeLine << "F ";
        }
        else {
            keyframeLine << "- ";
        }
        keyframeLine << kf._focusNode;

        saveKeyframeToFile(keyframeLine.str());
    }
}

void SessionRecording::saveTimeKeyframe() {
    if (_state != SessionState::Recording) {
        return;
    }

    //Create a time keyframe, then call to populate it with current time props
    datamessagestructures::TimeKeyframe kf = _externInteract.generateTimeKeyframe();

    if (_recordingDataMode == RecordedDataMode::Binary) {
        _bufferIndex = 0;
        _keyframeBuffer[_bufferIndex++] = 't';
        writeToFileBuffer(kf._timestamp);
        writeToFileBuffer(kf._timestamp - _timestampRecordStarted);
        writeToFileBuffer(kf._time);
        writeToFileBuffer(kf._dt);
        writeToFileBuffer(kf._paused);
        writeToFileBuffer(kf._requiresTimeJump);

        saveKeyframeToFileBinary(_keyframeBuffer, _bufferIndex);
    }
    else {
        std::stringstream keyframeLine = std::stringstream();
        //Add simulation timestamp, timestamp relative, simulation time to recording start
        keyframeLine << "time ";
        keyframeLine << kf._timestamp << ' ';
        keyframeLine << (kf._timestamp - _timestampRecordStarted) << ' ';

        keyframeLine << std::fixed << std::setprecision(3) << kf._time;

        keyframeLine << ' ' << kf._dt;
        if (kf._paused) {
            keyframeLine << " P";
        }
        else {
            keyframeLine << " R";
        }
        if (kf._requiresTimeJump) {
            keyframeLine << " J";
        }
        else {
            keyframeLine << " -";
        }
        saveKeyframeToFile(keyframeLine.str());
    }
}

void SessionRecording::saveScriptKeyframe(std::string scriptToSave) {
    if (_state != SessionState::Recording) {
        return;
    }

    datamessagestructures::ScriptMessage sm
        = _externInteract.generateScriptMessage(scriptToSave);

    if (_recordingDataMode == RecordedDataMode::Binary) {
        _bufferIndex = 0;
        _keyframeBuffer[_bufferIndex++] = 's';
        writeToFileBuffer(sm._timestamp);
        writeToFileBuffer(sm._timestamp - _timestampRecordStarted);
        writeToFileBuffer(global::timeManager.time().j2000Seconds());
        //Write header to file
        saveKeyframeToFileBinary(_keyframeBuffer, _bufferIndex);

        saveStringToFile(scriptToSave);
    }
    else {
        unsigned int numLinesInScript = static_cast<unsigned int>(
            std::count(scriptToSave.begin(), scriptToSave.end(), '\n')
        );
        std::stringstream keyframeLine = std::stringstream();
        //Add simulation timestamp, timestamp relative, simulation time to recording start
        keyframeLine << "script ";
        keyframeLine << sm._timestamp << ' ';
        keyframeLine << (sm._timestamp - _timestampRecordStarted) << ' ';
        keyframeLine << std::fixed << std::setprecision(3) <<
                        global::timeManager.time().j2000Seconds();
        keyframeLine << ' ';
        keyframeLine << (numLinesInScript + 1) << ' ';
        keyframeLine << scriptToSave;

        saveKeyframeToFile(keyframeLine.str());
    }
}

void SessionRecording::preSynchronization() {
    ZoneScoped

    if (_state == SessionState::Recording) {
        saveCameraKeyframe();
        if (UsingTimeKeyframes) {
            saveTimeKeyframe();
        }
    }
    else if (_state == SessionState::Playback) {
        moveAheadInTime();
    }
    else if (_cleanupNeeded) {
        cleanUpPlayback();
    }

    //Handle callback(s) for change in idle/record/playback state
    if (_state != _lastState) {
        using K = const CallbackHandle;
        using V = StateChangeCallback;
        for (const std::pair<K, V>& it : _stateChangeCallbacks) {
            it.second();
        }
    }
    _lastState = _state;
}

bool SessionRecording::isRecording() const {
    return (_state == SessionState::Recording);
}

bool SessionRecording::isPlayingBack() const {
    return (_state == SessionState::Playback);
}

bool SessionRecording::isSavingFramesDuringPlayback() const {
    return (_state == SessionState::Playback && _saveRenderingDuringPlayback);
}

SessionRecording::SessionState SessionRecording::state() const {
    return _state;
}

bool SessionRecording::playbackAddEntriesToTimeline() {
    bool parsingErrorsFound = false;

    if (_recordingDataMode == RecordedDataMode::Binary) {
        unsigned char frameType;
        bool fileReadOk = true;

        while (fileReadOk) {
            frameType = readFromPlayback<unsigned char>(_playbackFile);
            // Check if have reached EOF
            if (!_playbackFile) {
                LINFO(fmt::format(
                    "Finished parsing {} entries from playback file {}",
                    _playbackLineNum - 1, _playbackFilename
                ));
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
                LERROR(fmt::format(
                    "Unknown frame type {} @ index {} of playback file {}",
                    frameType, _playbackLineNum - 1, _playbackFilename
                ));
                parsingErrorsFound = true;
                break;
            }

            _playbackLineNum++;
        }
    }
    else {
        while (std::getline(_playbackFile, _playbackLineParsing)) {
            _playbackLineNum++;

            std::istringstream iss(_playbackLineParsing);
            std::string entryType;
            if (!(iss >> entryType)) {
                LERROR(fmt::format(
                    "Error reading entry type @ line {} of playback file {}",
                    _playbackLineNum, _playbackFilename
                ));
                break;
            }

            if (entryType == "camera") {
                playbackCamera();
            }
            else if (entryType == "time") {
                playbackTimeChange();
            }
            else if (entryType == "script") {
                playbackScript();
            }
            else {
                LERROR(fmt::format(
                    "Unknown frame type {} @ line {} of playback file {}",
                    entryType, _playbackLineNum, _playbackFilename
                ));
                parsingErrorsFound = true;
                break;
            }
        }
        LINFO(fmt::format(
            "Finished parsing {} entries from playback file {}",
            _playbackLineNum, _playbackFilename
        ));
    }

    return !parsingErrorsFound;
}

double SessionRecording::appropriateTimestamp(double timeOs, double timeRec,
                                              double timeSim)
{
    if (_playbackTimeReferenceMode == KeyframeTimeRef::Relative_recordedStart) {
        return timeRec;
    }
    else if (_playbackTimeReferenceMode == KeyframeTimeRef::Absolute_simTimeJ2000) {
        return timeSim;
    }
    else {
        return timeOs;
    }
}

double SessionRecording::equivalentSimulationTime(double timeOs, double timeRec,
                                                  double timeSim)
{
    if (_playbackTimeReferenceMode == KeyframeTimeRef::Relative_recordedStart) {
        return _timestampPlaybackStarted_simulation + timeRec;
    }
    else if (_playbackTimeReferenceMode == KeyframeTimeRef::Relative_applicationStart) {
        return _timestampApplicationStarted_simulation + timeOs;
    }
    else {
        return timeSim;
    }
}

double SessionRecording::equivalentApplicationTime(double timeOs, double timeRec,
                                                   double timeSim)
{
    if (_playbackTimeReferenceMode == KeyframeTimeRef::Relative_recordedStart) {
        return _timestampPlaybackStarted_application + timeRec;
    }
    else if (_playbackTimeReferenceMode == KeyframeTimeRef::Absolute_simTimeJ2000) {
        return timeSim - _timestampApplicationStarted_simulation;
    }
    else {
        return timeOs;
    }
}

double SessionRecording::currentTime() const {
    if (isSavingFramesDuringPlayback()) {
        return _saveRenderingCurrentRecordedTime;
    }
    else if (_playbackTimeReferenceMode == KeyframeTimeRef::Relative_recordedStart) {
        return (global::windowDelegate.applicationTime() -
                _timestampPlaybackStarted_application);
    }
    else if (_playbackTimeReferenceMode == KeyframeTimeRef::Absolute_simTimeJ2000) {
        return global::timeManager.time().j2000Seconds();
    }
    else {
        return global::windowDelegate.applicationTime();
    }
}

double SessionRecording::fixedDeltaTimeDuringFrameOutput() const {
    // Check if renderable in focus is still resolving tile loading
    // do not adjust time while we are doing this
    const SceneGraphNode* focusNode =
        global::navigationHandler.orbitalNavigator().anchorNode();
    const Renderable* focusRenderable = focusNode->renderable();
    if (!focusRenderable || focusRenderable->renderedWithDesiredData()) {
        return _saveRenderingDeltaTime;
    }
    else {
        return 0;
    }
}

void SessionRecording::playbackCamera() {
    double timeOs;
    double timeRec;
    double timeSim;
    std::string rotationFollowing;
    interaction::KeyframeNavigator::CameraPose pbFrame;
    datamessagestructures::CameraKeyframe kf;
    if (_recordingDataMode == RecordedDataMode::Binary) {
        timeOs = readFromPlayback<double>(_playbackFile);
        timeRec = readFromPlayback<double>(_playbackFile);
        timeSim = readFromPlayback<double>(_playbackFile);
        try {
            kf.read(&_playbackFile);
        }
        catch (std::bad_alloc&) {
            LERROR(fmt::format(
                "Allocation error with camera playback from keyframe entry {}",
                _playbackLineNum - 1
            ));
            return;
        }
        catch (std::length_error&) {
            LERROR(fmt::format(
                "length_error with camera playback from keyframe entry {}",
                _playbackLineNum - 1
            ));
            return;
        }

        timeOs = kf._timestamp;

        pbFrame.focusNode = kf._focusNode;
        pbFrame.position = kf._position;
        pbFrame.rotation = kf._rotation;
        pbFrame.scale = kf._scale;
        pbFrame.followFocusNodeRotation = kf._followNodeRotation;

        if (!_playbackFile) {
            LINFO(fmt::format(
                "Error reading camera playback from keyframe entry {}",
                _playbackLineNum - 1
            ));
            return;
        }
    }
    else {
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
            >> pbFrame.scale
            >> rotationFollowing
            >> pbFrame.focusNode;
        if (iss.fail() || !iss.eof()) {
            LERROR(fmt::format(
                "Error parsing camera line {} of playback file", _playbackLineNum
            ));
            return;
        }
        pbFrame.followFocusNodeRotation = (rotationFollowing == "F");
    }
    if (_setSimulationTimeWithNextCameraKeyframe) {
        global::timeManager.setTimeNextFrame(Time(timeSim));
        _setSimulationTimeWithNextCameraKeyframe = false;
        _saveRenderingCurrentRecordedTime = timeRec;
    }
    double timeRef = appropriateTimestamp(timeOs, timeRec, timeSim);

    //global::navigationHandler.keyframeNavigator().addKeyframe(timeRef, pbFrame);
    addKeyframe(timeRef, pbFrame);
}

void SessionRecording::playbackTimeChange() {
    double timeOs;
    double timeRec;
    double timeSim;
    datamessagestructures::TimeKeyframe pbFrame;
    if (_recordingDataMode == RecordedDataMode::Binary) {
        timeOs = readFromPlayback<double>(_playbackFile);
        timeRec = readFromPlayback<double>(_playbackFile);
        timeSim = readFromPlayback<double>(_playbackFile);
        pbFrame._dt = readFromPlayback<double>(_playbackFile);
        pbFrame._paused = readFromPlayback<bool>(_playbackFile);
        pbFrame._requiresTimeJump = readFromPlayback<bool>(_playbackFile);

        if (!_playbackFile) {
            LERROR(fmt::format(
                "Error reading time playback from keyframe entry {}", _playbackLineNum - 1
            ));
            return;
        }
    }
    else {
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
            LERROR(fmt::format(
                "Error parsing time line {} of playback file", _playbackLineNum
            ));
            return;
        }
        pbFrame._paused = (paused == "P");
        pbFrame._requiresTimeJump = (jump == "J");
    }
    pbFrame._timestamp = equivalentApplicationTime(timeOs, timeRec, timeSim);

    pbFrame._time = pbFrame._timestamp + _timestampApplicationStarted_simulation;
    //global::timeManager.addKeyframe(timeRef, pbFrame._timestamp);
    //_externInteract.timeInteraction(pbFrame);
    addKeyframe(pbFrame._timestamp, pbFrame);
}

void SessionRecording::playbackScript() {
    double timeOs;
    double timeRec;
    double timeSim;
    unsigned int numScriptLines;
    datamessagestructures::ScriptMessage pbFrame;

    if (_recordingDataMode == RecordedDataMode::Binary) {
        timeOs = readFromPlayback<double>(_playbackFile);
        timeRec = readFromPlayback<double>(_playbackFile);
        timeSim = readFromPlayback<double>(_playbackFile);
        try {
            pbFrame._script = readFromPlayback<std::string>(_playbackFile);
        }
        catch (std::bad_alloc&) {
            LERROR(fmt::format(
                "Allocation error with script playback from keyframe entry {}",
                _playbackLineNum - 1
            ));
            return;
        }
        catch (std::length_error&) {
            LERROR(fmt::format(
                "length_error with script playback from keyframe entry {}",
                _playbackLineNum - 1
            ));
            return;
        }

        if (!_playbackFile) {
            LERROR(fmt::format(
                "Error reading script playback from keyframe entry {}",
                _playbackLineNum - 1
            ));
            return;
        }
    }
    else {
        std::istringstream iss(_playbackLineParsing);
        std::string entryType;
        std::string tmpReadbackScript;

        iss >> entryType;
        iss >> timeOs >> timeRec >> timeSim;
        iss >> numScriptLines;
        std::getline(iss, tmpReadbackScript); //iss >> tmpReadbackScript;
        pbFrame._script.append(tmpReadbackScript);
        if (iss.fail()) {
            LERROR(fmt::format(
                "Error parsing script line {} of playback file", _playbackLineNum
            ));
            return;
        }
        else if (!iss.eof()) {
            LERROR(fmt::format(
                "Did not find an EOL at line {} of playback file", _playbackLineNum
            ));
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
    double timeRef = appropriateTimestamp(timeOs, timeRec, timeSim);
    //timeRef = getEquivalentSimulationTime(timeOs, timeRec, timeSim);

    //Call script scheduler with this new script entry
    //std::string timeDescription = SpiceManager::ref().dateFromEphemerisTime(
    //  timeRef,
    //  "YYYY MON DD HR:MN:SC.###"
    //);
    //ghoul::Dictionary scriptDict(ghoul::Dictionary{
    //  { KeyTime, timeDescription },
    //  { KeyForwardScript, pbFrame._script}
    //}
    //                            );
    //global::scriptScheduler.loadScripts({ { "1", scriptDict } });
    addKeyframe(timeRef, pbFrame._script);
}

void SessionRecording::addKeyframe(double timestamp,
                                   interaction::KeyframeNavigator::CameraPose keyframe)
{
    size_t indexIntoCameraKeyframesFromMainTimeline = _keyframesCamera.size();
    _keyframesCamera.push_back(std::move(keyframe));
    _timeline.push_back({
        RecordedType::Camera,
        static_cast<unsigned int>(indexIntoCameraKeyframesFromMainTimeline),
        timestamp
    });
}

void SessionRecording::addKeyframe(double timestamp,
                                   datamessagestructures::TimeKeyframe keyframe)
{
    size_t indexIntoTimeKeyframesFromMainTimeline = _keyframesTime.size();
    _keyframesTime.push_back(std::move(keyframe));
    _timeline.push_back({
        RecordedType::Time,
        static_cast<unsigned int>(indexIntoTimeKeyframesFromMainTimeline),
        timestamp
    });
}

void SessionRecording::addKeyframe(double timestamp, std::string scriptToQueue) {
    size_t indexIntoScriptKeyframesFromMainTimeline = _keyframesScript.size();
    _keyframesScript.push_back(std::move(scriptToQueue));
    _timeline.push_back({
        RecordedType::Script,
        static_cast<unsigned int>(indexIntoScriptKeyframesFromMainTimeline),
        timestamp
    });
}

void SessionRecording::moveAheadInTime() {
    double currTime = currentTime();
    lookForNonCameraKeyframesThatHaveComeDue(currTime);
    updateCameraWithOrWithoutNewKeyframes(currTime);
    if (isSavingFramesDuringPlayback()) {
        // Check if renderable in focus is still resolving tile loading
        // do not adjust time while we are doing this, or take screenshot
        const SceneGraphNode* focusNode =
            global::navigationHandler.orbitalNavigator().anchorNode();
        const Renderable* focusRenderable = focusNode->renderable();
        if (!focusRenderable || focusRenderable->renderedWithDesiredData()) {
            _saveRenderingCurrentRecordedTime += _saveRenderingDeltaTime;
            global::renderEngine.takeScreenshot();
        }
    }
}

void SessionRecording::lookForNonCameraKeyframesThatHaveComeDue(double currTime) {
    while (isTimeToHandleNextNonCameraKeyframe(currTime)) {
        if (!processNextNonCameraKeyframeAheadInTime()) {
            break;
        }

        if (++_idxTimeline_nonCamera >= _timeline.size()) {
            _idxTimeline_nonCamera--;
            if (_playbackActive_time) {
                signalPlaybackFinishedForComponent(RecordedType::Time);
            }
            if (_playbackActive_script) {
                signalPlaybackFinishedForComponent(RecordedType::Script);
            }
            break;
        }
    }
}

void SessionRecording::updateCameraWithOrWithoutNewKeyframes(double currTime) {
    if (!_playbackActive_camera) {
        return;
    }

    bool didFindFutureCameraKeyframes = findNextFutureCameraIndex(currTime);

    bool isPrevAtFirstKeyframe = (_idxTimeline_cameraPtrPrev ==
                                  _idxTimeline_cameraFirstInTimeline);
    bool isFirstTimelineCameraKeyframeInFuture = (currTime <
                                                  _cameraFirstInTimeline_timestamp);

    if (! (isPrevAtFirstKeyframe && isFirstTimelineCameraKeyframeInFuture)) {
        processCameraKeyframe(currTime);
    }
    if (!didFindFutureCameraKeyframes) {
        signalPlaybackFinishedForComponent(RecordedType::Camera);
    }
}

bool SessionRecording::isTimeToHandleNextNonCameraKeyframe(double currTime) {
    bool isNonCameraPlaybackActive = (_playbackActive_time || _playbackActive_script);
    return (currTime > getNextTimestamp()) && isNonCameraPlaybackActive;
}

bool SessionRecording::findNextFutureCameraIndex(double currTime) {
    unsigned int seekAheadIndex = _idxTimeline_cameraPtrPrev;
    while (true) {
        seekAheadIndex++;
        if (seekAheadIndex >= static_cast<unsigned int>(_timeline.size())) {
            seekAheadIndex = static_cast<unsigned int>(_timeline.size()) - 1;
        }

        if (doesTimelineEntryContainCamera(seekAheadIndex)) {
            unsigned int indexIntoCameraKeyframes =
                _timeline[seekAheadIndex].idxIntoKeyframeTypeArray;
            double seekAheadKeyframeTimestamp = _timeline[seekAheadIndex].timestamp;

            if (indexIntoCameraKeyframes >= (_keyframesCamera.size() - 1)) {
                _hasHitEndOfCameraKeyframes = true;
            }

            if (currTime < seekAheadKeyframeTimestamp) {
                if (seekAheadIndex > _idxTimeline_cameraPtrNext) {
                    _idxTimeline_cameraPtrPrev = _idxTimeline_cameraPtrNext;
                    _idxTimeline_cameraPtrNext = seekAheadIndex;
                }
                break;
            }
            else {
                // Force interpolation between consecutive keyframes
                _idxTimeline_cameraPtrPrev = seekAheadIndex;
            }
        }

        double interpolationUpperBoundTimestamp =
            _timeline[_idxTimeline_cameraPtrNext].timestamp;
        if ((currTime > interpolationUpperBoundTimestamp) && _hasHitEndOfCameraKeyframes)
        {
            _idxTimeline_cameraPtrPrev = _idxTimeline_cameraPtrNext;
            return false;
        }

        if (seekAheadIndex == (_timeline.size() - 1)) {
            break;
        }
    }
    return true;
}

bool SessionRecording::doesTimelineEntryContainCamera(unsigned int index) const {
    return (_timeline[index].keyframeType == RecordedType::Camera);
}

bool SessionRecording::processNextNonCameraKeyframeAheadInTime() {
    //LINFO(fmt::format(
    //    "Keyframe at {} frame={} timelineIndex={}",
    //    now, global::renderEngine._frameNumber, _idxTimeline
    //));

    switch (getNextKeyframeType()) {
        case RecordedType::Camera:
            // Just return true since this function no longer handles camera keyframes
            return true;
        case RecordedType::Time:
            _idxTime = _timeline[_idxTimeline_nonCamera].idxIntoKeyframeTypeArray;
            if (_keyframesTime.empty()) {
                return false;
            }
            LINFO("Time keyframe type");
            // TBD: the TimeManager restricts setting time directly
            return false;
        case RecordedType::Script:
            _idxScript = _timeline[_idxTimeline_nonCamera].idxIntoKeyframeTypeArray;
            return processScriptKeyframe();
        default:
            LERROR(fmt::format(
                "Bad keyframe type encountered during playback at index {}.",
                _idxTimeline_nonCamera
            ));
            return false;
    }
}

//void SessionRecording::moveBackInTime() { } //for future use

unsigned int SessionRecording::findIndexOfLastCameraKeyframeInTimeline() {
    unsigned int i = static_cast<unsigned int>(_timeline.size()) - 1;
    for (; i > 0; i--) {
        if (_timeline[i].keyframeType == RecordedType::Camera) {
            break;
        }
    }
    return i;
}

bool SessionRecording::processCameraKeyframe(double now) {
    interaction::KeyframeNavigator::CameraPose nextPose;
    interaction::KeyframeNavigator::CameraPose prevPose;

    unsigned int prevIdx;
    unsigned int nextIdx;
    if (!_playbackActive_camera) {
        return false;
    }
    else if (_keyframesCamera.empty()) {
        return false;
    }
    else {
        prevIdx = _timeline[_idxTimeline_cameraPtrPrev].idxIntoKeyframeTypeArray;
        prevPose = _keyframesCamera[prevIdx];
        nextIdx = _timeline[_idxTimeline_cameraPtrNext].idxIntoKeyframeTypeArray;
        nextPose = _keyframesCamera[nextIdx];
    }

    // getPrevTimestamp();
    double prevTime = _timeline[_idxTimeline_cameraPtrPrev].timestamp;
    // getNextTimestamp();
    double nextTime = _timeline[_idxTimeline_cameraPtrNext].timestamp;

    double t;
    if ((nextTime - prevTime) < 1e-7) {
        t = 0;
    }
    else {
        t = (now - prevTime) / (nextTime - prevTime);
    }

#ifdef INTERPOLATION_DEBUG_PRINT
    LINFOC("prev", std::to_string(prevTime));
    LINFOC("now", std::to_string(prevTime + t));
    LINFOC("next", std::to_string(nextTime));
#endif

    // Need to activly update the focusNode position of the camera in relation to
    // the rendered objects will be unstable and actually incorrect
    Camera* camera = global::navigationHandler.camera();
    Scene* scene = camera->parent()->scene();

    const SceneGraphNode* n = scene->sceneGraphNode(_keyframesCamera[prevIdx].focusNode);

    if (n) {
        global::navigationHandler.orbitalNavigator().setFocusNode(n->identifier());
    }

    return interaction::KeyframeNavigator::updateCamera(
        global::navigationHandler.camera(),
        prevPose,
        nextPose,
        t,
        false
    );
}

bool SessionRecording::processScriptKeyframe() {
    if (!_playbackActive_script) {
        return false;
    }
    else if (_keyframesScript.empty()) {
        return false;
    }
    else {
        std::string nextScript = nextKeyframeObj(
            _idxScript,
            _keyframesScript,
            ([this]() { signalPlaybackFinishedForComponent(RecordedType::Script); })
        );
        global::scriptEngine.queueScript(
            nextScript,
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }

    return true;
}

double SessionRecording::getNextTimestamp() {
    if (_timeline.empty()) {
        return 0.0;
    }
    else if (_idxTimeline_nonCamera < _timeline.size()) {
        return _timeline[_idxTimeline_nonCamera].timestamp;
    }
    else {
        return _timeline.back().timestamp;
    }
}

double SessionRecording::getPrevTimestamp() {
    if (_timeline.empty()) {
        return 0.0;
    }
    else if (_idxTimeline_nonCamera == 0) {
        return _timeline.front().timestamp;
    }
    else if (_idxTimeline_nonCamera < _timeline.size()) {
        return _timeline[_idxTimeline_nonCamera - 1].timestamp;
    }
    else {
        return _timeline.back().timestamp;
    }
}

SessionRecording::RecordedType SessionRecording::getNextKeyframeType() {
    if (_timeline.empty()) {
        return RecordedType::Invalid;
    }
    else if (_idxTimeline_nonCamera < _timeline.size()) {
        return _timeline[_idxTimeline_nonCamera].keyframeType;
    }
    else {
        return _timeline.back().keyframeType;
    }
}

SessionRecording::RecordedType SessionRecording::getPrevKeyframeType() {
    if (_timeline.empty()) {
        return RecordedType::Invalid;
    }
    else if (_idxTimeline_nonCamera < _timeline.size()) {
        if (_idxTimeline_nonCamera > 0) {
            return _timeline[_idxTimeline_nonCamera - 1].keyframeType;
        }
        else {
            return _timeline.front().keyframeType;
        }
    }
    else {
        return _timeline.back().keyframeType;
    }
}

void SessionRecording::saveKeyframeToFileBinary(unsigned char* buffer, size_t size) {
    _recordFile.write(reinterpret_cast<char*>(buffer), size);
}

void SessionRecording::saveKeyframeToFile(std::string entry) {
    _recordFile << std::move(entry) << std::endl;
}

SessionRecording::CallbackHandle SessionRecording::addStateChangeCallback(
                                                                   StateChangeCallback cb)
{
    CallbackHandle handle = _nextCallbackHandle++;
    _stateChangeCallbacks.emplace_back(handle, std::move(cb));
    return handle;
}

void SessionRecording::removeStateChangeCallback(CallbackHandle handle) {
    const auto it = std::find_if(
        _stateChangeCallbacks.begin(),
        _stateChangeCallbacks.end(),
        [handle](const std::pair<CallbackHandle, std::function<void()>>& cb) {
            return cb.first == handle;
        }
    );

    ghoul_assert(
        it != _stateChangeCallbacks.end(),
        "handle must be a valid callback handle"
    );

    _stateChangeCallbacks.erase(it);
}

std::vector<std::string> SessionRecording::playbackList() const {
    const std::string path = absPath("${RECORDINGS}");

    std::vector<std::string> fileList;
    ghoul::filesystem::Directory currentDir(path);
    std::vector<std::string> allInputFiles = currentDir.readFiles();
    for (const std::string& f : allInputFiles) {
        // Remove path and keep only the filename
        fileList.push_back(f.substr(path.length() + 1, f.length() - path.length() - 1));
    }
    return fileList;
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
                "startPlayback",
                &luascriptfunctions::startPlaybackDefault,
                {},
                "string",
                "Starts a playback session with keyframe times that are relative to "
                "the time since the recording was started (the same relative time "
                "applies to the playback). When playback starts, the simulation time "
                "is automatically set to what it was at recording time. The string "
                "argument is the filename to pull playback keyframes from."
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
            },
            {
                "enableTakeScreenShotDuringPlayback",
                &luascriptfunctions::enableTakeScreenShotDuringPlayback,
                {},
                "[int]",
                "Enables that rendered frames should be saved during playback. The "
                "parameter determines the number of frames that are exported per second "
                "if this value is not provided, 60 frames per second will be exported."
            },
            {
                "disableTakeScreenShotDuringPlayback",
                &luascriptfunctions::disableTakeScreenShotDuringPlayback,
                {},
                "void",
                "Used to disable that renderings are saved during playback"
            }
        }
    };
}

} // namespace openspace::interaction
