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
#include <openspace/interaction/tasks/convertrecformattask.h>
#include <openspace/rendering/luaconsole.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/scripting/scriptscheduler.h>
#include <openspace/util/camera.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/task.h>
#include <openspace/util/timemanager.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>
#include <iomanip>

namespace {
    constexpr const char* _loggerCat = "SessionRecording";

    constexpr openspace::properties::Property::PropertyInfo RenderPlaybackInfo = {
        "RenderInfo",
        "Render Playback Information",
        "If enabled, information about a currently played back session "
        "recording is rendering to screen"
    };

    constexpr const bool UsingTimeKeyframes = false;


} // namespace

#include "sessionrecording_lua.inl"

namespace openspace::interaction {

SessionRecording::SessionRecording()
    : properties::PropertyOwner({ "SessionRecording", "Session Recording" })
    , _renderPlaybackInformation(RenderPlaybackInfo, false)
{
    auto fTask = FactoryManager::ref().factory<Task>();
    ghoul_assert(fRenderable, "No task factory existed");
    fTask->registerClass<ConvertRecFormatTask>("ConvertRecFormatTask");
    addProperty(_renderPlaybackInformation);
}

SessionRecording::~SessionRecording() {} // NOLINT

void SessionRecording::deinitialize() {
    stopRecording();
    stopPlayback();
}

void SessionRecording::setRecordDataFormat(SessionRecordingDataMode dataMode) {
    _recordingDataMode = dataMode;
}

bool SessionRecording::hasFileExtension(std::string filename, std::string extension) {
    if (filename.length() <= extension.length()) {
        return false;
    }
    else {
        return (filename.substr(filename.length() - extension.length()) == extension);
    }
}

bool SessionRecording::handleRecordingFile(std::string filenameIn) {
    if (filenameIn.find("/") != std::string::npos) {
        LERROR("Recording filename must not contain path (/) elements");
        return false;
    }

    if (_recordingDataMode == SessionRecordingDataMode::Binary) {
        if (hasFileExtension(filenameIn, SessionRecordingFileExtensionAscii)) {
            LERROR("Specified filename for binary recording has ascii file extension");
            return false;
        }
        else if (!hasFileExtension(filenameIn, SessionRecordingFileExtensionBinary)) {
            filenameIn += SessionRecordingFileExtensionBinary;
        }
    }
    else if (_recordingDataMode == SessionRecordingDataMode::Ascii) {
        if (hasFileExtension(filenameIn, SessionRecordingFileExtensionBinary)) {
            LERROR("Specified filename for ascii recording has binary file extension");
            return false;
        }
        else if (!hasFileExtension(filenameIn, SessionRecordingFileExtensionAscii)) {
            filenameIn += SessionRecordingFileExtensionAscii;
        }
    }

    std::string absFilename = absPath("${RECORDINGS}/" + filenameIn);

    if (FileSys.fileExists(absFilename)) {
        LERROR(fmt::format(
            "Unable to start recording; file {} already exists.", absFilename.c_str()
        ));
        return false;
    }
    if (_recordingDataMode == SessionRecordingDataMode::Binary) {
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
    return true;
}

bool SessionRecording::startRecording(const std::string& filename) {
    if (_state == SessionState::Recording) {
        LERROR("Unable to start recording while already in recording mode");
        return false;
    }
    else if (_state == SessionState::Playback) {
        LERROR("Unable to start recording while in session playback mode");
        return false;
    }
    if (!FileSys.directoryExists(absPath("${RECORDINGS}"))) {
        FileSys.createDirectory(
            absPath("${RECORDINGS}"),
            ghoul::filesystem::FileSystem::Recursive::Yes
        );
    }

    bool recordingFileOK = handleRecordingFile(filename);

    if (recordingFileOK) {
        _state = SessionState::Recording;
        _playbackActive_camera = false;
        _playbackActive_time = false;
        _playbackActive_script = false;

        _recordFile << SessionRecordingFileHeaderTitle;
        _recordFile.write(FileHeaderVersion, FileHeaderVersionLength);
        if (_recordingDataMode == SessionRecordingDataMode::Binary) {
            _recordFile << DataFormatBinaryTag;
        }
        else {
            _recordFile << DataFormatAsciiTag;
        }
        _recordFile << '\n';

        _timestampRecordStarted = global::windowDelegate.applicationTime();

        //Record the current delta time so this is preserved in recording
        double currentDeltaTime = global::timeManager.deltaTime();
        std::string scriptCommandForInitializingDeltaTime =
            "openspace.time.setDeltaTime(" + std::to_string(currentDeltaTime) + ")";
        saveScriptKeyframe(scriptCommandForInitializingDeltaTime);

        LINFO("Session recording started");
    }

    return recordingFileOK;
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
                                     KeyframeTimeRef timeMode,
                                     bool forceSimTimeAtStart)
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
        SessionRecordingFileHeaderTitle.length()
    );
    if (readBackHeaderString != SessionRecordingFileHeaderTitle) {
        LERROR("Specified playback file does not contain expected header.");
        cleanUpPlayback();
        return false;
    }
    readHeaderElement(_playbackFile, FileHeaderVersionLength);
    std::string readDataMode = readHeaderElement(_playbackFile, 1);
    if (readDataMode[0] == DataFormatAsciiTag) {
        _recordingDataMode = SessionRecordingDataMode::Ascii;
    }
    else if (readDataMode[0] == DataFormatBinaryTag) {
        _recordingDataMode = SessionRecordingDataMode::Binary;
    }
    else {
        LERROR("Unknown data type in header (should be Ascii or Binary)");
        cleanUpPlayback();
    }
    std::string throwawayNewlineChar = readHeaderElement(_playbackFile, 1);

    if (_recordingDataMode == SessionRecordingDataMode::Binary) {
        //Close & re-open the file, starting from the beginning, and do dummy read
        // past the header, version, and data type
        _playbackFile.close();
        _playbackFile.open(_playbackFilename, std::ifstream::in | std::ios::binary);
        size_t headerSize = SessionRecordingFileHeaderTitle.length() +
            FileHeaderVersionLength +
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
    global::timeManager.stopPlayback();

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

void SessionRecording::writeToFileBuffer(unsigned char* buf,
                                         size_t& idx,
                                         double src)
{
    const size_t writeSize_bytes = sizeof(double);
    unsigned char const *p = reinterpret_cast<unsigned char const*>(&src);
    memcpy((buf + idx), p, writeSize_bytes);
    idx += writeSize_bytes;
}

void SessionRecording::writeToFileBuffer(unsigned char* buf,
                                         size_t& idx,
                                         std::vector<char>& cv)
{
    const size_t writeSize_bytes = cv.size() * sizeof(char);
    memcpy((buf + idx), cv.data(), writeSize_bytes);
    idx += writeSize_bytes;
}

void SessionRecording::writeToFileBuffer(unsigned char* buf,
                                         size_t& idx,
                                         unsigned char c)
{
    const size_t writeSize_bytes = sizeof(char);
    buf[idx] = c;
    idx += writeSize_bytes;
}

void SessionRecording::writeToFileBuffer(unsigned char* buf,
                                         size_t& idx,
                                         bool b)
{
    buf[idx] = b ? 1 : 0;
    idx += sizeof(char);
}

void SessionRecording::saveStringToFile(const std::string& s,
                                        unsigned char* kfBuffer,
                                        size_t& idx,
                                        std::ofstream& file)
{
    size_t strLen = s.size();
    size_t writeSize_bytes = sizeof(size_t);

    idx = 0;
    unsigned char const *p = reinterpret_cast<unsigned char const*>(&strLen);
    memcpy((kfBuffer + idx), p, writeSize_bytes);
    idx += static_cast<unsigned int>(writeSize_bytes);
    saveKeyframeToFileBinary(kfBuffer, idx, file);

    file.write(s.c_str(), s.size());
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

    timestamps times = {
        kf._timestamp,
        kf._timestamp - _timestampRecordStarted,
        global::timeManager.time().j2000Seconds()
    };
    if (_recordingDataMode == SessionRecordingDataMode::Binary) {
        saveCameraKeyframeBinary(times, kf, _keyframeBuffer, _recordFile);
    }
    else {
        saveCameraKeyframeAscii(times, kf, _recordFile);
    }
}

void SessionRecording::saveHeaderBinary(timestamps times,
                                        char type,
                                        unsigned char* kfBuffer,
                                        size_t& idx)
{
    kfBuffer[idx++] = type;
    writeToFileBuffer(kfBuffer, idx, times.timeOs);
    writeToFileBuffer(kfBuffer, idx, times.timeRec);
    writeToFileBuffer(kfBuffer, idx, times.timeSim);
}

void SessionRecording::saveHeaderAscii(timestamps times,
                                       const std::string& type,
                                       std::stringstream& line)
{
    line << type << ' ';
    line << times.timeOs << ' ';
    line << times.timeRec << ' ';
    line << std::fixed << std::setprecision(3) << times.timeSim << ' ';
}

void SessionRecording::saveCameraKeyframeBinary(timestamps times,
                                                datamessagestructures::CameraKeyframe& kf,
                                                unsigned char* kfBuffer,
                                                std::ofstream& file)
{
    // Writing to a binary session recording file
    size_t idx = 0;
    saveHeaderBinary(times, SessionRecordingHeaderCameraBinary, kfBuffer, idx);
    // Writing to internal buffer, and then to file, for performance reasons
    std::vector<char> writeBuffer;
    kf.serialize(writeBuffer);
    writeToFileBuffer(kfBuffer, idx, writeBuffer);
    saveKeyframeToFileBinary(kfBuffer, idx, file);
}

void SessionRecording::saveCameraKeyframeAscii(timestamps times,
                                               datamessagestructures::CameraKeyframe& kf,
                                               std::ofstream& file)
{
    std::stringstream keyframeLine = std::stringstream();
    saveHeaderAscii(times, SessionRecordingHeaderCameraAscii, keyframeLine);
    kf.write(keyframeLine);
    saveKeyframeToFile(keyframeLine.str(), file);
}

void SessionRecording::saveTimeKeyframe() {
    if (_state != SessionState::Recording) {
        return;
    }

    //Create a time keyframe, then call to populate it with current time props
    datamessagestructures::TimeKeyframe kf = _externInteract.generateTimeKeyframe();

    timestamps times = {
        kf._timestamp,
        kf._timestamp - _timestampRecordStarted,
        global::timeManager.time().j2000Seconds()
    };
    if (_recordingDataMode == SessionRecordingDataMode::Binary) {
        saveTimeKeyframeBinary(times, kf, _keyframeBuffer, _recordFile);
    } else {
        saveTimeKeyframeAscii(times, kf, _recordFile);
    }
}

void SessionRecording::saveTimeKeyframeBinary(timestamps times,
                                              datamessagestructures::TimeKeyframe& kf,
                                              unsigned char* kfBuffer,
                                              std::ofstream& file)
{
    size_t idx = 0;
    saveHeaderBinary(times, SessionRecordingHeaderTimeBinary, kfBuffer, idx);
    std::vector<char> writeBuffer;
    kf.serialize(writeBuffer);
    writeToFileBuffer(kfBuffer, idx, writeBuffer);
    saveKeyframeToFileBinary(kfBuffer, idx, file);
}

void SessionRecording::saveTimeKeyframeAscii(timestamps times,
                                             datamessagestructures::TimeKeyframe& kf,
                                             std::ofstream& file)
{
    std::stringstream keyframeLine = std::stringstream();
    saveHeaderAscii(times, SessionRecordingHeaderTimeAscii, keyframeLine);
    kf.write(keyframeLine);
    saveKeyframeToFile(keyframeLine.str(), file);
}

void SessionRecording::saveScriptKeyframe(std::string scriptToSave) {
    if (_state != SessionState::Recording) {
        return;
    }

    datamessagestructures::ScriptMessage sm
        = _externInteract.generateScriptMessage(scriptToSave);

    timestamps times = {
        sm._timestamp,
        sm._timestamp - _timestampRecordStarted,
        global::timeManager.time().j2000Seconds()
    };

    if (_recordingDataMode == SessionRecordingDataMode::Binary) {
        saveScriptKeyframeBinary(times, sm, _keyframeBuffer, _recordFile);
    }
    else {
        saveScriptKeyframeAscii(times, sm, _recordFile);
    }
}

void SessionRecording::saveScriptKeyframeBinary(timestamps times,
                                                datamessagestructures::ScriptMessage& sm,
                                                unsigned char* smBuffer,
                                                std::ofstream& file)
{
    size_t idx = 0;
    saveHeaderBinary(times, SessionRecordingHeaderScriptBinary, smBuffer, idx);
    // Writing to internal buffer, and then to file, for performance reasons
    std::vector<char> writeBuffer;
    sm.serialize(writeBuffer);
    writeToFileBuffer(smBuffer, idx, writeBuffer);
    saveKeyframeToFileBinary(smBuffer, idx, file);
}

void SessionRecording::saveScriptKeyframeAscii(timestamps times,
                                               datamessagestructures::ScriptMessage& sm,
                                               std::ofstream& file)
{

    std::stringstream keyframeLine = std::stringstream();
    saveHeaderAscii(times, SessionRecordingHeaderScriptAscii, keyframeLine);
    sm.write(keyframeLine);
    saveKeyframeToFile(keyframeLine.str(), file);
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

void SessionRecording::render() {
    ZoneScoped

    if (!(_renderPlaybackInformation && isPlayingBack())) {
        return;
    }


    constexpr const char* FontName = "Mono";
    constexpr const float FontSizeFrameinfo = 32.f;
    std::shared_ptr<ghoul::fontrendering::Font> font =
        global::fontManager.font(FontName, FontSizeFrameinfo);

    glm::vec2 res = global::renderEngine.fontResolution();
    glm::vec2 penPosition = glm::vec2(
        res.x / 2 - 150.f,
        res.y / 4
    );
    std::string text = std::to_string(currentTime());
    ghoul::fontrendering::RenderFont(*font, penPosition, text, glm::vec4(1.f));
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
    bool parsingStatusOk = true;

    if (_recordingDataMode == SessionRecordingDataMode::Binary) {
        unsigned char frameType;
        bool fileReadOk = true;

        while (parsingStatusOk && fileReadOk) {
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
            if (frameType == SessionRecordingHeaderCameraBinary) {
                parsingStatusOk = playbackCamera();
            }
            else if (frameType == SessionRecordingHeaderTimeBinary) {
                parsingStatusOk = playbackTimeChange();
            }
            else if (frameType == SessionRecordingHeaderScriptBinary) {
                parsingStatusOk = playbackScript();
            }
            else {
                LERROR(fmt::format(
                    "Unknown frame type {} @ index {} of playback file {}",
                    frameType, _playbackLineNum - 1, _playbackFilename
                ));
                parsingStatusOk = false;
                break;
            }

            _playbackLineNum++;
        }
    }
    else {
        while (parsingStatusOk && std::getline(_playbackFile, _playbackLineParsing)) {
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

            if (entryType == SessionRecordingHeaderCameraAscii) {
                parsingStatusOk = playbackCamera();
            }
            else if (entryType == SessionRecordingHeaderTimeAscii) {
                parsingStatusOk = playbackTimeChange();
            }
            else if (entryType == SessionRecordingHeaderScriptAscii) {
                parsingStatusOk = playbackScript();
            }
            else if (entryType.substr(0, 1) == SessionRecordingHeaderCommentAscii) {
                continue;
            }
            else {
                LERROR(fmt::format(
                    "Unknown frame type {} @ line {} of playback file {}",
                    entryType, _playbackLineNum, _playbackFilename
                ));
                parsingStatusOk = false;
                break;
            }
        }
        LINFO(fmt::format(
            "Finished parsing {} entries from playback file {}",
            _playbackLineNum, _playbackFilename
        ));
    }

    return parsingStatusOk;
}

double SessionRecording::appropriateTimestamp(double timeOs,
                                              double timeRec,
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

double SessionRecording::equivalentSimulationTime(double timeOs,
                                                  double timeRec,
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

double SessionRecording::equivalentApplicationTime(double timeOs,
                                                   double timeRec,
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

bool SessionRecording::playbackCamera() {
    timestamps times;
    datamessagestructures::CameraKeyframe kf;

    if (_recordingDataMode == SessionRecordingDataMode::Binary) {
        readCameraKeyframeBinary(times, kf, _playbackFile, _playbackLineNum);
    }
    else {
        readCameraKeyframeAscii(times, kf, _playbackLineParsing, _playbackLineNum);
    }

    if (_setSimulationTimeWithNextCameraKeyframe) {
        global::timeManager.setTimeNextFrame(Time(times.timeSim));
        _setSimulationTimeWithNextCameraKeyframe = false;
        _saveRenderingCurrentRecordedTime = times.timeRec;
    }
    double timeRef = appropriateTimestamp(times.timeOs, times.timeRec, times.timeSim);

    interaction::KeyframeNavigator::CameraPose pbFrame(kf);
    return addKeyframe(timeRef, pbFrame, _playbackLineNum);
}

void SessionRecording::readCameraKeyframeBinary(timestamps& times,
                                                datamessagestructures::CameraKeyframe& kf,
                                                std::ifstream& file, int lineN)
{
    times.timeOs = readFromPlayback<double>(file);
    times.timeRec = readFromPlayback<double>(file);
    times.timeSim = readFromPlayback<double>(file);
    try {
        kf.read(&file);
    }
    catch (std::bad_alloc&) {
        LERROR(fmt::format(
            "Allocation error with camera playback from keyframe entry {}",
            lineN - 1
        ));
        return;
    }
    catch (std::length_error&) {
        LERROR(fmt::format(
            "length_error with camera playback from keyframe entry {}",
            lineN - 1
        ));
        return;
    }
    times.timeOs = kf._timestamp;

    if (!file) {
        LINFO(fmt::format(
            "Error reading camera playback from keyframe entry {}",
            lineN - 1
        ));
        return;
    }
}

void SessionRecording::readCameraKeyframeAscii(timestamps& times,
                                               datamessagestructures::CameraKeyframe& kf,
                                               std::string currentParsingLine,
                                               int lineN)
{
    std::string rotationFollowing;
    std::string entryType;

    std::istringstream iss(currentParsingLine);
    iss >> entryType;
    iss >> times.timeOs >> times.timeRec >> times.timeSim;
    kf.read(iss);
    //ASCII format does not contain trailing timestamp so add it here
    kf._timestamp = times.timeOs;

    if (iss.fail() || !iss.eof()) {
        LERROR(fmt::format("Error parsing camera line {} of playback file", lineN));
        return;
    }
}

bool SessionRecording::playbackTimeChange() {
    timestamps times;
    datamessagestructures::TimeKeyframe kf;

    if (_recordingDataMode == SessionRecordingDataMode::Binary) {
        readTimeKeyframeBinary(times, kf, _playbackFile, _playbackLineNum);
    } else {
        readTimeKeyframeAscii(times, kf, _playbackLineParsing, _playbackLineNum);
    }
    kf._timestamp = equivalentApplicationTime(times.timeOs, times.timeRec, times.timeSim);

    kf._time = kf._timestamp + _timestampApplicationStarted_simulation;
    //global::timeManager.addKeyframe(timeRef, pbFrame._timestamp);
    //_externInteract.timeInteraction(pbFrame);
    return addKeyframe(kf._timestamp, kf, _playbackLineNum);
}

void SessionRecording::readTimeKeyframeBinary(timestamps& times,
                                              datamessagestructures::TimeKeyframe& kf,
                                              std::ifstream& file, int lineN)
{
    times.timeOs = readFromPlayback<double>(file);
    times.timeRec = readFromPlayback<double>(file);
    times.timeSim = readFromPlayback<double>(file);

    try {
        kf.read(&file);
    }
    catch (std::bad_alloc&) {
        LERROR(fmt::format(
            "Allocation error with time playback from keyframe entry {}",
            lineN - 1
        ));
        return;
    }
    catch (std::length_error&) {
        LERROR(fmt::format(
            "length_error with time playback from keyframe entry {}",
            lineN - 1
        ));
        return;
    }

    if (!file) {
        LERROR(fmt::format(
            "Error reading time playback from keyframe entry {}", lineN - 1
        ));
        return;
    }
}

void SessionRecording::readTimeKeyframeAscii(timestamps& times,
                                             datamessagestructures::TimeKeyframe& kf,
                                             std::string currentParsingLine,
                                             int lineN)
{
    std::string entryType;

    std::istringstream iss(currentParsingLine);
    iss >> entryType;
    iss >> times.timeOs >> times.timeRec >> times.timeSim;
    kf.read(iss);

    if (iss.fail() || !iss.eof()) {
        LERROR(fmt::format(
            "Error parsing time line {} of playback file", lineN
        ));
        return;
    }
}

std::string SessionRecording::readHeaderElement(std::ifstream& stream,
                                                size_t readLen_chars)
{
    std::vector<char> readTemp(readLen_chars);
    stream.read(&readTemp[0], readLen_chars);
    return std::string(readTemp.begin(), readTemp.end());
}

bool SessionRecording::playbackScript() {
    timestamps times;
    datamessagestructures::ScriptMessage kf;

    if (_recordingDataMode == SessionRecordingDataMode::Binary) {
        readScriptKeyframeBinary(times, kf, _playbackFile, _playbackLineNum);
    } else {
        readScriptKeyframeAscii(times, kf, _playbackLineParsing, _playbackLineNum);
    }

    double timeRef = appropriateTimestamp(times.timeOs, times.timeRec, times.timeSim);
    return addKeyframe(timeRef, kf._script, _playbackLineNum);
}

void SessionRecording::readScriptKeyframeBinary(timestamps& times,
                                                datamessagestructures::ScriptMessage& kf,
                                                std::ifstream& file, int lineN)
{
    times.timeOs = readFromPlayback<double>(file);
    times.timeRec = readFromPlayback<double>(file);
    times.timeSim = readFromPlayback<double>(file);

    try {
        kf.read(&file);
    }
    catch (std::bad_alloc&) {
        LERROR(fmt::format(
            "Allocation error with script playback from keyframe entry {}",
            lineN - 1
        ));
        return;
    }
    catch (std::length_error&) {
        LERROR(fmt::format(
            "length_error with script playback from keyframe entry {}",
            lineN - 1
        ));
        return;
    }

    if (!file) {
        LERROR(fmt::format(
            "Error reading script playback from keyframe entry {}",
            lineN - 1
        ));
        return;
    }
}

void SessionRecording::readScriptKeyframeAscii(timestamps& times,
                                               datamessagestructures::ScriptMessage& kf,
                                               std::string currentParsingLine,
                                               int lineN)
{
    std::string entryType;
    std::istringstream iss(currentParsingLine);
    iss >> entryType;
    iss >> times.timeOs >> times.timeRec >> times.timeSim;
    kf.read(iss);
    if (iss.fail()) {
        LERROR(fmt::format(
            "Error parsing script line {} of playback file", lineN
        ));
        return;
    } else if (!iss.eof()) {
        LERROR(fmt::format(
            "Did not find an EOL at line {} of playback file", lineN
        ));
        return;
    }
}

bool SessionRecording::addKeyframe(double timestamp,
                                   interaction::KeyframeNavigator::CameraPose keyframe,
                                   int lineNum)
{
    size_t indexIntoCameraKeyframesFromMainTimeline = _keyframesCamera.size();
    _keyframesCamera.push_back(std::move(keyframe));
    return addKeyframeToTimeline(
        RecordedType::Camera,
        indexIntoCameraKeyframesFromMainTimeline,
        timestamp,
        lineNum
    );
}

bool SessionRecording::addKeyframe(double timestamp,
                                   datamessagestructures::TimeKeyframe keyframe,
                                   int lineNum)
{
    size_t indexIntoTimeKeyframesFromMainTimeline = _keyframesTime.size();
    _keyframesTime.push_back(std::move(keyframe));
    return addKeyframeToTimeline(
        RecordedType::Time,
        indexIntoTimeKeyframesFromMainTimeline,
        timestamp,
        lineNum
    );
}

bool SessionRecording::addKeyframe(double timestamp,
                                   std::string scriptToQueue,
                                   int lineNum)
{
    size_t indexIntoScriptKeyframesFromMainTimeline = _keyframesScript.size();
    _keyframesScript.push_back(std::move(scriptToQueue));
    return addKeyframeToTimeline(
        RecordedType::Script,
        indexIntoScriptKeyframesFromMainTimeline,
        timestamp,
        lineNum
    );
}

bool SessionRecording::addKeyframeToTimeline(RecordedType type,
                                             size_t indexIntoTypeKeyframes,
                                             double timestamp,
                                             int lineNum)
{
    try {
        _timeline.push_back({
            type,
            static_cast<unsigned int>(indexIntoTypeKeyframes),
            timestamp
        });
    }
    catch(...) {
        LERROR(fmt::format(
            "Timeline memory allocation error trying to add keyframe {}. "
            "The playback file may be too large for system memory.",
            lineNum - 1
        ));
        return false;
    }
    return true;
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

void SessionRecording::saveKeyframeToFileBinary(unsigned char* buffer,
                                                size_t size,
                                                std::ofstream& file)
{
    file.write(reinterpret_cast<char*>(buffer), size);
}

void SessionRecording::saveKeyframeToFile(std::string entry, std::ofstream& file) {
    file << std::move(entry) << std::endl;
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
