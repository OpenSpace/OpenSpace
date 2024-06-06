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

#include <openspace/interaction/sessionrecording.h>

#include <openspace/camera/camera.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/events/eventengine.h>
#include <openspace/interaction/tasks/convertrecfileversiontask.h>
#include <openspace/interaction/tasks/convertrecformattask.h>
#include <openspace/navigation/keyframenavigator.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/orbitalnavigator.h>
#include <openspace/network/messagestructureshelper.h>
#include <openspace/query/query.h>
#include <openspace/rendering/luaconsole.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/scripting/scriptscheduler.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/task.h>
#include <openspace/util/timemanager.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/glm.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/misc/stringhelper.h>
#include <algorithm>
#include <filesystem>
#include <iomanip>

#ifdef WIN32
#include <Windows.h>
#endif // WIN32

#include "sessionrecording_lua.inl"

namespace {
    constexpr std::string_view _loggerCat = "SessionRecording";

    constexpr bool UsingTimeKeyframes = false;

    constexpr openspace::properties::Property::PropertyInfo RenderPlaybackInfo = {
        "RenderInfo",
        "Render Playback Information",
        "If enabled, information about a currently played back session recording is "
        "rendering to screen.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo IgnoreRecordedScaleInfo = {
        "IgnoreRecordedScale",
        "Ignore Recorded Scale",
        "If this value is enabled, the scale value from a recording is ignored and the "
        "computed values are used instead.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo AddModelMatrixinAsciiInfo = {
        "AddModelMatrixinAscii",
        "Add Model Matrix in ASCII recording",
        "If this is 'true', the model matrix is written into the ASCII recording format "
        "in the line before each camera keyframe. The model matrix is the full matrix "
        "that converts the position into a J2000+Galactic reference frame.",
        openspace::properties::Property::Visibility::Developer
    };
} // namespace

namespace openspace::interaction {

ConversionError::ConversionError(std::string msg)
    : ghoul::RuntimeError(std::move(msg), "conversionError")
{}

SessionRecording::SessionRecording()
    : properties::PropertyOwner({ "SessionRecording", "Session Recording" })
    , _renderPlaybackInformation(RenderPlaybackInfo, false)
    , _ignoreRecordedScale(IgnoreRecordedScaleInfo, false)
    , _addModelMatrixinAscii(AddModelMatrixinAsciiInfo, false)
{}

SessionRecording::SessionRecording(bool isGlobal)
    : SessionRecording()
{
    if (isGlobal) {
        ghoul::TemplateFactory<Task>* fTask = FactoryManager::ref().factory<Task>();
        ghoul_assert(fTask, "No task factory existed");
        fTask->registerClass<ConvertRecFormatTask>("ConvertRecFormatTask");
        fTask->registerClass<ConvertRecFileVersionTask>("ConvertRecFileVersionTask");
        addProperty(_renderPlaybackInformation);
        addProperty(_ignoreRecordedScale);
        addProperty(_addModelMatrixinAscii);
    }
}

void SessionRecording::deinitialize() {
    stopRecording();
    stopPlayback();
}

void SessionRecording::setRecordDataFormat(DataMode dataMode) {
    _recordingDataMode = dataMode;
}

bool SessionRecording::hasFileExtension(const std::string& filename,
                                        const std::string& extension)
{
    if (filename.length() <= extension.length()) {
        return false;
    }
    else {
        return (filename.substr(filename.length() - extension.length()) == extension);
    }
}

bool SessionRecording::isPath(std::string& filename) {
    const size_t unixDelimiter = filename.find('/');
    const size_t windowsDelimiter = filename.find('\\');
    return (unixDelimiter != std::string::npos || windowsDelimiter != std::string::npos);
}

void SessionRecording::removeTrailingPathSlashes(std::string& filename) const {
    while (filename.substr(filename.length() - 1, 1) == "/") {
        filename.pop_back();
    }
    while (filename.substr(filename.length() - 1, 1) == "\\") {
        filename.pop_back();
    }
}

bool SessionRecording::handleRecordingFile(std::string filenameIn) {
    if (_recordingDataMode == DataMode::Binary) {
        if (hasFileExtension(filenameIn, FileExtensionAscii)) {
            LERROR("Specified filename for binary recording has ascii file extension");
            return false;
        }
        else if (!hasFileExtension(filenameIn, FileExtensionBinary)) {
            filenameIn += FileExtensionBinary;
        }
    }
    else if (_recordingDataMode == DataMode::Ascii) {
        if (hasFileExtension(filenameIn, FileExtensionBinary)) {
            LERROR("Specified filename for ascii recording has binary file extension");
            return false;
        }
        else if (!hasFileExtension(filenameIn, FileExtensionAscii)) {
            filenameIn += FileExtensionAscii;
        }
    }

    std::filesystem::path absFilename = filenameIn;
    if (absFilename.parent_path().empty() || absFilename.parent_path() == absFilename) {
        absFilename = absPath("${RECORDINGS}/" + filenameIn);
    }
    else if (absFilename.parent_path().is_relative()) {
        LERROR("If path is provided with the filename, then it must be an absolute path");
        return false;
    }
    else if (!std::filesystem::exists(absFilename.parent_path())) {
        LERROR(std::format(
            "The recording filename path '{}' is not a valid location in the filesytem",
            absFilename.parent_path().string()
        ));
        return false;
    }

    if (std::filesystem::is_regular_file(absFilename)) {
        LERROR(std::format(
            "Unable to start recording; file '{}' already exists", absFilename
        ));
        return false;
    }
    if (_recordingDataMode == DataMode::Binary) {
        _recordFile.open(absFilename, std::ios::binary);
    }
    else {
        _recordFile.open(absFilename);
    }

    if (!_recordFile.is_open() || !_recordFile.good()) {
        LERROR(std::format(
            "Unable to open file '{}' for keyframe recording", absFilename
        ));
        return false;
    }
    return true;
}

bool SessionRecording::startRecording(const std::string& filename) {
    _timeline.clear();
    if (_state == SessionState::Recording) {
        LERROR("Unable to start recording while already in recording mode");
        return false;
    }
    else if (isPlayingBack()) {
        LERROR("Unable to start recording while in session playback mode");
        return false;
    }
    if (!std::filesystem::is_directory(absPath("${RECORDINGS}"))) {
        std::filesystem::create_directories(absPath("${RECORDINGS}"));
    }

    const bool recordingFileOK = handleRecordingFile(filename);

    if (recordingFileOK) {
        _state = SessionState::Recording;
        _playbackActive_camera = false;
        _playbackActive_time = false;
        _playbackActive_script = false;
        _propertyBaselinesSaved.clear();
        _keyframesSavePropertiesBaseline_scripts.clear();
        _keyframesSavePropertiesBaseline_timeline.clear();
        _recordingEntryNum = 1;

        _recordFile << FileHeaderTitle;
        _recordFile.write(FileHeaderVersion, FileHeaderVersionLength);
        if (_recordingDataMode == DataMode::Binary) {
            _recordFile << DataFormatBinaryTag;
        }
        else {
            _recordFile << DataFormatAsciiTag;
        }
        _recordFile << '\n';

        _timestampRecordStarted = global::windowDelegate->applicationTime();

        // Record the current delta time as the first property to save in the file.
        // This needs to be saved as a baseline whether or not it changes during recording
        _timestamps3RecordStarted = {
            .timeOs = _timestampRecordStarted,
            .timeRec = 0.0,
            .timeSim = global::timeManager->time().j2000Seconds()
        };

        recordCurrentTimePauseState();
        recordCurrentTimeRate();
        LINFO("Session recording started");
    }

    return recordingFileOK;
}

void SessionRecording::recordCurrentTimePauseState() {
    const bool isPaused = global::timeManager->isPaused();
    std::string initialTimePausedCommand = "openspace.time.setPause(" +
        std::string(isPaused ? "true" : "false") + ")";
    saveScriptKeyframeToPropertiesBaseline(std::move(initialTimePausedCommand));
}

void SessionRecording::recordCurrentTimeRate() {
    std::string initialTimeRateCommand = std::format(
        "openspace.time.setDeltaTime({})", global::timeManager->targetDeltaTime()
    );
    saveScriptKeyframeToPropertiesBaseline(std::move(initialTimeRateCommand));
}

void SessionRecording::stopRecording() {
    if (_state == SessionState::Recording) {
        // Add all property baseline scripts to the beginning of the recording file
        datamessagestructures::ScriptMessage smTmp;
        for (TimelineEntry& initPropScripts : _keyframesSavePropertiesBaseline_timeline) {
            if (initPropScripts.keyframeType == RecordedType::Script) {
                smTmp._script = _keyframesSavePropertiesBaseline_scripts
                    [initPropScripts.idxIntoKeyframeTypeArray];
                saveSingleKeyframeScript(
                    smTmp,
                    _timestamps3RecordStarted,
                    _recordingDataMode,
                    _recordFile,
                    _keyframeBuffer
                );
            }
        }
        for (TimelineEntry entry : _timeline) {
            switch (entry.keyframeType) {
                case RecordedType::Camera:
                {
                    interaction::KeyframeNavigator::CameraPose kf
                        = _keyframesCamera[entry.idxIntoKeyframeTypeArray];
                    datamessagestructures::CameraKeyframe kfMsg(
                        std::move(kf.position),
                        std::move(kf.rotation),
                        std::move(kf.focusNode),
                        kf.followFocusNodeRotation,
                        kf.scale
                    );
                    saveSingleKeyframeCamera(
                        kfMsg,
                        entry.t3stamps,
                        _recordingDataMode,
                        _recordFile,
                        _keyframeBuffer
                    );
                    break;
                }
                case RecordedType::Time:
                {
                    datamessagestructures::TimeKeyframe tf
                        = _keyframesTime[entry.idxIntoKeyframeTypeArray];
                    saveSingleKeyframeTime(
                        tf,
                        entry.t3stamps,
                        _recordingDataMode,
                        _recordFile,
                        _keyframeBuffer
                    );
                    break;
                }
                case RecordedType::Script:
                {
                    smTmp._script = _keyframesScript[entry.idxIntoKeyframeTypeArray];
                    saveSingleKeyframeScript(
                        smTmp,
                        entry.t3stamps,
                        _recordingDataMode,
                        _recordFile,
                        _keyframeBuffer
                    );
                    break;
                }
                default:
                {
                    break;
                }
            }
        }
        _state = SessionState::Idle;
        LINFO("Session recording stopped");
    }
    // Close the recording file
    _recordFile.close();
    _cleanupNeededRecording = true;
}

bool SessionRecording::startPlayback(std::string& filename,
                                     KeyframeTimeRef timeMode,
                                     bool forceSimTimeAtStart,
                                     bool loop, bool shouldWaitForFinishedTiles)
{
    std::string absFilename;
    if (std::filesystem::is_regular_file(filename)) {
        absFilename = filename;
    }
    else {
        absFilename = absPath("${RECORDINGS}/" + filename).string();
    }
    // Run through conversion in case file is older. Does nothing if the file format
    // is up-to-date
    absFilename = convertFile(absFilename);

    if (_state == SessionState::Recording) {
        LERROR("Unable to start playback while in session recording mode");
        return false;
    }
    else if (isPlayingBack()) {
        LERROR("Unable to start new playback while in session playback mode");
        return false;
    }

    if (!std::filesystem::is_regular_file(absFilename)) {
        LERROR("Cannot find the specified playback file");
        cleanUpPlayback();
        return false;
    }

    _playbackLineNum = 1;
    _playbackFilename = absFilename;
    _playbackLoopMode = loop;
    _shouldWaitForFinishLoadingWhenPlayback = shouldWaitForFinishedTiles;

    // Open in ASCII first
    _playbackFile.open(_playbackFilename, std::ifstream::in);
    // Read header
    const std::string readBackHeaderString = readHeaderElement(
        _playbackFile,
        FileHeaderTitle.length()
    );
    if (readBackHeaderString != FileHeaderTitle) {
        LERROR("Specified playback file does not contain expected header");
        cleanUpPlayback();
        return false;
    }
    readHeaderElement(_playbackFile, FileHeaderVersionLength);
    std::string readDataMode = readHeaderElement(_playbackFile, 1);
    if (readDataMode[0] == DataFormatAsciiTag) {
        _recordingDataMode = DataMode::Ascii;
    }
    else if (readDataMode[0] == DataFormatBinaryTag) {
        _recordingDataMode = DataMode::Binary;
    }
    else {
        LERROR("Unknown data type in header (should be Ascii or Binary)");
        cleanUpPlayback();
    }
    // throwaway newline character(s)
    std::string lineEnd = readHeaderElement(_playbackFile, 1);
    bool hasDosLineEnding = (lineEnd == "\r");
    if (hasDosLineEnding) {
        // throwaway the second newline character (\n) also
        readHeaderElement(_playbackFile, 1);
    }

    if (_recordingDataMode == DataMode::Binary) {
        // Close & re-open the file, starting from the beginning, and do dummy read
        // past the header, version, and data type
        _playbackFile.close();
        _playbackFile.open(_playbackFilename, std::ifstream::in | std::ios::binary);
        const size_t headerSize = FileHeaderTitle.length() + FileHeaderVersionLength
            + sizeof(DataFormatBinaryTag) + sizeof('\n');
        std::vector<char> hBuffer;
        hBuffer.resize(headerSize);
        _playbackFile.read(hBuffer.data(), headerSize);
    }

    if (!_playbackFile.is_open() || !_playbackFile.good()) {
        LERROR(std::format(
            "Unable to open file '{}' for keyframe playback", absFilename.c_str()
        ));
        stopPlayback();
        cleanUpPlayback();
        return false;
    }
    _saveRendering_isFirstFrame = true;
    // Set time reference mode
    _playbackForceSimTimeAtStart = forceSimTimeAtStart;
    double now = global::windowDelegate->applicationTime();
    _playbackTimeReferenceMode = timeMode;
    initializePlayback_time(now);

    global::scriptScheduler->setTimeReferenceMode(timeMode);
    _loadedNodes.clear();
    populateListofLoadedSceneGraphNodes();

    if (!playbackAddEntriesToTimeline()) {
        cleanUpPlayback();
        return false;
    }

    initializePlayback_modeFlags();
    if (!initializePlayback_timeline()) {
        cleanUpPlayback();
        return false;
    }

    const bool canTriggerPlayback = global::openSpaceEngine->setMode(
        OpenSpaceEngine::Mode::SessionRecordingPlayback
    );

    if (!canTriggerPlayback) {
        cleanUpPlayback();
        return false;
    }

    LINFO(std::format(
        "Playback session started: ({:8.3f},0.0,{:13.3f}) with {}/{}/{} entries, "
        "forceTime={}",
        now, _timestampPlaybackStarted_simulation, _keyframesCamera.size(),
        _keyframesTime.size(), _keyframesScript.size(),
        (_playbackForceSimTimeAtStart ? 1 : 0)
    ));

    global::eventEngine->publishEvent<events::EventSessionRecordingPlayback>(
        events::EventSessionRecordingPlayback::State::Started
    );
    initializePlayback_triggerStart();

    global::navigationHandler->orbitalNavigator().updateOnCameraInteraction();

    return true;
}

void SessionRecording::initializePlayback_time(double now) {
    using namespace std::chrono;
    _timestampPlaybackStarted_application = now;
    _timestampPlaybackStarted_simulation = global::timeManager->time().j2000Seconds();
    _timestampApplicationStarted_simulation = _timestampPlaybackStarted_simulation - now;
    _saveRenderingCurrentRecordedTime_interpolation = steady_clock::now();
    _saveRenderingCurrentApplicationTime_interpolation =
        global::windowDelegate->applicationTime();
    _saveRenderingClockInterpolation_countsPerSec =
        system_clock::duration::period::den / system_clock::duration::period::num;
    _playbackPauseOffset = 0.0;
    global::navigationHandler->keyframeNavigator().setTimeReferenceMode(
        _playbackTimeReferenceMode, now);
}

void SessionRecording::initializePlayback_modeFlags() {
    _playbackActive_camera = true;
    _playbackActive_script = true;
    if (UsingTimeKeyframes) {
        _playbackActive_time = true;
    }
    _hasHitEndOfCameraKeyframes = false;
}

bool SessionRecording::initializePlayback_timeline() {
    if (!findFirstCameraKeyframeInTimeline()) {
        return false;
    }
    if (_playbackForceSimTimeAtStart) {
        const Timestamps times = _timeline[_idxTimeline_cameraFirstInTimeline].t3stamps;
        global::timeManager->setTimeNextFrame(Time(times.timeSim));
        _saveRenderingCurrentRecordedTime = times.timeRec;
    }
    _idxTimeline_nonCamera = 0;
    _idxTime = 0;
    _idxScript = 0;
    _idxTimeline_cameraPtrNext = 0;
    _idxTimeline_cameraPtrPrev = 0;
    return true;
}

void SessionRecording::initializePlayback_triggerStart() {
    _state = SessionState::Playback;
}

bool SessionRecording::isPlaybackPaused() {
    return (_state == SessionState::PlaybackPaused);
}

void SessionRecording::setPlaybackPause(bool pause) {
    if (pause && _state == SessionState::Playback) {
        _playbackPausedWithinDeltaTimePause = global::timeManager->isPaused();
        if (!_playbackPausedWithinDeltaTimePause) {
            global::timeManager->setPause(true);
        }
        _state = SessionState::PlaybackPaused;
        global::eventEngine->publishEvent<events::EventSessionRecordingPlayback>(
            events::EventSessionRecordingPlayback::State::Paused
        );
    }
    else if (!pause && _state == SessionState::PlaybackPaused) {
        if (!_playbackPausedWithinDeltaTimePause) {
            global::timeManager->setPause(false);
        }
        _state = SessionState::Playback;
        global::eventEngine->publishEvent<events::EventSessionRecordingPlayback>(
            events::EventSessionRecordingPlayback::State::Resumed
        );
    }
}

bool SessionRecording::findFirstCameraKeyframeInTimeline() {
    bool foundCameraKeyframe = false;
    for (unsigned int i = 0; i < _timeline.size(); i++) {
        if (doesTimelineEntryContainCamera(i)) {
            _idxTimeline_cameraFirstInTimeline = i;
            _idxTimeline_cameraPtrPrev = _idxTimeline_cameraFirstInTimeline;
            _idxTimeline_cameraPtrNext = _idxTimeline_cameraFirstInTimeline;
            _cameraFirstInTimeline_timestamp = appropriateTimestamp(
                _timeline[_idxTimeline_cameraFirstInTimeline].t3stamps);
            foundCameraKeyframe = true;
            break;
        }
    }

    if (!foundCameraKeyframe) {
        signalPlaybackFinishedForComponent(RecordedType::Camera);
        return true;
    }
    else {
        return checkIfInitialFocusNodeIsLoaded(_idxTimeline_cameraFirstInTimeline);
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
        if (_playbackLoopMode) {
            // Loop back to the beginning to replay
            _saveRenderingDuringPlayback = false;
            initializePlayback_time(global::windowDelegate->applicationTime());
            initializePlayback_modeFlags();
            initializePlayback_timeline();
            initializePlayback_triggerStart();
        }
        else {
            LINFO("Playback session finished");
            handlePlaybackEnd();
        }
    }
}

void SessionRecording::handlePlaybackEnd() {
    _state = SessionState::Idle;
    _cleanupNeededPlayback = true;
    global::eventEngine->publishEvent<events::EventSessionRecordingPlayback>(
        events::EventSessionRecordingPlayback::State::Finished
    );
    global::openSpaceEngine->resetMode();
    global::navigationHandler->resetNavigationUpdateVariables();
}

void SessionRecording::enableTakeScreenShotDuringPlayback(int fps) {
    _saveRenderingDuringPlayback = true;
    _saveRenderingDeltaTime = 1.0 / fps;
    _saveRenderingDeltaTime_interpolation_usec =
        std::chrono::microseconds(static_cast<long>(_saveRenderingDeltaTime * 1000000));
}

void SessionRecording::disableTakeScreenShotDuringPlayback() {
    _saveRenderingDuringPlayback = false;
}

void SessionRecording::stopPlayback() {
    if (isPlayingBack()) {
        LINFO("Session playback stopped");
        handlePlaybackEnd();
    }
}

void SessionRecording::cleanUpPlayback() {
    Camera* camera = global::navigationHandler->camera();
    ghoul_assert(camera != nullptr, "Camera must not be nullptr");
    Scene* scene = camera->parent()->scene();
    if (!_timeline.empty()) {
        const unsigned int p =
            _timeline[_idxTimeline_cameraPtrPrev].idxIntoKeyframeTypeArray;
        if (!_keyframesCamera.empty()) {
            const SceneGraphNode* n = scene->sceneGraphNode(
                _keyframesCamera[p].focusNode
            );
            if (n) {
                global::navigationHandler->orbitalNavigator().setFocusNode(
                    n->identifier()
                );
            }
        }
    }

    _playbackFile.close();
    cleanUpTimelinesAndKeyframes();
    _cleanupNeededPlayback = false;
}

void SessionRecording::cleanUpRecording() {
    cleanUpTimelinesAndKeyframes();
    _cleanupNeededRecording = false;
}

void SessionRecording::cleanUpTimelinesAndKeyframes() {
    _timeline.clear();
    _keyframesCamera.clear();
    _keyframesTime.clear();
    _keyframesScript.clear();
    _keyframesSavePropertiesBaseline_scripts.clear();
    _keyframesSavePropertiesBaseline_timeline.clear();
    _propertyBaselinesSaved.clear();
    _loadedNodes.clear();
    _idxTimeline_nonCamera = 0;
    _idxTime = 0;
    _idxScript = 0;
    _idxTimeline_cameraPtrNext = 0;
    _idxTimeline_cameraPtrPrev = 0;
    _hasHitEndOfCameraKeyframes = false;
    _saveRenderingDuringPlayback = false;
    _saveRendering_isFirstFrame = true;
    _playbackPauseOffset = 0.0;
    _playbackLoopMode = false;
    _playbackForceSimTimeAtStart = false;
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
    const size_t writeSize_bytes = sizeof(size_t);

    idx = 0;
    unsigned char const *p = reinterpret_cast<unsigned char const*>(&strLen);
    memcpy((kfBuffer + idx), p, writeSize_bytes);
    idx += static_cast<unsigned int>(writeSize_bytes);
    saveKeyframeToFileBinary(kfBuffer, idx, file);

    file.write(s.c_str(), s.size());
}

bool SessionRecording::hasCameraChangedFromPrev(
                                       const datamessagestructures::CameraKeyframe& kfNew)
{
    constexpr double threshold = 1e-2;
    bool hasChanged = false;

    const glm::dvec3 position = kfNew._position - _prevRecordedCameraKeyframe._position;
    if (glm::length(position) > threshold) {
        hasChanged = true;
    }

    const double rotation = dot(kfNew._rotation, _prevRecordedCameraKeyframe._rotation);
    if (std::abs(rotation - 1.0) > threshold) {
        hasChanged = true;
    }

    _prevRecordedCameraKeyframe = kfNew;
    return hasChanged;
}

SessionRecording::Timestamps SessionRecording::generateCurrentTimestamp3(
                                                                double keyframeTime) const
{
    return {
        keyframeTime,
        keyframeTime - _timestampRecordStarted,
        global::timeManager->time().j2000Seconds()
    };
}

void SessionRecording::saveCameraKeyframeToTimeline() {
    const SceneGraphNode* an = global::navigationHandler->orbitalNavigator().anchorNode();
    if (!an) {
        return;
    }

    // Create a camera keyframe, then call to populate it with current position
    // & orientation of camera
    datamessagestructures::CameraKeyframe kf =
        datamessagestructures::generateCameraKeyframe();

    Timestamps times = generateCurrentTimestamp3(kf._timestamp);
    interaction::KeyframeNavigator::CameraPose pbFrame(std::move(kf));
    addKeyframe(std::move(times), std::move(pbFrame), _recordingEntryNum++);
}

void SessionRecording::saveHeaderBinary(Timestamps& times,
                                        char type,
                                        unsigned char* kfBuffer,
                                        size_t& idx)
{
    kfBuffer[idx++] = type;
    writeToFileBuffer(kfBuffer, idx, times.timeOs);
    writeToFileBuffer(kfBuffer, idx, times.timeRec);
    writeToFileBuffer(kfBuffer, idx, times.timeSim);
}

void SessionRecording::saveHeaderAscii(Timestamps& times,
                                       const std::string& type,
                                       std::stringstream& line)
{
    line << type << ' ';
    line << times.timeOs << ' ';
    line << times.timeRec << ' ';
    line << std::fixed << std::setprecision(3) << times.timeSim << ' ';
}

void SessionRecording::saveCameraKeyframeBinary(Timestamps& times,
                                                datamessagestructures::CameraKeyframe& kf,
                                                unsigned char* kfBuffer,
                                                std::ofstream& file)
{
    // Writing to a binary session recording file
    size_t idx = 0;
    saveHeaderBinary(times, HeaderCameraBinary, kfBuffer, idx);
    // Writing to internal buffer, and then to file, for performance reasons
    std::vector<char> writeBuffer;
    kf.serialize(writeBuffer);
    writeToFileBuffer(kfBuffer, idx, writeBuffer);
    saveKeyframeToFileBinary(kfBuffer, idx, file);
}

void SessionRecording::saveCameraKeyframeAscii(Timestamps& times,
                                               datamessagestructures::CameraKeyframe& kf,
                                               std::ofstream& file)
{
    if (_addModelMatrixinAscii) {
        SceneGraphNode* node = sceneGraphNode(kf._focusNode);
        const glm::dmat4 modelTransform = node->modelTransform();

        file << HeaderCommentAscii << ' ' << ghoul::to_string(modelTransform) << '\n';
    }

    std::stringstream keyframeLine = std::stringstream();
    saveHeaderAscii(times, HeaderCameraAscii, keyframeLine);
    kf.write(keyframeLine);
    saveKeyframeToFile(keyframeLine.str(), file);
}

void SessionRecording::saveTimeKeyframeToTimeline() {
    // Create a time keyframe, then call to populate it with current time props
    const datamessagestructures::TimeKeyframe kf =
        datamessagestructures::generateTimeKeyframe();

    Timestamps times = generateCurrentTimestamp3(kf._timestamp);
    addKeyframe(std::move(times), kf, _recordingEntryNum++);
}

void SessionRecording::saveTimeKeyframeBinary(Timestamps& times,
                                              datamessagestructures::TimeKeyframe& kf,
                                              unsigned char* kfBuffer,
                                              std::ofstream& file)
{
    size_t idx = 0;
    saveHeaderBinary(times, HeaderTimeBinary, kfBuffer, idx);
    std::vector<char> writeBuffer;
    kf.serialize(writeBuffer);
    writeToFileBuffer(kfBuffer, idx, writeBuffer);
    saveKeyframeToFileBinary(kfBuffer, idx, file);
}

void SessionRecording::saveTimeKeyframeAscii(Timestamps& times,
                                             datamessagestructures::TimeKeyframe& kf,
                                             std::ofstream& file)
{
    std::stringstream keyframeLine = std::stringstream();
    saveHeaderAscii(times, HeaderTimeAscii, keyframeLine);
    kf.write(keyframeLine);
    saveKeyframeToFile(keyframeLine.str(), file);
}

void SessionRecording::saveScriptKeyframeToTimeline(std::string script) {
    if (script.starts_with(scriptReturnPrefix)) {
        script = script.substr(scriptReturnPrefix.length());
    }
    for (const std::string& reject : _scriptRejects) {
        if (script.starts_with(reject)) {
            return;
        }
    }
    trimCommandsFromScriptIfFound(script);
    replaceCommandsFromScriptIfFound(script);
    const datamessagestructures::ScriptMessage sm
        = datamessagestructures::generateScriptMessage(script);

    Timestamps times = generateCurrentTimestamp3(sm._timestamp);
    addKeyframe(std::move(times), sm._script, _playbackLineNum);
}

void SessionRecording::saveScriptKeyframeToPropertiesBaseline(std::string script) {
    const Timestamps times = generateCurrentTimestamp3(
        global::windowDelegate->applicationTime()
    );
    const size_t indexIntoScriptKeyframesFromMainTimeline =
        _keyframesSavePropertiesBaseline_scripts.size();
    _keyframesSavePropertiesBaseline_scripts.push_back(std::move(script));
    addKeyframeToTimeline(
        _keyframesSavePropertiesBaseline_timeline,
        RecordedType::Script,
        indexIntoScriptKeyframesFromMainTimeline,
        times,
        0
    );
}

void SessionRecording::trimCommandsFromScriptIfFound(std::string& script) {
    for (const std::string& trimSnippet : _scriptsToBeTrimmed) {
        auto findIdx = script.find(trimSnippet);
        if (findIdx != std::string::npos) {
            auto findClosingParens = script.find_first_of(')', findIdx);
            script.erase(findIdx, findClosingParens + 1);
        }
    }
}

void SessionRecording::replaceCommandsFromScriptIfFound(std::string& script) {
    for (const ScriptSubstringReplace& replacementSnippet : _scriptsToBeReplaced) {
        auto findIdx = script.find(replacementSnippet.substringFound);
        if (findIdx != std::string::npos) {
            script.erase(findIdx, replacementSnippet.substringFound.length());
            script.insert(findIdx, replacementSnippet.substringReplacement);
        }
    }
}

void SessionRecording::saveScriptKeyframeBinary(Timestamps& times,
                                                datamessagestructures::ScriptMessage& sm,
                                                unsigned char* smBuffer,
                                                std::ofstream& file)
{
    size_t idx = 0;
    saveHeaderBinary(times, HeaderScriptBinary, smBuffer, idx);
    // Writing to internal buffer, and then to file, for performance reasons
    std::vector<char> writeBuffer;
    sm.serialize(writeBuffer);
    writeToFileBuffer(smBuffer, idx, writeBuffer);
    saveKeyframeToFileBinary(smBuffer, idx, file);
}

void SessionRecording::saveScriptKeyframeAscii(Timestamps& times,
                                               datamessagestructures::ScriptMessage& sm,
                                               std::ofstream& file)
{
    std::stringstream keyframeLine = std::stringstream();
    saveHeaderAscii(times, HeaderScriptAscii, keyframeLine);
    // Erase all \r (from windows newline), and all \n from line endings and replace with
    // ';' so that lua will treat them as separate lines. This is done in order to treat
    // a multi-line script as a single line in the file.
    size_t startPos = sm._script.find('\r', 0);
    while (startPos != std::string::npos) {
        sm._script.erase(startPos, 1);
        startPos = sm._script.find('\r', startPos);
    }
    startPos = sm._script.find('\n', 0);
    while (startPos != std::string::npos) {
        sm._script.replace(startPos, 1, ";");
        startPos = sm._script.find('\n', startPos);
    }
    sm.write(keyframeLine);
    saveKeyframeToFile(keyframeLine.str(), file);
}

void SessionRecording::savePropertyBaseline(properties::Property& prop) {
    const std::string propIdentifier = prop.fullyQualifiedIdentifier();
    if (isPropertyAllowedForBaseline(propIdentifier)) {
        const bool isPropAlreadySaved = (
            std::find(
                _propertyBaselinesSaved.begin(),
                _propertyBaselinesSaved.end(),
                propIdentifier
            )
            != _propertyBaselinesSaved.end()
            );
        if (!isPropAlreadySaved) {
            const std::string initialScriptCommand = std::format(
                "openspace.setPropertyValueSingle(\"{}\", {})",
                propIdentifier, prop.stringValue()
            );
            saveScriptKeyframeToPropertiesBaseline(initialScriptCommand);
            _propertyBaselinesSaved.push_back(propIdentifier);
        }
    }
}

bool SessionRecording::isPropertyAllowedForBaseline(const std::string& propString) {
    for (const std::string& reject : _propertyBaselineRejects) {
        if (propString.starts_with(reject)) {
            return false;
        }
    }
    return true;
}

void SessionRecording::preSynchronization() {
    ZoneScoped;

    if (_state == SessionState::Recording) {
        saveCameraKeyframeToTimeline();
        if (UsingTimeKeyframes) {
            saveTimeKeyframeToTimeline();
        }
    }
    else if (isPlayingBack()) {
        moveAheadInTime();
    }
    else if (_cleanupNeededPlayback) {
        cleanUpPlayback();
    }
    else if (_cleanupNeededRecording) {
        cleanUpRecording();
    }

    // Handle callback(s) for change in idle/record/playback state
    if (_state != _lastState) {
        using K = CallbackHandle;
        using V = StateChangeCallback;
        for (const std::pair<K, V>& it : _stateChangeCallbacks) {
            it.second();
        }
    }
    _lastState = _state;
}

void SessionRecording::render() {
    ZoneScoped;

    if (!(_renderPlaybackInformation && isPlayingBack())) {
        return;
    }


    constexpr std::string_view FontName = "Mono";
    constexpr float FontSizeFrameinfo = 32.f;
    const std::shared_ptr<ghoul::fontrendering::Font> font =
        global::fontManager->font(FontName, FontSizeFrameinfo);

    const glm::vec2 res = global::renderEngine->fontResolution();
    glm::vec2 penPosition = glm::vec2(
        res.x / 2 - 150.f,
        res.y / 4
    );
    const std::string text1 = std::to_string(currentTime());
    ghoul::fontrendering::RenderFont(
        *font,
        penPosition,
        text1,
        glm::vec4(1.f),
        ghoul::fontrendering::CrDirection::Down
    );
    const std::string text2 = std::format(
        "Scale: {}", global::navigationHandler->camera()->scaling()
    );
    ghoul::fontrendering::RenderFont(*font, penPosition, text2, glm::vec4(1.f));
}

bool SessionRecording::isRecording() const {
    return (_state == SessionState::Recording);
}

bool SessionRecording::isPlayingBack() const {
    return (_state == SessionState::Playback || _state == SessionState::PlaybackPaused);
}

bool SessionRecording::isSavingFramesDuringPlayback() const {
    return (isPlayingBack() && _saveRenderingDuringPlayback);
}

bool SessionRecording::shouldWaitForTileLoading() const {
    return _shouldWaitForFinishLoadingWhenPlayback;
}

SessionRecording::SessionState SessionRecording::state() const {
    return _state;
}

bool SessionRecording::playbackAddEntriesToTimeline() {
    bool parsingStatusOk = true;

    if (_recordingDataMode == DataMode::Binary) {
        while (parsingStatusOk) {
            unsigned char frameType = readFromPlayback<unsigned char>(_playbackFile);
            // Check if have reached EOF
            if (!_playbackFile) {
                LINFO(std::format(
                    "Finished parsing {} entries from playback file '{}'",
                    _playbackLineNum - 1, _playbackFilename
                ));
                break;
            }
            if (frameType == HeaderCameraBinary) {
                parsingStatusOk = playbackCamera();
            }
            else if (frameType == HeaderTimeBinary) {
                parsingStatusOk = playbackTimeChange();
            }
            else if (frameType == HeaderScriptBinary) {
                parsingStatusOk = playbackScript();
            }
            else {
                LERROR(std::format(
                    "Unknown frame type {} @ index {} of playback file '{}'",
                    frameType, _playbackLineNum - 1, _playbackFilename
                ));
                parsingStatusOk = false;
                break;
            }

            _playbackLineNum++;
        }
    }
    else {
        while (parsingStatusOk && ghoul::getline(_playbackFile, _playbackLineParsing)) {
            _playbackLineNum++;

            std::istringstream iss(_playbackLineParsing);
            std::string entryType;
            if (!(iss >> entryType)) {
                LERROR(std::format(
                    "Error reading entry type @ line {} of playback file '{}'",
                    _playbackLineNum, _playbackFilename
                ));
                break;
            }

            if (entryType == HeaderCameraAscii) {
                parsingStatusOk = playbackCamera();
            }
            else if (entryType == HeaderTimeAscii) {
                parsingStatusOk = playbackTimeChange();
            }
            else if (entryType == HeaderScriptAscii) {
                parsingStatusOk = playbackScript();
            }
            else if (entryType.substr(0, 1) == HeaderCommentAscii) {
                continue;
            }
            else {
                LERROR(std::format(
                    "Unknown frame type {} @ line {} of playback file '{}'",
                    entryType, _playbackLineNum, _playbackFilename
                ));
                parsingStatusOk = false;
                break;
            }
        }
        LINFO(std::format(
            "Finished parsing {} entries from playback file '{}'",
            _playbackLineNum, _playbackFilename
        ));
    }

    return parsingStatusOk;
}

double SessionRecording::appropriateTimestamp(Timestamps t3stamps)
{
    if (_playbackTimeReferenceMode == KeyframeTimeRef::Relative_recordedStart) {
        return t3stamps.timeRec;
    }
    else if (_playbackTimeReferenceMode == KeyframeTimeRef::Absolute_simTimeJ2000) {
        return t3stamps.timeSim;
    }
    else {
        return t3stamps.timeOs;
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
        return (global::windowDelegate->applicationTime() -
                _timestampPlaybackStarted_application) - _playbackPauseOffset;
    }
    else if (_playbackTimeReferenceMode == KeyframeTimeRef::Absolute_simTimeJ2000) {
        return global::timeManager->time().j2000Seconds();
    }
    else {
        return global::windowDelegate->applicationTime() - _playbackPauseOffset;
    }
}

double SessionRecording::fixedDeltaTimeDuringFrameOutput() const {
    // Check if renderable in focus is still resolving tile loading
    // do not adjust time while we are doing this
    const SceneGraphNode* focusNode =
        global::navigationHandler->orbitalNavigator().anchorNode();
    const Renderable* focusRenderable = focusNode->renderable();
    if (!focusRenderable || focusRenderable->renderedWithDesiredData()) {
        return _saveRenderingDeltaTime;
    }
    else {
        return 0;
    }
}

std::chrono::steady_clock::time_point
SessionRecording::currentPlaybackInterpolationTime() const {
    return _saveRenderingCurrentRecordedTime_interpolation;
}

double SessionRecording::currentApplicationInterpolationTime() const {
    return _saveRenderingCurrentApplicationTime_interpolation;
}

bool SessionRecording::playbackCamera() {
    Timestamps times;
    datamessagestructures::CameraKeyframe kf;

    bool success = readSingleKeyframeCamera(
        kf,
        times,
        _recordingDataMode,
        _playbackFile,
        _playbackLineParsing,
        _playbackLineNum
    );

    const interaction::KeyframeNavigator::CameraPose pbFrame(std::move(kf));
    if (success) {
        success = addKeyframe(
            {times.timeOs, times.timeRec, times.timeSim},
            pbFrame,
            _playbackLineNum
        );
    }
    return success;
}

bool SessionRecording::convertCamera(std::stringstream& inStream, DataMode mode,
                                     int lineNum, std::string& inputLine,
                                     std::ofstream& outFile, unsigned char* buffer)
{
    Timestamps times;
    datamessagestructures::CameraKeyframe kf;

    const bool success = readSingleKeyframeCamera(
        kf,
        times,
        mode,
        reinterpret_cast<std::ifstream&>(inStream),
        inputLine,
        lineNum
    );
    if (success) {
        saveSingleKeyframeCamera(
            kf,
            times,
            mode,
            outFile,
            buffer
        );
    }
    return success;
}

bool SessionRecording::readSingleKeyframeCamera(datamessagestructures::CameraKeyframe& kf,
                                                Timestamps& times, DataMode mode,
                                                std::ifstream& file, std::string& inLine,
                                                const int lineNum)
{
    if (mode == DataMode::Binary) {
        return readCameraKeyframeBinary(times, kf, file, lineNum);
    }
    else {
        return readCameraKeyframeAscii(times, kf, inLine, lineNum);
    }
}

void SessionRecording::saveSingleKeyframeCamera(datamessagestructures::CameraKeyframe& kf,
                                                Timestamps& times, DataMode mode,
                                                std::ofstream& file,
                                                unsigned char* buffer)
{
    if (mode == DataMode::Binary) {
        saveCameraKeyframeBinary(times, kf, buffer, file);
    }
    else {
        saveCameraKeyframeAscii(times, kf, file);
    }
}

bool SessionRecording::readCameraKeyframeBinary(Timestamps& times,
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
        LERROR(std::format(
            "Allocation error with camera playback from keyframe entry {}",
            lineN - 1
        ));
        return false;
    }
    catch (std::length_error&) {
        LERROR(std::format(
            "length_error with camera playback from keyframe entry {}",
            lineN - 1
        ));
        return false;
    }

    if (!file) {
        LINFO(std::format(
            "Error reading camera playback from keyframe entry {}",
            lineN - 1
        ));
        return false;
    }
    return true;
}

bool SessionRecording::readCameraKeyframeAscii(Timestamps& times,
                                               datamessagestructures::CameraKeyframe& kf,
                                               const std::string& currentParsingLine,
                                               int lineN)
{
    std::istringstream iss = std::istringstream(currentParsingLine);
    std::string entryType;
    iss >> entryType;
    iss >> times.timeOs >> times.timeRec >> times.timeSim;
    kf.read(iss);
    // ASCII format does not contain trailing timestamp so add it here
    kf._timestamp = times.timeOs;

    if (iss.fail() || !iss.eof()) {
        LERROR(std::format("Error parsing camera line {} of playback file", lineN));
        return false;
    }
    return true;
}

bool SessionRecording::playbackTimeChange() {
    Timestamps times;
    datamessagestructures::TimeKeyframe kf;

    bool success = readSingleKeyframeTime(
        kf,
        times,
        _recordingDataMode,
        _playbackFile,
        _playbackLineParsing,
        _playbackLineNum
    );
    kf._timestamp = equivalentApplicationTime(times.timeOs, times.timeRec, times.timeSim);
    kf._time = kf._timestamp + _timestampApplicationStarted_simulation;
    if (success) {
        success = addKeyframe(
            {times.timeOs, times.timeRec, times.timeSim},
            kf,
            _playbackLineNum
        );
    }
    return success;
}

bool SessionRecording::convertTimeChange(std::stringstream& inStream, DataMode mode,
                                         int lineNum, std::string& inputLine,
                                         std::ofstream& outFile, unsigned char* buffer)
{
    Timestamps times;
    datamessagestructures::TimeKeyframe kf;

    const bool success = readSingleKeyframeTime(
        kf,
        times,
        mode,
        reinterpret_cast<std::ifstream&>(inStream),
        inputLine,
        lineNum
    );
    if (success) {
        saveSingleKeyframeTime(
            kf,
            times,
            mode,
            outFile,
            buffer
        );
    }
    return success;
}

bool SessionRecording::readSingleKeyframeTime(datamessagestructures::TimeKeyframe& kf,
                                              Timestamps& times, DataMode mode,
                                              std::ifstream& file, std::string& inLine,
                                              const int lineNum)
{
    if (mode == DataMode::Binary) {
        return readTimeKeyframeBinary(times, kf, file, lineNum);
    } else {
        return readTimeKeyframeAscii(times, kf, inLine, lineNum);
    }
}

void SessionRecording::saveSingleKeyframeTime(datamessagestructures::TimeKeyframe& kf,
                                              Timestamps& times, DataMode mode,
                                              std::ofstream& file, unsigned char* buffer)
{
    if (mode == DataMode::Binary) {
        saveTimeKeyframeBinary(times, kf, buffer, file);
    } else {
        saveTimeKeyframeAscii(times, kf, file);
    }
}

bool SessionRecording::readTimeKeyframeBinary(Timestamps& times,
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
        LERROR(std::format(
            "Allocation error with time playback from keyframe entry {}",
            lineN - 1
        ));
        return false;
    }
    catch (std::length_error&) {
        LERROR(std::format(
            "length_error with time playback from keyframe entry {}",
            lineN - 1
        ));
        return false;
    }

    if (!file) {
        LERROR(std::format(
            "Error reading time playback from keyframe entry {}", lineN - 1
        ));
        return false;
    }
    return true;
}

bool SessionRecording::readTimeKeyframeAscii(Timestamps& times,
                                             datamessagestructures::TimeKeyframe& kf,
                                             const std::string& currentParsingLine,
                                             int lineN)
{
    std::string entryType;

    std::istringstream iss(currentParsingLine);
    iss >> entryType;
    iss >> times.timeOs >> times.timeRec >> times.timeSim;
    kf.read(iss);

    if (iss.fail() || !iss.eof()) {
        LERROR(std::format(
            "Error parsing time line {} of playback file", lineN
        ));
        return false;
    }
    return true;
}

std::string SessionRecording::readHeaderElement(std::ifstream& stream,
                                                size_t readLenChars)
{
    std::vector<char> readTemp(readLenChars);
    stream.read(readTemp.data(), readLenChars);
    return std::string(readTemp.begin(), readTemp.end());
}

std::string SessionRecording::readHeaderElement(std::stringstream& stream,
                                                size_t readLenChars)
{
    std::vector<char> readTemp = std::vector<char>(readLenChars);
    stream.read(readTemp.data(), readLenChars);
    return std::string(readTemp.begin(), readTemp.end());
}

bool SessionRecording::playbackScript() {
    Timestamps times;
    datamessagestructures::ScriptMessage kf;

    bool success = readSingleKeyframeScript(
        kf,
        times,
        _recordingDataMode,
        _playbackFile,
        _playbackLineParsing,
        _playbackLineNum
    );

    checkIfScriptUsesScenegraphNode(kf._script);

    if (success) {
        success = addKeyframe(
            {times.timeOs, times.timeRec, times.timeSim},
            kf._script,
            _playbackLineNum
        );
    }
    return success;
}

void SessionRecording::populateListofLoadedSceneGraphNodes() {
    const std::vector<SceneGraphNode*> nodes =
        global::renderEngine->scene()->allSceneGraphNodes();
    for (SceneGraphNode* n : nodes) {
        _loadedNodes.push_back(n->identifier());
    }
}

void SessionRecording::checkIfScriptUsesScenegraphNode(std::string s) {
    if (s.rfind(scriptReturnPrefix, 0) == 0) {
        s.erase(0, scriptReturnPrefix.length());
    }
    // This works for both setPropertyValue and setPropertyValueSingle
    const bool containsSetPropertyVal = (s.rfind("openspace.setPropertyValue", 0) == 0);
    const bool containsParensStart = (s.find('(') != std::string::npos);
    if (containsSetPropertyVal && containsParensStart) {
        std::string subjectOfSetProp = isolateTermFromQuotes(s.substr(s.find('(') + 1));
        if (checkForScenegraphNodeAccessNav(subjectOfSetProp)) {
            const size_t commaPos = s.find(',');
            std::string navNode = isolateTermFromQuotes(s.substr(commaPos + 1));
            if (navNode != "nil") {
                auto it = std::find(_loadedNodes.begin(), _loadedNodes.end(), navNode);
                if (it == _loadedNodes.end()) {
                    LWARNING(std::format(
                        "Playback file contains a property setting of navigation using "
                        "scenegraph node '{}', which is not currently loaded", navNode
                    ));
                }
            }
        }
        else if (checkForScenegraphNodeAccessScene(subjectOfSetProp)) {
            std::string found = extractScenegraphNodeFromScene(subjectOfSetProp);
            if (!found.empty()) {
                const std::vector<properties::Property*> matchHits =
                    global::renderEngine->scene()->propertiesMatchingRegex(
                        subjectOfSetProp
                    );
                if (matchHits.empty()) {
                    LWARNING(std::format(
                        "Playback file contains a property setting of scenegraph "
                        "node '{}', which is not currently loaded", found
                    ));
                }
            }
        }
    }
}

bool SessionRecording::checkForScenegraphNodeAccessScene(const std::string& s) {
    const std::string scene = "Scene.";
    return (s.find(scene) != std::string::npos);
}

std::string SessionRecording::extractScenegraphNodeFromScene(const std::string& s) {
    const std::string scene = "Scene.";
    std::string extracted;
    const size_t posScene = s.find(scene);
    if (posScene != std::string::npos) {
        const size_t posDot = s.find('.', posScene + scene.length() + 1);
        if (posDot > posScene && posDot != std::string::npos) {
            extracted = s.substr(posScene + scene.length(), posDot -
                (posScene + scene.length()));
        }
    }
    return extracted;
}

bool SessionRecording::checkForScenegraphNodeAccessNav(std::string& navTerm) {
    const std::string nextTerm = "NavigationHandler.OrbitalNavigator.";
    const size_t posNav = navTerm.find(nextTerm);
    if (posNav != std::string::npos) {
        for (const std::string& accessName : _navScriptsUsingNodes) {
              if (navTerm.find(accessName) != std::string::npos) {
                  return true;
              }
        }
    }
    return false;
}

std::string SessionRecording::isolateTermFromQuotes(std::string s) {
    //Remove any leading spaces
    while (s.front() == ' ') {
        s.erase(0, 1);
    }
    const std::string possibleQuotes = "\'\"[]";
    while (possibleQuotes.find(s.front()) != std::string::npos) {
        s.erase(0, 1);
    }
    for (const char q : possibleQuotes) {
        if (s.find(q) != std::string::npos) {
            s = s.substr(0, s.find(q));
            return s;
        }
    }
    //If no quotes found, remove other possible characters from end
    const std::string unwantedChars = " );";
    while (!s.empty() && (unwantedChars.find(s.back()) != std::string::npos)) {
        s.pop_back();
    }
    return s;
}

void SessionRecording::eraseSpacesFromString(std::string& s) {
    s.erase(std::remove_if(s.begin(), s.end(), ::isspace), s.end());
}

std::string SessionRecording::getNameFromSurroundingQuotes(std::string& s) {
    std::string result;
    const char quote = s.at(0);
    // Handle either ' or " marks
    if (quote == '\'' || quote == '\"') {
        const size_t quoteCount = std::count(s.begin(), s.end(), quote);
        // Must be an opening and closing quote char
        if (quoteCount == 2) {
            result = s.substr(1, s.rfind(quote) - 1);
        }
    }
    return result;
}

bool SessionRecording::checkIfInitialFocusNodeIsLoaded(unsigned int firstCamIndex) {
    if (_keyframesCamera.empty()) {
        return true;
    }

    std::string startFocusNode =
        _keyframesCamera[_timeline[firstCamIndex].idxIntoKeyframeTypeArray].focusNode;
    auto it = std::find(_loadedNodes.begin(), _loadedNodes.end(), startFocusNode);
    if (it == _loadedNodes.end()) {
        LERROR(std::format(
            "Playback file requires scenegraph node '{}', which is "
            "not currently loaded", startFocusNode
        ));
        return false;
    }
    return true;
}


bool SessionRecording::convertScript(std::stringstream& inStream, DataMode mode,
                                     int lineNum, std::string& inputLine,
                                     std::ofstream& outFile, unsigned char* buffer)
{
    Timestamps times;
    datamessagestructures::ScriptMessage kf;

    const bool success = readSingleKeyframeScript(
        kf,
        times,
        mode,
        reinterpret_cast<std::ifstream&>(inStream),
        inputLine,
        lineNum
    );
    if (success) {
        saveSingleKeyframeScript(
            kf,
            times,
            mode,
            outFile,
            buffer
        );
    }
    return success;
}

bool SessionRecording::readSingleKeyframeScript(datamessagestructures::ScriptMessage& kf,
                                                Timestamps& times, DataMode mode,
                                                std::ifstream& file, std::string& inLine,
                                                const int lineNum)
{
    if (mode == DataMode::Binary) {
        return readScriptKeyframeBinary(times, kf, file, lineNum);
    }
    else {
        return readScriptKeyframeAscii(times, kf, inLine, lineNum);
    }
}

void SessionRecording::saveSingleKeyframeScript(datamessagestructures::ScriptMessage& kf,
                                                Timestamps& times, DataMode mode,
                                                std::ofstream& file,
                                                unsigned char* buffer)
{
    if (mode == DataMode::Binary) {
        saveScriptKeyframeBinary(times, kf, buffer, file);
    }
    else {
        saveScriptKeyframeAscii(times, kf, file);
    }
}

bool SessionRecording::readScriptKeyframeBinary(Timestamps& times,
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
        LERROR(std::format(
            "Allocation error with script playback from keyframe entry {}",
            lineN - 1
        ));
        return false;
    }
    catch (std::length_error&) {
        LERROR(std::format(
            "length_error with script playback from keyframe entry {}",
            lineN - 1
        ));
        return false;
    }

    if (!file) {
        LERROR(std::format(
            "Error reading script playback from keyframe entry {}",
            lineN - 1
        ));
        return false;
    }
    return true;
}

bool SessionRecording::readScriptKeyframeAscii(Timestamps& times,
                                               datamessagestructures::ScriptMessage& kf,
                                               const std::string& currentParsingLine,
                                               int lineN)
{
    std::string entryType;
    std::istringstream iss(currentParsingLine);
    iss >> entryType;
    iss >> times.timeOs >> times.timeRec >> times.timeSim;
    kf.read(iss);
    if (iss.fail()) {
        LERROR(std::format("Error parsing script line {} of playback file", lineN));
        return false;
    }
    else if (!iss.eof()) {
        LERROR(std::format("Did not find an EOL at line {} of playback file", lineN));
        return false;
    }
    return true;
}

bool SessionRecording::addKeyframeToTimeline(std::vector<TimelineEntry>& timeline,
                                             RecordedType type,
                                             size_t indexIntoTypeKeyframes,
                                             Timestamps t3stamps, int lineNum)
{
    try {
        timeline.push_back({
            type,
            static_cast<unsigned int>(indexIntoTypeKeyframes),
            t3stamps
        });
    }
    catch(...) {
        LERROR(std::format(
            "Timeline memory allocation error trying to add keyframe {}. The playback "
            "file may be too large for system memory",
            lineNum - 1
        ));
        return false;
    }
    return true;
}

bool SessionRecording::addKeyframe(Timestamps t3stamps,
                                   interaction::KeyframeNavigator::CameraPose keyframe,
                                   int lineNum)
{
    const size_t indexIntoCameraKeyframesFromMainTimeline = _keyframesCamera.size();
    _keyframesCamera.push_back(std::move(keyframe));
    return addKeyframeToTimeline(
        _timeline,
        RecordedType::Camera,
        indexIntoCameraKeyframesFromMainTimeline,
        t3stamps,
        lineNum
    );
}

bool SessionRecording::addKeyframe(Timestamps t3stamps,
                                   datamessagestructures::TimeKeyframe keyframe,
                                   int lineNum)
{
    const size_t indexIntoTimeKeyframesFromMainTimeline = _keyframesTime.size();
    _keyframesTime.push_back(std::move(keyframe));
    return addKeyframeToTimeline(
        _timeline,
        RecordedType::Time,
        indexIntoTimeKeyframesFromMainTimeline,
        t3stamps,
        lineNum
    );
}

bool SessionRecording::addKeyframe(Timestamps t3stamps,
                                   std::string scriptToQueue,
                                   int lineNum)
{
    const size_t indexIntoScriptKeyframesFromMainTimeline = _keyframesScript.size();
    _keyframesScript.push_back(std::move(scriptToQueue));
    return addKeyframeToTimeline(
        _timeline,
        RecordedType::Script,
        indexIntoScriptKeyframesFromMainTimeline,
        t3stamps,
        lineNum
    );
}

void SessionRecording::moveAheadInTime() {
    using namespace std::chrono;

    const bool playbackPaused = (_state == SessionState::PlaybackPaused);
    if (playbackPaused) {
        _playbackPauseOffset
            += global::windowDelegate->applicationTime() - _previousTime;
    }
    _previousTime = global::windowDelegate->applicationTime();

    const double currTime = currentTime();
    lookForNonCameraKeyframesThatHaveComeDue(currTime);
    updateCameraWithOrWithoutNewKeyframes(currTime);
    // Unfortunately the first frame is sometimes rendered because globebrowsing reports
    // that all chunks are rendered when they apparently are not.
    if (_saveRendering_isFirstFrame) {
        _saveRendering_isFirstFrame = false;
        return;
    }
    if (_shouldWaitForFinishLoadingWhenPlayback && isSavingFramesDuringPlayback()) {
        // Check if renderable in focus is still resolving tile loading
        // do not adjust time while we are doing this, or take screenshot
        const SceneGraphNode* focusNode =
            global::navigationHandler->orbitalNavigator().anchorNode();
        const Renderable* focusRenderable = focusNode->renderable();
        if (!focusRenderable || focusRenderable->renderedWithDesiredData()) {
            if (!playbackPaused) {
                _saveRenderingCurrentRecordedTime_interpolation +=
                    _saveRenderingDeltaTime_interpolation_usec;
                _saveRenderingCurrentRecordedTime += _saveRenderingDeltaTime;
                _saveRenderingCurrentApplicationTime_interpolation +=
                    _saveRenderingDeltaTime;
                global::renderEngine->takeScreenshot();
            }
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

    const bool didFindFutureCameraKeyframes = findNextFutureCameraIndex(currTime);
    const bool isPrevAtFirstKeyframe =
        (_idxTimeline_cameraPtrPrev == _idxTimeline_cameraFirstInTimeline);
    const bool isFirstTimelineCameraKeyframeInFuture =
        (currTime < _cameraFirstInTimeline_timestamp);

    if (! (isPrevAtFirstKeyframe && isFirstTimelineCameraKeyframeInFuture)) {
        processCameraKeyframe(currTime);
    }
    if (!didFindFutureCameraKeyframes) {
        signalPlaybackFinishedForComponent(RecordedType::Camera);
    }
}

bool SessionRecording::isTimeToHandleNextNonCameraKeyframe(double currTime) {
    const bool nonCameraPlaybackActive = (_playbackActive_time || _playbackActive_script);
    return (currTime > getNextTimestamp()) && nonCameraPlaybackActive;
}

bool SessionRecording::findNextFutureCameraIndex(double currTime) {
    unsigned int seekAheadIndex = _idxTimeline_cameraPtrPrev;
    while (true) {
        seekAheadIndex++;
        if (seekAheadIndex >= static_cast<unsigned int>(_timeline.size())) {
            seekAheadIndex = static_cast<unsigned int>(_timeline.size()) - 1;
        }

        if (doesTimelineEntryContainCamera(seekAheadIndex)) {
            const unsigned int indexIntoCameraKeyframes =
                _timeline[seekAheadIndex].idxIntoKeyframeTypeArray;
            const double seekAheadKeyframeTimestamp
                = appropriateTimestamp(_timeline[seekAheadIndex].t3stamps);

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

        const double interpolationUpperBoundTimestamp =
            appropriateTimestamp(_timeline[_idxTimeline_cameraPtrNext].t3stamps);
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
            LERROR(std::format(
                "Bad keyframe type encountered during playback at index {}",
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

    unsigned int prevIdx = 0;
    unsigned int nextIdx = 0;
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
    const double prevTime = appropriateTimestamp(
        _timeline[_idxTimeline_cameraPtrPrev].t3stamps
    );
    // getNextTimestamp();
    const double nextTime = appropriateTimestamp(
        _timeline[_idxTimeline_cameraPtrNext].t3stamps
    );

    double t = 0.0;
    if ((nextTime - prevTime) >= 1e-7) {
        t = (now - prevTime) / (nextTime - prevTime);
    }

#ifdef INTERPOLATION_DEBUG_PRINT
    LINFOC("prev", std::to_string(prevTime));
    LINFOC("now", std::to_string(prevTime + t));
    LINFOC("next", std::to_string(nextTime));
#endif

    // Need to activly update the focusNode position of the camera in relation to
    // the rendered objects will be unstable and actually incorrect
    Camera* camera = global::navigationHandler->camera();
    Scene* scene = camera->parent()->scene();

    const SceneGraphNode* n = scene->sceneGraphNode(_keyframesCamera[prevIdx].focusNode);
    if (n) {
        global::navigationHandler->orbitalNavigator().setFocusNode(n->identifier());
    }

    return interaction::KeyframeNavigator::updateCamera(
        global::navigationHandler->camera(),
        prevPose,
        nextPose,
        t,
        _ignoreRecordedScale
    );
}

bool SessionRecording::processScriptKeyframe() {
    if (!_playbackActive_script || _keyframesScript.empty()) {
        return false;
    }

    const std::string nextScript = nextKeyframeObj(
        _idxScript,
        _keyframesScript,
        ([this]() { signalPlaybackFinishedForComponent(RecordedType::Script); })
    );
    global::scriptEngine->queueScript(
        nextScript,
        scripting::ScriptEngine::ShouldBeSynchronized::Yes,
        scripting::ScriptEngine::ShouldSendToRemote::Yes
    );

    return true;
}

double SessionRecording::getNextTimestamp() {
    if (_timeline.empty()) {
        return 0.0;
    }
    else if (_idxTimeline_nonCamera < _timeline.size()) {
        return appropriateTimestamp(_timeline[_idxTimeline_nonCamera].t3stamps);
    }
    else {
        return appropriateTimestamp(_timeline.back().t3stamps);
    }
}

double SessionRecording::getPrevTimestamp() {
    if (_timeline.empty()) {
        return 0.0;
    }
    else if (_idxTimeline_nonCamera == 0) {
        return appropriateTimestamp(_timeline.front().t3stamps);
    }
    else if (_idxTimeline_nonCamera < _timeline.size()) {
        return appropriateTimestamp(_timeline[_idxTimeline_nonCamera - 1].t3stamps);
    }
    else {
        return appropriateTimestamp(_timeline.back().t3stamps);
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

void SessionRecording::saveKeyframeToFile(const std::string& entry, std::ofstream& file) {
    file << entry << '\n';
}

SessionRecording::CallbackHandle SessionRecording::addStateChangeCallback(
                                                                   StateChangeCallback cb)
{
    const CallbackHandle handle = _nextCallbackHandle++;
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
    const std::filesystem::path path = absPath("${RECORDINGS}");

    std::vector<std::string> fileList;
    if (std::filesystem::is_directory(path)) {
        namespace fs = std::filesystem;
        for (const fs::directory_entry& e : fs::directory_iterator(path)) {
            if (!e.is_regular_file()) {
                continue;
            }

            // Remove path and keep only the filename
            const std::string filename = e.path().filename().string();
#ifdef WIN32
            DWORD attributes = GetFileAttributes(e.path().string().c_str());
            bool isHidden = attributes & FILE_ATTRIBUTE_HIDDEN;
#else
            const bool isHidden = filename.find('.') == 0;
#endif // WIN32
            if (!isHidden) {
                // Don't add hidden files
                fileList.push_back(filename);
            }
        }
    }
    std::sort(fileList.begin(), fileList.end());
    return fileList;
}

void SessionRecording::readPlaybackHeader_stream(std::stringstream& conversionInStream,
                                                 std::string& version, DataMode& mode)
{
    // Read header
    const std::string readBackHeaderString = readHeaderElement(
        conversionInStream,
        FileHeaderTitle.length()
    );

    if (readBackHeaderString != FileHeaderTitle) {
        throw ConversionError("File to convert does not contain expected header");
    }
    version = readHeaderElement(conversionInStream, FileHeaderVersionLength);
    std::string readDataMode = readHeaderElement(conversionInStream, 1);
    if (readDataMode[0] == DataFormatAsciiTag) {
        mode = DataMode::Ascii;
    }
    else if (readDataMode[0] == DataFormatBinaryTag) {
        mode = DataMode::Binary;
    }
    else {
        throw ConversionError("Unknown data type in header (needs Ascii or Binary)");
    }
    // Read to throw out newline at end of header
    readHeaderElement(conversionInStream, 1);
}

SessionRecording::DataMode SessionRecording::readModeFromHeader(
                                                              const std::string& filename)
{
    DataMode mode = DataMode::Unknown;
    std::ifstream inputFile;
    // Open in ASCII first
    inputFile.open(filename, std::ifstream::in);
    // Read header
    const std::string readBackHeaderString = readHeaderElement(
        inputFile,
        FileHeaderTitle.length()
    );
    if (readBackHeaderString != FileHeaderTitle) {
        LERROR("Specified playback file does not contain expected header");
    }
    readHeaderElement(inputFile, FileHeaderVersionLength);
    std::string readDataMode = readHeaderElement(inputFile, 1);
    if (readDataMode[0] == DataFormatAsciiTag) {
        mode = DataMode::Ascii;
    }
    else if (readDataMode[0] == DataFormatBinaryTag) {
        mode = DataMode::Binary;
    }
    else {
        throw ConversionError("Unknown data type in header (should be Ascii or Binary)");
    }
    return mode;
}

void SessionRecording::readFileIntoStringStream(std::string filename,
                                                std::ifstream& inputFstream,
                                                std::stringstream& stream)
{
    std::filesystem::path conversionInFilename = absPath(filename);
    if (!std::filesystem::is_regular_file(conversionInFilename)) {
        throw ConversionError(std::format(
            "Cannot find the specified playback file '{}' to convert",
            conversionInFilename
        ));
    }

    const DataMode mode = readModeFromHeader(conversionInFilename.string());

    stream.str("");
    stream.clear();
    inputFstream.close();
    if (mode == DataMode::Binary) {
        inputFstream.open(conversionInFilename, std::ifstream::in | std::ios::binary);
    }
    else {
        inputFstream.open(conversionInFilename, std::ifstream::in);
    }
    stream << inputFstream.rdbuf();
    if (!inputFstream.is_open() || !inputFstream.good()) {
        throw ConversionError(std::format(
            "Unable to open file '{}' for conversion", filename
        ));
    }
    inputFstream.close();
}

void SessionRecording::convertFileRelativePath(std::string filenameRelative) {
    const std::filesystem::path path = absPath(std::move(filenameRelative));
    convertFile(path.string());
}

std::string SessionRecording::convertFile(std::string filename, int depth) {
    std::string conversionOutFilename = filename;
    std::ifstream conversionInFile;
    std::stringstream conversionInStream;
    if (depth >= _maximumRecursionDepth) {
        LERROR("Runaway recursion in session recording conversion of file version");
        exit(EXIT_FAILURE);
    }
    std::string newFilename = filename;
    try {
        readFileIntoStringStream(filename, conversionInFile, conversionInStream);
        DataMode mode = DataMode::Unknown;
        std::string fileVersion;
        readPlaybackHeader_stream(
            conversionInStream,
            fileVersion,
            mode
        );
        const int conversionLineNum = 1;

        // If this instance of the SessionRecording class isn't the instance with the
        // correct version of the file to be converted, then call getLegacy() to recurse
        // to the next level down in the legacy subclasses until we get the right
        // version, then proceed with conversion from there.
        if (fileVersion != fileFormatVersion()) {
            //conversionInStream.seekg(conversionInStream.beg);
            newFilename = getLegacyConversionResult(filename, depth + 1);
            removeTrailingPathSlashes(newFilename);
            if (filename == newFilename) {
                return filename;
            }
            readFileIntoStringStream(newFilename, conversionInFile, conversionInStream);
            readPlaybackHeader_stream(
                conversionInStream,
                fileVersion,
                mode
            );
        }
        if (depth != 0) {
            conversionOutFilename = determineConversionOutFilename(filename, mode);
            LINFO(std::format(
                "Starting conversion on rec file '{}', version {} in {} mode. "
                "Writing result to '{}'",
                newFilename, fileVersion, (mode == DataMode::Ascii) ? "ascii" : "binary",
                conversionOutFilename
            ));
            std::ofstream conversionOutFile;
            if (mode == DataMode::Binary) {
                conversionOutFile.open(conversionOutFilename, std::ios::binary);
            }
            else {
                conversionOutFile.open(conversionOutFilename);
            }
            if (!conversionOutFile.is_open() || !conversionOutFile.good()) {
                LERROR(std::format(
                    "Unable to open file '{}' for conversion result",
                    conversionOutFilename
                ));
                return "";
            }
            conversionOutFile << FileHeaderTitle;
            conversionOutFile.write(
                targetFileFormatVersion().c_str(),
                FileHeaderVersionLength
            );
            if (mode == DataMode::Binary) {
                conversionOutFile << DataFormatBinaryTag;
            }
            else {
                conversionOutFile << DataFormatAsciiTag;
            }
            conversionOutFile << '\n';
            convertEntries(
                newFilename,
                conversionInStream,
                mode,
                conversionLineNum,
                conversionOutFile
            );
            conversionOutFile.close();
        }
        conversionInFile.close();
    }
    catch (ConversionError& c) {
        LERROR(c.message);
    }

    if (depth == 0) {
        return newFilename;
    }
    else {
        return conversionOutFilename;
    }
}

bool SessionRecording::convertEntries(std::string& inFilename,
                                      std::stringstream& inStream, DataMode mode,
                                      int lineNum, std::ofstream& outFile)
{
    bool conversionStatusOk = true;
    std::string lineParsing;

    if (mode == DataMode::Binary) {
        while (conversionStatusOk) {
            unsigned char frameType = readFromPlayback<unsigned char>(inStream);
            // Check if have reached EOF
            if (!inStream) {
                LINFO(std::format(
                    "Finished converting {} entries from playback file '{}'",
                    lineNum - 1, inFilename
                ));
                break;
            }
            if (frameType == HeaderCameraBinary) {
                conversionStatusOk = convertCamera(
                    inStream,
                    mode,
                    lineNum,
                    lineParsing,
                    outFile,
                    _keyframeBuffer
                );
            }
            else if (frameType == HeaderTimeBinary) {
                conversionStatusOk = convertTimeChange(
                    inStream,
                    mode,
                    lineNum,
                    lineParsing,
                    outFile,
                    _keyframeBuffer
                );
            }
            else if (frameType == HeaderScriptBinary) {
                try {
                    conversionStatusOk = convertScript(
                        inStream,
                        mode,
                        lineNum,
                        lineParsing,
                        outFile,
                        _keyframeBuffer
                    );
                }
                catch (ConversionError& c) {
                    LERROR(c.message);
                    conversionStatusOk = false;
                }
            }
            else {
                LERROR(std::format(
                    "Unknown frame type {} @ index {} of conversion file '{}'",
                    frameType, lineNum - 1, inFilename
                ));
                conversionStatusOk = false;
            }
            lineNum++;
        }
    }
    else {
        while (conversionStatusOk && ghoul::getline(inStream, lineParsing)) {
            lineNum++;

            std::istringstream iss(lineParsing);
            std::string entryType;
            if (!(iss >> entryType)) {
                LERROR(std::format(
                    "Error reading entry type @ line {} of conversion file '{}'",
                    lineNum, inFilename
                ));
                break;
            }

            if (entryType == HeaderCameraAscii) {
                conversionStatusOk = convertCamera(
                    inStream,
                    mode,
                    lineNum,
                    lineParsing,
                    outFile,
                    _keyframeBuffer
                );
            }
            else if (entryType == HeaderTimeAscii) {
                conversionStatusOk = convertTimeChange(
                    inStream,
                    mode,
                    lineNum,
                    lineParsing,
                    outFile,
                    _keyframeBuffer
                );
            }
            else if (entryType == HeaderScriptAscii) {
                try {
                    conversionStatusOk = convertScript(
                        inStream,
                        mode,
                        lineNum,
                        lineParsing,
                        outFile,
                        _keyframeBuffer
                    );
                }
                catch (ConversionError& c) {
                    LERROR(c.message);
                    conversionStatusOk = false;
                }
            }
            else if (entryType.substr(0, 1) == HeaderCommentAscii) {
                continue;
            }
            else {
                LERROR(std::format(
                    "Unknown frame type {} @ line {} of conversion file '{}'",
                    entryType, lineNum, inFilename
                ));
                conversionStatusOk = false;
            }
        }
        LINFO(std::format(
            "Finished parsing {} entries from conversion file '{}'",
            lineNum, inFilename
        ));
    }
    return conversionStatusOk;
}

std::string SessionRecording::getLegacyConversionResult(std::string filename, int depth) {
    SessionRecording_legacy_0085 legacy;
    return legacy.convertFile(std::move(filename), depth);
}

std::string SessionRecording_legacy_0085::getLegacyConversionResult(std::string filename,
                                                                    int)
{
    // This method is overriden in each legacy subclass, but does nothing in this instance
    // as the oldest supported legacy version.
    LERROR(std::format(
        "Version 00.85 is the oldest supported legacy file format; no conversion "
        "can be made. It is possible that file '{}' has a corrupted header or an invalid "
        "file format version number",
        filename
    ));
    return filename;
}

std::string SessionRecording::fileFormatVersion() {
    return std::string(FileHeaderVersion);
}

std::string SessionRecording::targetFileFormatVersion() {
    return std::string(FileHeaderVersion);
}

std::string SessionRecording::determineConversionOutFilename(const std::string& filename,
                                                             DataMode mode)
{
    std::string filenameSansExtension = filename;
    const std::string fileExtension = (mode == DataMode::Binary) ?
        FileExtensionBinary : FileExtensionAscii;

    if (filename.find_last_of('.') != std::string::npos) {
        filenameSansExtension = filename.substr(0, filename.find_last_of('.'));
    }
    filenameSansExtension += "_" + fileFormatVersion() + "-" + targetFileFormatVersion();
    return filenameSansExtension + fileExtension;
}

bool SessionRecording_legacy_0085::convertScript(std::stringstream& inStream,
                                                 DataMode mode, int lineNum,
                                                 std::string& inputLine,
                                                 std::ofstream& outFile,
                                                 unsigned char* buffer)
{
    Timestamps times;
    ScriptMessage_legacy_0085 kf;

    const bool success = readSingleKeyframeScript(
        kf,
        times,
        mode,
        reinterpret_cast<std::ifstream&>(inStream),
        inputLine,
        lineNum
    );
    if (success) {
        saveSingleKeyframeScript(kf, times, mode, outFile, buffer);
    }
    return success;
}

scripting::LuaLibrary SessionRecording::luaLibrary() {
    return {
        "sessionRecording",
        {
            codegen::lua::StartRecording,
            codegen::lua::StartRecordingAscii,
            codegen::lua::StopRecording,
            codegen::lua::StartPlaybackDefault,
            codegen::lua::StartPlaybackApplicationTime,
            codegen::lua::StartPlaybackRecordedTime,
            codegen::lua::StartPlaybackSimulationTime,
            codegen::lua::StopPlayback,
            codegen::lua::EnableTakeScreenShotDuringPlayback,
            codegen::lua::DisableTakeScreenShotDuringPlayback,
            codegen::lua::FileFormatConversion,
            codegen::lua::SetPlaybackPause,
            codegen::lua::TogglePlaybackPause,
            codegen::lua::IsPlayingBack,
            codegen::lua::IsRecording
        }
    };
}

} // namespace openspace::interaction
