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

#ifndef __OPENSPACE_CORE___SESSIONRECORDING___H__
#define __OPENSPACE_CORE___SESSIONRECORDING___H__

#include <openspace/properties/propertyowner.h>

#include <openspace/navigation/keyframenavigator.h>
#include <openspace/network/messagestructures.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/scripting/lualibrary.h>
#include <vector>
#include <chrono>

namespace openspace::interaction {

struct ConversionError : public ghoul::RuntimeError {
    explicit ConversionError(std::string msg);
};

struct Timestamps {
    double timeOs;
    double timeRec;
    double timeSim;
};

enum class RecordedType {
    Camera = 0,
    Time,
    Script
};

struct TimelineEntry {
    RecordedType keyframeType;
    unsigned int idxIntoKeyframeTypeArray;
    Timestamps t3stamps;
};

class SessionRecording : public properties::PropertyOwner {
public:
    enum class DataMode {
        Ascii = 0,
        Binary,
        Unknown
    };

    enum class SessionState {
        Idle = 0,
        Recording,
        Playback,
        PlaybackPaused
    };

    using CallbackHandle = int;
    using StateChangeCallback = std::function<void()>;

    SessionRecording();
    ~SessionRecording() override = default;

    /**
     * This is called with every rendered frame. If in recording state, the camera state
     * will be saved to the recording file (if its state has changed since last). If in
     * playback state, the next keyframe will be used (if it is time to do so).
     */
    void preSynchronization();

    /**
     * If enabled, calling this function will render information about the session
     * recording that is currently taking place to the screen.
     */
    void render();

    /**
     * Current time based on playback mode.
     */
    double currentTime() const;

    /**
     * Fixed delta time set by user for use during saving of frame during playback mode.
     */
    double fixedDeltaTimeDuringFrameOutput() const;

    /**
     * Returns the number of microseconds that have elapsed since playback started, if
     * playback is set to be in the mode where a screenshot is captured with every
     * rendered frame (enableTakeScreenShotDuringPlayback() is used to enable this mode).
     * At the start of playback, this timer is set to the current steady_clock value.
     * However, during playback it is incremented by the fixed framerate of the playback
     * rather than the actual clock value (as in normal operation).
     *
     * \return Number of microseconds elapsed since playback started in terms of the
     *         number of rendered frames multiplied by the fixed time increment per frame
     */
    std::chrono::steady_clock::time_point currentPlaybackInterpolationTime() const;

    /**
     * Returns the simulated application time. This simulated application time is only
     * used when playback is set to be in the mode where a screenshot is captured with
     * every rendered frame (enableTakeScreenShotDuringPlayback() is used to enable this
     * mode). At the start of playback, this timer is set to the value of the current
     * applicationTime function provided by the window delegate (used during normal mode
     * or playback). However, during playback it is incremented by the fixed framerate of
     * the playback rather than the actual clock value.
     *
     * \return Application time in seconds, for use in playback-with-frames mode
     */
    double currentApplicationInterpolationTime() const;

    /**
     * Starts a recording session, which will save data to the provided filename according
     * to the data format specified, and will continue until recording is stopped using
     * stopRecording() method.
     *
     * \param filename File saved with recorded keyframes
     * \return `true` if recording to file starts without errors
     */
    void startRecording(const std::string& filename);

    /**
     * Starts a recording session, which will save data to the provided filename in ASCII
     * data format until recording is stopped using stopRecording() method.
     *
     * \param dataMode The format in which the session recording is stored
     */
    void setRecordDataFormat(DataMode dataMode);

    /**
     * Used to stop a recording in progress. If open, the recording file will be closed,
     * and all keyframes deleted from memory.
     */
    void stopRecording();

    /**
     * Used to check if a session recording is in progress.
     *
     * \return `true` if recording is in progress
     */
    bool isRecording() const;

    /**
     * Starts a playback session, which can run in one of three different time modes.
     *
     * \param filename File containing recorded keyframes to play back. The file path is
     *                 relative to the base recordings directory specified in the config
     *                 file by the RECORDINGS variable
     * \param timeMode Which of the 3 time modes to use for time reference during
     * \param loop If true then the file will playback in loop mode, continuously looping
     *        back to the beginning until it is manually stopped
     * \param shouldWaitForFinishedTiles If true, the playback will wait for tiles to be
     *        finished before progressing to the next frame. This value is only used when
     *        `enableTakeScreenShotDuringPlayback` was called before. Otherwise this value
     *        will be ignored
     */
    void startPlayback(std::string& filename, bool loop, bool shouldWaitForFinishedTiles);

    /**
     * Used to stop a playback in progress. If open, the playback file will be closed, and
     * all keyframes deleted from memory.
     */
    void stopPlayback();

    /**
     * Returns playback pause status.
     *
     * \return `true` if playback is paused
     */
    bool isPlaybackPaused();

    /**
     * Pauses a playback session. This does both the normal pause functionality of setting
     * simulation delta time to zero, and pausing the progression through the timeline.
     *
     * \param pause If `true`, then will set playback timeline progression to zero
     */
    void setPlaybackPause(bool pause);

    /**
     * Enables that rendered frames should be saved during playback.
     *
     * \param fps Number of frames per second.
     */
    void enableTakeScreenShotDuringPlayback(int fps);

    /**
     * Used to disable that renderings are saved during playback.
     */
    void disableTakeScreenShotDuringPlayback();

    /**
     * Used to check if a session playback is in progress.
     *
     * \return `true` if playback is in progress
     */
    bool isPlayingBack() const;

    /**
     * Is saving frames during playback.
     */
    bool isSavingFramesDuringPlayback() const;

    bool shouldWaitForTileLoading() const;

    /**
     * Used to obtain the state of idle/recording/playback.
     *
     * \return int value of state as defined by struct SessionState
     */
    SessionState state() const;

    private:
    /**
     * Used to trigger a save of the camera states (position, rotation, focus node,
     * whether it is following the rotation of a node, and timestamp). The data will be
     * saved to the recording file only if a recording is currently in progress.
     */
    void saveCameraKeyframeToTimeline();
    public:
    /**
     * Used to trigger a save of a script to the recording file, but only if a recording
     * is currently in progress.
     *
     * \param script String of the Lua command to be saved
     */
    void saveScriptKeyframeToTimeline(std::string script);

    /**
     * \return The Lua library that contains all Lua functions available to affect the
     *         interaction
     */
    static openspace::scripting::LuaLibrary luaLibrary();

    /**
     * Used to request a callback for notification of playback state change.
     *
     * \param cb Function handle for callback
     * \return CallbackHandle value of callback number
     */
    CallbackHandle addStateChangeCallback(StateChangeCallback cb);

    /**
     * Removes the callback for notification of playback state change.
     *
     * \param callback Function handle for the callback
     */
    void removeStateChangeCallback(CallbackHandle handle);

    /**
     * Provides list of available playback files.
     *
     * \return Vector of filenames in recordings dir
     */
    std::vector<std::string> playbackList() const;

    /**
     * Since session recordings only record changes, the initial conditions aren't
     * preserved when a playback starts. This function is called whenever a property value
     * is set and a recording is in progress. Before the set happens, this function will
     * read the current value of the property and store it so that when the recording is
     * finished, the initial state will be added as a set property command at the
     * beginning of the recording file, to be applied when playback starts.
     *
     * \param prop The property being set
     */
    void savePropertyBaseline(properties::Property& prop);

    /**
     * Converts file format of a session recording file to the current format version
     * (will determine the file format conversion to convert from based on the file's
     * header version number).
     *
     * \param filename Name of the file to convert
     * \param depth iteration number to prevent runaway recursion (init call with zero)
     * \return String containing the filename of the previous conversion step. This is
     *         used if there are multiple conversion steps where each step has to use
     *         the output of the previous.
     */
    std::string convertFile(std::string filename, int depth = 0);

    /**
     * Goes to legacy session recording inherited class, and calls its #convertFile
     * method, and then returns the resulting conversion filename.
     *
     * \param filename Name of the file to convert
     * \param depth Iteration number to prevent runaway recursion (init call with zero)
     * \return string containing the filename of the conversion
     */
    virtual std::string getLegacyConversionResult(std::string filename, int depth);

    /**
     * Version string for file format version currently supported by this class.
     *
     * \return string of the file format version this class supports
     */
    virtual std::string fileFormatVersion();

    /**
     * Version string for file format version that a conversion operation will convert to
     * (e.g. upgrades to this version). This is only relevant for inherited classes that
     * support conversion from legacy versions (if called in the base class, it will
     * return its own current version).
     *
     * \return string of the file format version this class supports
     */
    virtual std::string targetFileFormatVersion();

    /**
     * Determines a filename for the conversion result based on the original filename and
     the file format version number.
     *
     * \param filename source filename to be converted
     * \param mode Whether the file is binary or text-based
     *
     * \return pathname of the converted version of the file
     */
    std::string determineConversionOutFilename(const std::string& filename,
        DataMode mode);

protected:
    properties::BoolProperty _renderPlaybackInformation;
    properties::BoolProperty _ignoreRecordedScale;
    properties::BoolProperty _addModelMatrixinAscii;


    Timestamps _timestamps3RecordStarted{ 0.0, 0.0, 0.0 };
    double _timestampPlaybackStarted_application = 0.0;
    double _timestampPlaybackStarted_simulation = 0.0;
    double _timestampApplicationStarted_simulation = 0.0;
    bool hasCameraChangedFromPrev(const datamessagestructures::CameraKeyframe& kfNew);
    void recordCurrentTimePauseState();
    void recordCurrentTimeRate();
    bool playbackCamera();
    bool playbackTimeChange();
    bool playbackScript();
    bool playbackAddEntriesToTimeline(std::string playbackFilename);
    void signalPlaybackFinishedForComponent(RecordedType type);
    void handlePlaybackEnd();

    bool findFirstCameraKeyframeInTimeline();
    Timestamps generateCurrentTimestamp3(double keyframeTime) const;

    void addKeyframe(Timestamps t3stamps,
        interaction::KeyframeNavigator::CameraPose keyframe);
    void addKeyframe(Timestamps t3stamps,
        datamessagestructures::TimeKeyframe keyframe);
    void addKeyframe(Timestamps t3stamps,
        std::string scriptToQueue);
    void addKeyframeToTimeline(std::vector<TimelineEntry>& timeline, RecordedType type,
            size_t indexIntoTypeKeyframes, Timestamps t3stamps);

    void initializePlayback_time(double now);
    void initializePlayback_modeFlags();
    bool initializePlayback_timeline();
    void moveAheadInTime();
    void lookForNonCameraKeyframesThatHaveComeDue(double currTime);
    void updateCameraWithOrWithoutNewKeyframes(double currTime);
    bool isTimeToHandleNextNonCameraKeyframe(double currTime);
    bool processNextNonCameraKeyframeAheadInTime();
    bool findNextFutureCameraIndex(double currTime);
    bool processCameraKeyframe(double now);
    bool processScriptKeyframe();
    void saveScriptKeyframeToPropertiesBaseline(std::string script);
    bool doesTimelineEntryContainCamera(unsigned int index) const;

    double getNextTimestamp();
    void cleanUpPlayback();
    void cleanUpRecording();
    void cleanUpTimelinesAndKeyframes();
    bool convertEntries(std::string& inFilename, std::stringstream& inStream,
        DataMode mode, int lineNum, std::ofstream& outFile);
    virtual bool convertCamera(std::stringstream& inStream, DataMode mode, int lineNum,
        std::string& inputLine, std::ofstream& outFile, unsigned char* buffer);
    virtual bool convertTimeChange(std::stringstream& inStream, DataMode mode,
        int lineNum, std::string& inputLine, std::ofstream& outFile,
        unsigned char* buffer);
    virtual bool convertScript(std::stringstream& inStream, DataMode mode, int lineNum,
        std::string& inputLine, std::ofstream& outFile, unsigned char* buffer);
    void populateListofLoadedSceneGraphNodes();

    void checkIfScriptUsesScenegraphNode(std::string s);
    bool checkIfInitialFocusNodeIsLoaded(unsigned int firstCamIndex);

    DataMode _recordingDataMode = DataMode::Binary;
    SessionState _state = SessionState::Idle;
    SessionState _lastState = SessionState::Idle;
    std::ifstream _playbackFile;
    std::string _playbackLineParsing;
    std::ofstream _recordFile;
    int _playbackLineNum = 1;
    int _recordingEntryNum = 1;
    datamessagestructures::CameraKeyframe _prevRecordedCameraKeyframe;
    bool _playbackActive_camera = false;
    bool _playbackActive_time = false;
    bool _playbackActive_script = false;
    bool _hasHitEndOfCameraKeyframes = false;
    bool _playbackPausedWithinDeltaTimePause = false;
    bool _playbackLoopMode = false;
    double _playbackPauseOffset = 0.0;
    double _previousTime = 0.0;

    bool _saveRenderingDuringPlayback = false;
    double _saveRenderingDeltaTime = 1.0 / 30.0;
    double _saveRenderingCurrentRecordedTime = 0.0;
    bool _shouldWaitForFinishLoadingWhenPlayback = false;
    std::chrono::steady_clock::time_point _saveRenderingCurrentRecordedTime_interpolation;
    double _saveRenderingCurrentApplicationTime_interpolation = 0.0;
    bool _saveRendering_isFirstFrame = true;

    static const size_t keyframeHeaderSize_bytes = 33;
    static const size_t saveBufferCameraSize_min = 82;
    static const size_t saveBufferStringSize_max = 2000;
    static const size_t _saveBufferMaxSize_bytes = keyframeHeaderSize_bytes +
        +saveBufferCameraSize_min + saveBufferStringSize_max;
    unsigned char _keyframeBuffer[_saveBufferMaxSize_bytes];

    bool _cleanupNeededRecording = false;
    bool _cleanupNeededPlayback = false;

    std::vector<interaction::KeyframeNavigator::CameraPose> _keyframesCamera;
    std::vector<datamessagestructures::TimeKeyframe> _keyframesTime;
    std::vector<std::string> _keyframesScript;
    std::vector<TimelineEntry> _timeline;

    std::vector<std::string> _keyframesSavePropertiesBaseline_scripts;
    std::vector<TimelineEntry> _keyframesSavePropertiesBaseline_timeline;
    std::vector<std::string> _propertyBaselinesSaved;

    std::vector<std::string> _loadedNodes;

    unsigned int _idxTimeline_nonCamera = 0;
    unsigned int _idxTime = 0;
    unsigned int _idxScript = 0;

    unsigned int _idxTimeline_cameraPtrNext = 0;
    unsigned int _idxTimeline_cameraPtrPrev = 0;

    unsigned int _idxTimeline_cameraFirstInTimeline = 0;
    double _cameraFirstInTimeline_timestamp = 0;

    int _nextCallbackHandle = 0;
    std::vector<std::pair<CallbackHandle, StateChangeCallback>> _stateChangeCallbacks;
};

// Instructions for bumping the file format version with new changes:
//
// 1. Create a new subclass with the current version # in its name, such as:
//    SessionRecording_legacy_####, which inherits from SessionRecording
// 2. Override any method that changes in the new version. This includes both
//    methods in SessionRecording class and structs in
//    openspace::datamessagestructure that do data read/writes. Make the modified
//    method/struct virtual, and override it in the new legacy subclass. This
//    override will contain the code as it is before the new changes. This will
//    need to be done in every legacy subclass/struct that exists, but only if that
//    subclass does NOT already contain an override of that method/struct.
// 3. Override FileHeaderVersion with the version # of the new subclass (which is
//    the version being replaced by the new changes).
// 4. Override TargetConvertVersion with the version # with the new changes. This
//    is now the version that this legacy subclass converts up to.
// 5. Override getLegacyConversionResult method so that it creates an instance of
//    the new version subclass. This is how the current version looks back to the
//    legacy version that preceded it.
// 6. The convert method for frame types that changed will need to be changed
//    (for example SessionRecording_legacy_0085::convertScript uses its own
//    override of script keyframe for the conversion functionality).

class SessionRecording_legacy_0085 : public SessionRecording {
public:
    static const size_t FileHeaderVersionLength = 5;
    char FileHeaderVersion[FileHeaderVersionLength+1] = "00.85";
    char TargetConvertVersion[FileHeaderVersionLength+1] = "01.00";
    std::string fileFormatVersion() override {
        return std::string(FileHeaderVersion);
    }
    std::string targetFileFormatVersion() override {
        return std::string(TargetConvertVersion);
    }
    std::string getLegacyConversionResult(std::string filename, int depth) override;

    struct ScriptMessage_legacy_0085 : public datamessagestructures::ScriptMessage {
        void read(std::istream* in) override {
            size_t strLen;
            //Read string length from file
            in->read(reinterpret_cast<char*>(&strLen), sizeof(strLen));
            if (strLen > saveBufferStringSize_max) {
                throw ConversionError("Invalid script size for conversion read");
            }
            //Read back full string
            std::vector<char> temp(strLen + 1);
            in->read(temp.data(), strLen);
            temp[strLen] = '\0';

            _script.erase();
            _script = temp.data();
        }
    };

protected:
    bool convertScript(std::stringstream& inStream, DataMode mode, int lineNum,
        std::string& inputLine, std::ofstream& outFile, unsigned char* buffer) override;
};


void convertTypes(SessionRecording::DataMode fileFormatType,
    std::filesystem::path inFilePath, std::filesystem::path outFilePath,
    std::string version);
std::tuple<SessionRecording::DataMode, std::string> determineFormatTypeAndVersion(
    std::filesystem::path inFilePath);

} // namespace openspace

#include "sessionrecording.inl"

#endif // __OPENSPACE_CORE___SESSIONRECORDING___H__
