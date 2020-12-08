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

#ifndef __OPENSPACE_CORE___SESSIONRECORDING___H__
#define __OPENSPACE_CORE___SESSIONRECORDING___H__

#include <openspace/interaction/externinteraction.h>
#include <openspace/interaction/keyframenavigator.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/scripting/lualibrary.h>
#include <vector>

namespace openspace::interaction {

struct ConversionError : public ghoul::RuntimeError {
    explicit ConversionError(std::string msg);
};

class SessionRecording : public properties::PropertyOwner {
public:

    inline static const std::string FileHeaderTitle = "OpenSpace_record/playback";
    inline static const std::string HeaderCameraAscii = "camera";
    inline static const std::string HeaderTimeAscii = "time";
    inline static const std::string HeaderScriptAscii = "script";
    inline static const std::string HeaderCommentAscii = "#";
    inline static const char HeaderCameraBinary = 'c';
    inline static const char HeaderTimeBinary = 't';
    inline static const char HeaderScriptBinary = 's';
    inline static const std::string FileExtensionBinary = ".osrec";
    inline static const std::string FileExtensionAscii = ".osrectxt";

    enum class DataMode {
        Ascii = 0,
        Binary,
        Unknown
    };

    enum class SessionState {
        Idle = 0,
        Recording,
        Playback
    };

    struct Timestamps {
        double timeOs;
        double timeRec;
        double timeSim;
    };

    static const size_t FileHeaderVersionLength = 5;
    char FileHeaderVersion[FileHeaderVersionLength+1] = "01.00";
    char TargetConvertVersion[FileHeaderVersionLength+1] = "01.00";
    static const char DataFormatAsciiTag = 'A';
    static const char DataFormatBinaryTag = 'B';
    static const size_t keyframeHeaderSize_bytes = 33;
    static const size_t saveBufferCameraSize_min = 82;
    static const size_t saveBufferStringSize_max = 2000;
    static const size_t _saveBufferMaxSize_bytes = keyframeHeaderSize_bytes +
        + saveBufferCameraSize_min + saveBufferStringSize_max;

    using CallbackHandle = int;
    using StateChangeCallback = std::function<void()>;

    SessionRecording();
    SessionRecording(bool isGlobal);

    ~SessionRecording();

    /**
     * Used to de-initialize the session recording feature. Any recording or playback
     * in progress will be stopped, files closed, and keyframes in memory deleted.
     */
    void deinitialize();

    /**
     * This is called with every rendered frame. If in recording state, the camera
     * state will be saved to the recording file (if its state has changed since last).
     * If in playback state, the next keyframe will be used (if it is time to do so).
     */
    void preSynchronization();

    /**
     * If enabled, calling this function will render information about the session
     * recording that is currently taking place to the screen.
     */
    void render();

    /**
     * Current time based on playback mode
     */
    double currentTime() const;

    /**
     * Fixed delta time set by user for use during saving of frame during playback mode
     */
    double fixedDeltaTimeDuringFrameOutput() const;

    /**
     * Starts a recording session, which will save data to the provided filename
     * according to the data format specified, and will continue until recording is
     * stopped using stopRecording() method.
     *
     * \param filename file saved with recorded keyframes.
     *
     * \returns true if recording to file starts without errors.
     */
    bool startRecording(const std::string& filename);

    /**
     * Starts a recording session, which will save data to the provided filename
     * in ASCII data format until recording is stopped using stopRecording() method.
     *
     * \param filename file saved with recorded keyframes.
     *
     * \return \c true if recording to file starts without errors
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
     * \return true if recording is in progress
     */
    bool isRecording() const;

    /**
     * Starts a playback session, which can run in one of three different time modes.
     *
     * \param filename file containing recorded keyframes to play back
     * \param timeMode which of the 3 time modes to use for time reference during
     * \param forceSimTimeAtStart if true simulation time is forced to that of playback
     *        playback: recorded time, application time, or simulation time. See the
     *        LuaLibrary entry for SessionRecording for details on these time modes
     *
     * \return \c true if recording to file starts without errors
     */
    bool startPlayback(std::string& filename, KeyframeTimeRef timeMode,
        bool forceSimTimeAtStart);

    /**
     * Used to stop a playback in progress. If open, the playback file will be closed,
     * and all keyframes deleted from memory.
     */
    void stopPlayback();

    /**
     * Enables that rendered frames should be saved during playback
     * \param fps Number of frames per second.
     */
    void enableTakeScreenShotDuringPlayback(int fps);

    /**
     * Used to disable that renderings are saved during playback
     */
    void disableTakeScreenShotDuringPlayback();

    /**
     * Used to check if a session playback is in progress.
     * \returns true if playback is in progress.
     */
    bool isPlayingBack() const;

    /**
     * Is saving frames during playback
     */
    bool isSavingFramesDuringPlayback() const;

    /**
     * Used to obtain the state of idle/recording/playback.
     *
     * \return int value of state as defined by struct SessionState
     */
    SessionState state() const;

    /**
     * Used to trigger a save of the camera states (position, rotation, focus node,
     * whether it is following the rotation of a node, and timestamp). The data will be
     * saved to the recording file only if a recording is currently in progress.
     */
    void saveCameraKeyframe();

    /**
     * Used to trigger a save of the current timing states. The data will be saved to the
     * recording file only if a recording is currently in progress.
     */
    void saveTimeKeyframe();

    /**
     * Used to trigger a save of a script to the recording file, but only if a recording
     * is currently in progress.
     *
     * \param scriptToSave String of the Lua command to be saved
     */
    void saveScriptKeyframe(std::string scriptToSave);

    /**
     * \return The Lua library that contains all Lua functions available to affect the
     * interaction
     */
    static openspace::scripting::LuaLibrary luaLibrary();

    /**
     * Used to request a callback for notification of playback state change.
     *
     * \param cb function handle for callback
     *
     * \return CallbackHandle value of callback number
     */
    CallbackHandle addStateChangeCallback(StateChangeCallback cb);

    /**
     * Removes the callback for notification of playback state change.
     *
     * \param callback function handle for the callback
     */
    void removeStateChangeCallback(CallbackHandle handle);

    /**
     * Provides list of available playback files.
     *
     * \return vector of filenames in recordings dir
     */
    std::vector<std::string> playbackList() const;

    /**
     * Reads a camera keyframe from a binary format playback file, and populates input
     * references with the parameters of the keyframe.
     *
     * \param times reference to a timestamps structure which contains recorded times
     * \param kf reference to a camera keyframe which contains camera details
     * \param file an ifstream reference to the playback file being read
     * \param lineN keyframe number in playback file where this keyframe resides
     *
     * \return true if data read has no errors
     */
    bool readCameraKeyframeBinary(Timestamps& times,
        datamessagestructures::CameraKeyframe& kf, std::ifstream& file, int lineN);

    /**
     * Reads a camera keyframe from an ascii format playback file, and populates input
     * references with the parameters of the keyframe.
     *
     * \param times reference to a timestamps structure which contains recorded times
     * \param kf reference to a camera keyframe which contains camera details
     * \param currentParsingLine string containing the most current line that was read
     * \param lineN line number in playback file where this keyframe resides
     *
     * \return true if data read has no errors
     */
    bool readCameraKeyframeAscii(Timestamps& times,
        datamessagestructures::CameraKeyframe& kf, std::string currentParsingLine,
        int lineN);

    /**
     * Reads a time keyframe from a binary format playback file, and populates input
     * references with the parameters of the keyframe.
     *
     * \param times reference to a timestamps structure which contains recorded times
     * \param kf reference to a time keyframe which contains time details
     * \param file an ifstream reference to the playback file being read
     * \param lineN keyframe number in playback file where this keyframe resides
     *
     * \return true if data read has no errors
     */
    bool readTimeKeyframeBinary(Timestamps& times,
        datamessagestructures::TimeKeyframe& kf, std::ifstream& file, int lineN);

    /**
     * Reads a time keyframe from an ascii format playback file, and populates input
     * references with the parameters of the keyframe.
     *
     * \param times reference to a timestamps structure which contains recorded times
     * \param kf reference to a time keyframe which contains time details
     * \param currentParsingLine string containing the most current line that was read
     * \param lineN line number in playback file where this keyframe resides
     *
     * \return true if data read has no errors
     */
    bool readTimeKeyframeAscii(Timestamps& times,
        datamessagestructures::TimeKeyframe& kf, std::string currentParsingLine,
        int lineN);

    /**
     * Reads a script keyframe from a binary format playback file, and populates input
     * references with the parameters of the keyframe.
     *
     * \param times reference to a timestamps structure which contains recorded times
     * \param kf reference to a script keyframe which contains the size of the script
     *        (in chars) and the text itself
     * \param file an ifstream reference to the playback file being read
     * \param lineN keyframe number in playback file where this keyframe resides
     *
     * \return true if data read has no errors
     */
    bool readScriptKeyframeBinary(Timestamps& times,
        datamessagestructures::ScriptMessage& kf, std::ifstream& file, int lineN);

    /**
     * Reads a script keyframe from an ascii format playback file, and populates input
     * references with the parameters of the keyframe.
     *
     * \param times reference to a timestamps structure which contains recorded times
     * \param kf reference to a script keyframe which contains the size of the script
     *        (in chars) and the text itself
     * \param currentParsingLine string containing the most current line that was read
     * \param lineN line number in playback file where this keyframe resides
     *
     * \return true if data read has no errors
     */
    bool readScriptKeyframeAscii(Timestamps& times,
        datamessagestructures::ScriptMessage& kf, std::string currentParsingLine,
        int lineN);

    /**
     * Writes a camera keyframe to a binary format recording file using a CameraKeyframe
     *
     * \param times reference to a timestamps structure which contains recorded times
     * \param kf reference to a camera keyframe which contains the camera details
     * \param kfBuffer a buffer temporarily used for preparing data to be written
     * \param file an ofstream reference to the recording file being written-to
     */
    void saveCameraKeyframeBinary(Timestamps& times,
        datamessagestructures::CameraKeyframe& kf, unsigned char* kfBuffer,
        std::ofstream& file);

    /**
     * Writes a camera keyframe to an ascii format recording file using a CameraKeyframe
     *
     * \param times reference to a timestamps structure which contains recorded times
     * \param kf reference to a camera keyframe which contains the camera details
     * \param file an ofstream reference to the recording file being written-to
     */
    void saveCameraKeyframeAscii(Timestamps& times,
        datamessagestructures::CameraKeyframe& kf, std::ofstream& file);

    /**
     * Writes a time keyframe to a binary format recording file using a TimeKeyframe
     *
     * \param times reference to a timestamps structure which contains recorded times
     * \param kf reference to a time keyframe which contains the time details
     * \param kfBuffer a buffer temporarily used for preparing data to be written
     * \param file an ofstream reference to the recording file being written-to
     */
    void saveTimeKeyframeBinary(Timestamps& times,
        datamessagestructures::TimeKeyframe& kf, unsigned char* kfBuffer,
        std::ofstream& file);

    /**
     * Writes a time keyframe to an ascii format recording file using a TimeKeyframe
     *
     * \param times reference to a timestamps structure which contains recorded times
     * \param kf reference to a time keyframe which contains the time details
     * \param file an ofstream reference to the recording file being written-to
     */
    void saveTimeKeyframeAscii(Timestamps& times,
        datamessagestructures::TimeKeyframe& kf, std::ofstream& file);

    /**
     * Writes a script keyframe to a binary format recording file using a ScriptMessage
     *
     * \param times reference to a timestamps structure which contains recorded times
     * \param sm reference to a ScriptMessage object which contains the script details
     * \param smBuffer a buffer temporarily used for preparing data to be written
     * \param file an ofstream reference to the recording file being written-to
     */
    void saveScriptKeyframeBinary(Timestamps& times,
        datamessagestructures::ScriptMessage& sm, unsigned char* smBuffer,
        std::ofstream& file);

    /**
     * Writes a script keyframe to an ascii format recording file using a ScriptMessage
     *
     * \param times reference to a timestamps structure which contains recorded times
     * \param sm reference to a ScriptMessage which contains the script details
     * \param file an ofstream reference to the recording file being written-to
     */
    void saveScriptKeyframeAscii(Timestamps& times,
        datamessagestructures::ScriptMessage& sm, std::ofstream& file);

    /**
     * Reads header information from a session recording file
     *
     * \param stream reference to ifstream that contains the session recording file data
     * \param readLen_chars number of characters to be read, which may be the expected
     *        length of the header line, or an arbitrary number of characters within it
     */
    static std::string readHeaderElement(std::ifstream& stream, size_t readLen_chars);

    /**
 * Reads header information from a session recording file
 *
 * \param stringstream reference to ifstream that contains the session recording file data
 * \param readLen_chars number of characters to be read, which may be the expected
 *        length of the header line, or an arbitrary number of characters within it
 */
    static std::string readHeaderElement(std::stringstream& stream, size_t readLen_chars);

    /**
     * Writes a header to a binary recording file buffer
     *
     * \param times reference to a timestamps structure which contains recorded times
     * \param type single character signifying the keyframe type
     * \param kfBuffer the char buffer holding the recording info to be written
     * \param idx index into write buffer (this is updated with the num of chars written)
     */
    static void saveHeaderBinary(Timestamps& times, char type, unsigned char* kfBuffer,
        size_t& idx);

    /**
     * Writes a header to an ascii recording file buffer
     *
     * \param times reference to a timestamps structure which contains recorded times
     * \param type string signifying the keyframe type
     * \param line the stringstream buffer being written to
     */
    static void saveHeaderAscii(Timestamps& times, const std::string& type,
        std::stringstream& line);

    /**
     * Saves a keyframe to an ascii recording file
     *
     * \param entry the ascii string version of the keyframe (any type)
     * \param file ofstream object to write to
     */
    static void saveKeyframeToFile(std::string entry, std::ofstream& file);

    /**
     * Checks if a specified recording file ends with a particular file extension
     *
     * \param filename the name of the file to record to
     * \param extension the file extension to check for
     */
    static bool hasFileExtension(std::string filename, std::string extension);

    /**
     * Converts file format of a session recording file to the current format version
     * (will determine the file format conversion to convert from based on the file's
     * header version number).
     *
     * \param filename name of the file to convert
     * \param depth iteration number to prevent runaway recursion (init call with zero)
     *
     * \return string containing the filename of the previous conversion step. This is
     *         used if there are multiple conversion steps where each step has to use
     *         the output of the previous.
     */
    std::string convertFile(std::string filename, int depth = 0);

    /**
     * Goes to legacy session recording inherited class, and calls its convertFile()
     * method, and then returns the resulting conversion filename.
     *
     * \param filename name of the file to convert
     * \param depth iteration number to prevent runaway recursion (init call with zero)
     *
     * \return string containing the filename of the conversion
     */
    virtual std::string getLegacyConversionResult(std::string filename, int depth);

    /*
     * Version string for file format version currently supported by this class
     *
     * \return string of the file format version this class supports
     */
    virtual std::string fileFormatVersion();

    /*
     * Version string for file format version that a conversion operation will convert
     * to (e.g. upgrades to this version). This is only relevant for inherited classes
     * that support conversion from legacy versions (if called in the base class, it
     * will return its own current version).
     *
     * \return string of the file format version this class supports
     */
    virtual std::string targetFileFormatVersion();

    /*
     * Determines a filename for the conversion result based on the original filename
     * and the file format version number.
     *
     * \param filename source filename to be converted
     *
     * \return pathname of the converted version of the file
     */
    std::string determineConversionOutFilename(const std::string filename, DataMode mode);

protected:
    properties::BoolProperty _renderPlaybackInformation;

    enum class RecordedType {
        Camera = 0,
        Time,
        Script,
        Invalid
    };
    struct timelineEntry {
        RecordedType keyframeType;
        unsigned int idxIntoKeyframeTypeArray;
        double timestamp;
    };
    ExternInteraction _externInteract;
    double _timestampRecordStarted = 0.0;
    double _timestampPlaybackStarted_application = 0.0;
    double _timestampPlaybackStarted_simulation = 0.0;
    double _timestampApplicationStarted_simulation = 0.0;
    bool hasCameraChangedFromPrev(datamessagestructures::CameraKeyframe kfNew);
    double appropriateTimestamp(double timeOs, double timeRec, double timeSim);
    double equivalentSimulationTime(double timeOs, double timeRec, double timeSim);
    double equivalentApplicationTime(double timeOs, double timeRec, double timeSim);
    bool handleRecordingFile(std::string filenameIn);
    static bool isPath(std::string& filename);
    void removeTrailingPathSlashes(std::string& filename);
    void extractFilenameFromPath(std::string& filename);
    bool playbackCamera();
    bool playbackTimeChange();
    bool playbackScript();
    bool playbackAddEntriesToTimeline();
    void signalPlaybackFinishedForComponent(RecordedType type);
    void findFirstCameraKeyframeInTimeline();
    static void saveStringToFile(const std::string& s, unsigned char* kfBuffer,
        size_t& idx, std::ofstream& file);
    static void saveKeyframeToFileBinary(unsigned char* bufferSource, size_t size,
        std::ofstream& file);

    bool addKeyframe(double timestamp,
        interaction::KeyframeNavigator::CameraPose keyframe, int lineNum);
    bool addKeyframe(double timestamp, datamessagestructures::TimeKeyframe keyframe,
        int lineNum);
    bool addKeyframe(double timestamp, std::string scriptToQueue, int lineNum);
    bool addKeyframeToTimeline(RecordedType type, size_t indexIntoTypeKeyframes,
        double timestamp, int lineNum);
    void moveAheadInTime();
    void lookForNonCameraKeyframesThatHaveComeDue(double currTime);
    void updateCameraWithOrWithoutNewKeyframes(double currTime);
    bool isTimeToHandleNextNonCameraKeyframe(double currTime);
    bool processNextNonCameraKeyframeAheadInTime();
    bool findNextFutureCameraIndex(double currTime);
    bool processCameraKeyframe(double now);
    bool processScriptKeyframe();
    bool readSingleKeyframeCamera(datamessagestructures::CameraKeyframe& kf,
        Timestamps& times, DataMode mode, std::ifstream& file,
        std::string& inLine, const int lineNum);
    void saveSingleKeyframeCamera(datamessagestructures::CameraKeyframe& kf,
        Timestamps& times, DataMode mode, std::ofstream& file, unsigned char* buffer);
    bool readSingleKeyframeTime(datamessagestructures::TimeKeyframe& kf,
        Timestamps& times, DataMode mode, std::ifstream& file, std::string& inLine,
        const int lineNum);
    void saveSingleKeyframeTime(datamessagestructures::TimeKeyframe& kf,
        Timestamps& times, DataMode mode, std::ofstream& file, unsigned char* buffer);
    bool readSingleKeyframeScript(datamessagestructures::ScriptMessage& kf,
        Timestamps& times, DataMode mode, std::ifstream& file, std::string& inLine,
        const int lineNum);
    void saveSingleKeyframeScript(datamessagestructures::ScriptMessage& kf,
        Timestamps& times, DataMode mode, std::ofstream& file, unsigned char* buffer);
    unsigned int findIndexOfLastCameraKeyframeInTimeline();
    bool doesTimelineEntryContainCamera(unsigned int index) const;
    std::vector<std::pair<CallbackHandle, StateChangeCallback>> _stateChangeCallbacks;

    RecordedType getNextKeyframeType();
    RecordedType getPrevKeyframeType();
    double getNextTimestamp();
    double getPrevTimestamp();
    void cleanUpPlayback();
    bool convertEntries(std::string& inFilename, std::stringstream& inStream,
        DataMode mode, int lineNum, std::ofstream& outFile);
    virtual bool convertCamera(std::stringstream& inStream, DataMode mode, int lineNum,
        std::string& inputLine, std::ofstream& outFile, unsigned char* buff);
    virtual bool convertTimeChange(std::stringstream& inStream, DataMode mode,
        int lineNum, std::string& inputLine, std::ofstream& outFile, unsigned char* buff);
    virtual bool convertScript(std::stringstream& inStream, DataMode mode, int lineNum,
        std::string& inputLine, std::ofstream& outFile, unsigned char* buff);
    DataMode readModeFromHeader(std::string filename);
    void readPlaybackHeader_stream(std::stringstream& conversionInStream,
        std::string& version, DataMode& mode);

    static void writeToFileBuffer(unsigned char* buf, size_t& idx, double src);
    static void writeToFileBuffer(unsigned char* buf, size_t& idx, std::vector<char>& cv);
    static void writeToFileBuffer(unsigned char* buf, size_t& idx, unsigned char c);
    static void writeToFileBuffer(unsigned char* buf, size_t& idx, bool b);
    void readFileIntoStringStream(std::string filename,
        std::ifstream& inputFstream, std::stringstream& stream);

    DataMode _recordingDataMode = DataMode::Binary;
    SessionState _state = SessionState::Idle;
    SessionState _lastState = SessionState::Idle;
    std::string _playbackFilename;
    std::ifstream _playbackFile;
    std::string _playbackLineParsing;
    std::ofstream _recordFile;
    int _playbackLineNum = 1;
    KeyframeTimeRef _playbackTimeReferenceMode;
    datamessagestructures::CameraKeyframe _prevRecordedCameraKeyframe;
    bool _playbackActive_camera = false;
    bool _playbackActive_time = false;
    bool _playbackActive_script = false;
    bool _hasHitEndOfCameraKeyframes = false;
    bool _setSimulationTimeWithNextCameraKeyframe = false;

    bool _saveRenderingDuringPlayback = false;
    double _saveRenderingDeltaTime = 1.0 / 30.0;
    double _saveRenderingCurrentRecordedTime;

    unsigned char _keyframeBuffer[_saveBufferMaxSize_bytes];

    bool _cleanupNeeded = false;

    std::vector<interaction::KeyframeNavigator::CameraPose> _keyframesCamera;
    std::vector<datamessagestructures::TimeKeyframe> _keyframesTime;
    std::vector<std::string> _keyframesScript;
    std::vector<timelineEntry> _timeline;

    unsigned int _idxTimeline_nonCamera = 0;
    unsigned int _idxTime = 0;
    unsigned int _idxScript = 0;

    unsigned int _idxTimeline_cameraPtrNext = 0;
    unsigned int _idxTimeline_cameraPtrPrev = 0;

    unsigned int _idxTimeline_cameraFirstInTimeline = 0;
    double _cameraFirstInTimeline_timestamp = 0;

    int _nextCallbackHandle = 0;

    DataMode _conversionDataMode = DataMode::Binary;
    int _conversionLineNum = 1;
    const int _maximumRecursionDepth = 50;
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
    SessionRecording_legacy_0085() : SessionRecording() {}
    ~SessionRecording_legacy_0085() {}
    char FileHeaderVersion[FileHeaderVersionLength+1] = "00.85";
    char TargetConvertVersion[FileHeaderVersionLength+1] = "01.00";
    std::string fileFormatVersion() {
        return std::string(FileHeaderVersion);
    }
    std::string targetFileFormatVersion() {
        return std::string(TargetConvertVersion);
    }
    std::string getLegacyConversionResult(std::string filename, int depth);

    struct ScriptMessage_legacy_0085 : public datamessagestructures::ScriptMessage {
        void read(std::istream* in) {
            size_t strLen;
            //Read string length from file
            in->read(reinterpret_cast<char*>(&strLen), sizeof(strLen));
            if (strLen > saveBufferStringSize_max) {
                throw ConversionError("Invalid script size for conversion read.");
            }
            //Read back full string
            std::vector<char> temp(strLen + 1);
            in->read(temp.data(), strLen);
            temp[strLen] = '\0';

            _script.erase();
            _script = temp.data();
        };
    };

protected:
    bool convertScript(std::stringstream& inStream, DataMode mode, int lineNum,
        std::string& inputLine, std::ofstream& outFile, unsigned char* buffer);
};

} // namespace openspace

#include "sessionrecording.inl"

#endif // __OPENSPACE_CORE___SESSIONRECORDING___H__
