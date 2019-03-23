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

#ifndef __OPENSPACE_CORE___SESSIONRECORDING___H__
#define __OPENSPACE_CORE___SESSIONRECORDING___H__

#include <openspace/interaction/externinteraction.h>
#include <openspace/interaction/keyframenavigator.h>
#include <openspace/network/messagestructures.h>
#include <openspace/scripting/lualibrary.h>
#include <ghoul/io/socket/tcpsocket.h>

#include <vector>
#include <fstream>
#include <iomanip>

namespace openspace::interaction {
#define RECORD_BINARY

class KeyframeNavigator;

class SessionRecording : public properties::PropertyOwner {
public:
    enum class RecordedDataMode {
        Ascii,
        Binary
    };

    SessionRecording();
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
    * Starts a recording session, which will save data to the provided filename
    * according to the data format specified, and will continue until recording is
    * stopped using stopRecording() method.
    * \param filename file saved with recorded keyframes.
    * \returns true if recording to file starts without errors.
    */
    bool startRecording(const std::string& filename);

    /**
    * Starts a recording session, which will save data to the provided filename
    * in ASCII data format until recording is stopped using stopRecording() method.
    * \param filename file saved with recorded keyframes.
    * \returns true if recording to file starts without errors.
    */
    void setRecordDataFormat(RecordedDataMode dataMode);

    /**
    * Used to stop a recording in progress. If open, the recording file will be closed,
    * and all keyframes deleted from memory.
    */
    void stopRecording();

    /**
    * Used to check if a session recording is in progress.
    * \returns true if recording is in progress.
    */
    bool isRecording() const;

    /**
    * Starts a playback session, which can run in one of three different time modes.
    * \param filename file containing recorded keyframes to play back
    * \param timeMode which of the 3 time modes to use for time reference during
    * \param forceSimTimeAtStart if true simulation time is forced to that of playback
    * playback: recorded time, application time, or simulation time. See the LuaLibrary
    * entry for SessionRecording for details on these time modes.
    * \returns true if recording to file starts without errors.
    */
    bool startPlayback(const std::string& filename, KeyframeTimeRef timeMode,
        bool forceSimTimeAtStart);

    /**
    * Used to stop a playback in progress. If open, the playback file will be closed,
    * and all keyframes deleted from memory.
    */
    void stopPlayback();

    /**
    * Used to check if a session playback is in progress.
    * \returns true if playback is in progress.
    */
    bool isPlayingBack() const;

    /**
    * Used to trigger a save of the camera states (position, rotation, focus node,
    * whether it is following the rotation of a node, and timestamp). The data will
    * be saved to the recording file only if a recording is currently in progress.
    */
    void saveCameraKeyframe();

    /**
    * Used to trigger a save of the current timing states. The data will be saved
    * to the recording file only if a recording is currently in progress.
    */
    void saveTimeKeyframe();

    /**
    * Used to trigger a save of a script to the recording file, but only if a recording
    * is currently in progress.
    * \param scriptToSave String of the Lua command to be saved.
    */
    void saveScriptKeyframe(std::string scriptToSave);

    /**
    * \return The Lua library that contains all Lua functions available to affect the
    * interaction
    */
    static openspace::scripting::LuaLibrary luaLibrary();

private:
    enum class SessionState {
        Idle = 0,
        Recording,
        Playback
    };
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
    bool _isRecording = false;
    double _timestampRecordStarted = 0.0;
    double _timestampPlaybackStarted_application = 0.0;
    double _timestampPlaybackStarted_simulation = 0.0;
    double _timestampApplicationStarted_simulation = 0.0;
    bool hasCameraChangedFromPrev(datamessagestructures::CameraKeyframe kfNew);
    double appropriateTimestamp(double timeOs, double timeRec, double timeSim);
    double equivalentSimulationTime(double timeOs, double timeRec, double timeSim);
    double equivalentApplicationTime(double timeOs, double timeRec, double timeSim);
    double currentTime() const;

    void playbackCamera();
    void playbackTimeChange();
    void playbackScript();
    bool playbackAddEntriesToTimeline();
    void signalPlaybackFinishedForComponent(RecordedType type);
    void writeToFileBuffer(const double src);
    void writeToFileBuffer(std::vector<char>& cvec);
    void writeToFileBuffer(const unsigned char c);
    void writeToFileBuffer(bool b);
    void saveStringToFile(const std::string& s);
    void saveKeyframeToFileBinary(unsigned char* bufferSource, size_t size);
    void findFirstCameraKeyframeInTimeline();
    std::string readHeaderElement(size_t readLen_chars);
    void readFromPlayback(unsigned char& result);
    void readFromPlayback(double& result);
    void readFromPlayback(float& result);
    void readFromPlayback(size_t& result);
    void readFromPlayback(bool& result);
    void readFromPlayback(std::string& result);
    void saveKeyframeToFile(std::string entry);

    void addKeyframe(double timestamp,
        interaction::KeyframeNavigator::CameraPose keyframe);
    void addKeyframe(double timestamp, datamessagestructures::TimeKeyframe keyframe);
    void addKeyframe(double timestamp, std::string scriptToQueue);
    void moveAheadInTime();
    void lookForNonCameraKeyframesThatHaveComeDue(double currTime);
    void updateCameraWithOrWithoutNewKeyframes(double currTime);
    bool isTimeToHandleNextNonCameraKeyframe(double currTime);
    bool processNextNonCameraKeyframeAheadInTime();
    bool findNextFutureCameraIndex(double currTime);
    bool processCameraKeyframe(double now);
    bool processScriptKeyframe();
    bool isDataModeBinary();
    unsigned int findIndexOfLastCameraKeyframeInTimeline();
    bool doesTimelineEntryContainCamera(unsigned int index) const;

    RecordedType getNextKeyframeType();
    RecordedType getPrevKeyframeType();
    double getNextTimestamp();
    double getPrevTimestamp();
    void cleanUpPlayback();

    const bool _usingTimeKeyframes = false;
    const std::string _fileHeaderTitle = "OpenSpace_record/playback";
    static const size_t _fileHeaderVersionLength = 5;
    const char _fileHeaderVersion[_fileHeaderVersionLength] = { '0', '0', '.', '8', '5' };
    const char dataFormatAsciiTag = 'A';
    const char dataFormatBinaryTag = 'B';

    RecordedDataMode _recordingDataMode = RecordedDataMode::Binary;
    SessionState _state = SessionState::Idle;
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

    static const size_t keyframeHeaderSize_bytes = 33;
    static const size_t saveBufferCameraSize_min = 82;
    static const size_t saveBufferStringSize_max = 500;
    static const size_t _saveBufferMaxSize_bytes = keyframeHeaderSize_bytes +
        + saveBufferCameraSize_min
        + saveBufferStringSize_max;
    unsigned char _keyframeBuffer[_saveBufferMaxSize_bytes];
    size_t _bufferIndex = 0;

    bool _cleanupNeeded = false;

    std::vector < interaction::KeyframeNavigator::CameraPose> _keyframesCamera;
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
};

} // namespace openspace

#include "sessionrecording.inl"

#endif // __OPENSPACE_CORE___SESSIONRECORDING___H__
