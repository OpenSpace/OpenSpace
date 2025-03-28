/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef __OPENSPACE_CORE___SESSIONRECORDINGHANDLER___H__
#define __OPENSPACE_CORE___SESSIONRECORDINGHANDLER___H__

#include <openspace/properties/propertyowner.h>

#include <openspace/interaction/sessionrecording.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/scripting/lualibrary.h>

namespace openspace::interaction {

class SessionRecordingHandler : public properties::PropertyOwner {
public:
    enum class SessionState {
        Idle = 0,
        Recording,
        Playback,
        PlaybackPaused
    };

    using CallbackHandle = int;
    using StateChangeCallback = std::function<void()>;

    SessionRecordingHandler();
    ~SessionRecordingHandler() override = default;

    /**
     * This is called with every rendered frame. If in recording state, the camera state
     * will be saved to the recording file (if its state has changed since last). If in
     * playback state, the next keyframe will be used (if it is time to do so).
     */
    void preSynchronization(double dt);

    /**
     * If enabled, calling this function will render information about the session
     * recording that is currently taking place to the screen.
     */
    void render() const;

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
     * \return `true` if recording to file starts without errors
     */
    void startRecording();

    /**
     * Used to stop a recording in progress. If open, the recording file will be closed,
     * and all keyframes deleted from memory.
     * \param filename File saved with recorded keyframes
     */
    void stopRecording(const std::filesystem::path& filename, DataMode dataMode, bool overwrite = false);

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
    void startPlayback(SessionRecording timeline, bool loop,
        bool shouldWaitForFinishedTiles, std::optional<int> saveScreenshotFps);

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
    bool isPlaybackPaused() const;

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
    //void enableTakeScreenShotDuringPlayback(int fps);

    /**
     * Used to disable that renderings are saved during playback.
     */
    //void disableTakeScreenShotDuringPlayback();

    /**
     * Used to check if a session playback is in progress.
     *
     * \return `true` if playback is in progress
     */
    bool isPlayingBack() const;

    void seek(double recordingTime);

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

private:
    void tickPlayback(double dt);
    void tickRecording(double dt);

    void setupPlayback(double startTime);

    void cleanUpTimelinesAndKeyframes();

    void checkIfScriptUsesScenegraphNode(std::string_view script) const;


    properties::BoolProperty _renderPlaybackInformation;
    properties::BoolProperty _ignoreRecordedScale;
    properties::BoolProperty _addModelMatrixinAscii;

    struct {
        double elapsedTime = 0.0;
        bool isLooping = false;
        bool playbackPausedWithDeltaTimePause = false;
        bool waitForLoading = false;

        struct {
            bool enabled = false;
            double deltaTime = 1.0 / 30.0;
            std::chrono::steady_clock::time_point currentRecordedTime;
            double currentApplicationTime = 0.0;
        } saveScreenshots;
    } _playback;

    struct {
        double elapsedTime = 0.0;
    } _recording;

    SessionState _state = SessionState::Idle;
    SessionState _lastState = SessionState::Idle;


    SessionRecording _timeline;
    std::vector<SessionRecording::Entry>::const_iterator _currentEntry =
        _timeline.entries.end();
    std::unordered_map<std::string, std::string> _savePropertiesBaseline;
    std::vector<std::string> _loadedNodes;

    int _nextCallbackHandle = 0;
    std::vector<std::pair<CallbackHandle, StateChangeCallback>> _stateChangeCallbacks;
};

} // namespace openspace::interaction

#endif // __OPENSPACE_CORE___SESSIONRECORDINGHANDLER___H__
