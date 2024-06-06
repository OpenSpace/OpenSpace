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

namespace {

/**
 * Starts a recording session. The string argument is the filename used for the file where
 * the recorded keyframes are saved. The file data format is binary.
 */
[[codegen::luawrap]] void startRecording(std::string recordFilePath) {
    using namespace openspace;

    if (recordFilePath.empty()) {
        throw ghoul::lua::LuaError("Filepath string is empty");
    }
    global::sessionRecording->setRecordDataFormat(
        interaction::SessionRecording::DataMode::Binary
    );
    global::sessionRecording->startRecording(recordFilePath);
}

/**
 * Starts a recording session. The string argument is the filename used for the file where
 * the recorded keyframes are saved. The file data format is ASCII.
 */
[[codegen::luawrap]] void startRecordingAscii(std::string recordFilePath) {
    using namespace openspace;

    if (recordFilePath.empty()) {
        throw ghoul::lua::LuaError("Filepath string is empty");
    }
    global::sessionRecording->setRecordDataFormat(
        interaction::SessionRecording::DataMode::Ascii
    );
    global::sessionRecording->startRecording(recordFilePath);
}

// Stops a recording session.
[[codegen::luawrap]] void stopRecording() {
    openspace::global::sessionRecording->stopRecording();
}

/**
 * Starts a playback session with keyframe times that are relative to the time since the
 * recording was started (the same relative time applies to the playback). When playback
 * starts, the simulation time is automatically set to what it was at recording time. The
 * string argument is the filename to pull playback keyframes from (the file path is
 * relative to the RECORDINGS variable specified in the config file). If a second input
 * value of true is given, then playback will continually loop until it is manually
 * stopped.
 */
[[codegen::luawrap("startPlayback")]] void startPlaybackDefault(std::string file,
                                                                bool loop = false,
                                                           bool shouldWaitForTiles = true)
{
    using namespace openspace;

    if (file.empty()) {
        throw ghoul::lua::LuaError("Filepath string is empty");
    }
    global::sessionRecording->startPlayback(
        file,
        interaction::KeyframeTimeRef::Relative_recordedStart,
        true,
        loop,
        shouldWaitForTiles
    );
}

/**
 * Starts a playback session with keyframe times that are relative to application time
 * (seconds since OpenSpace application started). The string argument is the filename to
 * pull playback keyframes from (the file path is relative to the RECORDINGS variable
 * specified in the config file).
 */
[[codegen::luawrap]] void startPlaybackApplicationTime(std::string file) {
    using namespace openspace;

    if (file.empty()) {
        throw ghoul::lua::LuaError("Filepath string is empty");
    }
    global::sessionRecording->startPlayback(
        file,
        interaction::KeyframeTimeRef::Relative_applicationStart,
        false,
        false,
        false
    );
}

/**
 * Starts a playback session with keyframe times that are relative to the time since the
 * recording was started (the same relative time applies to the playback). The string
 * argument is the filename to pull playback keyframes from (the file path is relative to
 * the RECORDINGS variable specified in the config file). If a second input value of true
 * is given, then playback will continually loop until it is manually stopped.
 */
[[codegen::luawrap]] void startPlaybackRecordedTime(std::string file, bool loop = false) {
    using namespace openspace;

    if (file.empty()) {
        throw ghoul::lua::LuaError("Filepath string is empty");
    }
    global::sessionRecording->startPlayback(
        file,
        interaction::KeyframeTimeRef::Relative_recordedStart,
        false,
        loop,
        false
    );
}

/**
 * Starts a playback session with keyframe times that are relative to the simulated date &
 * time. The string argument is the filename to pull playback keyframes from (the file
 * path is relative to the RECORDINGS variable specified in the config file).
 */
[[codegen::luawrap]] void startPlaybackSimulationTime(std::string file) {
    using namespace openspace;

    if (file.empty()) {
        throw ghoul::lua::LuaError("Filepath string is empty");
    }
    global::sessionRecording->startPlayback(
        file,
        interaction::KeyframeTimeRef::Absolute_simTimeJ2000,
        false,
        false,
        false
    );
}

// Stops a playback session before playback of all keyframes is complete.
[[codegen::luawrap]] void stopPlayback() {
    openspace::global::sessionRecording->stopPlayback();
}

/**
 * Enables that rendered frames should be saved during playback. The parameter determines
 * the number of frames that are exported per second if this value is not provided, 60
 * frames per second will be exported.
 */
[[codegen::luawrap]] void enableTakeScreenShotDuringPlayback(int fps = 60) {
    openspace::global::sessionRecording->enableTakeScreenShotDuringPlayback(fps);
}

// Used to disable that renderings are saved during playback.
[[codegen::luawrap]] void disableTakeScreenShotDuringPlayback() {
    openspace::global::sessionRecording->disableTakeScreenShotDuringPlayback();
}

/**
 * Performs a conversion of the specified file to the most most recent file format,
 * creating a copy of the recording file.
 */
[[codegen::luawrap]] void fileFormatConversion(std::string convertFilePath) {
    if (convertFilePath.empty()) {
        throw ghoul::lua::LuaError("Filepath string must not be empty");
    }
    openspace::global::sessionRecording->convertFile(convertFilePath);
}

// Pauses or resumes the playback progression through keyframes.
[[codegen::luawrap]] void setPlaybackPause(bool pause) {
    openspace::global::sessionRecording->setPlaybackPause(pause);
}

/**
 * Toggles the pause function, i.e. temporarily setting the delta time to 0 and restoring
 * it afterwards.
 */
[[codegen::luawrap]] void togglePlaybackPause() {
    using namespace openspace;

    bool isPlaybackPaused = global::sessionRecording->isPlaybackPaused();
    global::sessionRecording->setPlaybackPause(!isPlaybackPaused);
}

// Returns true if session recording is currently playing back a recording.
[[codegen::luawrap]] bool isPlayingBack() {
    return openspace::global::sessionRecording->isPlayingBack();
}

// Returns true if session recording is currently recording a recording.
[[codegen::luawrap]] bool isRecording() {
    return openspace::global::sessionRecording->isRecording();
}

#include "sessionrecording_lua_codegen.cpp"

} // namespace
