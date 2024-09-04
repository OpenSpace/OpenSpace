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
 * the recorded keyframes are saved.
 */
[[codegen::luawrap]] void startRecording() {
    openspace::global::sessionRecordingHandler->startRecording();
}

// Stops a recording session. `dataMode` has to be "Ascii" or "Binary"
[[codegen::luawrap]] void stopRecording(std::string recordFilePath, std::string dataMode)
{
    if (recordFilePath.empty()) {
        throw ghoul::lua::LuaError("Filepath string is empty");
    }

    using DataMode = openspace::interaction::DataMode;
    if (dataMode == "Ascii") {
        openspace::global::sessionRecordingHandler->stopRecording(
            recordFilePath,
            DataMode::Ascii
        );
    }
    else if (dataMode == "Binary") {
        openspace::global::sessionRecordingHandler->stopRecording(
            recordFilePath,
            DataMode::Binary
        );
    }
    else {
        throw ghoul::lua::LuaError(std::format("Invalid data mode {}", dataMode));
    }
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
[[codegen::luawrap]] void startPlayback(std::string file, bool loop = false,
                                        bool shouldWaitForTiles = true,
                                        bool saveScreenshots = false,
                                        int saveScreenshotFps = 30)
{
    using namespace openspace;

    if (file.empty()) {
        throw ghoul::lua::LuaError("Filepath string is empty");
    }

    std::optional<int> fps;
    if (saveScreenshots) {
        fps = saveScreenshotFps;
    }

    global::sessionRecordingHandler->startPlayback(file, loop, shouldWaitForTiles, fps);
}

// Stops a playback session before playback of all keyframes is complete.
[[codegen::luawrap]] void stopPlayback() {
    openspace::global::sessionRecordingHandler->stopPlayback();
}

// Pauses or resumes the playback progression through keyframes.
[[codegen::luawrap]] void setPlaybackPause(bool pause) {
    openspace::global::sessionRecordingHandler->setPlaybackPause(pause);
}

/**
 * Toggles the pause function, i.e. temporarily setting the delta time to 0 and restoring
 * it afterwards.
 */
[[codegen::luawrap]] void togglePlaybackPause() {
    using namespace openspace;

    bool isPlaybackPaused = global::sessionRecordingHandler->isPlaybackPaused();
    global::sessionRecordingHandler->setPlaybackPause(!isPlaybackPaused);
}

// Returns true if session recording is currently playing back a recording.
[[codegen::luawrap]] bool isPlayingBack() {
    return openspace::global::sessionRecordingHandler->isPlayingBack();
}

// Returns true if session recording is currently recording a recording.
[[codegen::luawrap]] bool isRecording() {
    return openspace::global::sessionRecordingHandler->isRecording();
}

#include "sessionrecordinghandler_lua_codegen.cpp"

} // namespace
