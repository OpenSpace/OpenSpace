/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <ghoul/format.h>
#include <ghoul/lua/lua_helper.h>
#include <ghoul/misc/exception.h>
#include <filesystem>
#include <optional>
#include <string>
#include <utility>

using namespace openspace;

namespace {

/**
 * Starts a recording session. The string argument is the filename used for the file where
 * the recorded keyframes are saved.
 */
[[codegen::luawrap]] void startRecording() {
    global::sessionRecordingHandler->startRecording();
}

/**
 * Stops a recording session. `dataMode` has to be "Ascii" or "Binary". If `overwrite` is
 * true, any existing session recording file will be overwritten, false by default.
 */
[[codegen::luawrap]] void stopRecording(std::filesystem::path recordFilePath,
                                        std::string dataMode,
                                        std::optional<bool> overwrite)
{
    if (recordFilePath.empty()) {
        throw ghoul::lua::LuaError("Filepath string is empty");
    }

    if (dataMode != "Ascii" && dataMode != "Binary") {
        throw ghoul::lua::LuaError(std::format("Invalid data mode {}", dataMode));
    }

    global::sessionRecordingHandler->stopRecording(
        recordFilePath,
        dataMode == "Ascii" ? DataMode::Ascii : DataMode::Binary,
        overwrite.value_or(false)
    );
}

/**
 * Starts a playback session with keyframe times that are relative to the time since the
 * recording was started (the same relative time applies to the playback). When playback
 * starts, the simulation time is automatically set to what it was at recording time. The
 * file argument is the filename to the session recording file. If a second input
 * value of true is given, then playback will continually loop until it is manually
 * stopped.
 */
[[codegen::luawrap]] void startPlayback(std::filesystem::path file, bool loop = false,
                                        bool shouldWaitForTiles = true,
                                        std::optional<int> screenshotFps = std::nullopt)
{
    if (file.empty()) {
        throw ghoul::lua::LuaError("Filepath string is empty");
    }

    if (!std::filesystem::is_regular_file(file)) {
        throw ghoul::RuntimeError(std::format(
            "Cannot find the specified playback file '{}'", file
        ));
    }

    SessionRecording timeline = loadSessionRecording(file);
    global::sessionRecordingHandler->startPlayback(
        std::move(timeline),
        loop,
        shouldWaitForTiles,
        screenshotFps
    );
}

// Stops a playback session before playback of all keyframes is complete.
[[codegen::luawrap]] void stopPlayback() {
    global::sessionRecordingHandler->stopPlayback();
}

// Pauses or resumes the playback progression through keyframes.
[[codegen::luawrap]] void setPlaybackPause(bool pause) {
    global::sessionRecordingHandler->setPlaybackPause(pause);
}

/**
 * Toggles the pause function, i.e. temporarily setting the delta time to 0 and restoring
 * it afterwards.
 */
[[codegen::luawrap]] void togglePlaybackPause() {
    bool isPlaybackPaused = global::sessionRecordingHandler->isPlaybackPaused();
    global::sessionRecordingHandler->setPlaybackPause(!isPlaybackPaused);
}

// Returns true if session recording is currently playing back a recording.
[[codegen::luawrap]] bool isPlayingBack() {
    return global::sessionRecordingHandler->isPlayingBack();
}

// Returns true if session recording is currently recording a recording.
[[codegen::luawrap]] bool isRecording() {
    return global::sessionRecordingHandler->isRecording();
}

} // namespace

#include "sessionrecordinghandler_lua_codegen.cpp"
