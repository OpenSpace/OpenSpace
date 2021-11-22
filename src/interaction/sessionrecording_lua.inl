/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

namespace openspace::luascriptfunctions {

int startRecording(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::startRecording");
    const std::string recordFilePath = ghoul::lua::value<std::string>(L);

    if (recordFilePath.empty()) {
        return luaL_error(L, "filepath string is empty");
    }
    global::sessionRecording->setRecordDataFormat(
        interaction::SessionRecording::DataMode::Binary
    );
    global::sessionRecording->startRecording(recordFilePath);
    return 0;
}

int startRecordingAscii(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::startRecordingAscii");
    const std::string recordFilePath = ghoul::lua::value<std::string>(L);

    if (recordFilePath.empty()) {
        return luaL_error(L, "filepath string is empty");
    }
    global::sessionRecording->setRecordDataFormat(
        interaction::SessionRecording::DataMode::Ascii
    );
    global::sessionRecording->startRecording(recordFilePath);
    return 0;
}

int stopRecording(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::stopRecording");
    global::sessionRecording->stopRecording();
    return 0;
}

int startPlayback(lua_State* L, interaction::KeyframeTimeRef timeMode,
                  bool forceSimTimeAtStart)
{
    ghoul::lua::checkArgumentsAndThrow(L, { 1, 2 }, "lua::startPlayback");
    auto [file, loop] = ghoul::lua::values<std::string, std::optional<bool>>(L);
    loop = loop.value_or(false);

    if (file.empty()) {
        return ghoul::lua::luaError(L, "Filepath string is empty");
    }
    global::sessionRecording->startPlayback(file, timeMode, forceSimTimeAtStart, *loop);
    return 0;
}

int startPlaybackDefault(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, { 1, 2 }, "lua::startPlaybackDefault");
    return startPlayback(L, interaction::KeyframeTimeRef::Relative_recordedStart, true);
}

int startPlaybackApplicationTime(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::startPlaybackApplicationTime");

    return startPlayback(
        L,
        interaction::KeyframeTimeRef::Relative_applicationStart,
        false
    );
}

int startPlaybackRecordedTime(lua_State* L) {
    using interaction::KeyframeNavigator;
    ghoul::lua::checkArgumentsAndThrow(L, { 1, 2 }, "lua::startPlaybackRecordedTime");
    return startPlayback(L, interaction::KeyframeTimeRef::Relative_recordedStart, false);
}

int startPlaybackSimulationTime(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::startPlaybackSimulationTime");
    return startPlayback(L, interaction::KeyframeTimeRef::Absolute_simTimeJ2000, false);
}

int stopPlayback(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::stopPlayback");
    global::sessionRecording->stopPlayback();
    return 0;
}

int enableTakeScreenShotDuringPlayback(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(
        L,
        { 0, 1 },
        "lua::enableTakeScreenShotDuringPlayback"
    );
    std::optional<int> fps = ghoul::lua::value<std::optional<int>>(L);
    fps = fps.value_or(60);

    global::sessionRecording->enableTakeScreenShotDuringPlayback(*fps);
    return 0;
}

int disableTakeScreenShotDuringPlayback(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::disableTakeScreenShotDuringPlayback");
    global::sessionRecording->disableTakeScreenShotDuringPlayback();
    return 0;
}

int fileFormatConversion(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::fileFormatConversion");
    const std::string convertFilePath = ghoul::lua::value<std::string>(L);

    if (convertFilePath.empty()) {
        return luaL_error(L, "Filepath string must not be empty");
    }
    global::sessionRecording->convertFile(convertFilePath);
    return 0;
}

int setPlaybackPause(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::setPlaybackPause");
    const bool pause = ghoul::lua::value<bool>(L);

    global::sessionRecording->setPlaybackPause(pause);
    return 0;
}

int togglePlaybackPause(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::togglePlaybackPause");
    bool isPlaybackPaused = global::sessionRecording->isPlaybackPaused();
    global::sessionRecording->setPlaybackPause(!isPlaybackPaused);
    return 0;
}

int isPlayingBack(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::isPlayingBack");
    ghoul::lua::push(L, global::sessionRecording->isPlayingBack());
    return 1;
}

} // namespace openspace::luascriptfunctions
