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

namespace openspace::luascriptfunctions {


int startRecording(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::startRecording");

    using ghoul::lua::luaTypeToString;

    const std::string recordFilePath = ghoul::lua::value<std::string>(
        L,
        1,
        ghoul::lua::PopValue::Yes
    );

    if (recordFilePath.empty()) {
        return luaL_error(L, "filepath string is empty");
    }
    global::sessionRecording.setRecordDataFormat(
        openspace::interaction::SessionRecording::RecordedDataMode::Binary
    );
    global::sessionRecording.startRecording(recordFilePath);

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int startRecordingAscii(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::startRecordingAscii");

    using ghoul::lua::luaTypeToString;

    const std::string recordFilePath = ghoul::lua::value<std::string>(
        L,
        1,
        ghoul::lua::PopValue::Yes
    );

    if (recordFilePath.empty()) {
        return luaL_error(L, "filepath string is empty");
    }
    global::sessionRecording.setRecordDataFormat(
        openspace::interaction::SessionRecording::RecordedDataMode::Ascii
    );
    global::sessionRecording.startRecording(recordFilePath);

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int stopRecording(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::stopRecording");

    global::sessionRecording.stopRecording();

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int startPlayback(lua_State* L, openspace::interaction::KeyframeTimeRef timeMode,
                  bool forceSimTimeAtStart)
{
    using ghoul::lua::luaTypeToString;

    const std::string playbackFilePath = ghoul::lua::value<std::string>(
        L,
        1,
        ghoul::lua::PopValue::Yes
    );

    if (playbackFilePath.empty()) {
        return luaL_error(L, "filepath string is empty");
    }

    global::sessionRecording.startPlayback(
        playbackFilePath,
        timeMode,
        forceSimTimeAtStart
    );

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int startPlaybackDefault(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::startPlaybackDefault");
    using openspace::interaction::KeyframeNavigator;
    return startPlayback(L,
        openspace::interaction::KeyframeTimeRef::Relative_recordedStart, true);
}

int startPlaybackApplicationTime(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::startPlaybackApplicationTime");

    return startPlayback(L,
        openspace::interaction::KeyframeTimeRef::Relative_applicationStart, false);
}

int startPlaybackRecordedTime(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::startPlaybackRecordedTime");
    using openspace::interaction::KeyframeNavigator;
    return startPlayback(L,
        openspace::interaction::KeyframeTimeRef::Relative_recordedStart, false);
}

int startPlaybackSimulationTime(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::startPlaybackSimulationTime");
    using openspace::interaction::KeyframeNavigator;
    return startPlayback(L,
        openspace::interaction::KeyframeTimeRef::Absolute_simTimeJ2000, false);
}

int stopPlayback(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::stopPlayback");

    global::sessionRecording.stopPlayback();

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int enableTakeScreenShotDuringPlayback(lua_State* L) {
    const int nArguments = ghoul::lua::checkArgumentsAndThrow(
        L,
        { 0, 1 },
        "lua::enableTakeScreenShotDuringPlayback"
    );

    const int fps = nArguments == 0 ? 60 : ghoul::lua::value<int>(L, 1);

    global::sessionRecording.enableTakeScreenShotDuringPlayback(fps);

    lua_settop(L, 0);
    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int disableTakeScreenShotDuringPlayback(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::disableTakeScreenShotDuringPlayback");

    global::sessionRecording.disableTakeScreenShotDuringPlayback();

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

} // namespace openspace::luascriptfunctions
