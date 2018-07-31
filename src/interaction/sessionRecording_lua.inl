/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

using openspace::interaction::KeyframeTimeRef;

int startRecording(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::startRecording");

    using ghoul::lua::luaTypeToString;

    const std::string recordFilePath = ghoul::lua::checkStringAndPop(L);

    if (recordFilePath.empty()) {
        return luaL_error(L, "filepath string is empty");
    }

    OsEng.sessionRecording().startRecording(recordFilePath);

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int stopRecording(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::stopRecording");

    OsEng.sessionRecording().stopRecording();

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int startPlayback(lua_State* L, KeyframeTimeRef timeMode) {
    using ghoul::lua::luaTypeToString;

    const std::string playbackFilePath = ghoul::lua::checkStringAndPop(L);

    if (playbackFilePath.empty()) {
        return luaL_error(L, "filepath string is empty");
    }

    OsEng.sessionRecording().startPlayback(playbackFilePath, timeMode);

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int startPlaybackApplicationTime(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::startPlaybackApplicationTime");

    return startPlayback(L, KeyframeTimeRef::relative_applicationStart);
}

int startPlaybackRecordedTime(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::startPlaybackRecordedTime");
    using openspace::interaction::KeyframeNavigator;
    return startPlayback(L, KeyframeTimeRef::relative_recordedStart);
}

int startPlaybackSimulationTime(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::startPlaybackSimulationTime");
    using openspace::interaction::KeyframeNavigator;
    return startPlayback(L, KeyframeTimeRef::absolute_simTimeJ2000);
}

int stopPlayback(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::stopPlayback");

    OsEng.sessionRecording().stopPlayback();

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

} // namespace openspace::luascriptfunctions
