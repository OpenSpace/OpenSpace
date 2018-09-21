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

#include <openspace/engine/globals.h>

namespace openspace::luascriptfunctions {

int loadMission(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::loadMission");

    const std::string& missionFileName = ghoul::lua::value<std::string>(
        L,
        1,
        ghoul::lua::PopValue::Yes
    );
    if (missionFileName.empty()) {
        return ghoul::lua::luaError(L, "Filepath is empty");
    }

    std::string name = global::missionManager.loadMission(absPath(missionFileName));
    ghoul::lua::push(L, name);

    ghoul_assert(lua_gettop(L) == 1, "Incorrect number of items left on stack");
    return 1;
}

int unloadMission(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::unloadMission");

    const std::string& missionName = ghoul::lua::value<std::string>(
        L,
        1,
        ghoul::lua::PopValue::Yes
    );
    if (missionName.empty()) {
        return ghoul::lua::luaError(L, "Mission name is empty");
    }

    if (!global::missionManager.hasMission(missionName)) {
        return ghoul::lua::luaError(L, "Mission was not previously loaded");
    }

    global::missionManager.unloadMission(missionName);

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int hasMission(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::hasMission");

    const std::string& missionName = ghoul::lua::value<std::string>(
        L,
        1,
        ghoul::lua::PopValue::Yes
    );
    if (missionName.empty()) {
        return ghoul::lua::luaError(L, "Missing name is empty");
    }

    const bool hasMission = global::missionManager.hasMission(missionName);

    ghoul::lua::push(L, hasMission);

    ghoul_assert(lua_gettop(L) == 1, "Incorrect number of items left on stack");
    return 1;
}

int setCurrentMission(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::setCurrentMission");

    const std::string& missionName = ghoul::lua::value<std::string>(
        L,
        1,
        ghoul::lua::PopValue::Yes
    );
    if (missionName.empty()) {
        return ghoul::lua::luaError(L, "Mission name is empty");
    }

    global::missionManager.setCurrentMission(missionName);

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

} // namespace openspace::luascriptfunction
