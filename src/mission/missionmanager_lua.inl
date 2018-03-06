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

int loadMission(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::loadMission");

    std::string missionFileName = ghoul::lua::checkStringAndPop(L);
    if (missionFileName.empty()) {
        return luaL_error(L, "Filepath is empty");
    }
    
    std::string name = MissionManager::ref().loadMission(absPath(missionFileName));
    lua_pushstring(L, name.c_str());

    ghoul_assert(lua_gettop(L) == 1, "Incorrect number of items left on stack");
    return 1;
}

int unloadMission(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::unloadMission");

    std::string missionName = ghoul::lua::checkStringAndPop(L);
    if (missionName.empty()) {
        return luaL_error(L, "Mission name is empty");
    }

    if (!MissionManager::ref().hasMission(missionName)) {
        return luaL_error(L, "Mission was not previously loaded");
    }

    MissionManager::ref().unloadMission(missionName);

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int hasMission(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::hasMission");

    std::string missionName = ghoul::lua::checkStringAndPop(L);
    if (missionName.empty()) {
        return luaL_error(L, "Missing name is empty");
    }
    
    bool hasMission = MissionManager::ref().hasMission(missionName);

    lua_pushboolean(L, hasMission);

    ghoul_assert(lua_gettop(L) == 1, "Incorrect number of items left on stack");
    return 1;
}

int setCurrentMission(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::setCurrentMission");

    std::string missionName = ghoul::lua::checkStringAndPop(L);
    if (missionName.empty()) {
        return luaL_error(L, "Mission name is empty");
    }

    MissionManager::ref().setCurrentMission(missionName);

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

} // namespace openspace::luascriptfunction
