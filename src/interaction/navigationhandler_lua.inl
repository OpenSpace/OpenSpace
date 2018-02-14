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

int restoreCameraStateFromFile(lua_State* L) {
    using ghoul::lua::luaTypeToString;

    int nArguments = lua_gettop(L);
    if (nArguments != 1) {
        return luaL_error(L, "Expected %i arguments, got %i", 1, nArguments);
    }

    std::string cameraStateFilePath = luaL_checkstring(L, -1);
    lua_settop(L, 0);

    if (cameraStateFilePath.empty()) {
        return luaL_error(L, "filepath string is empty");
    }

    OsEng.navigationHandler().restoreCameraStateFromFile(cameraStateFilePath);
    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int setCameraState(lua_State* L) {
    int nArguments = lua_gettop(L);
    if (nArguments != 1) {
        return luaL_error(L, "Expected %i arguments, got %i", 1, nArguments);
    }

    try {
        ghoul::Dictionary dictionary;
        ghoul::lua::luaDictionaryFromState(L, dictionary);
        OsEng.navigationHandler().setCameraStateFromDictionary(dictionary);
    } catch (const ghoul::RuntimeError& e) {
        lua_settop(L, 0);
        return luaL_error(L, "Could not set camera state: %s", e.what());
    }

    lua_settop(L, 0);
    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int saveCameraStateToFile(lua_State* L) {
    using ghoul::lua::luaTypeToString;

    int nArguments = lua_gettop(L);
    if (nArguments != 1) {
        return luaL_error(L, "Expected %i arguments, got %i", 1, nArguments);
    }

    std::string cameraStateFilePath = luaL_checkstring(L, -1);
    lua_settop(L, 0);

    if (cameraStateFilePath.empty()) {
        return luaL_error(L, "filepath string is empty");
    }

    OsEng.navigationHandler().saveCameraStateToFile(cameraStateFilePath);
    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int resetCameraDirection(lua_State* L) {
    using ghoul::lua::luaTypeToString;

    int nArguments = lua_gettop(L);
    if (nArguments != 0) {
        return luaL_error(L, "Expected %i arguments, got %i", 0, nArguments);
    }

    OsEng.navigationHandler().resetCameraDirection();
    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

} // namespace openspace::luascriptfunctions
