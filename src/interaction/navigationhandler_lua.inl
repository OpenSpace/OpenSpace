/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

/**
 * \ingroup LuaScripts
 * setOrigin():
 * Set the origin of the camera
 */
int setOrigin(lua_State* L) {
    using ghoul::lua::luaTypeToString;

    int nArguments = lua_gettop(L);
    if (nArguments != 1) {
        return luaL_error(L, "Expected %i arguments, got %i", 1, nArguments);
    }

    const int type = lua_type(L, -1);
    if (type != LUA_TSTRING) {
        return luaL_error(L, "Expected string, got %i", type);
    }

    std::string s = luaL_checkstring(L, -1);

    SceneGraphNode* node = sceneGraphNode(s);
    if (!node) {
        LWARNINGC(
            "lua.setOrigin",
            "Could not find a node in scenegraph called '" << s << "'"
        );
        return 0;
    }

    OsEng.navigationHandler().setFocusNode(node);

    return 0;
}

int restoreCameraStateFromFile(lua_State* L) {
    using ghoul::lua::luaTypeToString;

    int nArguments = lua_gettop(L);
    if (nArguments != 1) {
        return luaL_error(L, "Expected %i arguments, got %i", 1, nArguments);
    }

    std::string cameraStateFilePath = luaL_checkstring(L, -1);

    if (cameraStateFilePath.empty()) {
        return luaL_error(L, "filepath string is empty");
    }

    OsEng.navigationHandler().restoreCameraStateFromFile(cameraStateFilePath);
    return 0;
}

int saveCameraStateToFile(lua_State* L) {
    using ghoul::lua::luaTypeToString;

    int nArguments = lua_gettop(L);
    if (nArguments != 1) {
        return luaL_error(L, "Expected %i arguments, got %i", 1, nArguments);
    }

    std::string cameraStateFilePath = luaL_checkstring(L, -1);

    if (cameraStateFilePath.empty()) {
        return luaL_error(L, "filepath string is empty");
    }

    OsEng.navigationHandler().saveCameraStateToFile(cameraStateFilePath);
    return 0;
}

int resetCameraDirection(lua_State* L) {
    using ghoul::lua::luaTypeToString;

    int nArguments = lua_gettop(L);
    if (nArguments != 0) {
        return luaL_error(L, "Expected %i arguments, got %i", 0, nArguments);
    }

    OsEng.navigationHandler().resetCameraDirection();
    return 0;
}

} // namespace openspace::luascriptfunctions
