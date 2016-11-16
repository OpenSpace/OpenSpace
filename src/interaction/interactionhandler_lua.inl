/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

namespace openspace {

namespace luascriptfunctions {
/**
 * \ingroup LuaScripts
 * setOrigin():
 * Set the origin of the camera
 */
int setOrigin(lua_State* L) {
    using ghoul::lua::luaTypeToString;

    int nArguments = lua_gettop(L);
    if (nArguments != 1)
        return luaL_error(L, "Expected %i arguments, got %i", 1, nArguments);

    const int type = lua_type(L, -1);
    if (type != LUA_TSTRING)
        return luaL_error(L, "Expected string, got %i", type);

    std::string s = luaL_checkstring(L, -1);

    SceneGraphNode* node = sceneGraphNode(s);
    if (!node) {
        LWARNINGC(
            "lua.setOrigin",
            "Could not find a node in scenegraph called '" << s << "'"
        );
        return 0;
    }

    OsEng.interactionHandler().setFocusNode(node);

    return 0;
}

/**
* \ingroup LuaScripts
* bindKey():
* Binds a key to Lua command to both execute locally
* and broadcast to all clients if this node is hosting
* a parallel connection.
*/
int bindKey(lua_State* L) {
    using ghoul::lua::luaTypeToString;

    int nArguments = lua_gettop(L);
    if (nArguments != 2 && nArguments != 3) {
        return luaL_error(L, "Expected %i or %i arguments, got %i", 2, 3, nArguments);
    }

    int KeyLocation = nArguments == 3 ? -3 : -2;
    int CommandLocation = nArguments == 3 ? -2 : -1;
    int DocumentationLocation = -1;

    std::string key = luaL_checkstring(L, KeyLocation);
    std::string command = luaL_checkstring(L, CommandLocation);

    if (command.empty()) {
        return luaL_error(L, "Command string is empty");
    }

    openspace::KeyWithModifier iKey = openspace::stringToKey(key);

    if (iKey.key == openspace::Key::Unknown) {
        LERRORC("lua.bindKey", "Could not find key '"<< key << "'");
        return 0;
    }

    std::string documentation;
    if (nArguments == 3) {
        documentation = luaL_checkstring(L, DocumentationLocation);
    }

    OsEng.interactionHandler().bindKey(
        iKey.key,
        iKey.modifier,
        std::move(command),
        std::move(documentation)
    );
        
    return 0;
}

/**
* \ingroup LuaScripts
* bindKey():
* Binds a key to Lua command to execute only locally
*/
int bindKeyLocal(lua_State* L) {
    using ghoul::lua::luaTypeToString;

    int nArguments = lua_gettop(L);
    if (nArguments != 2 && nArguments != 3) {
        return luaL_error(L, "Expected %i or %i arguments, got %i", 2, 3, nArguments);
    }

    int KeyLocation = nArguments == 3 ? -3 : -2;
    int CommandLocation = nArguments == 3 ? -2 : -1;
    int DocumentationLocation = -1;

    std::string key = luaL_checkstring(L, KeyLocation);
    std::string command = luaL_checkstring(L, CommandLocation);


    if (command.empty())
        return luaL_error(L, "Command string is empty");

    openspace::KeyWithModifier iKey = openspace::stringToKey(key);

    if (iKey.key == openspace::Key::Unknown) {
        LERRORC("lua.bindKey", "Could not find key '" << key << "'");
        return 0;
    }

    std::string documentation;
    if (nArguments == 3) {
        documentation = luaL_checkstring(L, DocumentationLocation);
    }

    OsEng.interactionHandler().bindKeyLocal(
        iKey.key,
        iKey.modifier,
        std::move(command),
        std::move(documentation)
        );

    return 0;
}


/**
* \ingroup LuaScripts
* clearKeys():
* Clears all key bindings
*/
int clearKeys(lua_State* L) {
    using ghoul::lua::luaTypeToString;

    int nArguments = lua_gettop(L);
    if (nArguments != 0)
        return luaL_error(L, "Expected %i arguments, got %i", 0, nArguments);

    OsEng.interactionHandler().resetKeyBindings();

    return 0;
}

/**
* \ingroup LuaScripts
* setInteractionMode():
* Set the interaction mode
*/
int setInteractionMode(lua_State* L) {
    using ghoul::lua::luaTypeToString;

    int nArguments = lua_gettop(L);
    if (nArguments != 1)
        return luaL_error(L, "Expected %i arguments, got %i", 1, nArguments);


    std::string interactionModeKey = luaL_checkstring(L, -1);
    
    if (interactionModeKey.empty())
        return luaL_error(L, "interactionmode name string is empty");

    OsEng.interactionHandler().setInteractionMode(interactionModeKey);
    return 0;
}

int restoreCameraStateFromFile(lua_State* L) {
    using ghoul::lua::luaTypeToString;
    const std::string _loggerCat = "lua.restoreCameraStateFromFile";

    int nArguments = lua_gettop(L);
    if (nArguments != 1)
        return luaL_error(L, "Expected %i arguments, got %i", 1, nArguments);

    std::string cameraStateFilePath = luaL_checkstring(L, -1);

    if (cameraStateFilePath.empty())
        return luaL_error(L, "filepath string is empty");

    OsEng.interactionHandler().restoreCameraStateFromFile(cameraStateFilePath);
    return 0;
}

int saveCameraStateToFile(lua_State* L) {
    using ghoul::lua::luaTypeToString;
    const std::string _loggerCat = "lua.setCameraPosition";

    int nArguments = lua_gettop(L);
    if (nArguments != 1)
        return luaL_error(L, "Expected %i arguments, got %i", 1, nArguments);

    std::string cameraStateFilePath = luaL_checkstring(L, -1);

    if (cameraStateFilePath.empty())
        return luaL_error(L, "filepath string is empty");

    OsEng.interactionHandler().saveCameraStateToFile(cameraStateFilePath);
}

int resetCameraDirection(lua_State* L) {
    using ghoul::lua::luaTypeToString;
    const std::string _loggerCat = "lua.resetCameraDirection";

    int nArguments = lua_gettop(L);
    if (nArguments != 0)
        return luaL_error(L, "Expected %i arguments, got %i", 0, nArguments);

    OsEng.interactionHandler().resetCameraDirection();
}


} // namespace luascriptfunctions

} // namespace openspace
