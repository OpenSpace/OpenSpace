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

    OsEng.keyBindingManager().bindKey(
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

    OsEng.keyBindingManager().bindKeyLocal(
        iKey.key,
        iKey.modifier,
        std::move(command),
        std::move(documentation)
        );

    return 0;
}

/**
* \ingroup LuaScripts
* getKeyBindings(string):
* Returns the strings of the script that are bound to the passed key and whether they were
* local or remote key binds
*/
int getKeyBindings(lua_State* L) {
    int nArguments = lua_gettop(L);
    if (nArguments != 1) {
        return luaL_error(L, "Expected %i arguments, got %i", 1, nArguments);
    }

    std::string key = luaL_checkstring(L, -1);

    using KeyInformation = interaction::KeyBindingManager::KeyInformation;

    std::vector<std::pair<KeyWithModifier, KeyInformation>> info =
        OsEng.keyBindingManager().keyBinding(key);

    lua_createtable(L, static_cast<int>(info.size()), 0);
    int i = 1;
    for (const std::pair<KeyWithModifier, KeyInformation>& it : info) {
        lua_pushnumber(L, i);

        lua_createtable(L, 2, 0);
        lua_pushstring(L, "Command");
        lua_pushstring(L, it.second.command.c_str());
        lua_settable(L, -3);
        lua_pushstring(L, "Remote");
        lua_pushboolean(L, it.second.synchronization);
        lua_settable(L, -3);

        lua_settable(L, -3);
        ++i;
    }
    return 1;
}

/**
* \ingroup LuaScripts
* clearKey(string):
* Clears the keybinding of the key named as argument
*/
int clearKey(lua_State* L) {
    int nArguments = lua_gettop(L);
    if (nArguments != 1) {
        return luaL_error(L, "Expected %i arguments, got %i", 1, nArguments);
    }

    std::string key = luaL_checkstring(L, -1);

    OsEng.keyBindingManager().removeKeyBinding(key);

    return 0;
}

/**
* \ingroup LuaScripts
* clearKeys():
* Clears all key bindings
*/
int clearKeys(lua_State* L) {
    int nArguments = lua_gettop(L);
    if (nArguments != 0) {
        return luaL_error(L, "Expected %i arguments, got %i", 0, nArguments);
    }

    OsEng.keyBindingManager().resetKeyBindings();

    return 0;
}

} // namespace openspace::luascriptfunctions
