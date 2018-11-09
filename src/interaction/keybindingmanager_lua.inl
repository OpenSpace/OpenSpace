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
#include <ghoul/logging/logmanager.h>

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

    int nArguments = ghoul::lua::checkArgumentsAndThrow(L, 2, 3, "lua::bindKey");

    const std::string& key = ghoul::lua::value<std::string>(L, 1);
    const std::string& command = ghoul::lua::value<std::string>(L, 2);

    if (command.empty()) {
        lua_settop(L, 0);
        return ghoul::lua::luaError(L, "Command string is empty");
    }

    openspace::KeyWithModifier iKey = openspace::stringToKey(key);

    if (iKey.key == openspace::Key::Unknown) {
        std::string error = fmt::format("Could not find key '{}'", key);
        LERRORC("lua.bindKey", error);
        lua_settop(L, 0);
        return ghoul::lua::luaError(L, error);
    }

    std::string doc = (nArguments == 3) ? ghoul::lua::value<std::string>(L, 3) : "";

    global::keybindingManager.bindKey(
        iKey.key,
        iKey.modifier,
        std::move(command),
        std::move(doc)
    );

    lua_settop(L, 0);
    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

/**
* \ingroup LuaScripts
* bindKey():
* Binds a key to Lua command to execute only locally
*/
int bindKeyLocal(lua_State* L) {
    using ghoul::lua::luaTypeToString;

    int nArguments = ghoul::lua::checkArgumentsAndThrow(L, 2, 3, "lua::bindKeyLocal");

    const std::string& key = ghoul::lua::value<std::string>(L, 1);
    const std::string& command = ghoul::lua::value<std::string>(L, 2);

    if (command.empty()) {
        return ghoul::lua::luaError(L, "Command string is empty");
    }

    openspace::KeyWithModifier iKey = openspace::stringToKey(key);

    if (iKey.key == openspace::Key::Unknown) {
        std::string error = fmt::format("Could not find key '{}'", key);
        LERRORC("lua.bindKey", error);
        return ghoul::lua::luaError(L, error);
    }

    std::string doc = nArguments == 3 ? ghoul::lua::value<std::string>(L, 3) : "";

    global::keybindingManager.bindKeyLocal(
        iKey.key,
        iKey.modifier,
        std::move(command),
        std::move(doc)
    );

    lua_settop(L, 0);
    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

/**
* \ingroup LuaScripts
* getKeyBindings(string):
* Returns the strings of the script that are bound to the passed key and whether they were
* local or remote key binds
*/
int getKeyBindings(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::getKeyBindings");

    const std::string& key = ghoul::lua::value<std::string>(
        L,
        1,
        ghoul::lua::PopValue::Yes
    );

    using K = KeyWithModifier;
    using V = interaction::KeybindingManager::KeyInformation;

    const std::vector<std::pair<K, V>>& info = global::keybindingManager.keyBinding(key);

    lua_createtable(L, static_cast<int>(info.size()), 0);
    int i = 1;
    for (const std::pair<K, V>& it : info) {
        lua_pushnumber(L, i);

        lua_createtable(L, 2, 0);
        ghoul::lua::push(L, "Command");
        ghoul::lua::push(L, it.second.command);
        lua_settable(L, -3);
        ghoul::lua::push(L, "Remote");
        ghoul::lua::push(L, static_cast<bool>(it.second.synchronization));
        lua_settable(L, -3);

        lua_settable(L, -3);
        ++i;
    }

    ghoul_assert(lua_gettop(L) == 1, "Incorrect number of items left on stack");
    return 1;
}

/**
* \ingroup LuaScripts
* clearKey(string):
* Clears the keybinding of the key named as argument
*/
int clearKey(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::clearKey");

    const int t = lua_type(L, 1);
    if (t == LUA_TSTRING) {
        // The user provided a single key
        const std::string& key = ghoul::lua::value<std::string>(L, 1);
        global::keybindingManager.removeKeyBinding(key);
    }
    else {
        // The user provided a list of keys
        ghoul::Dictionary d;
        ghoul::lua::luaDictionaryFromState(L, d);
        for (size_t i = 1; i <= d.size(); ++i) {
            const std::string& k = d.value<std::string>(std::to_string(i));
            global::keybindingManager.removeKeyBinding(k);
        }
        lua_pop(L, 1);
    }

    lua_settop(L, 0);
    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

/**
* \ingroup LuaScripts
* clearKeys():
* Clears all key bindings
*/
int clearKeys(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::clearKeys");

    global::keybindingManager.resetKeyBindings();

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

} // namespace openspace::luascriptfunctions
