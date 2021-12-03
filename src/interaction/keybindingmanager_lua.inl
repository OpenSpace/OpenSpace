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

#include <openspace/engine/globals.h>
#include <ghoul/logging/logmanager.h>

namespace openspace::luascriptfunctions {

/**
 * \ingroup LuaScripts
 * bindKey():
 * Binds a key to Lua command to both execute locally and broadcast to all clients if this
 * node is hosting a parallel connection.
 */
int bindKey(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 2, "lua::bindKey");
    auto [key, action] = ghoul::lua::values<std::string, std::string>(L);

    if (action.empty()) {
        return ghoul::lua::luaError(L, "Action must not be empty");
    }
    if (!global::actionManager->hasAction(action)) {
        return ghoul::lua::luaError(L, fmt::format("Action '{}' does not exist", action));
    }

    openspace::KeyWithModifier iKey = openspace::stringToKey(key);
    if (iKey.key == openspace::Key::Unknown) {
        std::string error = fmt::format("Could not find key '{}'", key);
        LERRORC("lua.bindKey", error);
        return ghoul::lua::luaError(L, error);
    }

    global::keybindingManager->bindKey(iKey.key, iKey.modifier, std::move(action));
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
    const std::string& key = ghoul::lua::value<std::string>(L);

    using K = KeyWithModifier;
    using V = std::string;
    const std::vector<std::pair<K, V>>& info = global::keybindingManager->keyBinding(
        stringToKey(key)
    );

    lua_createtable(L, static_cast<int>(info.size()), 0);
    int i = 1;
    for (const std::pair<K, V>& it : info) {
        lua_pushnumber(L, i);
        ghoul::lua::push(L, it.second);
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
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::clearKey");

    std::variant key = ghoul::lua::value<std::variant<std::string, ghoul::Dictionary>>(L);
    if (std::holds_alternative<std::string>(key)) {
        KeyWithModifier k = stringToKey(std::get<std::string>(key));
        global::keybindingManager->removeKeyBinding(k);
    }
    else {
        ghoul::Dictionary d = std::get<ghoul::Dictionary>(key);
        for (size_t i = 1; i <= d.size(); ++i) {
            const std::string& k = d.value<std::string>(std::to_string(i));
            global::keybindingManager->removeKeyBinding(stringToKey(k));
        }
    }
    return 0;
}

/**
* \ingroup LuaScripts
* clearKeys():
* Clears all key bindings
*/
int clearKeys(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::clearKeys");
    global::keybindingManager->resetKeyBindings();
    return 0;
}

} // namespace openspace::luascriptfunctions
