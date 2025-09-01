/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <ghoul/lua/lua_helper.h>

namespace {

/**
 * Binds a key to Lua command to both execute locally and broadcast to all clients if this
 * node is hosting a parallel connection.
 */
[[codegen::luawrap]] void bindKey(std::string key, std::string action) {
    using namespace openspace;

    if (action.empty()) {
        throw ghoul::lua::LuaError("Action must not be empty");
    }
    if (!global::actionManager->hasAction(action)) {
        throw ghoul::lua::LuaError(std::format("Action '{}' does not exist", action));
    }

    openspace::KeyWithModifier iKey = openspace::stringToKey(key);
    if (iKey.key == openspace::Key::Unknown) {
        throw ghoul::lua::LuaError(std::format("Could not find key '{}'", key));
    }

    global::keybindingManager->bindKey(iKey.key, iKey.modifier, std::move(action));
}

/**
 * Returns the identifiers of the action that are bound to the passed key and whether they
 * were local or remote key binds. If no key is provided, all bound keybindings are
 * returned instead.
 *
 * \param key The key for which to return the keybindings. If no key is provided, all
 *        keybindings are returned
 */
[[codegen::luawrap]] std::vector<std::string> keyBindings(std::optional<std::string> key)
{
    using namespace openspace;

    std::vector<std::string> res;
    if (key.has_value()) {
        using K = KeyWithModifier;
        using V = std::string;
        const std::vector<std::pair<K, V>>& info = global::keybindingManager->keyBinding(
            stringToKey(*key)
        );

        res.reserve(info.size());
        for (const std::pair<K, V>& it : info) {
            res.push_back(it.second);
        }
    }
    else {
        const std::multimap<KeyWithModifier, std::string>& keybinds =
            global::keybindingManager->keyBindings();

        res.reserve(keybinds.size());
        for (const std::pair<const KeyWithModifier, std::string>& it : keybinds) {
            res.push_back(it.second);
        }

        // Actions might be bound to different actions
        res.erase(std::unique(res.begin(), res.end()), res.end());
    }
    return res;
}

/**
 * Returns the keybinds to which the provided action is bound. As actions can be bound to
 * multiple keys, this function returns a list of all keys
 */
[[codegen::luawrap]] std::vector<std::string> keyBindingsForAction(std::string action) {
    using namespace openspace;

    const std::multimap<KeyWithModifier, std::string>& keybinds =
        global::keybindingManager->keyBindings();

    std::vector<std::string> res;
    for (const std::pair<const KeyWithModifier, std::string>& it : keybinds) {
        if (it.second == action) {
            res.push_back(keyToString(it.first));
        }
    }
    return res;
}

/**
 * Unbinds the key or keys that have been provided. This function can be called with a
 * single key or with an array of keys to remove all of the provided keys at once.
 */
[[codegen::luawrap]] void clearKey(
                                  std::variant<std::string, std::vector<std::string>> key)
{
    using namespace openspace;

    if (std::holds_alternative<std::string>(key)) {
        KeyWithModifier k = stringToKey(std::get<std::string>(key));
        global::keybindingManager->removeKeyBinding(k);
    }
    else {
        for (const std::string& k : std::get<std::vector<std::string>>(key)) {
            global::keybindingManager->removeKeyBinding(stringToKey(k));
        }
    }
}

// Clear all key bindings
[[codegen::luawrap]] void clearKeys() {
    openspace::global::keybindingManager->resetKeyBindings();
}

#include "keybindingmanager_lua_codegen.cpp"

} // namespace
