/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#include <openspace/interaction/keybindingmanager.h>

#include <openspace/engine/globals.h>
#include <openspace/interaction/actionmanager.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/json_helper.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/glm.h>
#include <sstream>

namespace {
    /**
     * Binds a key to Lua command to both execute locally and broadcast to all clients if
     * this node is hosting a parallel connection.
     */
    [[codegen::luawrap]] void bindKey(std::string key, std::string action) {
        using namespace openspace;

        if (action.empty()) {
            throw ghoul::lua::LuaError("Action must not be empty");
        }
        if (!global::actionManager->hasAction(action)) {
            throw ghoul::lua::LuaError(fmt::format("Action '{}' does not exist", action));
        }

        openspace::KeyWithModifier iKey = openspace::stringToKey(key);
        if (iKey.key == openspace::Key::Unknown) {
            std::string error = fmt::format("Could not find key '{}'", key);
            LERRORC("lua.bindKey", error);
            throw ghoul::lua::LuaError(error);
        }

        global::keybindingManager->bindKey(iKey.key, iKey.modifier, std::move(action));
    }

    /**
     * Returns the strings of the script that are bound to the passed key and whether they
     * were local or remote key binds.
     */
    [[codegen::luawrap]] std::vector<std::string> keyBindings(std::string key) {
        using namespace openspace;

        using K = KeyWithModifier;
        using V = std::string;
        const std::vector<std::pair<K, V>>& info = global::keybindingManager->keyBinding(
            stringToKey(key)
        );

        std::vector<std::string> res;
        res.reserve(info.size());
        for (const std::pair<K, V>& it : info) {
            res.push_back(it.second);
        }
        return res;
    }

    /**
     * Unbinds the key or keys that have been provided. This function can be called with a
     * single key or with an array of keys to remove all of the provided keys at once.
     */
    [[codegen::luawrap]] void clearKey(std::variant<std::string, ghoul::Dictionary> key) {
        using namespace openspace;

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
    }

    // Clear all key bindings
    [[codegen::luawrap]] void clearKeys() {
        openspace::global::keybindingManager->resetKeyBindings();
    }

#include "keybindingmanager_codegen.cpp"
} // namespace

namespace openspace::interaction {

KeybindingManager::KeybindingManager()
    : DocumentationGenerator(
        "Keybindings",
        "keybinding",
        {
            { "keybindingTemplate", "${WEB}/documentation/keybinding.hbs" }
        }
    )
{}

void KeybindingManager::keyboardCallback(Key key, KeyModifier modifier, KeyAction action)
{
    if (action == KeyAction::Press || action == KeyAction::Repeat) {
        // iterate over key bindings
        auto ret = _keyLua.equal_range({ key, modifier });
        for (auto it = ret.first; it != ret.second; ++it) {
            ghoul_assert(!it->second.empty(), "Action must not be empty");
            ghoul_assert(
                global::actionManager->hasAction(it->second),
                "Action must be registered"
            );
            global::actionManager->triggerAction(it->second, ghoul::Dictionary());
        }
    }
}

void KeybindingManager::resetKeyBindings() {
    _keyLua.clear();
}

void KeybindingManager::bindKey(Key key, KeyModifier modifier, std::string action) {
#ifdef WIN32
    const bool isShift = hasKeyModifier(modifier, KeyModifier::Shift);
    if (isShift && isKeypadKey(key)) {
        LWARNINGC(
            "bindKey",
            "Windows does not support binding keys to Shift + Keypad as it will "
            "internally convert these into Home, End, etc, keys."
        );
    }
#endif // WIN32
    ghoul_assert(!action.empty(), "Action must not be empty");

    KeyWithModifier km = { key, modifier };
    _keyLua.insert({ km, std::move(action) });
}

void KeybindingManager::removeKeyBinding(const KeyWithModifier& key) {
    // Erase-remove idiom does not work for std::multimap so we have to do this on foot

    for (auto it = _keyLua.begin(); it != _keyLua.end(); ) {
        // If the current iterator is the key that we are looking for, delete it
        // (std::multimap::erase will return the iterator to the next element for us)
        if (it->first == key) {
            it = _keyLua.erase(it);
        }
        else {
            // If it is not, we continue iteration
            ++it;
        }
    }
}

std::vector<std::pair<KeyWithModifier, std::string>> KeybindingManager::keyBinding(
                                                         const KeyWithModifier& key) const
{
    std::vector<std::pair<KeyWithModifier, std::string>> result;

    auto itRange = _keyLua.equal_range(key);
    for (auto it = itRange.first; it != itRange.second; ++it) {
        result.emplace_back(it->first, it->second);
    }
    return result;
}

const std::multimap<KeyWithModifier, std::string>& KeybindingManager::keyBindings() const
{
    return _keyLua;
}

std::string KeybindingManager::generateJson() const {
    ZoneScoped

    std::stringstream json;
    json << "[";
    bool first = true;
    for (const std::pair<const KeyWithModifier, std::string>& p : _keyLua) {
        if (!first) {
            json << ",";
        }
        first = false;
        json << "{";
        json << R"("key": ")" << ghoul::to_string(p.first) << "\",";
        json << R"("action": ")" << p.second << "\"";
        json << "}";
    }
    json << "]";

    return json.str();
}

scripting::LuaLibrary KeybindingManager::luaLibrary() {
    return {
        "",
        {
            codegen::lua::bindKey,
            codegen::lua::keyBindings,
            codegen::lua::clearKey,
            codegen::lua::clearKeys
        }
    };
}

} // namespace openspace::interaction
