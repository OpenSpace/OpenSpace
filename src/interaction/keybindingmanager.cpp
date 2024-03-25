/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/glm.h>
#include <sstream>

#include "keybindingmanager_lua.inl"

namespace openspace::interaction {

void KeybindingManager::keyboardCallback(Key key, KeyModifier modifier, KeyAction action)
{
    if (action == KeyAction::Press || action == KeyAction::Repeat) {
        // iterate over key bindings
        auto ret = _keyLua.equal_range({ key, modifier });
        for (auto it = ret.first; it != ret.second; it++) {
            ghoul_assert(!it->second.empty(), "Action must not be empty");
            if (!global::actionManager->hasAction(it->second)) {
                // Silently ignoring the unknown action as the user might have intended to
                // bind a key to multiple actions, only one of which could be defined
                continue;
            }
            global::actionManager->triggerAction(
                it->second,
                ghoul::Dictionary(),
                interaction::ActionManager::ShouldBeSynchronized::Yes
            );
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
            "internally convert these into Home, End, etc, keys"
        );
    }
#endif // WIN32
    ghoul_assert(!action.empty(), "Action must not be empty");

    const KeyWithModifier km = { key, modifier };
    _keyLua.insert({ km, std::move(action) });
}

void KeybindingManager::removeKeyBinding(const KeyWithModifier& key) {
    // Erase-remove idiom does not work for std::multimap so we have to do this on foot

    for (auto it = _keyLua.begin(); it != _keyLua.end();) {
        // If the current iterator is the key that we are looking for, delete it
        // (std::multimap::erase will return the iterator to the next element for us)
        if (it->first == key) {
            it = _keyLua.erase(it);
        }
        else {
            // If it is not, we continue iteration
            it++;
        }
    }
}

std::vector<std::pair<KeyWithModifier, std::string>> KeybindingManager::keyBinding(
                                                         const KeyWithModifier& key) const
{
    std::vector<std::pair<KeyWithModifier, std::string>> result;

    auto itRange = _keyLua.equal_range(key);
    for (auto it = itRange.first; it != itRange.second; it++) {
        result.emplace_back(it->first, it->second);
    }
    return result;
}

const std::multimap<KeyWithModifier, std::string>& KeybindingManager::keyBindings() const
{
    return _keyLua;
}

scripting::LuaLibrary KeybindingManager::luaLibrary() {
    return {
        "",
        {
            codegen::lua::BindKey,
            codegen::lua::KeyBindings,
            codegen::lua::ClearKey,
            codegen::lua::ClearKeys
        }
    };
}

} // namespace openspace::interaction
