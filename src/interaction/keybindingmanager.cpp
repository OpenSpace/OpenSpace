/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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
#include <openspace/scripting/lualibrary.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/glm.h>
#include <sstream>

#include "keybindingmanager_lua.inl"

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
            using RS = scripting::ScriptEngine::RemoteScripting;

            global::scriptEngine.queueScript(
                it->second.command,
                it->second.synchronization ? RS::Yes : RS::No
            );
        }
    }
}

void KeybindingManager::resetKeyBindings() {
    _keyLua.clear();
}

void KeybindingManager::bindKeyLocal(Key key, KeyModifier modifier,
                                     std::string luaCommand, std::string documentation,
                                     std::string name, std::string guiPath)
{
#ifdef WIN32
    const bool isShift = hasKeyModifier(modifier, KeyModifier::Shift);
    const bool isKeypad = key == Key::Keypad0 || key == Key::Keypad1 ||
        key == Key::Keypad2 || key == Key::Keypad3 || key == Key::Keypad4 ||
        key == Key::Keypad5 || key == Key::Keypad6 || key == Key::Keypad7 ||
        key == Key::Keypad8 || key == Key::Keypad9 || key == Key::KeypadEnter ||
        key == Key::KeypadAdd || key == Key::KeypadSubtract ||
        key == Key::KeypadMultiply || key == Key::KeypadDivide;

    if (isShift && isKeypad) {
        LWARNINGC(
            "bindKey",
            "Windows does not support binding keys to Shift + Keyboard as it will "
            "internally convert these into Home, End, etc, keys."
        );
    }
#endif // WIN32

    _keyLua.insert({
        { key, modifier },
        {
            std::move(luaCommand),
            IsSynchronized::No,
            std::move(documentation),
            std::move(name),
            std::move(guiPath)
        }
    });
}

void KeybindingManager::bindKey(Key key, KeyModifier modifier, std::string luaCommand,
                                std::string documentation, std::string name,
                                std::string guiPath)
{
#ifdef WIN32
    const bool isShift = hasKeyModifier(modifier, KeyModifier::Shift);
    const bool isKeypad = key == Key::Keypad0 || key == Key::Keypad1 ||
        key == Key::Keypad2 || key == Key::Keypad3 || key == Key::Keypad4 ||
        key == Key::Keypad5 || key == Key::Keypad6 || key == Key::Keypad7 ||
        key == Key::Keypad8 || key == Key::Keypad9 || key == Key::KeypadEnter ||
        key == Key::KeypadAdd || key == Key::KeypadSubtract ||
        key == Key::KeypadMultiply || key == Key::KeypadDivide;

    if (isShift && isKeypad) {
        LWARNINGC(
            "bindKey",
            "Windows does not support binding keys to Shift + Keyboard as it will "
            "internally convert these into Home, End, etc, keys."
        );
    }
#endif // WIN32

    _keyLua.insert({
        { key, modifier },
        {
            std::move(luaCommand),
            IsSynchronized::Yes,
            std::move(documentation),
            std::move(name),
            std::move(guiPath)
        }
    });
}

void KeybindingManager::removeKeyBinding(const std::string& key) {
    // Erase-remove idiom does not work for std::multimap so we have to do this on foot

    KeyWithModifier k = stringToKey(key);

    for (auto it = _keyLua.begin(); it != _keyLua.end(); ) {
        // If the current iterator is the key that we are looking for, delete it
        // (std::multimap::erase will return the iterator to the next element for us)
        if (it->first == k) {
            it = _keyLua.erase(it);
        }
        else {
            // We if it is not, we continue iteration
            ++it;
        }
    }
}

std::vector<std::pair<KeyWithModifier, KeybindingManager::KeyInformation>>
KeybindingManager::keyBinding(const std::string& key) const
{
    std::vector<std::pair<KeyWithModifier, KeyInformation>> result;

    KeyWithModifier k = stringToKey(key);
    auto itRange = _keyLua.equal_range(k);
    for (auto it = itRange.first; it != itRange.second; ++it) {
        result.emplace_back(it->first, it->second);
    }
    return result;
}

const std::multimap<KeyWithModifier, KeybindingManager::KeyInformation>&
KeybindingManager::keyBindings() const
{
    return _keyLua;
}

std::string KeybindingManager::generateJson() const {
    ZoneScoped

    std::stringstream json;
    json << "[";
    bool first = true;
    for (const std::pair<const KeyWithModifier, KeyInformation>& p : _keyLua) {
        if (!first) {
            json << ",";
        }
        first = false;
        json << "{";
        json << R"("key": ")" << ghoul::to_string(p.first) << "\",";
        json << R"("script": ")" << escapedJson(p.second.command) << "\",";
        json << R"("remoteScripting": )"
             << (p.second.synchronization ? "true," : "false,");
        json << R"("documentation": ")" << escapedJson(p.second.documentation) << "\",";
        json << R"("name": ")" << escapedJson(p.second.name) << "\"";
        json << "}";
    }
    json << "]";

    return json.str();
}

scripting::LuaLibrary KeybindingManager::luaLibrary() {
    return {
        "",
        {
            {
                "clearKeys",
                &luascriptfunctions::clearKeys,
                {},
                "",
                "Clear all key bindings"
            },
            {
                "clearKey",
                &luascriptfunctions::clearKey,
                {},
                "string or strings",
                "Unbinds the key or keys that have been provided. This function can be "
                "called with a single key or with an array of keys to remove all of the "
                "provided keys at once"
            },
            {
                "bindKey",
                &luascriptfunctions::bindKey,
                {},
                "string, string [, string]",
                "Binds a key by name to a lua string command to execute both locally "
                "and to broadcast to clients if this is the host of a parallel session. "
                "The first argument is the key, the second argument is the Lua command "
                "that is to be executed, and the optional third argument is a human "
                "readable description of the command for documentation purposes."
            },
            {
                "bindKeyLocal",
                &luascriptfunctions::bindKeyLocal,
                {},
                "string, string [, string]",
                "Binds a key by name to a lua string command to execute only locally. "
                "The first argument is the key, the second argument is the Lua command "
                "that is to be executed, and the optional third argument is a human "
                "readable description of the command for documentation purposes."
            },
            {
                "getKeyBinding",
                &luascriptfunctions::getKeyBindings,
                {},
                "string",
                "Returns a list of information about the keybindings for the provided "
                "key. Each element in the list is a table describing the 'Command' that "
                "was bound and whether it was a 'Remote' script or not."
            }
        }
    };
}

} // namespace openspace::interaction
