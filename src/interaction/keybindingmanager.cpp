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

#include <openspace/interaction/keybindingmanager.h>

#include <openspace/openspace.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/query/query.h>
#include <openspace/util/keys.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>

#include <glm/gtx/string_cast.hpp>
#include <ghoul/glm.h>

#include <fstream>

namespace {
    const char* MainTemplateFilename = "${OPENSPACE_DATA}/web/keybindings/main.hbs";
    const char* KeybindingTemplateFilename = "${OPENSPACE_DATA}/web/keybindings/keybinding.hbs";
    const char* JsFilename = "${OPENSPACE_DATA}/web/keybindings/script.js";
} // namespace

#include "keybindingmanager_lua.inl"

namespace openspace::interaction {

KeyBindingManager::KeyBindingManager()
    : DocumentationGenerator(
        "Documentation",
        "keybindings",
        {
            { "keybindingTemplate",  KeybindingTemplateFilename },
            { "mainTemplate", MainTemplateFilename }
        },
        JsFilename
    )
{ }

void KeyBindingManager::keyboardCallback(Key key, KeyModifier modifier, KeyAction action) {
    if (action == KeyAction::Press || action == KeyAction::Repeat) {
        // iterate over key bindings
        std::pair<std::multimap<KeyWithModifier, KeyInformation>::iterator,
            std::multimap<KeyWithModifier, KeyInformation>::iterator> ret =
            _keyLua.equal_range({ key, modifier });
        for (std::multimap<KeyWithModifier, KeyInformation>::iterator it = ret.first;
            it != ret.second;
            ++it)
        {
            scripting::ScriptEngine::RemoteScripting remote =
                it->second.synchronization ?
                scripting::ScriptEngine::RemoteScripting::Yes :
                scripting::ScriptEngine::RemoteScripting::No;

            OsEng.scriptEngine().queueScript(it->second.command, remote);
        }
    }
}

void KeyBindingManager::resetKeyBindings() {
    _keyLua.clear();
}

void KeyBindingManager::bindKeyLocal(Key key, KeyModifier modifier,
                                      std::string luaCommand, std::string documentation)
{
    _keyLua.insert({
        { key, modifier },
        {
            std::move(luaCommand),
            Synchronized::No,
            std::move(documentation)
        }
    });
}

void KeyBindingManager::bindKey(Key key, KeyModifier modifier,
                                 std::string luaCommand, std::string documentation)
{
    _keyLua.insert({
        { key, modifier },
        {
            std::move(luaCommand),
            Synchronized::Yes,
            std::move(documentation)
        }
    });
}
    
std::string KeyBindingManager::generateJson() const {
    std::stringstream json;
    json << "[";
    bool first = true;
    for (const std::pair<KeyWithModifier, KeyInformation>& p : _keyLua) {
        if (!first) {
            json << ",";
        }
        first = false;
        json << "{";
        json << "\"key\": \"" << std::to_string(p.first) << "\",";
        json << "\"script\": \"" << p.second.command << "\",";
        json << "\"remoteScripting\": " << (p.second.synchronization ? "true," : "false,");
        json << "\"documentation\": \"" << escapedJson(p.second.documentation) << "\"";
        json << "}";
    }
    json << "]";
    
    std::string jsonString = "";
    for (const char& c : json.str()) {
        if (c == '\'') {
            jsonString += "\\'";
        } else {
            jsonString += c;
        }
    }

    return jsonString;
}    

scripting::LuaLibrary KeyBindingManager::luaLibrary() {
    return {
        "",
        {
            {
                "clearKeys",
                &luascriptfunctions::clearKeys,
                "",
                "Clear all key bindings"
            },
            {
                "bindKey",
                &luascriptfunctions::bindKey,
                "string, string [,string]",
                "Binds a key by name to a lua string command to execute both locally "
                "and to broadcast to clients if this is the host of a parallel session. "
                "The first argument is the key, the second argument is the Lua command "
                "that is to be executed, and the optional third argument is a human "
                "readable description of the command for documentation purposes."
            },
            {
                "bindKeyLocal",
                &luascriptfunctions::bindKeyLocal,
                "string, string [,string]",
                "Binds a key by name to a lua string command to execute only locally. "
                "The first argument is the key, the second argument is the Lua command "
                "that is to be executed, and the optional third argument is a human "
                "readable description of the command for documentation purposes."
            },
        }
    };
}

} // namespace openspace::interaction
