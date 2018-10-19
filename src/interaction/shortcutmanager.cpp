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

#include <openspace/interaction/shortcutmanager.h>

#include <openspace/engine/globals.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/glm.h>
#include <sstream>

#include "shortcutmanager_lua.inl"

namespace openspace::interaction {

void ShortcutManager::resetShortcuts() {
    _shortcuts.clear();
}

void ShortcutManager::addShortcut(ShortcutInformation info) {
    _shortcuts.push_back(std::move(info));
}

const std::vector<ShortcutManager::ShortcutInformation>&
ShortcutManager::shortcuts() const
{
    return _shortcuts;
}

scripting::LuaLibrary ShortcutManager::luaLibrary() {
    return {
        "",
        {
            {
                "clearShortcuts",
                &luascriptfunctions::clearShortcuts,
                {},
                "",
                "Clear all shortcuts in this scene"
            },
            {
                "bindShortcut",
                &luascriptfunctions::bindShortcut,
                {},
                "string, string [, string]",
                "Binds a Lua script to a new shortcut that is executed both locally and "
                "to be broadcast to clients if this is the host of a parallel session. "
                "The first argument is a human-readable name for this shortcut, the "
                "second argument is the Lua script that will be executed and the last "
                "argument is a describtive text for the shortcut for tooltips, etc."
            },
            {
                "bindShortcutLocal",
                &luascriptfunctions::bindShortcutLocal,
                {},
                "string, string [, string]",
                "Binds a Lua script to a new shortcut that is executed onlylocally. The "
                "first argument is a human-readable name for this shortcut, the second "
                "argument is the Lua script that will be executed and the last argument "
                "is a describtive text for the shortcut for tooltips, etc."
            }
        }
    };
}

} // namespace openspace::interaction
