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

#include <openspace/engine/globals.h>
#include <ghoul/logging/logmanager.h>

namespace {

openspace::interaction::ShortcutManager::ShortcutInformation extractInfo(lua_State* L,
                                                                         int nArguments,
                                                                         bool isSync)
{
    openspace::interaction::ShortcutManager::ShortcutInformation i = {
        ghoul::lua::value<std::string>(L, 1, ghoul::lua::PopValue::No),
        ghoul::lua::value<std::string>(L, 2, ghoul::lua::PopValue::No),
        openspace::interaction::ShortcutManager::IsSynchronized(isSync),
        nArguments >= 3 ?
            ghoul::lua::value<std::string>(L, 3, ghoul::lua::PopValue::No) :
            "",
        nArguments == 4 ?
            ghoul::lua::value<std::string>(L, 4, ghoul::lua::PopValue::No) :
            ""
    };
    lua_pop(L, nArguments);
    return i;
}

} // namespace

namespace openspace::luascriptfunctions {

int clearShortcuts(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::clearShortcuts");
    global::shortcutManager.resetShortcuts();
    return 0;
}

int bindShortcut(lua_State* L) {
    int n = ghoul::lua::checkArgumentsAndThrow(L, { 2, 4 }, "lua::bindShortcut");

    interaction::ShortcutManager::ShortcutInformation info = extractInfo(L, n, true);
    global::shortcutManager.addShortcut(std::move(info));

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

int bindShortcutLocal(lua_State* L) {
    int n = ghoul::lua::checkArgumentsAndThrow(L, { 2, 4 }, "lua::bindShortcutLocal");

    interaction::ShortcutManager::ShortcutInformation info = extractInfo(L, n, false);
    global::shortcutManager.addShortcut(std::move(info));

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

} // namespace openspace::luascriptfunctions
