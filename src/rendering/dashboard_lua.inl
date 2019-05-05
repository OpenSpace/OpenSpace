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

#include <modules/base/rendering/screenspacedashboard.h>
#include <openspace/engine/globals.h>
#include <ghoul/logging/logmanager.h>

namespace openspace::luascriptfunctions {

/**
* \ingroup LuaScripts
* addDashboardItem(table):
*/
int addDashboardItem(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::addDashboardItem");

    const int type = lua_type(L, -1);
    if (type == LUA_TTABLE) {
        ghoul::Dictionary d;
        try {
            ghoul::lua::luaDictionaryFromState(L, d);
        }
        catch (const ghoul::lua::LuaFormatException& e) {
            LERRORC("addDashboardItem", e.what());
            lua_settop(L, 0);
            return 0;
        }
        lua_settop(L, 0);

        global::dashboard.addDashboardItem(DashboardItem::createFromDictionary(d));

        ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
        return 0;
    }
    else {
        return ghoul::lua::luaError(L, "Expected argument of type 'table'");
    }
}

/**
* \ingroup LuaScripts
* removeDashboardItem(string):
*/
int removeDashboardItem(lua_State* L) {
    using ghoul::lua::errorLocation;

    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::removeDashbordItem");

    std::string identifier = luaL_checkstring(L, -1);

    global::dashboard.removeDashboardItem(identifier);

    lua_settop(L, 0);
    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}


/**
* \ingroup LuaScripts
* removeDashboardItems():
*/
int clearDashboardItems(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::clearDashboardItems");

    global::dashboard.clearDashboardItems();

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

}// namespace openspace::luascriptfunctions
