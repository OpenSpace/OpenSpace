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
 * addDashboardItem(table):
 */
int addDashboardItem(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::addDashboardItem");
    ghoul::Dictionary d = ghoul::lua::value<ghoul::Dictionary>(L);

    try {
        global::dashboard->addDashboardItem(
            DashboardItem::createFromDictionary(std::move(d))
        );
    }
    catch (const ghoul::RuntimeError& e) {
        LERRORC("addDashboardItem", e.what());
        return ghoul::lua::luaError(L, "Error adding dashboard item");
    }

    return 0;
}

/**
 * \ingroup LuaScripts
 * removeDashboardItem(string):
 */
int removeDashboardItem(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::removeDashbordItem");
    const std::string identifier = ghoul::lua::value<std::string>(L);

    global::dashboard->removeDashboardItem(identifier);
    return 0;
}

/**
 * \ingroup LuaScripts
 * removeDashboardItems():
 */
int clearDashboardItems(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::clearDashboardItems");
    global::dashboard->clearDashboardItems();
    return 0;
}

}// namespace openspace::luascriptfunctions
