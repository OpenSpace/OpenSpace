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

namespace openspace::luascriptfunctions {

/**
 * \ingroup LuaScripts
 * addDashboardItemToScreenSpace(string, table):
 */
int addDashboardItemToScreenSpace(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 2, "lua::addDashboardItemToScreenSpace");
    auto [name, d] = ghoul::lua::values<std::string, ghoul::Dictionary>(L);

    ScreenSpaceRenderable* ssr = global::renderEngine->screenSpaceRenderable(name);
    if (!ssr) {
        return ghoul::lua::luaError(L, "Provided name is not a ScreenSpace item");
    }

    ScreenSpaceDashboard* dash = dynamic_cast<ScreenSpaceDashboard*>(ssr);
    if (!dash) {
        return ghoul::lua::luaError(
            L,
            "Provided name is a ScreenSpace item but not a dashboard"
        );
    }

    dash->dashboard().addDashboardItem(DashboardItem::createFromDictionary(d));
    return 0;
}

/**
 * \ingroup LuaScripts
 * removeDashboardItemsFromScreenSpace(string):
 */
int removeDashboardItemsFromScreenSpace(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::removeDashboardItemsFromScreenSpace");
    const std::string name = ghoul::lua::value<std::string>(L);

    ScreenSpaceRenderable* ssr = global::renderEngine->screenSpaceRenderable(name);
    if (!ssr) {
        return ghoul::lua::luaError(L, "Provided name is not a ScreenSpace item");
    }

    ScreenSpaceDashboard* dash = dynamic_cast<ScreenSpaceDashboard*>(ssr);
    if (!dash) {
        return ghoul::lua::luaError(
            L,
            "Provided name is a ScreenSpace item but not a dashboard"
        );
    }

    dash->dashboard().clearDashboardItems();
    return 0;
}

} // namespace openspace::luascriptfunctions
