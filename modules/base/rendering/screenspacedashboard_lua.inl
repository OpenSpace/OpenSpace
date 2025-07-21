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

// Adds a new dashboard item to an existing SceenSpaceDashboard.
[[codegen::luawrap]] void addDashboardItemToScreenSpace(std::string identifier,
                                                        ghoul::Dictionary dashboard)
{
    using namespace openspace;

    ScreenSpaceRenderable* ssr = global::renderEngine->screenSpaceRenderable(identifier);
    if (!ssr) {
        throw ghoul::lua::LuaError("Provided name is not a ScreenSpace item");
    }

    ScreenSpaceDashboard* dash = dynamic_cast<ScreenSpaceDashboard*>(ssr);
    if (!dash) {
        throw ghoul::lua::LuaError(
            "Provided name is a ScreenSpace item but not a dashboard"
        );
    }

    dash->dashboard().addDashboardItem(DashboardItem::createFromDictionary(dashboard));
}

// Removes all dashboard items from an existing ScreenSpaceDashboard.
[[codegen::luawrap]] void removeDashboardItemsFromScreenSpace(std::string identifier) {
    using namespace openspace;

    ScreenSpaceRenderable* ssr = global::renderEngine->screenSpaceRenderable(identifier);
    if (!ssr) {
        throw ghoul::lua::LuaError("Provided identifier is not a ScreenSpace item");
    }

    ScreenSpaceDashboard* dash = dynamic_cast<ScreenSpaceDashboard*>(ssr);
    if (!dash) {
        throw ghoul::lua::LuaError(
            "Provided identifier is a ScreenSpace item but not a dashboard"
        );
    }

    dash->dashboard().clearDashboardItems();
}

#include "screenspacedashboard_lua_codegen.cpp"

} // namespace
