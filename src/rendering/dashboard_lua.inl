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

namespace {

 // Adds a new dashboard item to the main dashboard.
[[codegen::luawrap]] void addDashboardItem(ghoul::Dictionary dashboard) {
    using namespace openspace;
    try {
        global::dashboard->addDashboardItem(
            DashboardItem::createFromDictionary(std::move(dashboard))
        );
    }
    catch (const ghoul::RuntimeError& e) {
        throw ghoul::lua::LuaError(std::format(
            "Error adding dashboard item: {}", e.what()
        ));
    }
}

// Removes the dashboard item with the specified identifier.
[[codegen::luawrap]] void removeDashboardItem(
                                  std::variant<std::string, ghoul::Dictionary> identifier)
{
    std::string identifierStr;
    if (std::holds_alternative<std::string>(identifier)) {
        identifierStr = std::get<std::string>(identifier);
    }
    else {
        ghoul::Dictionary d = std::get<ghoul::Dictionary>(identifier);
        if (!d.hasValue<std::string>("Identifier")) {
            throw ghoul::lua::LuaError("Passed table does not contain an Identifier");
        }
        identifierStr = d.value<std::string>("Identifier");
    }

    openspace::global::dashboard->removeDashboardItem(identifierStr);
}

// Removes all dashboard items from the main dashboard.
[[codegen::luawrap]] void clearDashboardItems() {
    openspace::global::dashboard->clearDashboardItems();
}

#include "dashboard_lua_codegen.cpp"

} // namespace
