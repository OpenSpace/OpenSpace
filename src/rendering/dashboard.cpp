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

#include <openspace/rendering/dashboard.h>

#include <openspace/rendering/dashboarditem.h>
#include <openspace/scripting/scriptengine.h>

#include "dashboard_lua.inl"

namespace openspace {

Dashboard::Dashboard()
    : properties::PropertyOwner({ "Dashboard" })
{}

void Dashboard::addDashboardItem(std::unique_ptr<DashboardItem> item) {
    addPropertySubOwner(item.get());
    _items.push_back(std::move(item));
}

void Dashboard::removeDashboardItem(int index) {
    ghoul_assert(index < _items.size(), "Invalid index");
    removePropertySubOwner(_items[index].get());
    _items.erase(_items.begin() + index);
}

bool Dashboard::hasItem(int index) const {
    return (index >= 0) && (index < _items.size());
}

const DashboardItem& Dashboard::item(int index) const {
    ghoul_assert(index < _items.size(), "Invalid index");
    return *_items[index];
}

void Dashboard::removeDashboardItems() {
    for (const std::unique_ptr<DashboardItem>& item : _items) {
        removePropertySubOwner(item.get());
    }
    _items.clear();
}

void Dashboard::render(glm::vec2& penPosition) {
    for (const std::unique_ptr<DashboardItem>& item : _items) {
        if (item->isEnabled()) {
            item->render(penPosition);
        }
    }
}

scripting::LuaLibrary Dashboard::luaLibrary() {
    return {
        "dashboard",
        {
            {
                "addDashboardItem",
                &luascriptfunctions::addDashboardItem,
                "table",
                "Adds a new dashboard item to the main dashboard."
            },
            {
                "removeDashboardItems",
                &luascriptfunctions::removeDashboardItems,
                "",
                "Removes all dashboard items from the main dashboard."
            }
        }
    };
}

} // namespace openspace
