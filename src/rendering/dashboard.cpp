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

#include <openspace/rendering/dashboard.h>

#include <openspace/engine/globals.h>
#include <openspace/rendering/dashboarditem.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/lua/lua_helper.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/profiling.h>

#include "dashboard_lua.inl"

namespace {
    constexpr openspace::properties::Property::PropertyInfo EnabledInfo = {
        "IsEnabled",
        "Enabled",
        "If this value is 'false', this dashboard will be invisible, regardless of the "
        "state of the individual components.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo StartPositionOffsetInfo = {
        "StartPositionOffset",
        "Start Position Offset",
        "A 2D vector controlling where the dashboard rendering starts. Adding an offset "
        "in x and y-direction on screen.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo RefreshRateInfo = {
        "RefreshRate",
        "Refresh Rate (in ms)",
        "The number of milliseconds between refreshes of the dashboard items. If the "
        "value is 0 the dashboard is refreshed at the same rate as the main rendering.",
        openspace::properties::Property::Visibility::AdvancedUser
    };
} // namespace

namespace openspace {

Dashboard::Dashboard()
    : properties::PropertyOwner({ "Dashboard" })
    , _isEnabled(EnabledInfo, true)
    , _startPositionOffset(
        StartPositionOffsetInfo,
        glm::ivec2(10, 10),
        glm::ivec2(0),
        glm::ivec2(8192)
    )
    , _refreshRate(RefreshRateInfo, 0, 0, 1000)
    , _lastRefresh(std::chrono::steady_clock::now())
{
    addProperty(_isEnabled);
    addProperty(_startPositionOffset);
    addProperty(_refreshRate);
}

void Dashboard::addDashboardItem(std::unique_ptr<DashboardItem> item) {
    const std::string& originalIdentifier = item->identifier();
    int suffix = 1;
    while (true) {
        auto it = std::find_if(
            _items.begin(),
            _items.end(),
            [&item](const std::unique_ptr<DashboardItem>& i) {
                return (i->identifier() == item->identifier());
            }
        );

        if (it == _items.end()) {
            // We found a unique name
            break;
        }
        else {
            item->setIdentifier(originalIdentifier + std::to_string(suffix));
            item->setGuiName(originalIdentifier + " " + std::to_string(suffix));
            ++suffix;
        }
    }

    addPropertySubOwner(item.get());
    _items.push_back(std::move(item));
}

void Dashboard::removeDashboardItem(int index) {
    ghoul_assert(index < static_cast<int>(_items.size()), "Invalid index");

    removePropertySubOwner(_items[index].get());
    _items.erase(_items.begin() + index);
}

void Dashboard::removeDashboardItem(const std::string& identifier) {
    const auto it = std::find_if(
        _items.begin(),
        _items.end(),
        [&identifier](const std::unique_ptr<DashboardItem>& i) {
            return i->identifier() == identifier;
        }
    );

    if (it == _items.end()) {
        return;
    }

    removePropertySubOwner(it->get());
    _items.erase(it);
}

bool Dashboard::hasItem(int index) const {
    return (index >= 0) && (index < static_cast<int>(_items.size()));
}

const DashboardItem& Dashboard::item(int index) const {
    ghoul_assert(index < static_cast<int>(_items.size()), "Invalid index");

    return *_items[index];
}

void Dashboard::clearDashboardItems() {
    for (const std::unique_ptr<DashboardItem>& item : _items) {
        removePropertySubOwner(item.get());
    }
    _items.clear();
}

void Dashboard::render(glm::vec2& penPosition) {
    ZoneScoped;

    if (!_isEnabled || _items.empty()) {
        return;
    }

    auto now = std::chrono::steady_clock::now();
    bool needsUpdate = (now - _lastRefresh) > std::chrono::milliseconds(_refreshRate);
    if (needsUpdate) {
        _lastRefresh = now;
    }

    for (const std::unique_ptr<DashboardItem>& item : _items) {
        if (item->isEnabled()) {
            if (needsUpdate) {
                item->update();
            }
            item->render(penPosition);
        }
    }
}

glm::ivec2 Dashboard::startPositionOffset() {
    return _startPositionOffset;
}

std::vector<DashboardItem*> Dashboard::dashboardItems() const {
    std::vector<DashboardItem*> result;
    result.reserve(_items.size());
    for (const std::unique_ptr<DashboardItem>& d : _items) {
        result.push_back(d.get());
    }
    return result;
}

scripting::LuaLibrary Dashboard::luaLibrary() {
    return {
        "dashboard",
        {
            codegen::lua::AddDashboardItem,
            codegen::lua::RemoveDashboardItem,
            codegen::lua::ClearDashboardItems,
            codegen::lua::DashboardItems
        }
    };
}

} // namespace openspace
