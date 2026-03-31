/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/dashboarditem.h>
#include <openspace/scripting/lualibrary.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/exception.h>
#include <optional>
#include <utility>

#include "screenspacedashboard_lua.inl"

namespace {
    using namespace openspace;

    constexpr Property::PropertyInfo UseMainInfo = {
        "UseMainDashboard",
        "Use main dashboard",
        "If true, this ScreenSpaceDashboard will use the main dashboard instead of "
        "creating an independent one.",
        Property::Visibility::Developer
    };

    // ScreenSpaceDashboard is a screen-space renderable for displaying dashboard content
    // as a 2D overlay on top of the scene. Unlike world-space objects, it is anchored to
    // the screen rather than to any location in the 3D environment, which makes it
    // suitable for HUD-style information displays.
    //
    // The class acts as a container and renderer for [DashboardItem](#core_dashboarditem)
    // instances. These items can present textual or status-oriented information such as
    // time, camera state, mission data, frame rate, or other runtime values. The
    // dashboard is rendered into a framebuffer-backed screen-space surface and then
    // composited into the final view.
    //
    // ScreenSpaceDashboard supports two usage patterns. It can either render the
    // application’s shared main dashboard, or it can host its own independent dashboard
    // instance. This makes it useful both for reusing a global information overlay and
    // for creating dedicated screen-space panels with their own item composition.
    struct [[codegen::Dictionary(ScreenSpaceDashboard)]] Parameters {
        std::optional<std::string> identifier [[codegen::private()]];

        // [[codegen::verbatim(UseMainInfo.description)]]
        std::optional<bool> useMainDashboard;

        // A list of DashboardItems that are added automatically upon construction of the
        // ScreenSpaceDashboard. This value must not be specified if `UseMainDashboard` is
        // specified.
        std::optional<std::vector<ghoul::Dictionary>>
            items [[codegen::reference("core_dashboarditem")]];
    };
} // namespace
#include "screenspacedashboard_codegen.cpp"

namespace openspace {

Documentation ScreenSpaceDashboard::Documentation() {
    return codegen::doc<Parameters>("base_screenspace_dashboard");
}

ScreenSpaceDashboard::ScreenSpaceDashboard(const ghoul::Dictionary& dictionary)
    : ScreenSpaceRenderableFramebuffer(dictionary)
    , _useMainDashboard(UseMainInfo, false)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    std::string identifier = p.identifier.value_or("ScreenSpaceDashboard");
    setIdentifier(makeUniqueIdentifier(std::move(identifier)));

    _useMainDashboard = p.useMainDashboard.value_or(_useMainDashboard);
    addProperty(_useMainDashboard);

    if (_useMainDashboard && p.items.has_value()) {
        throw ghoul::RuntimeError("Cannot specify items when using the main dashboard");
    }

    if (!_useMainDashboard) {
        addPropertySubOwner(_dashboard);
    }

    if (p.items.has_value()) {
        ghoul_assert(!_useMainDashboard, "Cannot add items to the main dashboard");
        for (const ghoul::Dictionary& item : *p.items) {
            std::unique_ptr<DashboardItem> i = DashboardItem::createFromDictionary(item);
            _dashboard.addDashboardItem(std::move(i));
        }
    }
}

void ScreenSpaceDashboard::initializeGL() {
    ScreenSpaceRenderableFramebuffer::initializeGL();

    addRenderFunction([this]() {
        glm::vec2 penPosition = glm::vec2(0.f, _size.value().x);

        if (_useMainDashboard) {
            global::dashboard->render(penPosition);
        }
        else {
            _dashboard.render(penPosition);
        }
    });
}

void ScreenSpaceDashboard::update() {
    if (global::windowDelegate->windowHasResized()) {
        const glm::ivec2 size = global::windowDelegate->currentDrawBufferResolution();
        _size = glm::vec4(0.f, 0.f, size.x, size.y);
        createFramebuffer();
    }
}

Dashboard& ScreenSpaceDashboard::dashboard() {
    return _dashboard;
}

const Dashboard& ScreenSpaceDashboard::dashboard() const {
    return _dashboard;
}

LuaLibrary ScreenSpaceDashboard::luaLibrary() {
    return {
        "dashboard",
        {
            codegen::lua::AddDashboardItemToScreenSpace,
            codegen::lua::RemoveDashboardItemsFromScreenSpace
        }
    };
}

} // namespace openspace
