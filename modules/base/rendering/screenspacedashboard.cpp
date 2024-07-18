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

#include <modules/base/rendering/screenspacedashboard.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/dashboarditem.h>
#include <openspace/scripting/lualibrary.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/logging/logmanager.h>
#include <optional>

namespace {
    constexpr openspace::properties::Property::PropertyInfo UseMainInfo = {
        "UseMainDashboard",
        "Use main dashboard",
        "If true, this ScreenSpaceDashboard will use the main dashboard instead of "
        "creating an independent one.",
        openspace::properties::Property::Visibility::Developer
    };

    struct [[codegen::Dictionary(ScreenSpaceDashboard)]] Parameters {
        // [[codegen::verbatim(UseMainInfo.description)]]
        std::optional<bool> useMainDashboard;

        // A list of DashboardItems that are added automatically upon construction of the
        // ScreenSpaceDashboard. This value must not be specified if `UseMainDashboard` is
        // specified.
        std::optional<std::vector<ghoul::Dictionary>>
            items [[codegen::reference("dashboarditem")]];
    };
#include "screenspacedashboard_codegen.cpp"
} // namespace

#include "screenspacedashboard_lua.inl"

namespace openspace {

documentation::Documentation ScreenSpaceDashboard::Documentation() {
    return codegen::doc<Parameters>("base_screenspace_dashboard");
}

ScreenSpaceDashboard::ScreenSpaceDashboard(const ghoul::Dictionary& dictionary)
    : ScreenSpaceFramebuffer(dictionary)
    , _useMainDashboard(UseMainInfo, false)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    std::string identifier;
    if (dictionary.hasValue<std::string>(KeyIdentifier)) {
        identifier = dictionary.value<std::string>(KeyIdentifier);
    }
    else {
        identifier = "ScreenSpaceDashboard";
    }
    identifier = makeUniqueIdentifier(identifier);
    setIdentifier(std::move(identifier));

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

bool ScreenSpaceDashboard::initializeGL() {
    ScreenSpaceFramebuffer::initializeGL();

    addRenderFunction([this]() {
        glm::vec2 penPosition = glm::vec2(0.f, _size.value().w);

        if (_useMainDashboard) {
            global::dashboard->render(penPosition);
        }
        else {
            _dashboard.render(penPosition);
        }
    });

    return true;
}

bool ScreenSpaceDashboard::deinitializeGL() {
    return ScreenSpaceFramebuffer::deinitializeGL();
}

bool ScreenSpaceDashboard::isReady() const {
    return ScreenSpaceFramebuffer::isReady();
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

scripting::LuaLibrary ScreenSpaceDashboard::luaLibrary() {
    return {
        "dashboard",
        {
            codegen::lua::AddDashboardItemToScreenSpace,
            codegen::lua::RemoveDashboardItemsFromScreenSpace
        }
    };
}
} // namespace openspace
