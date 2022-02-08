/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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
        "If this value is set to 'true', this ScreenSpaceDashboard will use the "
        "main dashboard instead of creating an independent one."
    };

    struct [[codegen::Dictionary(ScreenSpaceDashboard)]] Parameters {
        // Specifies the GUI name of the ScreenSpaceDashboard
        std::optional<std::string> name;

        // [[codegen::verbatim(UseMainInfo.description)]]
        std::optional<bool> useMainDashboard;
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

    // @TODO (abock, 2021-01-29) Should this be the name variable? The identifier wasn't
    // declared in the documentation
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

    _scale = 1.f;
    _scale.setMaxValue(15.f);
}

bool ScreenSpaceDashboard::initializeGL() {
    ScreenSpaceFramebuffer::initializeGL();

    addRenderFunction([this]() {
        glm::vec2 penPosition = glm::vec2(10.f, _size.value().w );

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
    //_fontRenderer = nullptr;
    return ScreenSpaceFramebuffer::deinitializeGL();
}

bool ScreenSpaceDashboard::isReady() const {
    return /*(_fontRenderer != nullptr) &&
           (_fontDate != nullptr) &&
           (_fontInfo != nullptr) &&*/
           ScreenSpaceFramebuffer::isReady();
}

void ScreenSpaceDashboard::update() {
    if (global::windowDelegate->windowHasResized()) {
        const glm::ivec2 size = global::windowDelegate->currentDrawBufferResolution();
        _size = { 0.f, 0.f, size.x, size.y };
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
            {
                "addDashboardItemToScreenSpace",
                &luascriptfunctions::addDashboardItemToScreenSpace,
                "string, table",
                "Adds a new dashboard item to an existing SceenSpaceDashboard."
            },
            {
                "removeDashboardItemsFromScreenSpace",
                &luascriptfunctions::removeDashboardItemsFromScreenSpace,
                "string",
                "Removes all dashboard items from an existing ScreenSpaceDashboard."
            }
        }
    };
}
} // namespace openspace
