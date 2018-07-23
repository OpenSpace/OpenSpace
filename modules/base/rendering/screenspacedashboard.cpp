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

namespace {
    constexpr openspace::properties::Property::PropertyInfo UseMainInfo = {
        "UseMainDashboard",
        "Use main dashboard",
        "If this value is set to 'true', this ScreenSpaceDashboard will use the "
        "main dashboard instead of creating an independent one."
    };
} // namespace

namespace openspace {

namespace luascriptfunctions {

/**
* \ingroup LuaScripts
* addDashboardItemToScreenSpace(string, table):
*/
int addDashboardItemToScreenSpace(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 2, "lua::addDashboardItemToScreenSpace");

    const std::string& name = ghoul::lua::value<std::string>(L, 1);
    const int type = lua_type(L, 2);
    if (type != LUA_TTABLE) {
        return ghoul::lua::luaError(L, "Expected argument of type 'table'");
    }

    ghoul::Dictionary d;
    try {
        ghoul::lua::luaDictionaryFromState(L, d);
    }
    catch (const ghoul::lua::LuaFormatException& e) {
        LERRORC("addDashboardItem", e.what());
        return 0;
    }

    ScreenSpaceRenderable* ssr = global::renderEngine.screenSpaceRenderable(name);

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

    lua_settop(L, 0);
    return 0;
}

/**
* \ingroup LuaScripts
* removeDashboardItemsFromScreenSpace(string):
*/
int removeDashboardItemsFromScreenSpace(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::removeDashboardItemsFromScreenSpace");

    const std::string& name = ghoul::lua::value<std::string>(L, 1);
    ScreenSpaceRenderable* ssr = global::renderEngine.screenSpaceRenderable(name);

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

} // namespace luascriptfunctions


documentation::Documentation ScreenSpaceDashboard::Documentation() {
    using namespace openspace::documentation;
    return {
        "ScreenSpace Dashboard",
        "base_screenspace_dashboard",
        {
            {
                KeyName,
                new StringVerifier,
                Optional::Yes,
                "Specifies the GUI name of the ScreenSpaceDashboard"
            },
            {
                UseMainInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                UseMainInfo.description
            }
        }
    };
}

ScreenSpaceDashboard::ScreenSpaceDashboard(const ghoul::Dictionary& dictionary)
    : ScreenSpaceFramebuffer(dictionary)
    , _useMainDashboard(UseMainInfo, false)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "ScreenSpaceDashboard"
    );

    int iIdentifier = 0;
    if (_identifier.empty()) {
        static int id = 0;
        iIdentifier = id;

        if (iIdentifier == 0) {
            setIdentifier("ScreenSpaceDashboard");
        }
        else {
            setIdentifier("ScreenSpaceDashboard" + std::to_string(iIdentifier));
        }
        ++id;
    }

    if (_guiName.empty()) {
        // Adding an extra space to the user-facing name as it looks nicer
        setGuiName("ScreenSpaceDashboard " + std::to_string(iIdentifier));
    }

    if (dictionary.hasKey(UseMainInfo.identifier)) {
        _useMainDashboard = dictionary.value<bool>(UseMainInfo.identifier);
    }
    addProperty(_useMainDashboard);

    _scale = 1.f;
    _scale.setMaxValue(15.f);
}

bool ScreenSpaceDashboard::initializeGL() {
    ScreenSpaceFramebuffer::initializeGL();

    addRenderFunction([this]() {
        glm::vec2 penPosition = glm::vec2(10.f, _size.value().w );

        if (_useMainDashboard) {
            global::dashboard.render(penPosition);
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
    if (global::windowDelegate.windowHasResized()) {
        const glm::ivec2 size = global::windowDelegate.currentWindowResolution();
        _size = { 0.f, 0.f, size.x, size.y };
        _originalViewportSize = size;
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
                {},
                "string, table",
                "Adds a new dashboard item to an existing SceenSpaceDashboard."
            },
            {
                "removeDashboardItemsFromScreenSpace",
                &luascriptfunctions::removeDashboardItemsFromScreenSpace,
                {},
                "string",
                "Removes all dashboard items from an existing ScreenSpaceDashboard."
            }
        }
    };
}
} // namespace openspace
