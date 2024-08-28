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

#include <modules/debugging/debuggingmodule.h>

#include <modules/debugging/rendering/renderabledebugplane.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/configuration.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/path.h>
#include <openspace/navigation/pathnavigator.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/util/factorymanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/templatefactory.h>

#include "debuggingmodule_lua.inl"

namespace {
    constexpr std::string_view KeyFontMono = "Mono";

    constexpr openspace::properties::Property::PropertyInfo ShowStatisticsInfo = {
        "ShowStatistics",
        "Show Statistics",
        "Show updating, rendering, and network statistics on all rendering nodes.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo StatisticsScaleInfo = {
        "StatisticsScale",
        "Statistics Scale",
        "This value is scaling the statatistics window by the provided amount. For flat "
        "projections this is rarely necessary, but it is important when using a setup "
        "where the cornders of the image are masked out.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ShowFrameNumberInfo = {
        "ShowFrameInformation",
        "Show Frame Information",
        "If this value is enabled, the current frame number and frame times are rendered "
        "into the window.",
        openspace::properties::Property::Visibility::AdvancedUser
    };
} // namespace

namespace openspace {

DebuggingModule::DebuggingModule()
    : OpenSpaceModule(Name)
    , _showStatistics(ShowStatisticsInfo, false)
    , _statisticsScale(StatisticsScaleInfo, 1.f, 0.f, 1.f)
    , _showFrameInformation(ShowFrameNumberInfo, false)
{
    _showStatistics.onChange([this]() {
        global::windowDelegate->showStatistics(_showStatistics);
        // We need to reset the scale as it is not updated when the statistics window is
        // not currently shown
        global::windowDelegate->setStatisticsGraphScale(_statisticsScale);
    });
    addProperty(_showStatistics);

    _statisticsScale.onChange([this]() {
        global::windowDelegate->setStatisticsGraphScale(_statisticsScale);
    });
    addProperty(_statisticsScale);

    addProperty(_showFrameInformation);

    global::callback::render->push_back([this]() {
        if (_showFrameInformation) {
            ZoneScopedN("Show Frame Information");
            WindowDelegate* del = global::windowDelegate;

            glm::vec2 penPosition = glm::vec2(
                global::renderEngine->fontResolution().x / 2 - 50,
                global::renderEngine->fontResolution().y / 3
            );

            std::string fn = std::to_string(global::renderEngine->frameNumber());
            const WindowDelegate::Frustum frustum = del->frustumMode();
            std::string fr = [](WindowDelegate::Frustum f) -> std::string {
                switch (f) {
                    case WindowDelegate::Frustum::Mono:     return "";
                    case WindowDelegate::Frustum::LeftEye:  return "(left)";
                    case WindowDelegate::Frustum::RightEye: return "(right)";
                    default:                          throw ghoul::MissingCaseException();

                }
            }(frustum);

            std::string sgFn = std::to_string(del->swapGroupFrameNumber());
            std::string dt = std::to_string(del->deltaTime());
            std::string avgDt = std::to_string(del->averageDeltaTime());

            const std::string res = std::format(
                "Frame: {} {}\nSwap group frame: {}\nDt: {}\nAvg Dt: {}",
                fn, fr, sgFn, dt, avgDt
            );
            RenderFont(*_fontFrameInfo, penPosition, res);
        }
    });
}

void DebuggingModule::internalInitialize(const ghoul::Dictionary&) {
    ghoul::TemplateFactory<Renderable>* fRenderable =
        FactoryManager::ref().factory<Renderable>();
    ghoul_assert(fRenderable, "No renderable factory existed");

    fRenderable->registerClass<RenderableDebugPlane>("RenderableDebugPlane");
}

void DebuggingModule::internalInitializeGL() {
    const Configuration::FontSizes fontSize = global::configuration->fontSize;
    _fontFrameInfo = global::fontManager->font(KeyFontMono, fontSize.frameInfo);
}

std::vector<documentation::Documentation> DebuggingModule::documentations() const {
    return {
        RenderableDebugPlane::Documentation()
    };
}

scripting::LuaLibrary DebuggingModule::luaLibrary() const {
    return {
        .name = "debugging",
        .functions = {
            codegen::lua::RenderCameraPath,
            codegen::lua::RemoveRenderedCameraPath,
            codegen::lua::RenderPathControlPoints,
            codegen::lua::RemovePathControlPoints
        },
        .scripts = {
            absPath("${MODULE_DEBUGGING}/scripts/axes.lua")
        }
    };
}

} // namespace openspace
