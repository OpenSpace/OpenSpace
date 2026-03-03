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

#include <modules/debugging/debuggingmodule.h>

#include <modules/debugging/rendering/screenspacedebugplane.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/configuration.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/util/factorymanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/format.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/misc/templatefactory.h>

#include "debuggingmodule_lua.inl"

namespace {
    using namespace openspace;

    constexpr std::string_view KeyFontMono = "Mono";

    constexpr Property::PropertyInfo ShowStatisticsInfo = {
        "ShowStatistics",
        "Show statistics",
        "Show updating, rendering, and network statistics on all rendering nodes.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo StatisticsScaleInfo = {
        "StatisticsScale",
        "Statistics scale",
        "This value is scaling the statistics window by the provided amount. For flat "
        "projections this is rarely necessary, but it is important when using a setup "
        "where the corners of the image are masked out.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo StatisticsOffsetInfo = {
        "StatisticsOffset",
        "Statistics offset",
        "This value is offsetting the center of the statistics window by the provided "
        "amount. For flat projections this is rarely necessary, but it is important when "
        "using a setup the center of the image is distorted in some form.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo ShowFrameNumberInfo = {
        "ShowFrameInformation",
        "Show frame information",
        "If this value is enabled, the current frame number and frame times are rendered "
        "into the window.",
        Property::Visibility::AdvancedUser
    };
} // namespace

namespace openspace {

DebuggingModule::DebuggingModule()
    : OpenSpaceModule(Name)
    , _showStatistics(ShowStatisticsInfo, false)
    , _statisticsScale(StatisticsScaleInfo, 1.f, 0.f, 1.f)
    , _statisticsOffset(
        StatisticsOffsetInfo,
        glm::vec2(0.f),
        glm::vec2(-2.f),
        glm::vec2(2.f)
    )
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

    _statisticsOffset.onChange([this]() {
        global::windowDelegate->setStatisticsGraphOffset(_statisticsOffset);
    });
    addProperty(_statisticsOffset);

    addProperty(_showFrameInformation);

    global::callback::render->push_back(
        [this](const glm::mat4&, const glm::mat4&, const glm::mat4&)
        {
            if (_showFrameInformation) {
                ZoneScopedN("Show Frame Information");
                WindowDelegate* del = global::windowDelegate;

                glm::vec2 penPosition = glm::vec2(
                    global::renderEngine->fontResolution().x / 2 - 70,
                    global::renderEngine->fontResolution().y / 3
                );

                std::string fn = std::to_string(global::renderEngine->frameNumber());
                const WindowDelegate::Frustum frustum = del->frustumMode();
                std::string fr = [](WindowDelegate::Frustum f) -> std::string {
                    switch (f) {
                        case WindowDelegate::Frustum::Mono:     return "";
                        case WindowDelegate::Frustum::LeftEye:  return "(left)";
                        case WindowDelegate::Frustum::RightEye: return "(right)";
                        default:                      throw ghoul::MissingCaseException();

                    }
                }(frustum);

                std::string node = std::to_string(del->currentNode());
                std::string sgFn = std::to_string(del->swapGroupFrameNumber());
                std::string dt = std::to_string(del->deltaTime());
                std::string avgDt = std::to_string(del->averageDeltaTime());

                const std::string res = std::format(
                    "Node: {}\n\nFrame: {} {}\nSwap group frame: {}\nDt: {}\nAvg Dt: {}",
                    node, fn, fr, sgFn, dt, avgDt
                );
                RenderFont(*_fontFrameInfo, penPosition, res);
            }
        }
    );
}

void DebuggingModule::internalInitialize(const ghoul::Dictionary&) {
    ghoul::TemplateFactory<ScreenSpaceRenderable>* fSsRenderable =
        FactoryManager::ref().factory<ScreenSpaceRenderable>();
    ghoul_assert(fSsRenderable, "ScreenSpaceRenderable factory was not created");

    fSsRenderable->registerClass<ScreenSpaceDebugPlane>("ScreenSpaceDebugPlane");
}

void DebuggingModule::internalInitializeGL() {
    const Configuration::FontSizes fontSize = global::configuration->fontSize;
    _fontFrameInfo = global::fontManager->font(KeyFontMono, fontSize.frameInfo);
}

std::vector<Documentation> DebuggingModule::documentations() const {
    return {
        ScreenSpaceDebugPlane::Documentation()
    };
}

LuaLibrary DebuggingModule::luaLibrary() const {
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
