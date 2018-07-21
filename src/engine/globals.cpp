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

#include <openspace/engine/globals.h>

#include <openspace/engine/downloadmanager.h>
#include <openspace/engine/configuration.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/interaction/keybindingmanager.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/network/networkengine.h>
#include <openspace/network/parallelpeer.h>
#include <openspace/performance/performancemanager.h>
#include <openspace/rendering/deferredcastermanager.h>
#include <openspace/rendering/raycastermanager.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/scripting/scriptscheduler.h>
#include <openspace/util/timemanager.h>
#include <ghoul/glm.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/misc/sharedmemory.h>
#include <ghoul/opengl/texture.h>

namespace openspace::detail {

ghoul::fontrendering::FontManager& gFontManager() {
    static ghoul::fontrendering::FontManager g({ 1536, 1536, 1 });
    return g;
}

Configuration& gConfiguration() {
    static Configuration g;
    return g;
}

DeferredcasterManager& gDeferredcasterManager() {
    static DeferredcasterManager g;
    return g;
}

DownloadManager& gDownloadManager() {
    static DownloadManager g;
    return g;
}

ModuleEngine& gModuleEngine() {
    static ModuleEngine g;
    return g;
}

NetworkEngine& gNetworkEngine() {
    static NetworkEngine g;
    return g;
}

ParallelPeer& gParallelPeer() {
    static ParallelPeer g;
    return g;
}

RaycasterManager& gRaycasterManager() {
    static RaycasterManager g;
    return g;
}

RenderEngine& gRenderEngine() {
    static RenderEngine g;
    return g;
}

TimeManager& gTimeManager() {
    static TimeManager g;
    return g;
}

WindowDelegate& gWindowDelegate() {
    static WindowDelegate g;
    return g;
}

interaction::KeybindingManager& gKeybindingManager() {
    static interaction::KeybindingManager g;
    return g;
}

interaction::NavigationHandler& gNavigationHandler() {
    static interaction::NavigationHandler g;
    return g;
}

performance::PerformanceManager& gPerformanceManager() {
    static performance::PerformanceManager g;
    return g;
}

scripting::ScriptEngine& gScriptEngine() {
    static scripting::ScriptEngine g;
    return g;
}

scripting::ScriptScheduler& gScriptScheduler() {
    static scripting::ScriptScheduler g;
    return g;
}

} // namespace openspace::detail
