/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#ifndef __OPENSPACE_CORE___GLOBALS___H__
#define __OPENSPACE_CORE___GLOBALS___H__

#include <functional>
#include <memory>
#include <vector>

namespace ghoul::fontrendering { class FontManager; }

namespace openspace {

class Dashboard;
class DeferredcasterManager;
class DownloadManager;
class LuaConsole;
class MissionManager;
class ModuleEngine;
class OpenSpaceEngine;
class ParallelPeer;
class RaycasterManager;
class RenderEngine;
class ScreenSpaceRenderable;
class SyncEngine;
class TimeManager;
class VersionChecker;
class VirtualPropertyManager;
struct WindowDelegate;
namespace configuration { struct Configuration; }
namespace interaction {
    struct JoystickInputStates;
    struct WebsocketInputStates;
    class InteractionMonitor;
    class KeybindingManager;
    class NavigationHandler;
    class SessionRecording;
    class ShortcutManager;
} // namespace interaction
namespace performance { class PerformanceManager; }
namespace properties { class PropertyOwner; }
namespace scripting {
    class ScriptEngine;
    class ScriptScheduler;
} // namespace scripting

namespace global {

namespace detail {

ghoul::fontrendering::FontManager& gFontManager();
Dashboard& gDashboard();
DeferredcasterManager& gDeferredcasterManager();
DownloadManager& gDownloadManager();
LuaConsole& gLuaConsole();
MissionManager& gMissionManager();
ModuleEngine& gModuleEngine();
OpenSpaceEngine& gOpenSpaceEngine();
ParallelPeer& gParallelPeer();
RaycasterManager& gRaycasterManager();
RenderEngine& gRenderEngine();
std::vector<std::unique_ptr<ScreenSpaceRenderable>>& gScreenspaceRenderables();
SyncEngine& gSyncEngine();
TimeManager& gTimeManager();
VersionChecker& gVersionChecker();
VirtualPropertyManager& gVirtualPropertyManager();
WindowDelegate& gWindowDelegate();
configuration::Configuration& gConfiguration();
interaction::InteractionMonitor& gInteractionMonitor();
interaction::JoystickInputStates& gJoystickInputStates();
interaction::WebsocketInputStates& gWebsocketInputStates();
interaction::KeybindingManager& gKeybindingManager();
interaction::NavigationHandler& gNavigationHandler();
interaction::SessionRecording& gSessionRecording();
interaction::ShortcutManager& gShortcutManager();
performance::PerformanceManager& gPerformanceManager();
properties::PropertyOwner& gRootPropertyOwner();
properties::PropertyOwner& gScreenSpaceRootPropertyOwner();
scripting::ScriptEngine& gScriptEngine();
scripting::ScriptScheduler& gScriptScheduler();

} // namespace detail

static ghoul::fontrendering::FontManager& fontManager = detail::gFontManager();
static Dashboard& dashboard = detail::gDashboard();
static DeferredcasterManager& deferredcasterManager = detail::gDeferredcasterManager();
static DownloadManager& downloadManager = detail::gDownloadManager();
static LuaConsole& luaConsole = detail::gLuaConsole();
static MissionManager& missionManager = detail::gMissionManager();
static ModuleEngine& moduleEngine = detail::gModuleEngine();
static OpenSpaceEngine& openSpaceEngine = detail::gOpenSpaceEngine();
static ParallelPeer& parallelPeer = detail::gParallelPeer();
static RaycasterManager& raycasterManager = detail::gRaycasterManager();
static RenderEngine& renderEngine = detail::gRenderEngine();
static std::vector<std::unique_ptr<ScreenSpaceRenderable>>& screenSpaceRenderables =
    detail::gScreenspaceRenderables();
static SyncEngine& syncEngine = detail::gSyncEngine();
static TimeManager& timeManager = detail::gTimeManager();
static VersionChecker& versionChecker = detail::gVersionChecker();
static VirtualPropertyManager& virtualPropertyManager = detail::gVirtualPropertyManager();
static WindowDelegate& windowDelegate = detail::gWindowDelegate();
static configuration::Configuration& configuration = detail::gConfiguration();
static interaction::InteractionMonitor& interactionMonitor =
    detail::gInteractionMonitor();
static interaction::JoystickInputStates& joystickInputStates =
    detail::gJoystickInputStates();
static interaction::WebsocketInputStates& websocketInputStates =
    detail::gWebsocketInputStates();
static interaction::KeybindingManager& keybindingManager = detail::gKeybindingManager();
static interaction::NavigationHandler& navigationHandler = detail::gNavigationHandler();
static interaction::SessionRecording& sessionRecording = detail::gSessionRecording();
static interaction::ShortcutManager& shortcutManager = detail::gShortcutManager();
static performance::PerformanceManager& performanceManager =
    detail::gPerformanceManager();
static properties::PropertyOwner& rootPropertyOwner = detail::gRootPropertyOwner();
static properties::PropertyOwner& screenSpaceRootPropertyOwner =
    detail::gScreenSpaceRootPropertyOwner();
static scripting::ScriptEngine& scriptEngine = detail::gScriptEngine();
static scripting::ScriptScheduler& scriptScheduler = detail::gScriptScheduler();

void initialize();
void initializeGL();
void deinitialize();
void deinitializeGL();

} // namespace global

} // namespace openspace

#endif // __OPENSPACE_CORE___GLOBALS___H__
