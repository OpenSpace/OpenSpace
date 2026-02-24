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

#ifndef __OPENSPACE_CORE___GLOBALS___H__
#define __OPENSPACE_CORE___GLOBALS___H__

#include <memory>
#include <vector>

namespace ghoul::fontrendering { class FontManager; }

namespace openspace {

class ActionManager;
struct Configuration;
class Dashboard;
class DeferredcasterManager;
class DownloadEventEngine;
class DownloadManager;
class EventEngine;
class InteractionMonitor;
struct JoystickInputStates;
class KeybindingManager;
class KeyframeRecordingHandler;
class LuaConsole;
class MemoryManager;
class MissionManager;
class ModuleEngine;
class NavigationHandler;
class OpenSpaceEngine;
class ParallelPeer;
class Profile;
class PropertyOwner;
class RaycasterManager;
class RenderEngine;
class ScreenSpaceRenderable;
class ScriptEngine;
class ScriptScheduler;
class SessionRecordingHandler;
class SyncEngine;
class TimeManager;
class VersionChecker;
struct WebsocketInputStates;
struct WindowDelegate;

namespace global {

inline ghoul::fontrendering::FontManager* fontManager;
inline Dashboard* dashboard;
inline DeferredcasterManager* deferredcasterManager;
inline DownloadEventEngine* downloadEventEngine;
inline DownloadManager* downloadManager;
inline EventEngine* eventEngine;
inline LuaConsole* luaConsole;
inline MemoryManager* memoryManager;
inline MissionManager* missionManager;
inline ModuleEngine* moduleEngine;
inline OpenSpaceEngine* openSpaceEngine;
inline ParallelPeer* parallelPeer;
inline RaycasterManager* raycasterManager;
inline RenderEngine* renderEngine;
inline std::vector<std::unique_ptr<ScreenSpaceRenderable>>* screenSpaceRenderables;
inline SyncEngine* syncEngine;
inline TimeManager* timeManager;
inline VersionChecker* versionChecker;
inline WindowDelegate* windowDelegate;
inline Configuration* configuration;
inline ActionManager* actionManager;
inline InteractionMonitor* interactionMonitor;
inline JoystickInputStates* joystickInputStates;
inline WebsocketInputStates* websocketInputStates;
inline KeybindingManager* keybindingManager;
inline KeyframeRecordingHandler* keyframeRecording;
inline NavigationHandler* navigationHandler;
inline SessionRecordingHandler* sessionRecordingHandler;
inline PropertyOwner* rootPropertyOwner;
inline PropertyOwner* screenSpaceRootPropertyOwner;
inline PropertyOwner* userPropertyOwner;
inline ScriptEngine* scriptEngine;
inline ScriptScheduler* scriptScheduler;
inline Profile* profile;

void create();
void initialize();
void initializeGL();
void destroy();
void deinitialize();
void deinitializeGL();

} // namespace global

} // namespace openspace

#endif // __OPENSPACE_CORE___GLOBALS___H__
