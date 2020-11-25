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

#include <openspace/engine/globals.h>

#include <openspace/engine/downloadmanager.h>
#include <openspace/engine/configuration.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/syncengine.h>
#include <openspace/engine/virtualpropertymanager.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/interaction/interactionmonitor.h>
#include <openspace/interaction/keybindingmanager.h>
#include <openspace/interaction/joystickinputstate.h>
#include <openspace/interaction/websocketinputstate.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/interaction/sessionrecording.h>
#include <openspace/interaction/shortcutmanager.h>
#include <openspace/mission/missionmanager.h>
#include <openspace/network/parallelpeer.h>
#include <openspace/properties/propertyowner.h>
#include <openspace/rendering/dashboard.h>
#include <openspace/rendering/deferredcastermanager.h>
#include <openspace/rendering/luaconsole.h>
#include <openspace/rendering/raycastermanager.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/screenspacerenderable.h>
#include <openspace/scene/profile.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/scripting/scriptscheduler.h>
#include <openspace/util/memorymanager.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/versionchecker.h>
#include <ghoul/misc/assert.h>
#include <ghoul/glm.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/misc/sharedmemory.h>
#include <ghoul/opengl/texture.h>
#include <array>

namespace openspace {
namespace {
    constexpr const int TotalSize =
        sizeof(ghoul::fontrendering::FontManager) +
        sizeof(Dashboard) +
        sizeof(DeferredcasterManager) +
        sizeof(DownloadManager) +
        sizeof(LuaConsole) +
        sizeof(MemoryManager) +
        sizeof(MissionManager) +
        sizeof(ModuleEngine) +
        sizeof(OpenSpaceEngine) +
        sizeof(ParallelPeer) +
        sizeof(RaycasterManager) +
        sizeof(RenderEngine) +
        sizeof(std::vector<std::unique_ptr<ScreenSpaceRenderable>>) +
        sizeof(SyncEngine) +
        sizeof(TimeManager) +
        sizeof(VersionChecker) +
        sizeof(VirtualPropertyManager) +
        sizeof(WindowDelegate) +
        sizeof(configuration::Configuration) +
        sizeof(interaction::InteractionMonitor) +
        sizeof(interaction::WebsocketInputStates) +
        sizeof(interaction::KeybindingManager) +
        sizeof(interaction::NavigationHandler) +
        sizeof(interaction::SessionRecording) +
        sizeof(interaction::ShortcutManager) +
        sizeof(properties::PropertyOwner) +
        sizeof(properties::PropertyOwner) +
        sizeof(scripting::ScriptEngine) +
        sizeof(scripting::ScriptScheduler) +
        sizeof(Profile);

    std::array<std::byte, TotalSize> DataStorage;
} // namespace
} // namespace openspace

namespace openspace::global {

void create() {
    ZoneScoped

    callback::create();

    std::byte* currentPos = DataStorage.data();

    fontManager = new (currentPos) ghoul::fontrendering::FontManager({ 1536, 1536, 1 });
    ghoul_assert(fontManager, "No fontManager");
    currentPos += sizeof(ghoul::fontrendering::FontManager);

    dashboard = new (currentPos) Dashboard;
    ghoul_assert(dashboard, "No dashboard");
    currentPos += sizeof(Dashboard);

    deferredcasterManager = new (currentPos) DeferredcasterManager;
    ghoul_assert(deferredcasterManager, "No deferredcasterManager");
    currentPos += sizeof(DeferredcasterManager);

    downloadManager = new (currentPos) DownloadManager;
    ghoul_assert(downloadManager, "No downloadManager");
    currentPos += sizeof(DownloadManager);

    luaConsole = new (currentPos) LuaConsole;
    ghoul_assert(luaConsole, "No luaConsole");
    currentPos += sizeof(LuaConsole);

    memoryManager = new (currentPos) MemoryManager;
    ghoul_assert(memoryManager, "No memoryManager");
    currentPos += sizeof(MemoryManager);

    missionManager = new (currentPos) MissionManager;
    ghoul_assert(missionManager, "No missionManager");
    currentPos += sizeof(MissionManager);

    moduleEngine = new (currentPos) ModuleEngine;
    ghoul_assert(moduleEngine, "No moduleEngine");
    currentPos += sizeof(ModuleEngine);

    openSpaceEngine = new (currentPos) OpenSpaceEngine;
    ghoul_assert(openSpaceEngine, "No openSpaceEngine");
    currentPos += sizeof(OpenSpaceEngine);

    parallelPeer = new (currentPos) ParallelPeer;
    ghoul_assert(parallelPeer, "No parallelPeer");
    currentPos += sizeof(ParallelPeer);

    raycasterManager = new (currentPos) RaycasterManager;
    ghoul_assert(raycasterManager, "No raycasterManager");
    currentPos += sizeof(RaycasterManager);

    renderEngine = new (currentPos) RenderEngine;
    ghoul_assert(renderEngine, "No renderEngine");
    currentPos += sizeof(RenderEngine);

    screenSpaceRenderables =
        new (currentPos) std::vector<std::unique_ptr<ScreenSpaceRenderable>>;
    ghoul_assert(screenSpaceRenderables, "No screenSpaceRenderables");
    currentPos += sizeof(std::vector<std::unique_ptr<ScreenSpaceRenderable>>);

    syncEngine = new (currentPos) SyncEngine(4096);
    ghoul_assert(syncEngine, "No syncEngine");
    currentPos += sizeof(SyncEngine);

    timeManager = new (currentPos) TimeManager;
    ghoul_assert(timeManager, "No timeManager");
    currentPos += sizeof(TimeManager);

    versionChecker = new (currentPos) VersionChecker;
    ghoul_assert(versionChecker, "No versionChecker");
    currentPos += sizeof(VersionChecker);

    virtualPropertyManager = new (currentPos) VirtualPropertyManager;
    ghoul_assert(virtualPropertyManager, "No virtualPropertyManager");
    currentPos += sizeof(VirtualPropertyManager);

    windowDelegate = new (currentPos) WindowDelegate;
    ghoul_assert(windowDelegate, "No windowDelegate");
    currentPos += sizeof(WindowDelegate);

    configuration = new (currentPos) configuration::Configuration;
    ghoul_assert(configuration, "No configuration");
    currentPos += sizeof(configuration::Configuration);

    interactionMonitor = new (currentPos) interaction::InteractionMonitor;
    ghoul_assert(interactionMonitor, "No interactionMonitor");
    currentPos += sizeof(interaction::InteractionMonitor);

    joystickInputStates = new (currentPos) interaction::JoystickInputStates;
    ghoul_assert(joystickInputStates, "No joystickInputStates");
    currentPos += sizeof(interaction::JoystickInputStates);

    websocketInputStates = new (currentPos) interaction::WebsocketInputStates;
    ghoul_assert(websocketInputStates, "No websocketInputStates");
    currentPos += sizeof(interaction::WebsocketInputStates);

    keybindingManager = new (currentPos) interaction::KeybindingManager;
    ghoul_assert(keybindingManager, "No keybindingManager");
    currentPos += sizeof(interaction::KeybindingManager);

    navigationHandler = new (currentPos) interaction::NavigationHandler;
    ghoul_assert(navigationHandler, "No navigationHandler");
    currentPos += sizeof(interaction::NavigationHandler);

    sessionRecording = new (currentPos) interaction::SessionRecording(true);
    ghoul_assert(sessionRecording, "No sessionRecording");
    currentPos += sizeof(interaction::SessionRecording);

    shortcutManager = new (currentPos) interaction::ShortcutManager;
    ghoul_assert(shortcutManager, "No shortcutManager");
    currentPos += sizeof(interaction::ShortcutManager);

    rootPropertyOwner = new (currentPos) properties::PropertyOwner({ "" });
    ghoul_assert(rootPropertyOwner, "No rootPropertyOwner");
    currentPos += sizeof(properties::PropertyOwner);

    screenSpaceRootPropertyOwner =
        new (currentPos) properties::PropertyOwner({ "ScreenSpace" });
    ghoul_assert(screenSpaceRootPropertyOwner, "No screenSpaceRootPropertyOwner");
    currentPos += sizeof(properties::PropertyOwner);

    scriptEngine = new (currentPos) scripting::ScriptEngine;
    ghoul_assert(scriptEngine, "No scriptEngine");
    currentPos += sizeof(scripting::ScriptEngine);

    scriptScheduler = new (currentPos) scripting::ScriptScheduler;
    ghoul_assert(scriptScheduler, "No scriptScheduler");
    currentPos += sizeof(scripting::ScriptScheduler);

    profile = new (currentPos) Profile;
    ghoul_assert(profile, "No profile");
    currentPos += sizeof(Profile);
}

void initialize() {
    ZoneScoped

    rootPropertyOwner->addPropertySubOwner(global::moduleEngine);

    navigationHandler->setPropertyOwner(global::rootPropertyOwner);
    // New property subowners also have to be added to the ImGuiModule callback!
    rootPropertyOwner->addPropertySubOwner(global::navigationHandler);
    rootPropertyOwner->addPropertySubOwner(global::interactionMonitor);
    rootPropertyOwner->addPropertySubOwner(global::sessionRecording);
    rootPropertyOwner->addPropertySubOwner(global::timeManager);

    rootPropertyOwner->addPropertySubOwner(global::renderEngine);
    rootPropertyOwner->addPropertySubOwner(global::screenSpaceRootPropertyOwner);

    rootPropertyOwner->addPropertySubOwner(global::parallelPeer);
    rootPropertyOwner->addPropertySubOwner(global::luaConsole);
    rootPropertyOwner->addPropertySubOwner(global::dashboard);

    syncEngine->addSyncable(global::scriptEngine);
}

void initializeGL() {
    ZoneScoped

}

void destroy() {
    LDEBUGC("Globals", "Destroying 'Profile'");
    profile->~Profile();

    LDEBUGC("Globals", "Destroying 'ScriptScheduler'");
    scriptScheduler->~ScriptScheduler();

    LDEBUGC("Globals", "Destroying 'ScriptEngine'");
    scriptEngine->~ScriptEngine();

    LDEBUGC("Globals", "Destroying 'ScreenSpace Root Owner'");
    screenSpaceRootPropertyOwner->~PropertyOwner();

    LDEBUGC("Globals", "Destroying 'Root Owner'");
    rootPropertyOwner->~PropertyOwner();

    LDEBUGC("Globals", "Destroying 'ShortcutManager'");
    shortcutManager->~ShortcutManager();

    LDEBUGC("Globals", "Destroying 'SessionRecording'");
    sessionRecording->~SessionRecording();

    LDEBUGC("Globals", "Destroying 'NavigationHandler'");
    navigationHandler->~NavigationHandler();

    LDEBUGC("Globals", "Destroying 'KeybindingManager'");
    keybindingManager->~KeybindingManager();

    LDEBUGC("Globals", "Destroying 'WebsocketInputStates'");
    websocketInputStates->~WebsocketInputStates();

    LDEBUGC("Globals", "Destroying 'JoystickInputStates'");
    joystickInputStates->~JoystickInputStates();

    LDEBUGC("Globals", "Destroying 'InteractionMonitor'");
    interactionMonitor->~InteractionMonitor();

    LDEBUGC("Globals", "Destroying 'Configuration'");
    configuration->~Configuration();

    LDEBUGC("Globals", "Destroying 'WindowDelegate'");
    windowDelegate->~WindowDelegate();

    LDEBUGC("Globals", "Destroying 'VirtualPropertyManager'");
    virtualPropertyManager->~VirtualPropertyManager();

    LDEBUGC("Globals", "Destroying 'VersionChecker'");
    versionChecker->~VersionChecker();

    LDEBUGC("Globals", "Destroying 'TimeManager'");
    timeManager->~TimeManager();

    LDEBUGC("Globals", "Destroying 'SyncEngine'");
    syncEngine->~SyncEngine();

    LDEBUGC("Globals", "Destroying 'ScreenSpaceRenderables'");
    screenSpaceRenderables->~vector<std::unique_ptr<ScreenSpaceRenderable>>();

    LDEBUGC("Globals", "Destroying 'RenderEngine'");
    renderEngine->~RenderEngine();

    LDEBUGC("Globals", "Destroying 'RaycasterManager'");
    raycasterManager->~RaycasterManager();

    LDEBUGC("Globals", "Destroying 'ParallelPeer'");
    parallelPeer->~ParallelPeer();

    LDEBUGC("Globals", "Destroying 'OpenSpaceEngine'");
    openSpaceEngine->~OpenSpaceEngine();

    LDEBUGC("Globals", "Destroying 'ModuleEngine'");
    moduleEngine->~ModuleEngine();

    LDEBUGC("Globals", "Destroying 'MissionManager'");
    missionManager->~MissionManager();

    LDEBUGC("Globals", "Destroying 'MemoryManager'");
    memoryManager->~MemoryManager();

    LDEBUGC("Globals", "Destroying 'LuaConsole'");
    luaConsole->~LuaConsole();

    LDEBUGC("Globals", "Destroying 'DownloadManager'");
    downloadManager->~DownloadManager();

    LDEBUGC("Globals", "Destroying 'DeferredcasterManager'");
    deferredcasterManager->~DeferredcasterManager();

    LDEBUGC("Globals", "Destroying 'Dashboard'");
    dashboard->~Dashboard();

    LDEBUGC("Globals", "Destroying 'FontManager'");
    fontManager->~FontManager();

    std::fill(DataStorage.begin(), DataStorage.end(), std::byte(0));

    callback::destroy();
}

void deinitialize() {
    ZoneScoped

    for (std::unique_ptr<ScreenSpaceRenderable>& ssr : *screenSpaceRenderables) {
        ssr->deinitialize();
    }

    syncEngine->removeSyncables(timeManager->getSyncables());

    moduleEngine->deinitialize();
    luaConsole->deinitialize();
    scriptEngine->deinitialize();
    fontManager->deinitialize();
}

void deinitializeGL() {
    ZoneScoped

    for (std::unique_ptr<ScreenSpaceRenderable>& ssr : *screenSpaceRenderables) {
        ssr->deinitializeGL();
    }

    renderEngine->deinitializeGL();
    moduleEngine->deinitializeGL();
}

} // namespace openspace::global
