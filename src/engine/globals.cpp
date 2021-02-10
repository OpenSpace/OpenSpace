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

    std::byte* currentPos = DataStorage.data();

    fontManager = new (currentPos) ghoul::fontrendering::FontManager({ 1536, 1536, 1 });
    assert(fontManager);
    currentPos += sizeof(ghoul::fontrendering::FontManager);

    dashboard = new (currentPos) Dashboard;
    assert(dashboard);
    currentPos += sizeof(Dashboard);

    deferredcasterManager = new (currentPos) DeferredcasterManager;
    assert(deferredcasterManager);
    currentPos += sizeof(DeferredcasterManager);

    downloadManager = new (currentPos) DownloadManager;
    assert(downloadManager);
    currentPos += sizeof(DownloadManager);

    luaConsole = new (currentPos) LuaConsole;
    assert(luaConsole);
    currentPos += sizeof(LuaConsole);

    memoryManager = new (currentPos) MemoryManager;
    assert(memoryManager);
    currentPos += sizeof(MemoryManager);

    missionManager = new (currentPos) MissionManager;
    assert(missionManager);
    currentPos += sizeof(MissionManager);

    moduleEngine = new (currentPos) ModuleEngine;
    assert(moduleEngine);
    currentPos += sizeof(ModuleEngine);

    openSpaceEngine = new (currentPos) OpenSpaceEngine;
    assert(openSpaceEngine);
    currentPos += sizeof(OpenSpaceEngine);

    parallelPeer = new (currentPos) ParallelPeer;
    assert(parallelPeer);
    currentPos += sizeof(ParallelPeer);

    raycasterManager = new (currentPos) RaycasterManager;
    assert(raycasterManager);
    currentPos += sizeof(RaycasterManager);

    renderEngine = new (currentPos) RenderEngine;
    assert(renderEngine);
    currentPos += sizeof(RenderEngine);

    screenSpaceRenderables = new (currentPos) std::vector<std::unique_ptr<ScreenSpaceRenderable>>;
    assert(screenSpaceRenderables);
    currentPos += sizeof(std::vector<std::unique_ptr<ScreenSpaceRenderable>>);

    syncEngine = new (currentPos) SyncEngine(4096);
    assert(syncEngine);
    currentPos += sizeof(SyncEngine);

    timeManager = new (currentPos) TimeManager;
    assert(timeManager);
    currentPos += sizeof(TimeManager);

    versionChecker = new (currentPos) VersionChecker;
    assert(versionChecker);
    currentPos += sizeof(VersionChecker);

    virtualPropertyManager = new (currentPos) VirtualPropertyManager;
    assert(virtualPropertyManager);
    currentPos += sizeof(VirtualPropertyManager);

    windowDelegate = new (currentPos) WindowDelegate;
    assert(windowDelegate);
    currentPos += sizeof(WindowDelegate);

    configuration = new (currentPos) configuration::Configuration;
    assert(configuration);
    currentPos += sizeof(configuration::Configuration);

    interactionMonitor = new (currentPos) interaction::InteractionMonitor;
    assert(interactionMonitor);
    currentPos += sizeof(interaction::InteractionMonitor);

    joystickInputStates = new (currentPos) interaction::JoystickInputStates;
    assert(joystickInputStates);
    currentPos += sizeof(interaction::JoystickInputStates);

    websocketInputStates = new (currentPos) interaction::WebsocketInputStates;
    assert(websocketInputStates);
    currentPos += sizeof(interaction::WebsocketInputStates);

    keybindingManager = new (currentPos) interaction::KeybindingManager;
    assert(keybindingManager);
    currentPos += sizeof(interaction::KeybindingManager);

    navigationHandler = new (currentPos) interaction::NavigationHandler;
    assert(navigationHandler);
    currentPos += sizeof(interaction::NavigationHandler);

    sessionRecording = new (currentPos) interaction::SessionRecording;
    assert(sessionRecording);
    currentPos += sizeof(interaction::SessionRecording);

    shortcutManager = new (currentPos) interaction::ShortcutManager;
    assert(shortcutManager);
    currentPos += sizeof(interaction::ShortcutManager);

    rootPropertyOwner = new (currentPos) properties::PropertyOwner({ "" });
    assert(rootPropertyOwner);
    currentPos += sizeof(properties::PropertyOwner);

    screenSpaceRootPropertyOwner = new (currentPos) properties::PropertyOwner({ "ScreenSpace" });
    assert(screenSpaceRootPropertyOwner);
    currentPos += sizeof(properties::PropertyOwner);

    scriptEngine = new (currentPos) scripting::ScriptEngine;
    assert(scriptEngine);
    currentPos += sizeof(scripting::ScriptEngine);

    scriptScheduler = new (currentPos) scripting::ScriptScheduler;
    assert(scriptScheduler);
    currentPos += sizeof(scripting::ScriptScheduler);

    profile = new (currentPos) Profile;
    assert(profile);
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
