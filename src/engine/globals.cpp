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

#include <openspace/engine/globals.h>

#include <openspace/engine/configuration.h>
#include <openspace/engine/downloadmanager.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/syncengine.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/events/eventengine.h>
#include <openspace/interaction/actionmanager.h>
#include <openspace/interaction/interactionmonitor.h>
#include <openspace/interaction/keybindingmanager.h>
#include <openspace/interaction/joystickinputstate.h>
#include <openspace/interaction/websocketinputstate.h>
#include <openspace/interaction/sessionrecording.h>
#include <openspace/mission/missionmanager.h>
#include <openspace/navigation/navigationhandler.h>
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
    // This is kind of weird.  Optimally, we would want to use the std::array also on
    // non-Windows platforms but that causes some issues with nullptrs being thrown
    // around and invalid accesses.  Switching this to a std::vector with dynamic memory
    // allocation works on Linux, but it fails on Windows in some SGCT function and on Mac
    // in some random global randoms
#ifdef WIN32
    constexpr int TotalSize =
        sizeof(MemoryManager) +
        sizeof(EventEngine) +
        sizeof(ghoul::fontrendering::FontManager) +
        sizeof(Dashboard) +
        sizeof(DeferredcasterManager) +
        sizeof(DownloadManager) +
        sizeof(LuaConsole) +
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
        sizeof(WindowDelegate) +
        sizeof(Configuration) +
        sizeof(interaction::ActionManager) +
        sizeof(interaction::InteractionMonitor) +
        sizeof(interaction::JoystickInputStates) +
        sizeof(interaction::WebsocketInputStates) +
        sizeof(interaction::KeybindingManager) +
        sizeof(interaction::NavigationHandler) +
        sizeof(interaction::SessionRecording) +
        sizeof(properties::PropertyOwner) +
        sizeof(properties::PropertyOwner) +
        sizeof(properties::PropertyOwner) +
        sizeof(scripting::ScriptEngine) +
        sizeof(scripting::ScriptScheduler) +
        sizeof(Profile);

    std::array<std::byte, TotalSize> DataStorage;
#endif // WIN32
} // namespace
} // namespace openspace

namespace openspace::global {

void create() {
    ZoneScoped;

    callback::create();

#ifdef WIN32
    std::fill(DataStorage.begin(), DataStorage.end(), std::byte(0));
    std::byte* currentPos = DataStorage.data();
#endif // WIN32

#ifdef WIN32
    memoryManager = new (currentPos) MemoryManager;
    ghoul_assert(memoryManager, "No memoryManager");
    currentPos += sizeof(MemoryManager);
#else // ^^^ WIN32 / !WIN32 vvv
    memoryManager = new MemoryManager;
#endif // WIN32

#ifdef WIN32
    eventEngine = new (currentPos) EventEngine;
    ghoul_assert(eventEngine, "No eventEngine");
    currentPos += sizeof(EventEngine);
#else // ^^^ WIN32 / !WIN32 vvv
    eventEngine = new EventEngine;
#endif // WIN32

#ifdef WIN32
    fontManager = new (currentPos) ghoul::fontrendering::FontManager({ 1536, 1536, 1 });
    ghoul_assert(fontManager, "No fontManager");
    currentPos += sizeof(ghoul::fontrendering::FontManager);
#else // ^^^ WIN32 / !WIN32 vvv
    fontManager = new ghoul::fontrendering::FontManager({ 1536, 1536, 1 });
#endif // WIN32

#ifdef WIN32
    dashboard = new (currentPos) Dashboard;
    ghoul_assert(dashboard, "No dashboard");
    currentPos += sizeof(Dashboard);
#else // ^^^ WIN32 / !WIN32 vvv
    dashboard = new Dashboard;
#endif // WIN32

#ifdef WIN32
    deferredcasterManager = new (currentPos) DeferredcasterManager;
    ghoul_assert(deferredcasterManager, "No deferredcasterManager");
    currentPos += sizeof(DeferredcasterManager);
#else // ^^^ WIN32 / !WIN32 vvv
    deferredcasterManager = new DeferredcasterManager;
#endif // WIN32

#ifdef WIN32
    downloadManager = new (currentPos) DownloadManager;
    ghoul_assert(downloadManager, "No downloadManager");
    currentPos += sizeof(DownloadManager);
#else // ^^^ WIN32 / !WIN32 vvv
    downloadManager = new DownloadManager;
#endif // WIN32

#ifdef WIN32
    luaConsole = new (currentPos) LuaConsole;
    ghoul_assert(luaConsole, "No luaConsole");
    currentPos += sizeof(LuaConsole);
#else // ^^^ WIN32 / !WIN32 vvv
    luaConsole = new LuaConsole;
#endif // WIN32

#ifdef WIN32
    missionManager = new (currentPos) MissionManager;
    ghoul_assert(missionManager, "No missionManager");
    currentPos += sizeof(MissionManager);
#else // ^^^ WIN32 / !WIN32 vvv
    missionManager = new MissionManager;
#endif // WIN32

#ifdef WIN32
    moduleEngine = new (currentPos) ModuleEngine;
    ghoul_assert(moduleEngine, "No moduleEngine");
    currentPos += sizeof(ModuleEngine);
#else // ^^^ WIN32 / !WIN32 vvv
    moduleEngine = new ModuleEngine;
#endif // WIN32

#ifdef WIN32
    openSpaceEngine = new (currentPos) OpenSpaceEngine;
    ghoul_assert(openSpaceEngine, "No openSpaceEngine");
    currentPos += sizeof(OpenSpaceEngine);
#else // ^^^ WIN32 / !WIN32 vvv
    openSpaceEngine = new OpenSpaceEngine;
#endif // WIN32

#ifdef WIN32
    parallelPeer = new (currentPos) ParallelPeer;
    ghoul_assert(parallelPeer, "No parallelPeer");
    currentPos += sizeof(ParallelPeer);
#else // ^^^ WIN32 / !WIN32 vvv
    parallelPeer = new ParallelPeer;
#endif // WIN32

#ifdef WIN32
    raycasterManager = new (currentPos) RaycasterManager;
    ghoul_assert(raycasterManager, "No raycasterManager");
    currentPos += sizeof(RaycasterManager);
#else // ^^^ WIN32 / !WIN32 vvv
    raycasterManager = new RaycasterManager;
#endif // WIN32

#ifdef WIN32
    renderEngine = new (currentPos) RenderEngine;
    ghoul_assert(renderEngine, "No renderEngine");
    currentPos += sizeof(RenderEngine);
#else // ^^^ WIN32 / !WIN32 vvv
    renderEngine = new RenderEngine;
#endif // WIN32

#ifdef WIN32
    screenSpaceRenderables =
        new (currentPos) std::vector<std::unique_ptr<ScreenSpaceRenderable>>;
    ghoul_assert(screenSpaceRenderables, "No screenSpaceRenderables");
    currentPos += sizeof(std::vector<std::unique_ptr<ScreenSpaceRenderable>>);
#else // ^^^ WIN32 / !WIN32 vvv
    screenSpaceRenderables = new std::vector<std::unique_ptr<ScreenSpaceRenderable>>;
#endif // WIN32

#ifdef WIN32
    syncEngine = new (currentPos) SyncEngine(4096);
    ghoul_assert(syncEngine, "No syncEngine");
    currentPos += sizeof(SyncEngine);
#else // ^^^ WIN32 / !WIN32 vvv
    syncEngine = new SyncEngine(4096);
#endif // WIN32

#ifdef WIN32
    timeManager = new (currentPos) TimeManager;
    ghoul_assert(timeManager, "No timeManager");
    currentPos += sizeof(TimeManager);
#else // ^^^ WIN32 / !WIN32 vvv
    timeManager = new TimeManager;
#endif // WIN32

#ifdef WIN32
    versionChecker = new (currentPos) VersionChecker;
    ghoul_assert(versionChecker, "No versionChecker");
    currentPos += sizeof(VersionChecker);
#else
    versionChecker = new VersionChecker;
#endif // WIN32

#ifdef WIN32
    windowDelegate = new (currentPos) WindowDelegate;
    ghoul_assert(windowDelegate, "No windowDelegate");
    currentPos += sizeof(WindowDelegate);
#else // ^^^ WIN32 / !WIN32 vvv
    windowDelegate = new WindowDelegate;
#endif // WIN32

#ifdef WIN32
    configuration = new (currentPos) Configuration;
    ghoul_assert(configuration, "No configuration");
    currentPos += sizeof(Configuration);
#else // ^^^ WIN32 / !WIN32 vvv
    configuration = new Configuration;
#endif // WIN32

#ifdef WIN32
    actionManager = new (currentPos) interaction::ActionManager;
    ghoul_assert(actionManager, "No action manager");
    currentPos += sizeof(interaction::ActionManager);
#else // ^^^ WIN32 / !WIN32 vvv
    actionManager = new interaction::ActionManager;
#endif // WIN32

#ifdef WIN32
    interactionMonitor = new (currentPos) interaction::InteractionMonitor;
    ghoul_assert(interactionMonitor, "No interactionMonitor");
    currentPos += sizeof(interaction::InteractionMonitor);
#else // ^^^ WIN32 / !WIN32 vvv
    interactionMonitor = new interaction::InteractionMonitor;
#endif // WIN32

#ifdef WIN32
    joystickInputStates = new (currentPos) interaction::JoystickInputStates;
    ghoul_assert(joystickInputStates, "No joystickInputStates");
    currentPos += sizeof(interaction::JoystickInputStates);
#else // ^^^ WIN32 / !WIN32 vvv
    joystickInputStates = new interaction::JoystickInputStates;
#endif // WIN32

#ifdef WIN32
    websocketInputStates = new (currentPos) interaction::WebsocketInputStates;
    ghoul_assert(websocketInputStates, "No websocketInputStates");
    currentPos += sizeof(interaction::WebsocketInputStates);
#else // ^^^ WIN32 / !WIN32 vvv
    websocketInputStates = new interaction::WebsocketInputStates;
#endif // WIN32

#ifdef WIN32
    keybindingManager = new (currentPos) interaction::KeybindingManager;
    ghoul_assert(keybindingManager, "No keybindingManager");
    currentPos += sizeof(interaction::KeybindingManager);
#else // ^^^ WIN32 / !WIN32 vvv
    keybindingManager = new interaction::KeybindingManager;
#endif // WIN32

#ifdef WIN32
    navigationHandler = new (currentPos) interaction::NavigationHandler;
    ghoul_assert(navigationHandler, "No navigationHandler");
    currentPos += sizeof(interaction::NavigationHandler);
#else // ^^^ WIN32 / !WIN32 vvv
    navigationHandler = new interaction::NavigationHandler;
#endif // WIN32

#ifdef WIN32
    sessionRecording = new (currentPos) interaction::SessionRecording(true);
    ghoul_assert(sessionRecording, "No sessionRecording");
    currentPos += sizeof(interaction::SessionRecording);
#else // ^^^ WIN32 / !WIN32 vvv
    sessionRecording = new interaction::SessionRecording(true);
#endif // WIN32

#ifdef WIN32
    rootPropertyOwner = new (currentPos) properties::PropertyOwner({ "" });
    ghoul_assert(rootPropertyOwner, "No rootPropertyOwner");
    currentPos += sizeof(properties::PropertyOwner);
#else // ^^^ WIN32 / !WIN32 vvv
    rootPropertyOwner = new properties::PropertyOwner({ "" });
#endif // WIN32

#ifdef WIN32
    screenSpaceRootPropertyOwner =
        new (currentPos) properties::PropertyOwner({ "ScreenSpace" });
    ghoul_assert(screenSpaceRootPropertyOwner, "No screenSpaceRootPropertyOwner");
    currentPos += sizeof(properties::PropertyOwner);
#else // ^^^ WIN32 / !WIN32 vvv
    screenSpaceRootPropertyOwner = new properties::PropertyOwner({ "ScreenSpace" });
#endif // WIN32

#ifdef WIN32
    userPropertyOwner = new (currentPos) properties::PropertyOwner({ "UserProperties" });
    ghoul_assert(userPropertyOwner, "No userPropertyOwner");
    currentPos += sizeof(properties::PropertyOwner);
#else // ^^^ WIN32 / !WIN32 vvv
    userPropertyOwner = new properties::PropertyOwner({ "UserProperties" });
#endif // WIN32

#ifdef WIN32
    scriptEngine = new (currentPos) scripting::ScriptEngine;
    ghoul_assert(scriptEngine, "No scriptEngine");
    currentPos += sizeof(scripting::ScriptEngine);
#else // ^^^ WIN32 / !WIN32 vvv
    scriptEngine = new scripting::ScriptEngine;
#endif // WIN32

#ifdef WIN32
    scriptScheduler = new (currentPos) scripting::ScriptScheduler;
    ghoul_assert(scriptScheduler, "No scriptScheduler");
    currentPos += sizeof(scripting::ScriptScheduler);
#else // ^^^ WIN32 / !WIN32 vvv
    scriptScheduler = new scripting::ScriptScheduler;
#endif // WIN32

#ifdef WIN32
    profile = new (currentPos) Profile;
    ghoul_assert(profile, "No profile");
    //currentPos += sizeof(Profile);
#else // ^^^ WIN32 / !WIN32 vvv
    profile = new Profile;
#endif // WIN32
}

void initialize() {
    ZoneScoped;

    rootPropertyOwner->addPropertySubOwner(global::moduleEngine);

    // New property subowners also have to be added to the ImGuiModule callback!
    rootPropertyOwner->addPropertySubOwner(global::navigationHandler);
    rootPropertyOwner->addPropertySubOwner(global::interactionMonitor);
    rootPropertyOwner->addPropertySubOwner(global::sessionRecording);
    rootPropertyOwner->addPropertySubOwner(global::timeManager);
    rootPropertyOwner->addPropertySubOwner(global::scriptScheduler);

    rootPropertyOwner->addPropertySubOwner(global::renderEngine);
    rootPropertyOwner->addPropertySubOwner(global::screenSpaceRootPropertyOwner);

    rootPropertyOwner->addPropertySubOwner(global::parallelPeer);
    rootPropertyOwner->addPropertySubOwner(global::luaConsole);
    rootPropertyOwner->addPropertySubOwner(global::dashboard);

    rootPropertyOwner->addPropertySubOwner(global::userPropertyOwner);
    rootPropertyOwner->addPropertySubOwner(global::openSpaceEngine);

    syncEngine->addSyncable(global::scriptEngine);
}

void initializeGL() {
    ZoneScoped;
}

void destroy() {
    LDEBUGC("Globals", "Destroying 'Profile'");
#ifdef WIN32
    profile->~Profile();
#else // ^^^ WIN32 / !WIN32 vvv
    delete profile;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'ScriptScheduler'");
#ifdef WIN32
    scriptScheduler->~ScriptScheduler();
#else // ^^^ WIN32 / !WIN32 vvv
    delete scriptScheduler;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'ScriptEngine'");
#ifdef WIN32
    scriptEngine->~ScriptEngine();
#else // ^^^ WIN32 / !WIN32 vvv
    delete scriptEngine;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'ScreenSpace Root Owner'");
#ifdef WIN32
    screenSpaceRootPropertyOwner->~PropertyOwner();
#else // ^^^ WIN32 / !WIN32 vvv
    delete screenSpaceRootPropertyOwner;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'Root Owner'");
#ifdef WIN32
    rootPropertyOwner->~PropertyOwner();
#else // ^^^ WIN32 / !WIN32 vvv
    delete rootPropertyOwner;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'SessionRecording'");
#ifdef WIN32
    sessionRecording->~SessionRecording();
#else // ^^^ WIN32 / !WIN32 vvv
    delete sessionRecording;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'NavigationHandler'");
#ifdef WIN32
    navigationHandler->~NavigationHandler();
#else // ^^^ WIN32 / !WIN32 vvv
    delete navigationHandler;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'KeybindingManager'");
#ifdef WIN32
    keybindingManager->~KeybindingManager();
#else // ^^^ WIN32 / !WIN32 vvv
    delete keybindingManager;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'WebsocketInputStates'");
#ifdef WIN32
    websocketInputStates->~WebsocketInputStates();
#else // ^^^ WIN32 / !WIN32 vvv
    delete websocketInputStates;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'JoystickInputStates'");
#ifdef WIN32
    joystickInputStates->~JoystickInputStates();
#else // ^^^ WIN32 / !WIN32 vvv
    delete joystickInputStates;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'InteractionMonitor'");
#ifdef WIN32
    interactionMonitor->~InteractionMonitor();
#else // ^^^ WIN32 / !WIN32 vvv
    delete interactionMonitor;
#endif // WIN32

    LDEBUGC("Globals", "Destorying 'ActionManager'");
#ifdef WIN32
    actionManager->~ActionManager();
#else // ^^^ WIN32 / !WIN32 vvv
    delete actionManager;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'Configuration'");
#ifdef WIN32
    configuration->~Configuration();
#else // ^^^ WIN32 / !WIN32 vvv
    delete configuration;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'WindowDelegate'");
#ifdef WIN32
    windowDelegate->~WindowDelegate();
#else // ^^^ WIN32 / !WIN32 vvv
    delete windowDelegate;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'VersionChecker'");
#ifdef WIN32
    versionChecker->~VersionChecker();
#else // ^^^ WIN32 / !WIN32 vvv
    delete versionChecker;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'TimeManager'");
#ifdef WIN32
    timeManager->~TimeManager();
#else // ^^^ WIN32 / !WIN32 vvv
    delete timeManager;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'SyncEngine'");
#ifdef WIN32
    syncEngine->~SyncEngine();
#else // ^^^ WIN32 / !WIN32 vvv
    delete syncEngine;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'ScreenSpaceRenderables'");
#ifdef WIN32
    screenSpaceRenderables->~vector();
#else // ^^^ WIN32 / !WIN32 vvv
    delete screenSpaceRenderables;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'RenderEngine'");
#ifdef WIN32
    renderEngine->~RenderEngine();
#else // ^^^ WIN32 / !WIN32 vvv
    delete renderEngine;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'RaycasterManager'");
#ifdef WIN32
    raycasterManager->~RaycasterManager();
#else // ^^^ WIN32 / !WIN32 vvv
    delete raycasterManager;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'ParallelPeer'");
#ifdef WIN32
    parallelPeer->~ParallelPeer();
#else // ^^^ WIN32 / !WIN32 vvv
    delete parallelPeer;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'OpenSpaceEngine'");
#ifdef WIN32
    openSpaceEngine->~OpenSpaceEngine();
#else // ^^^ WIN32 / !WIN32 vvv
    delete openSpaceEngine;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'ModuleEngine'");
#ifdef WIN32
    moduleEngine->~ModuleEngine();
#else // ^^^ WIN32 / !WIN32 vvv
    delete moduleEngine;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'MissionManager'");
#ifdef WIN32
    missionManager->~MissionManager();
#else // ^^^ WIN32 / !WIN32 vvv
    delete missionManager;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'LuaConsole'");
#ifdef WIN32
    luaConsole->~LuaConsole();
#else // ^^^ WIN32 / !WIN32 vvv
    delete luaConsole;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'DownloadManager'");
#ifdef WIN32
    downloadManager->~DownloadManager();
#else // ^^^ WIN32 / !WIN32 vvv
    delete downloadManager;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'DeferredcasterManager'");
#ifdef WIN32
    deferredcasterManager->~DeferredcasterManager();
#else // ^^^ WIN32 / !WIN32 vvv
    delete deferredcasterManager;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'Dashboard'");
#ifdef WIN32
    dashboard->~Dashboard();
#else // ^^^ WIN32 / !WIN32 vvv
    delete dashboard;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'FontManager'");
#ifdef WIN32
    fontManager->~FontManager();
#else // ^^^ WIN32 / !WIN32 vvv
    delete fontManager;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'EventEngine'");
#ifdef WIN32
    eventEngine->~EventEngine();
#else // ^^^ WIN32 / !WIN32 vvv
    delete eventEngine;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'MemoryManager'");
#ifdef WIN32
    memoryManager->~MemoryManager();
#else // ^^^ WIN32 / !WIN32 vvv
    delete memoryManager;
#endif // WIN32

    callback::destroy();
}

void deinitialize() {
    ZoneScoped;

    for (std::unique_ptr<ScreenSpaceRenderable>& ssr : *screenSpaceRenderables) {
        ssr->deinitialize();
    }

    syncEngine->removeSyncables(timeManager->syncables());

    moduleEngine->deinitialize();
    luaConsole->deinitialize();
    scriptEngine->deinitialize();
    fontManager->deinitialize();
}

void deinitializeGL() {
    ZoneScoped;

    for (std::unique_ptr<ScreenSpaceRenderable>& ssr : *screenSpaceRenderables) {
        ssr->deinitializeGL();
    }

    renderEngine->deinitializeGL();
    moduleEngine->deinitializeGL();
}

} // namespace openspace::global
