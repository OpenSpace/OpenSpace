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

#include <openspace/engine/globals.h>

#include <openspace/engine/configuration.h>
#include <openspace/engine/downloadmanager.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/syncengine.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/events/eventengine.h>
#include <openspace/font/fontmanager.h>
#include <openspace/interaction/actionmanager.h>
#include <openspace/interaction/interactionhandler.h>
#include <openspace/interaction/keybindingmanager.h>
#include <openspace/interaction/keyframerecordinghandler.h>
#include <openspace/interaction/sessionrecordinghandler.h>
#include <openspace/logging/logmanager.h>
#include <openspace/misc/assert.h>
#include <openspace/mission/missionmanager.h>
#include <openspace/misc/profiling.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/network/astrocast.h>
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
#include <openspace/topic/server.h>
#include <openspace/util/downloadeventengine.h>
#include <openspace/util/memorymanager.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/versionchecker.h>
#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <new>
#include <utility>

namespace openspace {
namespace {
    // This is kind of weird.  Optimally, we would want to use the std::array also on
    // non-Windows platforms but that causes some issues with nullptrs being thrown
    // around and invalid accesses.  Switching this to a std::vector with dynamic memory
    // allocation works on Linux, but it fails on Windows in some SGCT function and on Mac
    // in some random global randoms
#ifdef WIN32
    // The number of objects that are placed into the DataStorage below
    constexpr int NumberOfGlobals = 33;

    constexpr int TotalSize =
        sizeof(MemoryManager) +
        sizeof(Server) +
        sizeof(OpenSpaceEngine) +
        sizeof(DownloadEventEngine) +
        sizeof(EventEngine) +
        sizeof(fontrendering::FontManager) +
        sizeof(Dashboard) +
        sizeof(DeferredcasterManager) +
        sizeof(DownloadManager) +
        sizeof(LuaConsole) +
        sizeof(MissionManager) +
        sizeof(ModuleEngine) +
        sizeof(Astrocast) +
        sizeof(RaycasterManager) +
        sizeof(RenderEngine) +
        sizeof(std::vector<std::unique_ptr<ScreenSpaceRenderable>>) +
        sizeof(SyncEngine) +
        sizeof(TimeManager) +
        sizeof(VersionChecker) +
        sizeof(WindowDelegate) +
        sizeof(Configuration) +
        sizeof(ActionManager) +
        sizeof(InteractionHandler) +
        sizeof(KeybindingManager) +
        sizeof(KeyframeRecordingHandler) +
        sizeof(NavigationHandler) +
        sizeof(SessionRecordingHandler) +
        sizeof(PropertyOwner) +
        sizeof(PropertyOwner) +
        sizeof(PropertyOwner) +
        sizeof(ScriptEngine) +
        sizeof(ScriptScheduler) +
        sizeof(Profile) +
        // Every object has to start at an address that satisfies its own alignment
        // requirement, so there might be some padding in front of each of them. Reserve
        // enough slack for a maximally sized padding per object
        NumberOfGlobals * alignof(std::max_align_t);

    alignas(std::max_align_t) std::array<std::byte, TotalSize> DataStorage;
#endif // WIN32

/**
 * Creates the global object of type \p T. On Windows the object is placed into the
 * statically allocated DataStorage at the position \p pos, which is advanced past the
 * newly created object. On all other platforms the object is heap-allocated instead and
 * \p pos is unused.
 */
template <typename T, typename... Args>
T* createGlobal([[maybe_unused]] std::byte*& pos, Args&&... args) {
#ifdef WIN32
    constexpr std::uintptr_t Alignment = alignof(T);
    const std::uintptr_t p = reinterpret_cast<std::uintptr_t>(pos);
    // Align 'pos' up to the next address that satisfies T's alignment requirement
    pos = reinterpret_cast<std::byte*>((p + Alignment - 1) & ~(Alignment - 1));

    assert_msg(
        pos + sizeof(T) <= DataStorage.data() + TotalSize,
        "Ran out of space in the global DataStorage"
    );

    T* obj = new (pos) T(std::forward<Args>(args)...);
    pos += sizeof(T);
    return obj;
#else // ^^^^ WIN32 / !WIN32 vvvv
    return new T(std::forward<Args>(args)...);
#endif // WIN32
}

} // namespace
} // namespace openspace

namespace openspace::global {

void create() {
    ZoneScoped;

    callback::create();

#ifdef WIN32
    std::fill(DataStorage.begin(), DataStorage.end(), std::byte(0));
    std::byte* currentPos = DataStorage.data();
#else // ^^^^ WIN32 / !WIN32 vvvv
    std::byte* currentPos = nullptr;
#endif // WIN32

    memoryManager = createGlobal<MemoryManager>(currentPos);
    syncEngine = createGlobal<SyncEngine>(currentPos, 4096);
    server = createGlobal<Server>(currentPos);
    openSpaceEngine = createGlobal<OpenSpaceEngine>(currentPos);
    downloadEventEngine = createGlobal<DownloadEventEngine>(currentPos);
    eventEngine = createGlobal<EventEngine>(currentPos);
    fontManager = createGlobal<fontrendering::FontManager>(
        currentPos,
        glm::ivec3(1536, 1536, 1)
    );
    dashboard = createGlobal<Dashboard>(currentPos);
    deferredcasterManager = createGlobal<DeferredcasterManager>(currentPos);
    downloadManager = createGlobal<DownloadManager>(currentPos);
    luaConsole = createGlobal<LuaConsole>(currentPos);
    missionManager = createGlobal<MissionManager>(currentPos);
    moduleEngine = createGlobal<ModuleEngine>(currentPos);
    astrocast = createGlobal<Astrocast>(currentPos);
    raycasterManager = createGlobal<RaycasterManager>(currentPos);
    renderEngine = createGlobal<RenderEngine>(currentPos);
    screenSpaceRenderables =
        createGlobal<std::vector<std::unique_ptr<ScreenSpaceRenderable>>>(currentPos);
    timeManager = createGlobal<TimeManager>(currentPos);
    versionChecker = createGlobal<VersionChecker>(currentPos);
    windowDelegate = createGlobal<WindowDelegate>(currentPos);
    configuration = createGlobal<Configuration>(currentPos);
    actionManager = createGlobal<ActionManager>(currentPos);
    interactionHandler = createGlobal<InteractionHandler>(currentPos);
    keybindingManager = createGlobal<KeybindingManager>(currentPos);
    keyframeRecording = createGlobal<KeyframeRecordingHandler>(currentPos);
    navigationHandler = createGlobal<NavigationHandler>(currentPos);
    sessionRecordingHandler = createGlobal<SessionRecordingHandler>(currentPos);
    rootPropertyOwner = createGlobal<PropertyOwner>(
        currentPos,
        PropertyOwner::PropertyOwnerInfo{ .identifier = "" }
    );
    screenSpaceRootPropertyOwner = createGlobal<PropertyOwner>(
        currentPos,
        PropertyOwner::PropertyOwnerInfo{ .identifier = "ScreenSpace" }
    );
    userPropertyOwner = createGlobal<PropertyOwner>(
        currentPos,
        PropertyOwner::PropertyOwnerInfo{ .identifier = "UserProperties" }
    );
    scriptEngine = createGlobal<ScriptEngine>(currentPos);
    scriptScheduler = createGlobal<ScriptScheduler>(currentPos);
    profile = createGlobal<Profile>(currentPos);
}

void initialize() {
    ZoneScoped;

    rootPropertyOwner->addPropertySubOwner(global::moduleEngine);

    // New property subowners also have to be added to the ImGuiModule callback
    rootPropertyOwner->addPropertySubOwner(global::navigationHandler);
    rootPropertyOwner->addPropertySubOwner(global::keyframeRecording);
    rootPropertyOwner->addPropertySubOwner(global::interactionHandler);
    rootPropertyOwner->addPropertySubOwner(global::sessionRecordingHandler);
    rootPropertyOwner->addPropertySubOwner(global::timeManager);
    rootPropertyOwner->addPropertySubOwner(global::scriptScheduler);

    rootPropertyOwner->addPropertySubOwner(global::renderEngine);
    rootPropertyOwner->addPropertySubOwner(global::screenSpaceRootPropertyOwner);

    rootPropertyOwner->addPropertySubOwner(global::server);

    rootPropertyOwner->addPropertySubOwner(global::astrocast);
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
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete profile;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'ScriptScheduler'");
#ifdef WIN32
    scriptScheduler->~ScriptScheduler();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete scriptScheduler;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'ScriptEngine'");
#ifdef WIN32
    scriptEngine->~ScriptEngine();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete scriptEngine;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'ScreenSpace Root Owner'");
#ifdef WIN32
    screenSpaceRootPropertyOwner->~PropertyOwner();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete screenSpaceRootPropertyOwner;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'Root Owner'");
#ifdef WIN32
    rootPropertyOwner->~PropertyOwner();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete rootPropertyOwner;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'SessionRecordingHandler'");
#ifdef WIN32
    sessionRecordingHandler->~SessionRecordingHandler();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete sessionRecordingHandler;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'NavigationHandler'");
#ifdef WIN32
    navigationHandler->~NavigationHandler();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete navigationHandler;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'KeyframeRecordingHandler'");
#ifdef WIN32
    keyframeRecording->~KeyframeRecordingHandler();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete keyframeRecording;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'KeybindingManager'");
#ifdef WIN32
    keybindingManager->~KeybindingManager();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete keybindingManager;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'InteractionHandler'");
#ifdef WIN32
    interactionHandler->~InteractionHandler();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete interactionHandler;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'ActionManager'");
#ifdef WIN32
    actionManager->~ActionManager();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete actionManager;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'Configuration'");
#ifdef WIN32
    configuration->~Configuration();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete configuration;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'WindowDelegate'");
#ifdef WIN32
    windowDelegate->~WindowDelegate();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete windowDelegate;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'VersionChecker'");
#ifdef WIN32
    versionChecker->~VersionChecker();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete versionChecker;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'TimeManager'");
#ifdef WIN32
    timeManager->~TimeManager();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete timeManager;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'ScreenSpaceRenderables'");
#ifdef WIN32
    screenSpaceRenderables->~vector();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete screenSpaceRenderables;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'RenderEngine'");
#ifdef WIN32
    renderEngine->~RenderEngine();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete renderEngine;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'RaycasterManager'");
#ifdef WIN32
    raycasterManager->~RaycasterManager();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete raycasterManager;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'Astrocast'");
#ifdef WIN32
    astrocast->~Astrocast();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete astrocast;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'ModuleEngine'");
#ifdef WIN32
    moduleEngine->~ModuleEngine();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete moduleEngine;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'MissionManager'");
#ifdef WIN32
    missionManager->~MissionManager();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete missionManager;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'LuaConsole'");
#ifdef WIN32
    luaConsole->~LuaConsole();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete luaConsole;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'DownloadManager'");
#ifdef WIN32
    downloadManager->~DownloadManager();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete downloadManager;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'DeferredcasterManager'");
#ifdef WIN32
    deferredcasterManager->~DeferredcasterManager();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete deferredcasterManager;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'Dashboard'");
#ifdef WIN32
    dashboard->~Dashboard();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete dashboard;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'FontManager'");
#ifdef WIN32
    fontManager->~FontManager();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete fontManager;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'DownloadEventEngine'");
#ifdef WIN32
    downloadEventEngine->~DownloadEventEngine();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete downloadEventEngine;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'EventEngine'");
#ifdef WIN32
    eventEngine->~EventEngine();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete eventEngine;
#endif // WIN32

    // We need to destroy the Server before the OpenSpace engine since there may be Topics
    // that references the engine and or assetManager for example
    LDEBUGC("Globals", "Destroying 'Server'");
#ifdef WIN32
    server->~Server();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete server;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'OpenSpaceEngine'");
#ifdef WIN32
    openSpaceEngine->~OpenSpaceEngine();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete openSpaceEngine;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'SyncEngine'");
#ifdef WIN32
    syncEngine->~SyncEngine();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete syncEngine;
#endif // WIN32

    LDEBUGC("Globals", "Destroying 'MemoryManager'");
#ifdef WIN32
    memoryManager->~MemoryManager();
#else // ^^^^ WIN32 / !WIN32 vvvv
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
