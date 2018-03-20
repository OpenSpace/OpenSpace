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

#ifndef __OPENSPACE_CORE___OPENSPACEENGINE___H__
#define __OPENSPACE_CORE___OPENSPACEENGINE___H__

#include <openspace/properties/stringproperty.h>
#include <openspace/util/keys.h>
#include <openspace/util/mouse.h>
#include <ghoul/glm.h>
#include <ghoul/misc/assert.h>

#include <functional>
#include <memory>
#include <string>
#include <vector>

namespace ghoul { class Dictionary; }
namespace ghoul::cmdparser { class CommandlineParser; }
namespace ghoul::fontrendering { class FontManager; }

namespace openspace {

class AssetManager;
class ConfigurationManager;
class Dashboard;
class DownloadManager;
class GUI;
class LoadingScreen;
class LuaConsole;
class ModuleEngine;
class NetworkEngine;
class ParallelPeer;
class RenderEngine;
class Scene;
class SyncEngine;
class TimeManager;
class VirtualPropertyManager;
class WindowWrapper;

namespace interaction {
    class NavigationHandler;
    class KeyBindingManager;
}
namespace gui { class GUI; }
namespace properties { class PropertyOwner; }
namespace scripting {
    struct LuaLibrary;
    class ScriptEngine;
    class ScriptScheduler;
} // namespace scripting

  // Structure that is responsible for the delayed shutdown of the application
struct ShutdownInformation {
    // Whether the application is currently in shutdown mode (i.e. counting down the
    // timer and closing it at '0'
    bool inShutdown;
    // Total amount of time the application will wait before actually shutting down
    float waitTime;
    // Current state of the countdown; if it reaches '0', the application will
    // close
    float timer;
};

class OpenSpaceEngine {
public:

    static void create(int argc, char** argv,
        std::unique_ptr<WindowWrapper> windowWrapper,
        std::vector<std::string>& sgctArguments,
        bool& requestClose, bool consoleLog = true);
    static void destroy();
    static OpenSpaceEngine& ref();
    static bool isCreated();

    ~OpenSpaceEngine();

    // callbacks
    void initialize();
    void initializeGL();
    void deinitialize();
    void preSynchronization();
    void postSynchronizationPreDraw();
    void render(const glm::mat4& sceneMatrix, const glm::mat4& viewMatrix,
        const glm::mat4& projectionMatrix);
    void drawOverlays();
    void postDraw();
    void keyboardCallback(Key key, KeyModifier mod, KeyAction action);
    void charCallback(unsigned int codepoint, KeyModifier mod);
    void mouseButtonCallback(MouseButton button, MouseAction action);
    void mousePositionCallback(double x, double y);
    void mouseScrollWheelCallback(double posX, double posY);
    void externalControlCallback(const char* receivedChars, int size, int clientId);
    void encode();
    void decode();


    void scheduleLoadSingleAsset(std::string assetPath);
    void toggleShutdownMode();

    // Guaranteed to return a valid pointer
    ConfigurationManager& configurationManager();
    LuaConsole& console();
    AssetManager& assetManager();
    Dashboard& dashboard();
    DownloadManager& downloadManager();
    ModuleEngine& moduleEngine();
    LoadingScreen& loadingScreen();
    NetworkEngine& networkEngine();
    ParallelPeer& parallelPeer();
    RenderEngine& renderEngine();
    TimeManager& timeManager();
    WindowWrapper& windowWrapper();
    ghoul::fontrendering::FontManager& fontManager();
    interaction::NavigationHandler& navigationHandler();
    interaction::KeyBindingManager& keyBindingManager();
    properties::PropertyOwner& rootPropertyOwner();
    properties::PropertyOwner& globalPropertyOwner();
    scripting::ScriptEngine& scriptEngine();
    scripting::ScriptScheduler& scriptScheduler();
    VirtualPropertyManager& virtualPropertyManager();


    // This method is only to be called from Modules
    enum class CallbackOption {
        Initialize = 0,  // Callback for the end of the initialization
        Deinitialize,    // Callback for the end of the deinitialization
        InitializeGL,    // Callback for the end of the OpenGL initialization
        DeinitializeGL,  // Callback for the end of the OpenGL deinitialization
        PreSync,         // Callback for the end of the pre-sync function
        PostSyncPreDraw, // Callback for the end of the post-sync-pre-draw function
        Render,          // Callback for the end of the render function
        Draw2D,          // Callback for the two-dimensional rendering functions
        PostDraw         // Callback for the end of the post-draw function
    };

    // Registers a callback for a specific CallbackOption
    void registerModuleCallback(CallbackOption option, std::function<void()> function);

    // Registers a callback that is called when a new keyboard event is received
    void registerModuleKeyboardCallback(
        std::function<bool (Key, KeyModifier, KeyAction)> function);

    // Registers a callback that is called when a new character event is received
    void registerModuleCharCallback(
        std::function<bool (unsigned int, KeyModifier)> function);

    // Registers a callback that is called when a new mouse button is received
    void registerModuleMouseButtonCallback(
       std::function<bool (MouseButton, MouseAction)> function);

    // Registers a callback that is called when a new mouse movement is received
    void registerModuleMousePositionCallback(
        std::function<void (double, double)> function);

    // Registers a callback that is called when a scroll wheel change is received
    void registerModuleMouseScrollWheelCallback(
        std::function<bool (double, double)> function
    );

    void writeSceneDocumentation();
    void writeStaticDocumentation();


    /**
     * Returns the Lua library that contains all Lua functions available to affect the
     * application.
     */
    static scripting::LuaLibrary luaLibrary();

private:
    OpenSpaceEngine(std::string programName,
        std::unique_ptr<WindowWrapper> windowWrapper);

    std::unique_ptr<LoadingScreen> createLoadingScreen();
    void loadSingleAsset(const std::string& assetPath);
    void gatherCommandlineArguments();
    void loadFonts();

    void configureLogging(bool consoleLog);

    void runGlobalCustomizationScripts();
    void configureLogging();

    // Components
    std::unique_ptr<ConfigurationManager> _configurationManager;
    std::unique_ptr<Scene> _scene;
    std::unique_ptr<AssetManager> _assetManager;
    std::unique_ptr<Dashboard> _dashboard;
    std::unique_ptr<DownloadManager> _downloadManager;
    std::unique_ptr<LuaConsole> _console;
    std::unique_ptr<ModuleEngine> _moduleEngine;
    std::unique_ptr<NetworkEngine> _networkEngine;
    std::unique_ptr<ParallelPeer> _parallelPeer;
    std::unique_ptr<RenderEngine> _renderEngine;
    std::unique_ptr<SyncEngine> _syncEngine;
    std::unique_ptr<TimeManager> _timeManager;
    std::unique_ptr<WindowWrapper> _windowWrapper;
    std::unique_ptr<ghoul::cmdparser::CommandlineParser> _commandlineParser;
    std::unique_ptr<ghoul::fontrendering::FontManager> _fontManager;
    std::unique_ptr<interaction::NavigationHandler> _navigationHandler;
    std::unique_ptr<interaction::KeyBindingManager> _keyBindingManager;

    std::unique_ptr<scripting::ScriptEngine> _scriptEngine;
    std::unique_ptr<scripting::ScriptScheduler> _scriptScheduler;
    std::unique_ptr<VirtualPropertyManager> _virtualPropertyManager;

    // Others
    std::unique_ptr<properties::PropertyOwner> _rootPropertyOwner;
    std::unique_ptr<properties::PropertyOwner> _globalPropertyOwner;

    std::unique_ptr<LoadingScreen> _loadingScreen;

    struct {
        properties::StringProperty versionString;
        properties::StringProperty sourceControlInformation;
    } _versionInformation;

    bool _hasScheduledAssetLoading;
    std::string _scheduledAssetPathToLoad;

    struct {
        std::vector<std::function<void()>> initialize;
        std::vector<std::function<void()>> deinitialize;

        std::vector<std::function<void()>> initializeGL;
        std::vector<std::function<void()>> deinitializeGL;

        std::vector<std::function<void()>> preSync;
        std::vector<std::function<void()>> postSyncPreDraw;
        std::vector<std::function<void()>> render;
        std::vector<std::function<void()>> draw2D;
        std::vector<std::function<void()>> postDraw;

        std::vector<std::function<bool (Key, KeyModifier, KeyAction)>> keyboard;
        std::vector<std::function<bool (unsigned int, KeyModifier)>> character;

        std::vector<std::function<bool (MouseButton, MouseAction)>> mouseButton;
        std::vector<std::function<void (double, double)>> mousePosition;
        std::vector<std::function<bool (double, double)>> mouseScrollWheel;
    } _moduleCallbacks;

    ShutdownInformation _shutdown;

    // The first frame might take some more time in the update loop, so we need to know to
    // disable the synchronization; otherwise a hardware sync will kill us after 1 minute
    bool _isFirstRenderingFirstFrame;

    static OpenSpaceEngine* _engine;
};

#define OsEng (openspace::OpenSpaceEngine::ref())

} // namespace openspace

#endif // __OPENSPACE_CORE___OPENSPACEENGINE___H__
