/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <openspace/util/keys.h>
#include <openspace/util/mouse.h>

#include <ghoul/glm.h>

#include <functional>
#include <memory>
#include <string>
#include <vector>

namespace ghoul {
    
class Dictionary;

namespace cmdparser { class CommandlineParser; }
namespace fontrendering { class FontManager; }

} // namespace ghoul

namespace openspace {

class ConfigurationManager;
class DownloadManager;
class GUI;
class LuaConsole;
class ModuleEngine;
class NetworkEngine;
class ParallelConnection;
class RenderEngine;
class SettingsEngine;
class SceneManager;

class SyncEngine;
class TimeManager;
class WindowWrapper;

namespace interaction { class InteractionHandler; }
namespace gui { class GUI; }
namespace properties { class PropertyOwner; }
namespace scripting {
    struct LuaLibrary;
    class ScriptEngine;
    class ScriptScheduler;
} // namespace scripting

class OpenSpaceEngine {
public:
    static void create(int argc, char** argv,
        std::unique_ptr<WindowWrapper> windowWrapper,
        std::vector<std::string>& sgctArguments, bool& requestClose);
    static void destroy();
    static OpenSpaceEngine& ref();

    double runTime();
    void setRunTime(double t);

    // callbacks
    void initialize();
    void initializeGL();
    void deinitialize();
    void preSynchronization();
    void postSynchronizationPreDraw();
    void render(const glm::mat4& viewMatrix, const glm::mat4& projectionMatrix);
    void postDraw();
    void keyboardCallback(Key key, KeyModifier mod, KeyAction action);
    void charCallback(unsigned int codepoint, KeyModifier mod);
    void mouseButtonCallback(MouseButton button, MouseAction action);
    void mousePositionCallback(double x, double y);
    void mouseScrollWheelCallback(double pos);
    void externalControlCallback(const char* receivedChars, int size, int clientId);
    void encode();
    void decode();

    void scheduleLoadScene(std::string scenePath);

    void enableBarrier();
    void disableBarrier();
    
    void writeDocumentation();
    void toggleShutdownMode();
    
    void runPostInitializationScripts(const std::string& sceneDescription);

    // Guaranteed to return a valid pointer
    ConfigurationManager& configurationManager();
    LuaConsole& console();
    DownloadManager& downloadManager();
    ModuleEngine& moduleEngine();
    NetworkEngine& networkEngine();
    ParallelConnection& parallelConnection();
    RenderEngine& renderEngine();
    SettingsEngine& settingsEngine();
    TimeManager& timeManager();
    WindowWrapper& windowWrapper();
    ghoul::fontrendering::FontManager& fontManager();
    interaction::InteractionHandler& interactionHandler();
    properties::PropertyOwner& globalPropertyOwner();
    scripting::ScriptEngine& scriptEngine();
    scripting::ScriptScheduler& scriptScheduler();

    
    // This method is only to be called from Modules
    enum class CallbackOption {
        Initialize = 0,  // Callback for the end of the initialization
        Deinitialize,    // Callback for the end of the deinitialization
        InitializeGL,    // Callback for the end of the OpenGL initialization
        DeinitializeGL,  // Callback for the end of the OpenGL deinitialization
        PreSync,         // Callback for the end of the pre-sync function
        PostSyncPreDraw, // Callback for the end of the post-sync-pre-draw function
        Render,          // Callback for the end of the render function
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
    void registerModuleMouseScrollWheelCallback(std::function<bool (double)> function);
    
    /**
     * Returns the Lua library that contains all Lua functions available to affect the
     * application.
     */
    static scripting::LuaLibrary luaLibrary();

private:
    OpenSpaceEngine(std::string programName,
        std::unique_ptr<WindowWrapper> windowWrapper);
    ~OpenSpaceEngine() = default;

    void loadScene(const std::string& scenePath);
    void gatherCommandlineArguments();
    void loadFonts();
    void runPreInitializationScripts(const std::string& sceneDescription);
    void configureLogging();
    
    // Components
    std::unique_ptr<ConfigurationManager> _configurationManager;
    std::unique_ptr<SceneManager> _sceneManager;
    std::unique_ptr<DownloadManager> _downloadManager;
    std::unique_ptr<LuaConsole> _console;
    std::unique_ptr<ModuleEngine> _moduleEngine;
    std::unique_ptr<NetworkEngine> _networkEngine;
    std::unique_ptr<ParallelConnection> _parallelConnection;
    std::unique_ptr<RenderEngine> _renderEngine;
    std::unique_ptr<SettingsEngine> _settingsEngine;
    std::unique_ptr<SyncEngine> _syncEngine;
    std::unique_ptr<TimeManager> _timeManager;
    std::unique_ptr<WindowWrapper> _windowWrapper;
    std::unique_ptr<ghoul::cmdparser::CommandlineParser> _commandlineParser;
    std::unique_ptr<ghoul::fontrendering::FontManager> _fontManager;
    std::unique_ptr<interaction::InteractionHandler> _interactionHandler;
    std::unique_ptr<scripting::ScriptEngine> _scriptEngine;
    std::unique_ptr<scripting::ScriptScheduler> _scriptScheduler;

    // Others
    std::unique_ptr<properties::PropertyOwner> _globalPropertyNamespace;
    
    bool _scheduledSceneSwitch;
    std::string _scenePath;

    struct {
        std::vector<std::function<void()>> initialize;
        std::vector<std::function<void()>> deinitialize;
        
        std::vector<std::function<void()>> initializeGL;
        std::vector<std::function<void()>> deinitializeGL;
        
        std::vector<std::function<void()>> preSync;
        std::vector<std::function<void()>> postSyncPreDraw;
        std::vector<std::function<void()>> render;
        std::vector<std::function<void()>> postDraw;
        
        std::vector<std::function<bool (Key, KeyModifier, KeyAction)>> keyboard;
        std::vector<std::function<bool (unsigned int, KeyModifier)>> character;
        
        std::vector<std::function<bool (MouseButton, MouseAction)>> mouseButton;
        std::vector<std::function<void (double, double)>> mousePosition;
        std::vector<std::function<bool (double)>> mouseScrollWheel;
    } _moduleCallbacks;
    
    double _runTime;

    // Structure that is responsible for the delayed shutdown of the application
    struct {
        // Whether the application is currently in shutdown mode (i.e. counting down the
        // timer and closing it at '0'
        bool inShutdown;
        // Total amount of time the application will wait before actually shutting down
        float waitTime;
        // Current state of the countdown; if it reaches '0', the application will
        // close
        float timer;
    } _shutdown;

    // The first frame might take some more time in the update loop, so we need to know to
    // disable the synchronization; otherwise a hardware sync will kill us after 1 minute
    bool _isFirstRenderingFirstFrame;

    static OpenSpaceEngine* _engine;
};

#define OsEng (openspace::OpenSpaceEngine::ref())

} // namespace openspace

#endif // __OPENSPACE_CORE___OPENSPACEENGINE___H__
