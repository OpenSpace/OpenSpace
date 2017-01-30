/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#include <memory>
#include <string>
#include <vector>

namespace ghoul {
class Dictionary;
namespace cmdparser { class CommandlineParser; }
namespace fontrendering { class FontManager; }
}

namespace openspace {

class ConfigurationManager;
class DownloadManager;
class LuaConsole;
class NetworkEngine;
class GUI;
class RenderEngine;
class ModuleEngine;
class WindowWrapper;
class SettingsEngine;
class TimeManager;
class SceneManager;
class SyncEngine;
class ParallelConnection;

namespace interaction { class InteractionHandler; }
namespace gui { class GUI; }
//namespace scripting { class ScriptEngine; }
namespace properties { class PropertyOwner; }
namespace scripting { struct LuaLibrary; }
namespace scripting { class ScriptScheduler; }
namespace scripting { class ScriptEngine; }
 
class OpenSpaceEngine {
public:
    static bool create(int argc, char** argv,
        std::unique_ptr<WindowWrapper> windowWrapper,
        std::vector<std::string>& sgctArguments);
    static void destroy();
    static bool isInitialized();
    static OpenSpaceEngine& ref();

    bool isMaster();
    void setMaster(bool master);
    double runTime();
    void setRunTime(double t);
    void loadScene(const std::string& scenePath);

    // Guaranteed to return a valid pointer
    ConfigurationManager& configurationManager();
    interaction::InteractionHandler& interactionHandler();
    RenderEngine& renderEngine();
    scripting::ScriptEngine& scriptEngine();
    scripting::ScriptScheduler& scriptScheduler();
    NetworkEngine& networkEngine();
    LuaConsole& console();
    ModuleEngine& moduleEngine();
    ParallelConnection& parallelConnection();
    properties::PropertyOwner& globalPropertyOwner();
    WindowWrapper& windowWrapper();
    ghoul::fontrendering::FontManager& fontManager();
    DownloadManager& downloadManager();
    TimeManager& timeManager();

#ifdef OPENSPACE_MODULE_ONSCREENGUI_ENABLED
    gui::GUI& gui();
#endif

    // SGCT callbacks
    bool initialize();
    bool initializeGL();
    void preSynchronization();
    void postSynchronizationPreDraw();
    void render(const glm::mat4& projectionMatrix, const glm::mat4& viewMatrix);
    void postDraw();
    void keyboardCallback(Key key, KeyModifier mod, KeyAction action);
    void charCallback(unsigned int codepoint, KeyModifier mod);
    void mouseButtonCallback(MouseButton button, MouseAction action);
    void mousePositionCallback(double x, double y);
    void mouseScrollWheelCallback(double pos);
    void externalControlCallback(const char* receivedChars, int size, int clientId);
    void encode();
    void decode();
    void scheduleLoadScene(const std::string& scenePath);

    void enableBarrier();
    void disableBarrier();

    void writeDocumentation();
    void toggleShutdownMode();
    
    bool useBusyWaitForDecode();
    bool logSGCTOutOfOrderErrors();

    void runPostInitializationScripts(const std::string& sceneDescription);

    /**
    * Returns the Lua library that contains all Lua functions available to affect the
    * application.
    */
    static scripting::LuaLibrary luaLibrary();

private:
    OpenSpaceEngine(std::string programName, std::unique_ptr<WindowWrapper> windowWrapper);
    ~OpenSpaceEngine();

    void clearAllWindows();
    void gatherCommandlineArguments();
    void loadFonts();
    void runScripts(const ghoul::Dictionary& scripts);
    void runPreInitializationScripts(const std::string& sceneDescription);
    void configureLogging();
    
    // Components
    std::unique_ptr<ConfigurationManager> _configurationManager;
    std::unique_ptr<interaction::InteractionHandler> _interactionHandler;
    std::unique_ptr<RenderEngine> _renderEngine;
    std::unique_ptr<SceneManager> _sceneManager;
    std::unique_ptr<scripting::ScriptEngine> _scriptEngine;
    std::unique_ptr<scripting::ScriptScheduler> _scriptScheduler;
    std::unique_ptr<NetworkEngine> _networkEngine;
    std::unique_ptr<SyncEngine> _syncEngine;
    std::unique_ptr<ghoul::cmdparser::CommandlineParser> _commandlineParser;
    std::unique_ptr<LuaConsole> _console;
    std::unique_ptr<ModuleEngine> _moduleEngine;
    std::unique_ptr<SettingsEngine> _settingsEngine;
    std::unique_ptr<TimeManager> _timeManager;
    std::unique_ptr<DownloadManager> _downloadManager;
#ifdef OPENSPACE_MODULE_ONSCREENGUI_ENABLED
    std::unique_ptr<gui::GUI> _gui;
#endif
    std::unique_ptr<ParallelConnection> _parallelConnection;
    std::unique_ptr<WindowWrapper> _windowWrapper;
    std::unique_ptr<ghoul::fontrendering::FontManager> _fontManager;

    // Others
    std::unique_ptr<properties::PropertyOwner> _globalPropertyNamespace;
    
    bool _switchScene;
    std::string _scenePath;

    bool _isMaster;
    double _runTime;

    // Whether the application is currently in shutdown mode (i.e. counting down the timer
    // and closing it at '0'
    bool _isInShutdownMode;
    // The total amount of time the application will wait before actually shutting down
    float _shutdownWait;
    // The current state of the countdown; if it reaches '0', the application will close
    float _shutdownCountdown;

    // The first frame might take some more time in the update loop, so we need to know to
    // disable the synchronization; otherwise a hardware sync will kill us after 1 sec
    bool _isFirstRenderingFirstFrame;

    static OpenSpaceEngine* _engine;
};

#define OsEng (openspace::OpenSpaceEngine::ref())

} // namespace openspace

#endif  // __OPENSPACE_CORE___OPENSPACEENGINE___H__
