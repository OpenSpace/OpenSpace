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

#ifndef __OPENSPACEENGINE_H__
#define __OPENSPACEENGINE_H__

#include <openspace/util/keys.h>
#include <openspace/util/mouse.h>

#include <ghoul/glm.h>
#include <ghoul/misc/dictionary.h>

#include <memory>
#include <string>
#include <vector>

namespace ghoul {
namespace cmdparser { class CommandlineParser; }
namespace fontrendering { class FontManager; }
}

namespace openspace {

class ConfigurationManager;
class LuaConsole;
class NetworkEngine;
class GUI;
class RenderEngine;
class SyncBuffer;
class ModuleEngine;
class WindowWrapper;

namespace interaction { class InteractionHandler; }
namespace gui { class GUI; }
namespace scripting { class ScriptEngine; }
namespace network { class ParallelConnection; }
namespace properties { class PropertyOwner; }
 
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

    // Guaranteed to return a valid pointer
    ConfigurationManager& configurationManager();
    interaction::InteractionHandler& interactionHandler();
    RenderEngine& renderEngine();
    scripting::ScriptEngine& scriptEngine();
    NetworkEngine& networkEngine();
    LuaConsole& console();
    ModuleEngine& moduleEngine();
    network::ParallelConnection& parallelConnection();
    properties::PropertyOwner& globalPropertyOwner();
    WindowWrapper& windowWrapper();
    ghoul::fontrendering::FontManager& fontManager();
    gui::GUI& gui();

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

    void enableBarrier();
    void disableBarrier();

    void runPostInitializationScripts(const std::string& sceneDescription);

private:
    OpenSpaceEngine(std::string programName, std::unique_ptr<WindowWrapper> windowWrapper);
    ~OpenSpaceEngine();

    void clearAllWindows();
    bool gatherCommandlineArguments();
    bool loadSpiceKernels();
    void loadFonts();
    void runScripts(const ghoul::Dictionary& scripts);
    void runPreInitializationScripts(const std::string& sceneDescription);
    void configureLogging();
    
    // Components
    std::unique_ptr<ConfigurationManager> _configurationManager;
    std::unique_ptr<interaction::InteractionHandler> _interactionHandler;
    std::unique_ptr<RenderEngine> _renderEngine;
    std::unique_ptr<scripting::ScriptEngine> _scriptEngine;
    std::unique_ptr<NetworkEngine> _networkEngine;
    std::unique_ptr<ghoul::cmdparser::CommandlineParser> _commandlineParser;
    std::unique_ptr<LuaConsole> _console;
    std::unique_ptr<ModuleEngine> _moduleEngine;
    std::unique_ptr<gui::GUI> _gui;
    std::unique_ptr<network::ParallelConnection> _parallelConnection;
    std::unique_ptr<WindowWrapper> _windowWrapper;
    std::unique_ptr<ghoul::fontrendering::FontManager> _fontManager;
    
    // Others
    std::unique_ptr<properties::PropertyOwner> _globalPropertyNamespace;
    std::unique_ptr<SyncBuffer> _syncBuffer;
    
    bool _isMaster;
    double _runTime;

    static OpenSpaceEngine* _engine;
};

#define OsEng (openspace::OpenSpaceEngine::ref())

}  // namespace openspace

#endif  // __OPENSPACEENGINE_H__
