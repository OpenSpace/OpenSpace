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

#include <openspace/documentation/documentation.h>
#include <openspace/engine/configuration.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/util/keys.h>
#include <openspace/util/mouse.h>
#include <ghoul/fmt.h>
#include <ghoul/ghoul.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/consolelog.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/logging/visualstudiooutputlog.h>
#include <api/MinVR.h>
#include <GLFW/glfw3.h>

// @TODO(abock):  Add Spout support
// @TODO(abock):  Reintroduce commandline-parsing
// @TODO(abock):  Pull OpenSpace branch and remove explicit linking against GLFW from application

using namespace MinVR;
using namespace openspace;

namespace {

class Handler : public VREventHandler, public VRRenderHandler {
public:
    void onVREvent(const VRDataIndex& eventData) override;
    void onVRRenderContext(const VRDataIndex& stateData) override;
    void onVRRenderScene(const VRDataIndex& stateData) override;
};

constexpr const char* _loggerCat = "main_minvr";

VRMain engine;
Handler handler;

struct {
    int nWindows = 0;
    glm::ivec2 windowSize;
    glm::ivec2 framebufferSize;

    glm::vec2 mousePosition;
    uint32_t mouseButtons = 0;
} windowingGlobals;

struct {
    bool modifierShift = false;
    bool modifierCtrl = false;
    bool modifierAlt = false;
} keyboardState;

std::vector<char> SynchronizationBuffer;
bool HasInitializedGL = false;

} // namespace

#pragma optimize ("", off)

void Handler::onVREvent(const VRDataIndex& eventData) {
    std::string type;
    if (eventData.exists("EventType")) {
        type = static_cast<VRString>(eventData.getValue("EventType"));
    }
    else {
        LERRORC(
            "onVREvent()",
            fmt::format("Received an event named {} of unknown type", eventData.getName())
        );
    }

    const bool isMaster = global::windowDelegate.isMaster();
    const bool isButtonEvent = type == "ButtonDown" || type == "ButtonUp" ||
                               type == "ButtonRepeat";

    if (type == "AnalogUpdate") {
        //_app->onAnalogChange(VRAnalogEvent(eventData));
    }
    else if (isMaster && isButtonEvent) {
        const VRButtonEvent& event = static_cast<const VRButtonEvent&>(eventData);

        const std::string& buttonName = event.getName();

        if (buttonName.size() >= 3 && buttonName.substr(0, 3) == "Kbd") {
            // We have a keyboard event
            const size_t beg = 3; // "Kbd" prefix
            const size_t sep = buttonName.find('_');

            std::string keyName = buttonName.substr(beg, sep - beg);
            std::string actionName = buttonName.substr(sep + 1);

            Key key = KeyMapping.find(keyName)->second;

            KeyAction action;
            if (actionName == "Up") {
                action = KeyAction::Release;
            }
            else if (actionName == "Down") {
                action = KeyAction::Press;
            }
            else if (actionName == "Repeat") {
                action = KeyAction::Repeat;
            }
            else {
                LWARNINGC("Key", "Unknown key action " + actionName);
            }

            if (key == Key::LeftShift || key == Key::RightShift) {
                keyboardState.modifierShift = action != KeyAction::Release;
            }
            if (key == Key::LeftControl || key == Key::RightControl) {
                keyboardState.modifierCtrl = action != KeyAction::Release;
            }
            if (key == Key::LeftAlt || key == Key::RightAlt) {
                keyboardState.modifierAlt = action != KeyAction::Release;
            }

            using KM = KeyModifier;
            KM mod = KM::NoModifier;
            mod |= keyboardState.modifierShift ? KM::Shift : KM::NoModifier;
            mod |= keyboardState.modifierCtrl ? KM::Control : KM::NoModifier;
            mod |= keyboardState.modifierAlt ? KM::Alt : KM::NoModifier;

            openspace::global::openSpaceEngine.keyboardCallback(key, mod, action);
        }

        if (buttonName.size() >= 8 && buttonName.substr(0, 8) == "MouseBtn") {
            const size_t beg = 8; // "MouseBtn" prefix
            const size_t sep = buttonName.find('_');

            std::string keyName = buttonName.substr(beg, sep - beg);
            std::string actionName = buttonName.substr(sep + 1);

            MouseButton button;
            if (keyName == "Left") {
                button = MouseButton::Left;
            }
            else if (keyName == "Middle") {
                button = MouseButton::Middle;
            }
            else if (keyName == "Right") {
                button = MouseButton::Right;
            }

            MouseAction action;
            if (actionName == "Down") {
                action = MouseAction::Press;
            }
            else if (actionName == "Up") {
                action = MouseAction::Release;
            }

            windowingGlobals.mouseButtons = 0;
            if (button == MouseButton::Left && action == MouseAction::Press) {
                windowingGlobals.mouseButtons |= 1 << 0;
            }
            if (button == MouseButton::Middle && action == MouseAction::Press) {
                windowingGlobals.mouseButtons |= 1 << 1;
            }
            if (button == MouseButton::Right && action == MouseAction::Press) {
                windowingGlobals.mouseButtons |= 1 << 2;
            }
            global::openSpaceEngine.mouseButtonCallback(button, action);
        }

    }
    else if (isMaster && type == "CursorMove") {
        const VRCursorEvent& event = static_cast<const VRCursorEvent&>(eventData);

        const float* pos = event.getPos();
        windowingGlobals.mousePosition = glm::vec2(pos[0], pos[1]);
        openspace::global::openSpaceEngine.mousePositionCallback(pos[0], pos[1]);

        // @TODO(abock): Support mouse wheel
        //openspace::global::openSpaceEngine.mouseScrollWheelCallback(posX, posY);
    }
    else if (type == "TrackerMove") {
        const VRTrackerEvent& event = static_cast<const VRTrackerEvent&>(eventData);
    }
    else {
        LERRORC("onVREvent()", fmt::format("Received an event of unknown type {}", type));
    }
}

void Handler::onVRRenderContext(const VRDataIndex& stateData) {
    if (stateData.exists("IsGraphics")) {
        const VRGraphicsState& state = static_cast<const VRGraphicsState&>(stateData);

        if (state.isInitialRenderCall()) {
            windowingGlobals.nWindows = std::max(
                windowingGlobals.nWindows,
                static_cast<int>(stateData.getValue("WindowID")) + 1
            );
            windowingGlobals.windowSize.x = stateData.getValue("WindowWidth");
            windowingGlobals.windowSize.y = stateData.getValue("WindowHeight");
            
            windowingGlobals.framebufferSize.x = stateData.getValue("FramebufferWidth");
            windowingGlobals.framebufferSize.y = stateData.getValue("FramebufferHeight");

            global::openSpaceEngine.initializeGL();

            HasInitializedGL = true;
        }

        //_app->onRenderGraphicsContext(VRGraphicsState(renderData));
    }
}

void Handler::onVRRenderScene(const VRDataIndex& stateData) {
    if (stateData.exists("IsGraphics")) {
        glClearColor(0.f, 0.f, 0.f, 0.f);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

        const VRGraphicsState& state = static_cast<const VRGraphicsState&>(stateData);
        glm::mat4 projectionMatrix = glm::make_mat4(state.getProjectionMatrix());
        glm::mat4 viewMatrix = glm::make_mat4(state.getViewMatrix());
        try {
            openspace::global::openSpaceEngine.render(
                glm::mat4(1.f), // @TODO(abock) we should probably use the user position here?
                viewMatrix,
                projectionMatrix
            );
            openspace::global::openSpaceEngine.drawOverlays();

            openspace::global::openSpaceEngine.postDraw();
        }
        catch (const ghoul::RuntimeError& e) {
            LERRORC(e.component, e.message);
        }
    }
}

void setupMinVrDelegateFunctions(VRMain& main) {
    // Sets up the OpenSpace WindowDelegate callback functions
    WindowDelegate& delegate = global::windowDelegate;



    delegate.nWindows = []() { return windowingGlobals.nWindows; };
    delegate.currentWindowSize = []() { return windowingGlobals.windowSize; };
    delegate.currentWindowResolution = delegate.currentWindowSize;
    delegate.currentDrawBufferResolution = delegate.currentWindowResolution;
    delegate.currentViewportSize = delegate.currentWindowResolution;

    delegate.averageDeltaTime = []() { return 1.0 / 60.0; };
    delegate.deltaTime = []() { return 1.0 / 60.0; };

    delegate.mousePosition = []() {
        return windowingGlobals.mousePosition;
    };
    delegate.mouseButtons = [](int) {
        return windowingGlobals.mouseButtons;
    };

    delegate.isMaster = []() {
        //const std::string& name = engine.getName();
        //return name == "foobar"; // make the server with the name foobar the master
        return true;
    };

    delegate.openGLProcedureAddress = [](const char* func) {
        VRWindowToolkit* wtk = engine.getWindowToolkit("VRGLFWWindowToolkit");
        VRglproc procAddress = wtk->getProcAddress(func);
        return procAddress;
    };
}

int main(int argc, char** argv) {
    // Initialize the LogManager and add the console log as this will be used every time
    // and we need a fall back if something goes wrong between here and when we add the
    // logs from the configuration file. If the user requested as specific loglevel in the
    // configuration file, we will deinitialize this LogManager and reinitialize it later
    // with the correct LogLevel
    {
        using namespace ghoul::logging;
        LogManager::initialize(LogLevel::Debug, LogManager::ImmediateFlush::Yes);
        LogMgr.addLog(std::make_unique<ConsoleLog>());
#ifdef WIN32
        if (IsDebuggerPresent()) {
            LogMgr.addLog(std::make_unique<VisualStudioOutputLog>());
        }
#endif // WIN32
    }

    ghoul::initialize();

    // Create the OpenSpace engine and get arguments for the SGCT engine
    std::string windowConfiguration;
    try {
        // Find configuration
        //std::string configurationFilePath = commandlineArguments.configurationName;
        //if (commandlineArguments.configurationName.empty()) {
        LDEBUG("Finding configuration");
        std::string configurationFilePath = configuration::findConfiguration();
        //}
        configurationFilePath = absPath(configurationFilePath);

        if (!FileSys.fileExists(configurationFilePath)) {
            LFATALC("main", "Could not find configuration: " + configurationFilePath);
            exit(EXIT_FAILURE);
        }
        LINFO(fmt::format("Configuration Path: '{}'", configurationFilePath));

        // Loading configuration from disk
        LDEBUG("Loading configuration from disk");
        global::configuration = configuration::loadConfigurationFromFile(
            configurationFilePath
        );

        // If the user requested a commandline-based configuation script that should
        // overwrite some of the values, this is the time to do it
        //if (!commandlineArguments.configurationOverride.empty()) {
        //    LDEBUG("Executing Lua script passed through the commandline:");
        //    LDEBUG(commandlineArguments.configurationOverride);
        //    ghoul::lua::runScript(
        //        global::configuration.state,
        //        commandlineArguments.configurationOverride
        //    );
        //    parseLuaState(global::configuration);
        //}

        // Determining MinVR configuration file
        LDEBUG("MinVR Configuration file: " + global::configuration.windowConfiguration);

        windowConfiguration = global::configuration.windowConfiguration;
    }
    catch (const documentation::SpecificationError& e) {
        LFATALC("main", "Loading of configuration file failed");
        for (const documentation::TestResult::Offense& o : e.result.offenses) {
            LERRORC(o.offender, ghoul::to_string(o.reason));
        }
        for (const documentation::TestResult::Warning& w : e.result.warnings) {
            LWARNINGC(w.offender, ghoul::to_string(w.reason));
        }
        exit(EXIT_FAILURE);
    }
    catch (const ghoul::RuntimeError& e) {
        // Write out all of the information about the exception and flush the logs
        LFATALC(e.component, e.message);
        if (ghoul::logging::LogManager::isInitialized()) {
            LogMgr.flushLogs();
        }
        return EXIT_FAILURE;
    }

    global::openSpaceEngine.registerPathTokens();
    global::openSpaceEngine.initialize();

    engine.addEventHandler(&handler);
    engine.addRenderHandler(&handler);
    engine.loadConfig(global::configuration.windowConfiguration);
    // Yes, this still contains the OpenSpace-specific commandline arguments, but no one
    // will ever know if we use the remaining arguments or not; both commandline parsers
    // just ignore the arguments they don't understand
    engine.initialize(argc, argv);

    setupMinVrDelegateFunctions(engine);


    // run loop-di-loop
    do {
        if (HasInitializedGL) {
            openspace::global::openSpaceEngine.preSynchronization();
            engine.synchronizeAndProcessEvents();

            engine.updateAllModels();

            // @TODO(abock): Not sure if this should be before updateAllModels or here
            openspace::global::openSpaceEngine.postSynchronizationPreDraw();
        }

        engine.renderOnAllDisplays();
    } while (!engine.getShutdown());


 
    global::openSpaceEngine.deinitializeGL();
    
    // This assumes that `shutdown` destroys the OpenGL state and thus have to happen
    // after the deinitializeGL function
    engine.shutdown();
    global::openSpaceEngine.deinitialize();

    exit(EXIT_SUCCESS);
}
