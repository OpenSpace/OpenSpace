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

#include <openspace/documentation/documentation.h>
#include <openspace/engine/configuration.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/settings.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/util/keys.h>
#include <openspace/util/mouse.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/format.h>
#include <ghoul/ghoul.h>
#include <ghoul/logging/consolelog.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/logging/visualstudiooutputlog.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <api/MinVR.h>
#include <numeric>

// @TODO:  Add Spout support
// @TODO:  Reintroduce commandline-parsing
// @TODO:  Prevent a third window to open that immediately crashes

// @TODO:  Proper check if the current instance is the master:

using namespace MinVR;
using namespace openspace;


class Handler : public VREventHandler, public VRRenderHandler, public VRInputDevice {
public:
    void onVREvent(const VRDataIndex& eventData) override;
    void onVRRenderContext(const VRDataIndex& stateData) override;
    void onVRRenderScene(const VRDataIndex& stateData) override;

    void appendNewInputEventsSinceLastCall(VRDataQueue* queue) override;
};

namespace {

constexpr std::string_view _loggerCat = "main_minvr";

VRMain engine;
Handler handler;
VRDataQueue eventQueue;

struct {
    int nWindows = 0;
    glm::ivec2 windowSize;
    glm::ivec2 framebufferSize;

    glm::vec2 mousePosition;
    uint32_t mouseButtons = 0;

    float averageDeltatime = -1.f;
    float deltaTime = -1.f;
} windowingGlobals;

struct {
    bool modifierShift = false;
    bool modifierCtrl = false;
    bool modifierAlt = false;
} keyboardState;

bool HasInitializedGL = false;
std::array<float, 30> LastFrametimes = { 1.f / 60.f }; // we can be optimistic here
constexpr std::string_view MasterNode = "/MinVR/Desktop1";
bool IsMasterNode = false;
uint64_t FrameNumber = 0;
std::chrono::time_point<std::chrono::high_resolution_clock> lastFrameTime;

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
            std::format("Received an event named {} of unknown type", eventData.getName())
        );
    }

    const bool isButtonEvent = type == "ButtonDown" || type == "ButtonUp" ||
                               type == "ButtonRepeat";

    if (type == "AnalogUpdate") {
        const VRAnalogEvent& event = static_cast<const VRAnalogEvent&>(eventData);
    }
    else if (isButtonEvent) {
        if (!global::windowDelegate->isMaster()) {
            return;
        }
        const VRButtonEvent& event = static_cast<const VRButtonEvent&>(eventData);

        const std::string& buttonName = event.getName();

        if (buttonName.size() >= 3 && buttonName.substr(0, 3) == "Kbd") {
            // We have a keyboard event
            const size_t beg = 3; // "Kbd" prefix
            const size_t sep = buttonName.find('_');

            std::string keyName = buttonName.substr(beg, sep - beg);
            std::string actionName = buttonName.substr(sep + 1);

            Key key = stringToKey(keyName).key;

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
            KM mod = KM::None;
            mod |= keyboardState.modifierShift ? KM::Shift : KM::None;
            mod |= keyboardState.modifierCtrl ? KM::Control : KM::None;
            mod |= keyboardState.modifierAlt ? KM::Alt : KM::None;

            global::openSpaceEngine->keyboardCallback(key, mod, action, IsGuiWindow::Yes);
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

            using KM = KeyModifier;
            KM mod = KM::None;
            mod |= keyboardState.modifierShift ? KM::Shift : KM::None;
            mod |= keyboardState.modifierCtrl ? KM::Control : KM::None;
            mod |= keyboardState.modifierAlt ? KM::Alt : KM::None;

            global::openSpaceEngine->mouseButtonCallback(
                button,
                action,
                mod,
                IsGuiWindow::Yes
            );
        }

    }
    else if (type == "CursorMove") {
        if (!global::windowDelegate->isMaster()) {
            return;
        }
        const VRCursorEvent& event = static_cast<const VRCursorEvent&>(eventData);

        const float* pos = event.getPos();
        windowingGlobals.mousePosition = glm::vec2(pos[0], pos[1]);
        global::openSpaceEngine->mousePositionCallback(pos[0], pos[1], IsGuiWindow::Yes);

        // @TODO(abock): Support mouse wheel
        //openspace::global::openSpaceEngine.mouseScrollWheelCallback(posX, posY);
    }
    else if (type == "TrackerMove") {
        const VRTrackerEvent& event = static_cast<const VRTrackerEvent&>(eventData);
    }
    else if (type == "OpenSpaceMessage") {
        if (global::windowDelegate->isMaster()) {
            // We don't want the message if we are the master as we already have the state
            return;
        }
        const int frameNumber = eventData.getValue("FrameNumber");
        const int nBytes = eventData.getValue("NBytes");
        std::vector<int> intData = eventData.getValue("SynchronizationData");
        std::byte* data = reinterpret_cast<std::byte*>(intData.data());

        std::vector<std::byte> synchronizationBuffer(nBytes);
        std::copy(data, data + nBytes, synchronizationBuffer.begin());

        global::openSpaceEngine->decode(std::move(synchronizationBuffer));
    }
    else {
        LERRORC("onVREvent()", std::format("Received an event of unknown type {}", type));
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

            global::openSpaceEngine->initializeGL();

            HasInitializedGL = true;
        }
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
            openspace::global::openSpaceEngine->render(
                // @TODO(abock) we should probably use the user position here?
                glm::mat4(1.f),
                viewMatrix,
                projectionMatrix
            );
            openspace::global::openSpaceEngine->drawOverlays();
            openspace::global::openSpaceEngine->postDraw();
        }
        catch (const ghoul::RuntimeError& e) {
            LERRORC(e.component, e.message);
        }
    }
}

void Handler::appendNewInputEventsSinceLastCall(VRDataQueue* queue) {
    queue->addQueue(eventQueue);
    eventQueue.clear();
}

void setupMinVrDelegateFunctions(VRMain& main) {
    // Sets up the OpenSpace WindowDelegate callback functions
    WindowDelegate* delegate = global::windowDelegate;

    delegate->nWindows = []() { return windowingGlobals.nWindows; };
    delegate->currentWindowSize = []() { return windowingGlobals.windowSize; };
    delegate->currentDrawBufferResolution = delegate->currentWindowSize;
    delegate->currentViewportSize = delegate->currentWindowSize;

    delegate->averageDeltaTime = []() -> double {
        return windowingGlobals.averageDeltatime;
    };
    delegate->deltaTime = []() -> double { return windowingGlobals.deltaTime; };


    delegate->isMaster = []() { return IsMasterNode; };

    delegate->openGLProcedureAddress = [](const char* func) {
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
    global::create();

    // Register the path of the executable,
    // to make it possible to find other files in the same directory.
    FileSys.registerPathToken(
        "${BIN}",
        absPath(argv[0]).parent_path(),
        ghoul::filesystem::FileSystem::Override::Yes
    );

    // Create the OpenSpace engine and get arguments for the SGCT engine
    std::string windowConfiguration;
    try {
        // Find configuration
        //std::string configurationFilePath = commandlineArguments.configurationName;
        LDEBUG("Finding configuration");
        std::filesystem::path configurationFilePath = findConfiguration();
        configurationFilePath = absPath(configurationFilePath);

        if (!std::filesystem::exists(configurationFilePath)) {
            LFATALC(
                "main",
                std::format("Could not find configuration: {}", configurationFilePath)
            );
            exit(EXIT_FAILURE);
        }
        LINFO(std::format("Configuration Path: '{}'", configurationFilePath));

        // Register the base path as the directory where the configuration file lives
        std::filesystem::path base = configurationFilePath.parent_path();
        FileSys.registerPathToken("${BASE}", std::move(base));

#ifdef WIN32
        glm::ivec2 size = glm::ivec2(1920, 1080);
        DEVMODEW dm = { 0 };
        dm.dmSize = sizeof(DEVMODEW);
        BOOL success = EnumDisplaySettingsW(nullptr, ENUM_CURRENT_SETTINGS, &dm);
        if (success) {
            size.x = dm.dmPelsWidth;
            size.y = dm.dmPelsHeight;
        }
#else // ^^^^ WIN32 // !WIN32 vvvv
        const glm::ivec2 size = glm::ivec2(1920, 1080);
#endif // WIN32

        // Loading configuration from disk
        LDEBUG("Loading configuration from disk");
        *global::configuration = loadConfigurationFromFile(
            configurationFilePath,
            findSettings(),
            size
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
        LDEBUG("MinVR Configuration file: " + global::configuration->windowConfiguration);

        windowConfiguration = global::configuration->windowConfiguration;
    }
    catch (const SpecificationError& e) {
        LFATALC("main", "Loading of configuration file failed");
        for (const TestResult::Offense& o : e.result.offenses) {
            LERRORC(o.offender, ghoul::to_string(o.reason));
        }
        for (const TestResult::Warning& w : e.result.warnings) {
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

    global::openSpaceEngine->registerPathTokens();
    global::openSpaceEngine->initialize();

    engine.addEventHandler(&handler);
    engine.addRenderHandler(&handler);
    engine.loadConfig(global::configuration->windowConfiguration);
    // Yes, this still contains the OpenSpace-specific commandline arguments, but no one
    // will ever know if we use the remaining arguments or not; both commandline parsers
    // just ignore the arguments they don't understand
    engine.initialize(argc, argv);

    setupMinVrDelegateFunctions(engine);

    const std::string& name = engine.getName();
    IsMasterNode = (name == MasterNode);

    if (global::windowDelegate->isMaster()) {
        engine.addInputDevice(&handler);
    }

    lastFrameTime = std::chrono::high_resolution_clock::now();
    // run loop-di-loop
    do {
        if (HasInitializedGL) {
            auto now = std::chrono::high_resolution_clock::now();
            std::chrono::nanoseconds dt = now - lastFrameTime;
            double dtSec = dt.count() / (1000.0 * 1000.0 * 1000.0);
            windowingGlobals.deltaTime = static_cast<float>(dtSec);
            std::rotate(
                LastFrametimes.begin(),
                LastFrametimes.begin() + 1,
                LastFrametimes.end()
            );
            LastFrametimes.back() = windowingGlobals.deltaTime;
            lastFrameTime = now;

            windowingGlobals.averageDeltatime = std::accumulate(
                LastFrametimes.begin(),
                LastFrametimes.end(),
                0.f
            ) / LastFrametimes.size();



            global::openSpaceEngine->preSynchronization();

            if (global::windowDelegate->isMaster()) {
                std::vector<std::byte> syncBuffer = global::openSpaceEngine->encode();
                VRDataIndex e("OpenSpace_Sync");

                e.addData("EventType", "OpenSpaceMessage");

                // Pad the buffer to a multiple of 4
                const int nBytes = syncBuffer.size();
                const int nInts = nBytes % 4 != 0 ? (nBytes / 4 + 1) : nBytes;
                syncBuffer.resize((nInts) * 4);

                // Just look away for a bit!
                int* data = reinterpret_cast<int*>(syncBuffer.data());
                std::vector<int> intData(nInts);
                std::copy(data, data + nInts, intData.begin());

                e.addData("NBytes", static_cast<int>(nBytes));
                e.addData("FrameNumber", static_cast<int>(FrameNumber));
                e.addData("SynchronizationData", intData);
                eventQueue.push(e);
            }

            engine.synchronizeAndProcessEvents();

            engine.updateAllModels();

            // @TODO(abock): Not sure if this should be before updateAllModels or here
            global::openSpaceEngine->postSynchronizationPreDraw();

            ++FrameNumber;
        }

        engine.renderOnAllDisplays();
    } while (!engine.getShutdown());

    global::openSpaceEngine->deinitializeGL();

    // This assumes that `shutdown` destroys the OpenGL state and thus have to happen
    // after the deinitializeGL function
    engine.shutdown();
    global::openSpaceEngine->deinitialize();

    exit(EXIT_SUCCESS);
}
