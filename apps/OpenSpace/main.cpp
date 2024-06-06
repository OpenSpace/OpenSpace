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

#include <openspace/documentation/documentation.h>
#include <openspace/engine/configuration.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/settings.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/interaction/joystickinputstate.h>
#include <openspace/openspace.h>
#include <ghoul/format.h>
#include <ghoul/ghoul.h>
#include <ghoul/glm.h>
#include <ghoul/cmdparser/commandlineparser.h>
#include <ghoul/cmdparser/multiplecommand.h>
#include <ghoul/cmdparser/singlecommand.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/visualstudiooutputlog.h>
#include <ghoul/misc/stacktrace.h>
#ifdef WIN32
#define GLFW_EXPOSE_NATIVE_WIN32
#endif
#include <GLFW/glfw3.h>
#include <GLFW/glfw3native.h>
#include <sgct/clustermanager.h>
#include <sgct/commandline.h>
#include <sgct/engine.h>
#include <sgct/log.h>
#include <sgct/projection/fisheye.h>
#include <sgct/projection/nonlinearprojection.h>
#include <sgct/settings.h>
#include <sgct/user.h>
#include <sgct/window.h>
#include <stb_image.h>
#include <tracy/Tracy.hpp>
#include <iostream>
#include <string_view>

#ifdef WIN32
#include <Windows.h>
#include <dbghelp.h>
#endif // WIN32

#ifdef OPENVR_SUPPORT
#include <SGCTOpenVR.h>
#endif // OPENVR_SUPPORT

#ifdef OPENSPACE_HAS_SPOUT
#include <modules/spout/spoutwrapper.h>
#endif // OPENSPACE_HAS_SPOUT

#ifdef OPENSPACE_BREAK_ON_FLOATING_POINT_EXCEPTION
#include <float.h>
#endif // OPENSPACE_BREAK_ON_FLOATING_POINT_EXCEPTION

#include <launcherwindow.h>
#include <QApplication>
#include <QMessageBox>

using namespace openspace;
using namespace sgct;

namespace {

constexpr std::string_view _loggerCat = "main";
constexpr std::string_view SpoutTag = "Spout";
constexpr std::string_view OpenVRTag = "OpenVR";

// @TODO (abock, 2020-04-09): These state variables should disappear
const Window* currentWindow = nullptr;
const BaseViewport* currentViewport = nullptr;
Frustum::Mode currentFrustumMode;
glm::mat4 currentModelMatrix;
glm::ivec2 currentDrawResolution;

#ifdef OPENVR_SUPPORT
Window* FirstOpenVRWindow = nullptr;
#endif

//
//  SPOUT-support
//

#ifdef OPENSPACE_HAS_SPOUT
/**
 * This struct stores all information about a single render window. Depending on the
 * frame setup, each window can be mono or stereo, the information of which is stored in
 * the `leftOrMain` and `right` members respectively.
 */
struct SpoutWindow {
    /// The left framebuffer (or main, if there is no stereo rendering)
    openspace::spout::SpoutSender leftOrMain;

    /// The right framebuffer
    openspace::spout::SpoutSender right;

    /// The window ID of this windows
    size_t windowId = size_t(-1);
};

/// The list of all windows with spout senders
std::vector<SpoutWindow> SpoutWindows;

#endif // OPENSPACE_HAS_SPOUT


//
//  MiniDump generation
//
#ifdef WIN32
LONG WINAPI generateMiniDump(EXCEPTION_POINTERS* exceptionPointers) {
    SYSTEMTIME stLocalTime;
    GetLocalTime(&stLocalTime);

    LFATAL("Printing Stack Trace that lead to the crash:");
    std::vector<std::string> stackTrace = ghoul::stackTrace();
    for (const std::string& s : stackTrace) {
        LINFO(s);
    }

    std::string dumpFile = std::format(
        "OpenSpace_{}_{}_{}-{}-{}-{}-{}-{}-{}--{}--{}.dmp",
        OPENSPACE_VERSION_MAJOR,
        OPENSPACE_VERSION_MINOR,
        OPENSPACE_VERSION_PATCH,
        stLocalTime.wYear,
        stLocalTime.wMonth,
        stLocalTime.wDay,
        stLocalTime.wHour,
        stLocalTime.wMinute,
        stLocalTime.wSecond,
        GetCurrentProcessId(),
        GetCurrentThreadId()
    );

    LINFO(std::format("Creating dump file: {}", dumpFile));

    HANDLE hDumpFile = CreateFileA(
        dumpFile.c_str(),
        GENERIC_READ | GENERIC_WRITE,
        FILE_SHARE_WRITE | FILE_SHARE_READ,
        nullptr,
        CREATE_ALWAYS,
        0,
        nullptr
    );

    MINIDUMP_EXCEPTION_INFORMATION exceptionParameter;
    exceptionParameter.ThreadId = GetCurrentThreadId();
    exceptionParameter.ExceptionPointers = exceptionPointers;
    exceptionParameter.ClientPointers = TRUE;

    BOOL success = MiniDumpWriteDump(
        GetCurrentProcess(),
        GetCurrentProcessId(),
        hDumpFile,
        MiniDumpWithDataSegs,
        &exceptionParameter,
        nullptr,
        nullptr
    );

    CloseHandle(hDumpFile);

    if (success) {
        LINFO("Created successfully");
    }
    else {
        LERROR("Dumpfile created unsuccessfully");
    }

    return EXCEPTION_EXECUTE_HANDLER;
}
#endif // WIN32

void checkJoystickStatus() {
    using namespace interaction;

    for (int i = GLFW_JOYSTICK_1; i <= GLFW_JOYSTICK_LAST; i++) {
        ZoneScopedN("Joystick state");

        JoystickInputState& state = global::joystickInputStates->at(i);

        const int present = glfwJoystickPresent(i);
        if (present == GLFW_FALSE) {
            state.isConnected = false;
            continue;
        }

        if (!state.isConnected) {
            // Joystick was added
            state.isConnected = true;
            state.name = glfwGetJoystickName(i);

            // Check axes and buttons
            glfwGetJoystickAxes(i, &state.nAxes);
            glfwGetJoystickButtons(i, &state.nButtons);
            state.axes.resize(state.nAxes);
            state.buttons.resize(state.nButtons);

            std::fill(state.axes.begin(), state.axes.end(), 0.f);
            std::fill(state.buttons.begin(), state.buttons.end(), JoystickAction::Idle);
        }

        const float* axes = glfwGetJoystickAxes(i, &state.nAxes);
        std::memcpy(state.axes.data(), axes, state.nAxes * sizeof(float));

        const unsigned char* buttons = glfwGetJoystickButtons(i, &state.nButtons);
        for (int j = 0; j < state.nButtons; j++) {
            const bool currentlyPressed = buttons[j] == GLFW_PRESS;

            if (currentlyPressed) {
                switch (state.buttons[j]) {
                    case JoystickAction::Idle:
                    case JoystickAction::Release:
                        state.buttons[j] = JoystickAction::Press;
                        break;
                    case JoystickAction::Press:
                    case JoystickAction::Repeat:
                        state.buttons[j] = JoystickAction::Repeat;
                        break;
                }
            }
            else {
                switch (state.buttons[j]) {
                    case JoystickAction::Idle:
                    case JoystickAction::Release:
                        state.buttons[j] = JoystickAction::Idle;
                        break;
                    case JoystickAction::Press:
                    case JoystickAction::Repeat:
                        state.buttons[j] = JoystickAction::Release;
                        break;
                }
            }
        }
    }
}

bool isGuiWindow(sgct::Window* window) {
    if (global::windowDelegate->hasGuiWindow()) {
        return window->hasTag("GUI");
    }

    const sgct::Window* first = Engine::instance().windows().front().get();
    return window->id() == first->id();
}

//
//  Init function
//
void mainInitFunc(GLFWwindow*) {
    ZoneScoped;

    LTRACE("main::mainInitFunc(begin)");

    //
    //  Screenshots
    //
    // We save the startup value of the screenshots just in case we want to add a date
    // to them later in the RenderEngine
    std::filesystem::path screenshotPath = absPath("${SCREENSHOTS}");
    sgct::Settings::instance().setCapturePath(screenshotPath);
    FileSys.registerPathToken("${STARTUP_SCREENSHOT}", std::move(screenshotPath));

    LDEBUG("Initializing OpenSpace Engine started");
    global::openSpaceEngine->initialize();
    LDEBUG("Initializing OpenSpace Engine finished");

#ifndef __APPLE__
    // Apparently: "Cocoa: Regular windows do not have icons on macOS"
    {
        const std::filesystem::path path = absPath("${DATA}/openspace-icon.png");
        int x = 0;
        int y = 0;
        int n = 0;
        const std::string p = path.string();
        unsigned char* data = stbi_load(p.c_str(), &x, &y, &n, 0);

        GLFWimage icon;
        icon.pixels = data;
        icon.width = x;
        icon.height = y;

        for (const std::unique_ptr<Window>& window : Engine::instance().windows()) {
            glfwSetWindowIcon(window->windowHandle(), 1, &icon);
        }

        stbi_image_free(icon.pixels);
    }
#endif // __APPLE__

    currentWindow = Engine::instance().windows().front().get();
    currentViewport = currentWindow->viewports().front().get();

    LDEBUG("Initializing OpenGL in OpenSpace Engine started");
    global::openSpaceEngine->initializeGL();
    LDEBUG("Initializing OpenGL in OpenSpace Engine finished");



    // Find if we have at least one OpenVR window
    // Save reference to first OpenVR window, which is the one we will copy to the HMD.
    for (const std::unique_ptr<Window>& window : Engine::instance().windows()) {
        if (window->hasTag(OpenVRTag)) {
#ifdef OPENVR_SUPPORT
            FirstOpenVRWindow = window.get();

            // If we have an OpenVRWindow, initialize OpenVR.
            sgct::OpenVR::initialize(
                Engine::instance().nearClippingPlane(),
                Engine::instance().farClippingPlane()
            );
#else
            LWARNING("OpenVR was requested, but program was compiled without VR support");
#endif

            break;
        }
    }

    for (size_t i = 0; i < Engine::instance().windows().size(); i++) {
        const Window& window = *Engine::instance().windows()[i];
        if (!window.hasTag(SpoutTag)) {
            continue;
        }

#ifdef OPENSPACE_HAS_SPOUT
        SpoutWindow w;
        bool retValue = true;
        std::string mainWindowName = window.name();

        const Window::StereoMode sm = window.stereoMode();
        const bool hasStereo = (sm != Window::StereoMode::NoStereo) &&
                               (sm < Window::StereoMode::SideBySide);

        if (hasStereo) {
            mainWindowName = window.name() + "_left";
            retValue &= w.right.updateSenderName((window.name() + "_right").c_str());
            retValue &= w.right.updateSenderSize(
                window.framebufferResolution().x,
                window.framebufferResolution().y
            );
        }

        retValue &= w.leftOrMain.updateSenderName(mainWindowName.c_str());
        retValue &= w.leftOrMain.updateSenderSize(
            window.framebufferResolution().x,
            window.framebufferResolution().y
        );

        w.windowId = i;

        if (retValue) {
            SpoutWindows.push_back(std::move(w));
        }
#else
        LWARNING("Spout was requested, but program was compiled without Spout support");
#endif // OPENSPACE_HAS_SPOUT
    }

    // Query joystick status, those connected before start up
    checkJoystickStatus();

    LTRACE("main::mainInitFunc(end)");
}



void mainPreSyncFunc() {
    ZoneScoped;
    LTRACE("main::mainPreSyncFunc(begin)");

    try {
        global::openSpaceEngine->preSynchronization();
    }
    catch (const ghoul::RuntimeError& e) {
        LFATALC(e.component, e.message);
        Engine::instance().terminate();
    }

    // Query joystick status, those connected at run time
    checkJoystickStatus();

    LTRACE("main::mainPreSyncFunc(end)");
}



void mainPostSyncPreDrawFunc() {
    ZoneScoped;

    LTRACE("main::postSynchronizationPreDraw(begin)");

    global::openSpaceEngine->postSynchronizationPreDraw();

#ifdef OPENVR_SUPPORT
    if (FirstOpenVRWindow) {
        // Update pose matrices for all tracked OpenVR devices once per frame
        sgct::SGCTOpenVR::updatePoses();
    }
#endif // OPENVR_SUPPORT

    LTRACE("main::postSynchronizationPreDraw(end)");
}



void mainRenderFunc(const sgct::RenderData& data) {
    ZoneScoped;

    LTRACE("main::mainRenderFunc(begin)");

    currentWindow = &data.window;
    currentViewport = &data.viewport;
    currentFrustumMode = data.frustumMode;
    currentDrawResolution = glm::ivec2(data.bufferSize.x, data.bufferSize.y);

    glm::vec3 pos;
    std::memcpy(glm::value_ptr(pos), &Engine::defaultUser().posMono().x, sizeof(vec3));

    glm::mat4 viewMatrix;
    std::memcpy(
        glm::value_ptr(viewMatrix),
        data.viewMatrix.values,
        sizeof(mat4)
    );
    viewMatrix = viewMatrix * glm::translate(glm::mat4(1.f), pos);

    glm::mat4 projectionMatrix;
    std::memcpy(
        glm::value_ptr(projectionMatrix),
        data.projectionMatrix.values,
        sizeof(mat4)
    );
#ifdef OPENVR_SUPPORT
    bool currentWindowIsHMD = FirstOpenVRWindow == SgctEngine->getCurrentWindowPtr();
    if (sgct::SGCTOpenVR::isHMDActive() && currentWindowIsHMD) {
        projectionMatrix = sgct::SGCTOpenVR::getHMDCurrentViewProjectionMatrix(
            SgctEngine->getCurrentFrustumMode()
        );
    }
#endif

    try {
        glm::mat4 modelMatrix;
        std::memcpy(
            glm::value_ptr(modelMatrix),
            data.modelMatrix.values,
            sizeof(mat4)
        );
        currentModelMatrix = modelMatrix;
        global::openSpaceEngine->render(modelMatrix, viewMatrix, projectionMatrix);

#ifdef OPENSPACE_HAS_SPOUT
        for (SpoutWindow& w : SpoutWindows) {
            sgct::Window& window = *Engine::instance().windows()[w.windowId];
            int width = window.framebufferResolution().x;
            int height = window.framebufferResolution().y;

            w.leftOrMain.saveGLState();

            if (w.leftOrMain.isCreated() && w.leftOrMain.updateSenderSize(width, height))
            {
                GLuint texId = window.frameBufferTexture(Window::TextureIndex::LeftEye);
                w.leftOrMain.updateSender(texId, static_cast<int>(GL_TEXTURE_2D));
            }

            if (w.right.isCreated() && w.right.updateSenderSize(width, height)) {
                GLuint texId = window.frameBufferTexture(Window::TextureIndex::RightEye);
                w.right.updateSender(texId, static_cast<int>(GL_TEXTURE_2D));
            }

            w.leftOrMain.restoreGLState();
        }
#endif // OPENSPACE_HAS_SPOUT
    }
    catch (const ghoul::RuntimeError& e) {
        LERRORC(e.component, e.message);
    }

    LTRACE("main::mainRenderFunc(end)");
}



void mainDraw2DFunc(const sgct::RenderData& data) {
    ZoneScoped;
    LTRACE("main::mainDraw2DFunc(begin)");

    currentWindow = &data.window;
    currentViewport = &data.viewport;
    currentFrustumMode = data.frustumMode;
    currentDrawResolution = glm::ivec2(data.bufferSize.x, data.bufferSize.y);

    try {
        global::openSpaceEngine->drawOverlays();
    }
    catch (const ghoul::RuntimeError& e) {
        LERRORC(e.component, e.message);
    }

    // SGCT gets angry if we change this in our function
    glEnable(GL_BLEND);
    glDisable(GL_CULL_FACE);
    glDisable(GL_DEPTH_TEST);

    LTRACE("main::mainDraw2DFunc(end)");
}



void mainPostDrawFunc() {
    ZoneScoped;
    LTRACE("main::mainPostDrawFunc(begin)");

#ifdef OPENVR_SUPPORT
    if (FirstOpenVRWindow) {
        // Copy the first OpenVR window to the HMD
        sgct::OpenVR::copyWindowToHMD(FirstOpenVRWindow);
    }
#endif // OPENVR_SUPPORT

    global::openSpaceEngine->postDraw();

    LTRACE("main::mainPostDrawFunc(end)");
}



void mainKeyboardCallback(sgct::Key key, sgct::Modifier modifiers, sgct::Action action,
                          int, sgct::Window* window)
{
    ZoneScoped;
    LTRACE("main::mainKeyboardCallback(begin)");

    const openspace::Key k = openspace::Key(key);
    const KeyModifier m = KeyModifier(modifiers);
    const KeyAction a = KeyAction(action);
    const IsGuiWindow isGui = IsGuiWindow(isGuiWindow(window));

    global::openSpaceEngine->keyboardCallback(k, m, a, isGui);

    LTRACE("main::mainKeyboardCallback(begin)");
}



void mainMouseButtonCallback(sgct::MouseButton key, sgct::Modifier modifiers,
                             sgct::Action action, sgct::Window* window)
{
    ZoneScoped;
    LTRACE("main::mainMouseButtonCallback(begin)");

    const openspace::MouseButton k = openspace::MouseButton(key);
    const openspace::MouseAction a = openspace::MouseAction(action);
    const openspace::KeyModifier m = openspace::KeyModifier(modifiers);
    const IsGuiWindow isGui = IsGuiWindow(isGuiWindow(window));

    global::openSpaceEngine->mouseButtonCallback(k, a, m, isGui);

    LTRACE("main::mainMouseButtonCallback(end)");
}



void mainMousePosCallback(double x, double y, sgct::Window* window) {
    ZoneScoped;
    const IsGuiWindow isGui = IsGuiWindow(isGuiWindow(window));
    global::openSpaceEngine->mousePositionCallback(x, y, isGui);
}



void mainMouseScrollCallback(double posX, double posY, sgct::Window* window) {
    ZoneScoped;
    LTRACE("main::mainMouseScrollCallback(begin");

    const IsGuiWindow isGui = IsGuiWindow(isGuiWindow(window));
    global::openSpaceEngine->mouseScrollWheelCallback(posX, posY, isGui);

    LTRACE("main::mainMouseScrollCallback(end)");
}



void mainCharCallback(unsigned int codepoint, int modifiers, sgct::Window* window) {
    ZoneScoped;

    const KeyModifier m = KeyModifier(modifiers);
    const IsGuiWindow isGui = IsGuiWindow(isGuiWindow(window));

    global::openSpaceEngine->charCallback(codepoint, m, isGui);
}



void mainDropCallback(const std::vector<std::string_view>& paths) {
    ZoneScoped;

    for (const std::string_view path : paths) {
        global::openSpaceEngine->handleDragDrop(path);
    }
}



std::vector<std::byte> mainEncode() {
    ZoneScoped;
    LTRACE("main::mainEncode(begin)");

    std::vector<std::byte> data = global::openSpaceEngine->encode();

    LTRACE("main::mainEncode(end)");
    return data;
}



void mainDecode(const std::vector<std::byte>& data) {
    ZoneScoped;
    LTRACE("main::mainDecode(begin)");

    global::openSpaceEngine->decode(data);

    LTRACE("main::mainDecode(end)");
}



void mainLogCallback(Log::Level level, std::string_view message) {
    ZoneScoped;

    switch (level) {
        case Log::Level::Debug:
            LDEBUGC("SGCT", message);
            break;
        case Log::Level::Info:
            LINFOC("SGCT", message);
            break;
        case Log::Level::Warning:
            LWARNINGC("SGCT", message);
            break;
        case Log::Level::Error:
            LERRORC("SGCT", message);
            break;
    }
}


void setSgctDelegateFunctions() {
    WindowDelegate& sgctDelegate = *global::windowDelegate;
    sgctDelegate.terminate = []() { Engine::instance().terminate(); };
    sgctDelegate.setBarrier = [](bool enabled) {
        ZoneScoped;

        sgct::Window::setBarrier(enabled);
    };
    sgctDelegate.setSynchronization = [](bool enabled) {
        ZoneScoped;

        sgct::ClusterManager::instance().setUseIgnoreSync(enabled);
    };
    sgctDelegate.windowHasResized = []() {
        ZoneScoped;

        return currentWindow->isWindowResized();
    };
    sgctDelegate.averageDeltaTime = []() {
        ZoneScoped;

        return Engine::instance().statistics().avgDt();
    };
    sgctDelegate.minDeltaTime = []() {
        ZoneScoped;

        return Engine::instance().statistics().minDt();
    };
    sgctDelegate.maxDeltaTime = []() {
        ZoneScoped;

        return Engine::instance().statistics().maxDt();
    };
    sgctDelegate.deltaTime = []() {
        ZoneScoped;

        return Engine::instance().statistics().dt();
    };
    sgctDelegate.applicationTime = []() {
        ZoneScoped;

        return time();
    };
    sgctDelegate.currentWindowSize = []() {
        ZoneScoped;

        return glm::ivec2(currentWindow->resolution().x, currentWindow->resolution().y);
    };
    sgctDelegate.currentSubwindowSize = []() {
        ZoneScoped;

        switch (currentWindow->stereoMode()) {
            case Window::StereoMode::SideBySide:
            case Window::StereoMode::SideBySideInverted:
                return glm::ivec2(
                    currentWindow->resolution().x / 2,
                    currentWindow->resolution().y
                );
            case Window::StereoMode::TopBottom:
            case Window::StereoMode::TopBottomInverted:
                return glm::ivec2(
                    currentWindow->resolution().x,
                    currentWindow->resolution().y / 2
                );
            default:
                return glm::ivec2(
                    currentWindow->resolution().x * currentViewport->size().x,
                    currentWindow->resolution().y * currentViewport->size().y
                );
        }
    };
    sgctDelegate.currentDrawBufferResolution = []() {
        ZoneScoped;

        const Viewport* viewport = dynamic_cast<const Viewport*>(currentViewport);
        if (viewport) {
            if (viewport->hasSubViewports() && viewport->nonLinearProjection()) {
                const ivec2 dim = viewport->nonLinearProjection()->cubemapResolution();
                return glm::ivec2(dim.x, dim.y);
            }
            else {
                const ivec2 dim = currentWindow->finalFBODimensions();
                return glm::ivec2(dim.x, dim.y);
            }
        }
        else {
            return currentDrawResolution;
        }
    };
    sgctDelegate.currentViewportSize = []() {
        ZoneScoped;

        if (currentViewport) {
            const vec2 size = currentViewport->size();
            return glm::ivec2(size.x, size.y);
        }
        return glm::ivec2(-1, -1);
    };
    sgctDelegate.currentViewportResolution = []() {
        ZoneScoped;

        if (currentViewport) {
            const ivec2 res = currentWindow->resolution();
            const vec2 size = currentViewport->size();
            return glm::ivec2(size.x * res.x, size.y * res.y);
        }
        return glm::ivec2(-1, -1);
    };
    sgctDelegate.dpiScaling = []() {
        ZoneScoped;

        const vec2 scale = currentWindow->scale();
        return glm::vec2(scale.x, scale.y);
    };
    sgctDelegate.firstWindowResolution = []() {
        ZoneScoped;
        sgct::Window* window = Engine::instance().windows().front().get();
        return glm::ivec2(window->resolution().x, window->resolution().y);
    };
    sgctDelegate.guiWindowResolution = []() {
        ZoneScoped;
        const Window* guiWindow = nullptr;
        for (const std::unique_ptr<Window>& window : Engine::instance().windows()) {
            if (window->hasTag("GUI")) {
                guiWindow = window.get();
                break;
            }
        }

        if (!guiWindow) {
            guiWindow = Engine::instance().windows().front().get();
        }

        return glm::ivec2(guiWindow->resolution().x, guiWindow->resolution().y);
    };
    sgctDelegate.osDpiScaling = []() {
        ZoneScoped;

        // Detect which DPI scaling to use
        // 1. If there is a GUI window, use the GUI window's content scale value
        const Window* dpiWindow = nullptr;
        for (const std::unique_ptr<Window>& window : Engine::instance().windows()) {
            if (window->hasTag("GUI")) {
                dpiWindow = window.get();
                break;
            }
        }

        // 2. If there isn't a GUI window, use the first window's value
        if (!dpiWindow) {
            dpiWindow = Engine::instance().windows().front().get();
        }

        glm::vec2 scale = glm::vec2(1.f, 1.f);
        glfwGetWindowContentScale(dpiWindow->windowHandle(), &scale.x, &scale.y);

        if (scale.x != scale.y) {
            LWARNING(std::format(
                "Non-square window scaling detected ({0}x{1}), using {0}x{0} instead",
                scale.x, scale.y
            ));
        }

        return scale.x;
    };
    sgctDelegate.hasGuiWindow = []() {
        ZoneScoped;

        for (const std::unique_ptr<Window>& window : Engine::instance().windows()) {
            if (window->hasTag("GUI")) {
                return true;
            }
        }
        return false;
    };
    sgctDelegate.isGuiWindow = []() {
        ZoneScoped;

        return currentWindow->hasTag("GUI");
    };
    sgctDelegate.isMaster = []() {
        ZoneScoped;

        return Engine::instance().isMaster();
    };
    sgctDelegate.modelMatrix = []() {
        ZoneScoped;

        return currentModelMatrix;
    };
    sgctDelegate.setNearFarClippingPlane = [](float nearPlane, float farPlane) {
        ZoneScoped;

        Engine::instance().setNearAndFarClippingPlanes(nearPlane, farPlane);
    };
    sgctDelegate.isFisheyeRendering = []() {
        ZoneScoped;

        return dynamic_cast<FisheyeProjection*>(
            currentWindow->viewports().front()->nonLinearProjection()
        ) != nullptr;
    };
    sgctDelegate.takeScreenshot = [](bool applyWarping, std::vector<int> windowIds) {
        ZoneScoped;

        sgct::Settings::instance().setCaptureFromBackBuffer(applyWarping);
        Engine::instance().takeScreenshot(std::move(windowIds));
        return Engine::instance().screenShotNumber();
    };
    sgctDelegate.resetScreenshotNumber = []() {
        ZoneScoped;
        Engine::instance().resetScreenshotNumber();
    };
    sgctDelegate.swapBuffer = []() {
        ZoneScoped;

        GLFWwindow* w = glfwGetCurrentContext();
        glfwSwapBuffers(w);
        glfwPollEvents();
    };
    sgctDelegate.nWindows = []() {
        ZoneScoped;

        return static_cast<int>(Engine::instance().windows().size());
    };
    sgctDelegate.currentWindowId = []() {
        ZoneScoped;

        return currentWindow->id();
    };
    sgctDelegate.firstWindowId = []() {
        ZoneScoped;

        return Engine::instance().windows().front()->id();
    };
    sgctDelegate.openGLProcedureAddress = [](const char* func) {
        ZoneScoped;

        return glfwGetProcAddress(func);
    };
    sgctDelegate.getHorizFieldOfView = []() {
        ZoneScoped;

        return static_cast<double>(
            Engine::instance().windows().front()->horizFieldOfViewDegrees()
        );
    };
    sgctDelegate.setHorizFieldOfView = [](float hFovDeg) {
        ZoneScoped;

        for (std::unique_ptr<sgct::Window> const& w : Engine::instance().windows()) {
            w->setHorizFieldOfView(hFovDeg);
        }
    };
    #ifdef WIN32
    sgctDelegate.getNativeWindowHandle = [](size_t windowIndex) -> void* {
        ZoneScoped;

        Window* w = Engine::instance().windows()[windowIndex].get();
        if (w) {
            HWND hWnd = glfwGetWin32Window(w->windowHandle());
            return reinterpret_cast<void*>(hWnd);
        }
        return nullptr;
    };
    #endif // WIN32
    sgctDelegate.frustumMode = []() {
        ZoneScoped;

        switch (currentFrustumMode) {
            default:
            case Frustum::Mode::MonoEye: return WindowDelegate::Frustum::Mono;
            case Frustum::Mode::StereoLeftEye: return WindowDelegate::Frustum::LeftEye;
            case Frustum::Mode::StereoRightEye: return WindowDelegate::Frustum::RightEye;
        }
    };
    sgctDelegate.swapGroupFrameNumber = []() -> uint64_t {
        ZoneScoped;
        return sgct::Window::swapGroupFrameNumber();
    };
    sgctDelegate.setScreenshotFolder = [](std::filesystem::path path) {
        sgct::Settings::instance().setCapturePath(std::move(path));
    };
    sgctDelegate.showStatistics = [](bool enabled) {
        Engine::instance().setStatsGraphVisibility(enabled);
    };
    sgctDelegate.numberOfNodes = []() {
        return ClusterManager::instance().numberOfNodes();
    };
    sgctDelegate.currentNode = []() {
        return ClusterManager::instance().thisNodeId();
    };
    sgctDelegate.mousePositionViewportRelative = [](const glm::vec2& mousePosition) {
        for (const std::unique_ptr<Window>& window : Engine::instance().windows()) {
            if (!isGuiWindow(window.get())) {
                continue;
            }

            const sgct::ivec2 res = window->resolution();
            for (const std::unique_ptr<Viewport>& viewport : window->viewports()) {
                const sgct::vec2 pos = viewport->position();
                const sgct::vec2 size = viewport->size();
                const glm::vec4 bounds = glm::vec4(
                    pos.x * res.x,
                    (1.0 - pos.y - size.y) * res.y,
                    (pos.x + size.x) * res.x,
                    (1.0 - pos.y) * res.y
                );

                if ((mousePosition.x >= bounds.x && mousePosition.x <= bounds.z) &&
                    (mousePosition.y >= bounds.y && mousePosition.y <= bounds.w))
                {
                    return glm::vec2(
                        res.x * (mousePosition.x - bounds.x) / (bounds.z - bounds.x),
                        res.y * (mousePosition.y - bounds.y) / (bounds.w - bounds.y)
                    );
                }
            }
        }

        return mousePosition;
    };
}

std::string setWindowConfigPresetForGui(const std::string& labelFromCfgFile,
                                        bool haveCliSGCTConfig)
{
    openspace::Configuration& config = *global::configuration;

    std::string preset;
    const bool sgctCfgFileSpecifiedByLua = !config.sgctConfigNameInitialized.empty();
    if (haveCliSGCTConfig) {
        preset = std::format("{} (from CLI)", config.windowConfiguration);
    }
    else if (sgctCfgFileSpecifiedByLua) {
        preset = config.sgctConfigNameInitialized + labelFromCfgFile;
    }
    else {
        preset = config.windowConfiguration;
    }
    return preset;
}

std::string selectedSgctProfileFromLauncher(LauncherWindow& lw, bool hasCliSGCTConfig,
                                            const std::string& windowConfiguration,
                                            const std::string& labelFromCfgFile)
{
    std::string config = windowConfiguration;
    if (!hasCliSGCTConfig) {
        config = lw.selectedWindowConfig();
        if (config.find(labelFromCfgFile) != std::string::npos) {
            if (config.find("sgct.config") == std::string::npos) {
                config = config.substr(0, config.length() - labelFromCfgFile.length());
            }
            else {
                config = windowConfiguration;
            }
        }
        else {
            const std::filesystem::path c = absPath(config);

            std::filesystem::path cj = c;
            cj.replace_extension(".json");

            if (c.extension().empty()) {
                if (std::filesystem::exists(cj)) {
                    config += ".json";
                }
                else {
                    throw ghoul::RuntimeError(std::format(
                        "Error loading configuration file '{}'. File could not be found",
                        config
                    ));
                }
            }
            else {
                // user customized SGCT config
            }
        }
        global::configuration->windowConfiguration = config;
    }
    return config;
}

} // namespace


int main(int argc, char* argv[]) {
    ZoneScoped;

#ifdef OPENSPACE_BREAK_ON_FLOATING_POINT_EXCEPTION
    _clearfp();
    _controlfp(_controlfp(0, 0) & ~(_EM_ZERODIVIDE | _EM_OVERFLOW), _MCW_EM);
#endif // OPENSPACE_BREAK_ON_FLOATING_POINT_EXCEPTION

#ifdef WIN32
    SetUnhandledExceptionFilter(generateMiniDump);
#endif // WIN32

    // Initialize the LogManager and add the console log as this will be used every time
    // and we need a fall back if something goes wrong between here and when we add the
    // logs from the configuration file. If the user requested as specific loglevel in the
    // configuration file, we will deinitialize this LogManager and reinitialize it later
    // with the correct LogLevel
    {
        using namespace ghoul::logging;
        LogManager::initialize(LogLevel::Debug, LogManager::ImmediateFlush::Yes);
#ifdef WIN32
        if (IsDebuggerPresent()) {
            LogMgr.addLog(std::make_unique<ghoul::logging::VisualStudioOutputLog>());
        }
#endif // WIN32
    }

    ghoul::initialize();
    global::create();

    // Register the path of the executable,
    // to make it possible to find other files in the same directory.
    FileSys.registerPathToken(
        "${BIN}",
        std::filesystem::current_path() / std::filesystem::path(argv[0]).parent_path(),
        ghoul::filesystem::FileSystem::Override::Yes
    );
    LDEBUG(std::format("Registering ${{BIN}} to '{}'", absPath("${BIN}")));

    //
    // Parse commandline arguments
    //
    char* prgName = argv[0];
    ghoul::cmdparser::CommandlineParser parser(
        std::string(prgName),
        ghoul::cmdparser::CommandlineParser::AllowUnknownCommands::Yes
    );

    CommandlineArguments commandlineArguments;
    parser.addCommand(std::make_unique<ghoul::cmdparser::SingleCommand<std::string>>(
        commandlineArguments.configuration, "--file", "-f",
        "Provides the path to the OpenSpace configuration file. Only the '${TEMPORARY}' "
        "path token is available and any other path has to be specified relative to the "
        "current working directory"
    ));
    parser.addCommand(std::make_unique<ghoul::cmdparser::SingleCommand<std::string>>(
        commandlineArguments.windowConfig, "--config", "-c",
        "Specifies the window configuration file that should be used to start OpenSpace "
        "and that will override whatever is specified in the `openspace.cfg` or the "
        "settings. This value can include path tokens, so for example "
        "`${CONFIG}/single.json` is a valid value."
    ));
    parser.addCommand(std::make_unique<ghoul::cmdparser::SingleCommand<std::string>>(
        commandlineArguments.profile, "--profile", "-p",
        "Specifies the profile that should be used to start OpenSpace and that overrides "
        "the profile specified in the `openspace.cfg` and the settings."
    ));
    parser.addCommand(std::make_unique<ghoul::cmdparser::SingleCommand<std::string>>(
        commandlineArguments.propertyVisibility, "--propertyVisibility", "",
        "Specifies UI visibility settings for properties that this OpenSpace is using. "
        "This value overrides the values specified in the `openspace.cfg` and the "
        "settings and also the environment variable, if that value is provided. Allowed "
        "values for this parameter are: `Developer`, `AdvancedUser`, `User`, and "
        "`NoviceUser`."
    ));
    parser.addCommand(std::make_unique<ghoul::cmdparser::SingleCommandZeroArguments>(
        commandlineArguments.bypassLauncher, "--bypassLauncher", "-b",
        "Specifies whether the Launcher should be shown at startup or not. This value "
        "overrides the value specified in the `openspace.cfg` and the settings."
    ));

    // setCommandLine returns a reference to the vector that will be filled later
    const std::vector<std::string>& sgctArguments = parser.setCommandLine(
        { argv, argv + argc }
    );

    try {
        const bool showHelp = parser.execute();
        if (showHelp) {
            std::cout << parser.helpText();
            exit(EXIT_SUCCESS);
        }
    }
    catch (const ghoul::RuntimeError& e) {
        LFATALC(e.component, e.message);
        exit(EXIT_FAILURE);
    }
    // Take an actual copy of the arguments
    std::vector<std::string> arguments = sgctArguments;

    //
    // Set up SGCT functions for window delegate
    //
    setSgctDelegateFunctions();

    // Create the OpenSpace engine and get arguments for the SGCT engine
    std::string windowConfiguration;
    try {
        // Find configuration
        std::filesystem::path configurationFilePath;
        if (commandlineArguments.configuration.has_value()) {
            configurationFilePath = absPath(*commandlineArguments.configuration);
        }
        else {
            LDEBUG("Finding configuration");
            configurationFilePath = findConfiguration();
        }

        if (!std::filesystem::is_regular_file(configurationFilePath)) {
            LFATALC(
                "main",
                std::format("Could not find configuration '{}'", configurationFilePath)
            );
            exit(EXIT_FAILURE);
        }
        LINFO(std::format("Configuration Path '{}'", configurationFilePath));

        // Register the base path as the directory where the configuration file lives
        std::filesystem::path base = configurationFilePath.parent_path();
        FileSys.registerPathToken("${BASE}", std::move(base));

        // The previous incarnation of this was initializing GLFW to get the primary
        // monitor's resolution, but that had some massive performance implications as
        // there was some issue with the swap buffer handling inside of GLFW. My
        // assumption is that GLFW doesn't like being initialized, destroyed, and then
        // initialized again. Therefore we are using the platform specific functions now
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

        // Override configuration with commandline arguments
        if (commandlineArguments.windowConfig.has_value()) {
            global::configuration->windowConfiguration =
                *commandlineArguments.windowConfig;
        }
        if (commandlineArguments.profile.has_value()) {
            global::configuration->profile = *commandlineArguments.profile;
        }
        if (commandlineArguments.propertyVisibility.has_value()) {
            if (commandlineArguments.propertyVisibility == "NoviceUser") {
                global::configuration->propertyVisibility =
                    properties::Property::Visibility::NoviceUser;
            }
            else if (commandlineArguments.propertyVisibility == "User") {
                global::configuration->propertyVisibility =
                    properties::Property::Visibility::User;
            }
            else if (commandlineArguments.propertyVisibility == "AdvancedUser") {
                global::configuration->propertyVisibility =
                    properties::Property::Visibility::AdvancedUser;
            }
            else if (commandlineArguments.propertyVisibility == "Developer") {
                global::configuration->propertyVisibility =
                    properties::Property::Visibility::Developer;
            }
            else {
                throw ghoul::RuntimeError(std::format(
                    "Unknown property visibility value '{}'",
                    *commandlineArguments.propertyVisibility
                ));
            }
        }
        if (commandlineArguments.bypassLauncher.has_value()) {
            global::configuration->bypassLauncher = *commandlineArguments.bypassLauncher;
        }

        // Determining SGCT configuration file
        LDEBUG("SGCT Configuration file: " + global::configuration->windowConfiguration);

        windowConfiguration = global::configuration->windowConfiguration;
    }
    catch (const documentation::SpecificationError& e) {
        LFATALC("main", "Loading of configuration file failed");
        logError(e);
        ghoul::deinitialize();
        exit(EXIT_FAILURE);
    }
    catch (const ghoul::RuntimeError& e) {
        // Write out all of the information about the exception and flush the logs
        LFATALC(e.component, e.message);
        if (ghoul::logging::LogManager::isInitialized()) {
            LogMgr.flushLogs();
        }
        ghoul::deinitialize();
        return EXIT_FAILURE;
    }

    global::openSpaceEngine->registerPathTokens();

    // Call profile GUI
    const std::string labelFromCfgFile = " (from .cfg)";
    std::string windowCfgPreset = setWindowConfigPresetForGui(
        labelFromCfgFile,
        commandlineArguments.windowConfig.has_value()
    );

    //TODO consider LFATAL if ${USER} doens't exist rather then recurisve create.
    global::openSpaceEngine->createUserDirectoriesIfNecessary();

    // (abock, 2020-12-07)  For some reason on Apple the keyboard handler in CEF will call
    // the Qt one even if the QApplication was destroyed, leading to invalid memory
    // access.  The only way we could fix this for the release was to keep the
    // QApplication object around until the end of the program.  Even though the Qt
    // keyboard handler gets called, it doesn't do anything so everything still works.
#ifdef __APPLE__
    int qac = 0;
    QApplication app(qac, nullptr);
#endif // __APPLE__

    if (!global::configuration->bypassLauncher) {
#ifndef __APPLE__
        int qac = 0;
        QApplication app(qac, nullptr);
#endif // __APPLE__

        std::string pwd = std::filesystem::current_path().string();
        if (const size_t it = pwd.find_first_of("'\"[]");  it != std::string::npos) {
            QMessageBox::warning(
                nullptr,
                "OpenSpace",
                QString::fromStdString(std::format(
                    "The OpenSpace folder is started from must not contain any of \"'\", "
                    "\"\"\", [, or ]. Path is: {}. Unexpected errors will occur when "
                    "proceeding to run the software", pwd
                ))
            );
        }

        LauncherWindow win = LauncherWindow(
            !commandlineArguments.profile.has_value(),
            *global::configuration,
            !commandlineArguments.windowConfig.has_value(),
            std::move(windowCfgPreset),
            nullptr
        );
        win.show();
        QApplication::exec();

        if (!win.wasLaunchSelected()) {
            exit(EXIT_SUCCESS);
        }
        glfwInit();

        // We are just reloading the configuration file here in case the user has changed
        // anything in the settings panel. In that case want to apply these settings
        // immediately rather than waiting for the next startup.
        // What follows is some copy-paste code that should be cleaned up at some point

        LDEBUG("Reloading configuration from disk");
        // Find configuration
        std::filesystem::path configurationFilePath;
        if (commandlineArguments.configuration.has_value()) {
            configurationFilePath = absPath(*commandlineArguments.configuration);
        }
        else {
            LDEBUG("Finding configuration");
            configurationFilePath = findConfiguration();
        }

        // The previous incarnation of this was initializing GLFW to get the primary
        // monitor's resolution, but that had some massive performance implications as
        // there was some issue with the swap buffer handling inside of GLFW. My
        // assumption is that GLFW doesn't like being initialized, destroyed, and then
        // initialized again. Therefore we are using the platform specific functions now
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

        *global::configuration = loadConfigurationFromFile(
            configurationFilePath,
            findSettings(),
            size
        );

        global::configuration->profile = win.selectedProfile();
        windowConfiguration = selectedSgctProfileFromLauncher(
            win,
            commandlineArguments.windowConfig.has_value(),
            windowConfiguration,
            labelFromCfgFile
        );
    }
    else {
        glfwInit();
    }
    if (global::configuration->profile.empty()) {
        LFATAL("Cannot launch without a profile");
        exit(EXIT_FAILURE);
    }


    // Prepend the outgoing sgctArguments with the program name
    // as well as the configuration file that sgct is supposed to use
    arguments.insert(arguments.begin(), argv[0]);
    arguments.insert(arguments.begin() + 1, "-config");
    arguments.insert(arguments.begin() + 2, absPath(windowConfiguration).string());

    // Need to set this before the creation of the sgct::Engine

    Log::instance().setLogToConsole(false);
    Log::instance().setShowTime(false);
    Log::instance().setShowLogLevel(false);
    Log::instance().setLogCallback(mainLogCallback);

#ifdef __APPLE__
    glfwWindowHint(GLFW_STENCIL_BITS, 8);
#endif

    LDEBUG("Creating SGCT Engine");
    std::vector<std::string> arg(argv + 1, argv + argc);
    LDEBUG("Parsing commandline arguments");
    const sgct::Configuration config = parseArguments(arg);
    LDEBUG("Loading cluster information");
    config::Cluster cluster = loadCluster(absPath(windowConfiguration).string());

    LDEBUG("Setting callbacks");
    Engine::Callbacks callbacks = {
        .initOpenGL = mainInitFunc,
        .preSync = mainPreSyncFunc,
        .postSyncPreDraw = mainPostSyncPreDrawFunc,
        .draw = mainRenderFunc,
        .draw2D = mainDraw2DFunc,
        .postDraw = mainPostDrawFunc,
        .encode = mainEncode,
        .decode = mainDecode,
        .keyboard = mainKeyboardCallback,
        .character = mainCharCallback,
        .mouseButton = mainMouseButtonCallback,
        .mousePos = mainMousePosCallback,
        .mouseScroll = mainMouseScrollCallback,
        .drop = mainDropCallback
    };
    Log::instance().setNotifyLevel(Log::Level::Debug);

    try {
        Engine::create(std::move(cluster), std::move(callbacks), config);
    }
    catch (const std::runtime_error& e) {
        LFATALC("main", e.what());
        Engine::destroy();
        global::openSpaceEngine->deinitialize();
        ghoul::deinitialize();
        throw;
    }
    catch (...) {
        global::openSpaceEngine->deinitialize();
        ghoul::deinitialize();
        Engine::destroy();
        throw;
    }

#ifdef __APPLE__
    // Workaround for OpenGL bug that Apple introduced in 10.14 Mojave that prevents an
    // OpenGL context to display anything until it is first moved or resized in dark
    // mode. So we are going through all windows here and resize them a bit larger and
    // then back to the desired resolution. Resizing the window to the same size doesn't
    // work as GLFW probably has a check for setting the current values.
    // This can be removed once the OpenGL bug is fixed.
    // In order to check, comment out the following lines and start OpenSpace on a 10.14
    // machine. If the loading screen shows up without doing anything to the window, it
    // is fixed. With the bug, the rendering stays gray even well after the main render
    // loop has started     -- 2018-10-28   abock
    for (const std::unique_ptr<Window>& window : Engine::instance().windows()) {
        GLFWwindow* w = window->windowHandle();
        int x, y;
        glfwGetWindowPos(w, &x, &y);
        glfwSetWindowPos(w, x + 1, y + 1);
        glfwSwapBuffers(w);
        glfwPollEvents();
        glfwSetWindowPos(w, x, y);
        glfwSwapBuffers(w);
        glfwPollEvents();
    }
#endif // __APPLE__


    // Do not print message if clients are waiting for the master
    // Only timeout after 15 minutes
    Engine::instance().setSyncParameters(false, 15.f * 60.f);

    {
        openspace::Settings settings = loadSettings();
        settings.hasStartedBefore = true;

        if (settings.rememberLastProfile) {
            const std::filesystem::path p = global::configuration->profile;
            const std::filesystem::path reducedName = p.filename().replace_extension();
            settings.profile = reducedName.string();
        }

        if (settings.rememberLastConfiguration &&
            !global::configuration->sgctConfigNameInitialized.empty())
        {
            // We only want to store the window configuration if it was not a dynamically
            // created one
            settings.configuration = global::configuration->windowConfiguration;
        }

        saveSettings(settings, findSettings());
    }

    LINFO("Starting rendering loop");
    Engine::instance().exec();
    LINFO("Ending rendering loop");

    global::openSpaceEngine->deinitializeGL();
    global::openSpaceEngine->deinitialize();
    global::destroy();

    // Clear function bindings to avoid crash after destroying the OpenSpace Engine
    Log::instance().setLogCallback(nullptr);

    LDEBUG("Destroying SGCT Engine");
    Engine::destroy();

#ifdef OPENVR_SUPPORT
    // Clean up OpenVR
    sgct::SGCTOpenVR::shutdown();
#endif

#ifdef OPENSPACE_HAS_SPOUT
    for (SpoutWindow& w : SpoutWindows) {
        w.leftOrMain.release();
        w.right.release();
    }
#endif // OPENSPACE_HAS_SPOUT

    ghoul::deinitialize();
    exit(EXIT_SUCCESS);
}
