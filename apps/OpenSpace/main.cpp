/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#include <openspace/engine/configuration.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/interaction/joystickinputstate.h>
#include <openspace/util/keys.h>
#include <ghoul/ghoul.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/cmdparser/commandlineparser.h>
#include <ghoul/cmdparser/singlecommand.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/consolelog.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/logging/visualstudiooutputlog.h>
#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/boolean.h>
#include <sgct.h>
#include <chrono>
#include <ctime>
#include <stb_image.h>

#ifdef WIN32
#include <openspace/openspace.h>
#include <ghoul/misc/stacktrace.h>
#include <ghoul/fmt.h>
#include <dbghelp.h>
#include <shellapi.h>
#include <shlobj.h>
#include <Windows.h>
#endif // WIN32

#ifdef OPENVR_SUPPORT
#include <SGCTOpenVR.h>
#endif // OPENVR_SUPPORT

#ifdef OPENSPACE_HAS_SPOUT
#include "SpoutLibrary.h"
#endif // OPENSPACE_HAS_SPOUT

#ifdef OPENSPACE_HAS_VTUNE
#include <ittnotify.h>

// If this is set to 'true', it will disable all frame markers in this file and expect
// you to place them in the code you actually want to inspect
constexpr const bool EnableDetailedVtune = false;
#endif // OPENSPACE_HAS_VTUNE

#ifdef OPENSPACE_HAS_NVTOOLS
#include "nvToolsExt.h"
#endif // OPENSPACE_HAS_NVTOOLS

using namespace openspace;

namespace {

constexpr const char* _loggerCat = "main";
constexpr const char* SpoutTag = "Spout";
constexpr const char* OpenVRTag = "OpenVR";

sgct::Engine* SgctEngine;
sgct::SharedVector<char> _synchronizationBuffer;

#ifdef OPENVR_SUPPORT
sgct::SGCTWindow* FirstOpenVRWindow = nullptr;
#endif

#ifdef OPENSPACE_HAS_VTUNE

struct {
    __itt_domain* init;
    __itt_domain* preSync;
    __itt_domain* postSyncPreDraw;
    __itt_domain* render;
    __itt_domain* draw2D;
    __itt_domain* postDraw;
    __itt_domain* keyboard;
    __itt_domain* mouseButton;
    __itt_domain* mousePos;
    __itt_domain* mouseScroll;
    __itt_domain* character;
    __itt_domain* encode;
    __itt_domain* decode;
} _vTune;

#endif // OPENSPACE_HAS_VTUNE

//
//  SPOUT-support
//

#ifdef OPENSPACE_HAS_SPOUT
/**
 * This struct stores all information about a single render window. Depending on the
 * frame setup, each window can be mono or stereo, the information of which is stored in
 * the \c leftOrMain and \c right members respectively.
 */
struct SpoutWindow {
    struct SpoutData {
        SPOUTHANDLE handle = nullptr;
        bool initialized = false;
    };

    /// The left framebuffer (or main, if there is no stereo rendering)
    SpoutData leftOrMain;

    /// The right framebuffer
    SpoutData right;

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

    std::string dumpFile = fmt::format(
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

    LINFO(fmt::format("Creating dump file: {}", dumpFile));

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

//
//  Detect OpenGL version
//
std::pair<int, int> supportedOpenGLVersion() {
    // Just create a window in order to retrieve the available OpenGL version before we
    // create the real window
    glfwInit();

    // On OS X we need to explicitly set the version and specify that we are using CORE
    // profile to be able to use glGetIntegerv(GL_MAJOR_VERSION, &major) and
    // glGetIntegerv(GL_MINOR_VERSION, &minor) explicitly setting to OGL 3.3 CORE works
    // since all Mac's now support at least 3.3
#ifdef __APPLE__
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
#endif

    glfwWindowHint(GLFW_VISIBLE, GL_FALSE);

    // By creating an offscreen window, the user will not know that we created this window
    GLFWwindow* offscreen = glfwCreateWindow(128, 128, "", nullptr, nullptr);
    glfwMakeContextCurrent(offscreen);

    // Get the OpenGL version
    int major, minor;
    glGetIntegerv(GL_MAJOR_VERSION, &major);
    glGetIntegerv(GL_MINOR_VERSION, &minor);

    // And get rid of the window again
    glfwDestroyWindow(offscreen);
    glfwWindowHint(GLFW_VISIBLE, GL_TRUE);

    return { major, minor };
}


//
//  Init function
//
void mainInitFunc() {
#ifdef OPENSPACE_HAS_VTUNE
    if (EnableDetailedVtune) {
        __itt_frame_begin_v3(_vTune.init, nullptr);
    }
#endif // OPENSPACE_HAS_VTUNE
    LTRACE("main::mainInitFunc(begin)");

    LDEBUG("Initializing OpenSpace Engine started");
    global::openSpaceEngine.initialize();
    LDEBUG("Initializing OpenSpace Engine finished");

    {
        std::string path = absPath("${DATA}/openspace-icon.png");
        int x;
        int y;
        int n;
        unsigned char* data = stbi_load(path.c_str(), &x, &y, &n, 0);

        GLFWimage icons[1];
        icons[0].pixels = data;
        icons[0].width = x;
        icons[0].height = y;

        const size_t nWindows = SgctEngine->getNumberOfWindows();
        for (size_t i = 0; i < nWindows; ++i) {
            const sgct::SGCTWindow* windowPtr = SgctEngine->getWindowPtr(i);
            glfwSetWindowIcon(windowPtr->getWindowHandle(), 1, icons);
        }

        stbi_image_free(icons[0].pixels);
    }



    LDEBUG("Initializing OpenGL in OpenSpace Engine started");
    global::openSpaceEngine.initializeGL();
    LDEBUG("Initializing OpenGL in OpenSpace Engine finished");


    // Find if we have at least one OpenVR window
    // Save reference to first OpenVR window, which is the one we will copy to the HMD.
    for (size_t i = 0; i < SgctEngine->getNumberOfWindows(); ++i) {
        if (SgctEngine->getWindowPtr(i)->checkIfTagExists(OpenVRTag)) {
#ifdef OPENVR_SUPPORT
            FirstOpenVRWindow = SgctEngine->getWindowPtr(i);

            // If we have an OpenVRWindow, initialize OpenVR.
            sgct::SGCTOpenVR::initialize(
                SgctEngine->getNearClippingPlane(), SgctEngine->getFarClippingPlane()
            );
#else
            LWARNING("OpenVR was requested, but program was compiled without VR support");
#endif

            break;
        }
    }

    const size_t nWindows = SgctEngine->getNumberOfWindows();
    for (size_t i = 0; i < nWindows; ++i) {
        const sgct::SGCTWindow* windowPtr = SgctEngine->getWindowPtr(i);

        if (!windowPtr->checkIfTagExists(SpoutTag)) {
            continue;
        }

#ifdef OPENSPACE_HAS_SPOUT
        SpoutWindow w;

        w.windowId = i;

        const sgct::SGCTWindow::StereoMode sm = windowPtr->getStereoMode();
        const bool hasStereo = (sm != sgct::SGCTWindow::No_Stereo) &&
                               (sm < sgct::SGCTWindow::Side_By_Side_Stereo);

        if (hasStereo) {
            SpoutWindow::SpoutData& left = w.leftOrMain;
            left.handle = GetSpout();
            left.initialized = left.handle->CreateSender(
                (windowPtr->getName() + "_left").c_str(),
                windowPtr->getXFramebufferResolution(),
                windowPtr->getYFramebufferResolution()
            );

            SpoutWindow::SpoutData& right = w.right;
            right.handle = GetSpout();
            right.initialized = right.handle->CreateSender(
                (windowPtr->getName() + "_right").c_str(),
                windowPtr->getXFramebufferResolution(),
                windowPtr->getYFramebufferResolution()
            );
        }
        else {
            SpoutWindow::SpoutData& main = w.leftOrMain;
            main.handle = GetSpout();
            main.initialized = main.handle->CreateSender(
                windowPtr->getName().c_str(),
                windowPtr->getXFramebufferResolution(),
                windowPtr->getYFramebufferResolution()
            );
        }

        SpoutWindows.push_back(std::move(w));
#else
        LWARNING("Spout was requested, but program was compiled without Spout support");
#endif // OPENSPACE_HAS_SPOUT
    }


    //
    //  Screenshots
    //

    std::string screenshotPath = "${SCREENSHOTS}";
    if (global::configuration.shouldUseScreenshotDate) {
        std::time_t now = std::time(nullptr);
        std::tm* nowTime = std::localtime(&now);
        char mbstr[128];
        strftime(mbstr, sizeof(mbstr), "%Y-%m-%d-%H-%M", nowTime);

        FileSys.registerPathToken(
            "${SCREENSHOTS}",
            absPath(screenshotPath + '/' + std::string(mbstr)),
            ghoul::filesystem::FileSystem::Override::Yes
        );
    }

    for (size_t i = 0; i < nWindows; ++i) {
        sgct::SGCTWindow* w = SgctEngine->getWindowPtr(i);
        const std::string screenshotNames = nWindows > 1 ?
            fmt::format("OpenSpace_{}", i) :
            "OpenSpace";

        sgct_core::ScreenCapture* cpt0 = w->getScreenCapturePointer(0);
        sgct_core::ScreenCapture* cpt1 = w->getScreenCapturePointer(1);

        if (cpt0) {
            cpt0->setPathAndFileName(absPath(screenshotPath), screenshotNames);
        }

        if (cpt1) {
            cpt1->setPathAndFileName(absPath(screenshotPath), screenshotNames);
        }
    }

    LTRACE("main::mainInitFunc(end)");
#ifdef OPENSPACE_HAS_VTUNE
    if (EnableDetailedVtune) {
        __itt_frame_end_v3(_vTune.init, nullptr);
    }
#endif // OPENSPACE_HAS_VTUNE
}



void mainPreSyncFunc() {
#ifdef OPENSPACE_HAS_VTUNE
    if (EnableDetailedVtune) {
        __itt_frame_begin_v3(_vTune.preSync, nullptr);
    }
#endif // OPENSPACE_HAS_VTUNE
    LTRACE("main::mainPreSyncFunc(begin)");

    global::openSpaceEngine.preSynchronization();

    // Query joystick status
    using namespace interaction;

    for (int i = GLFW_JOYSTICK_1; i <= GLFW_JOYSTICK_LAST; ++i) {
        JoystickInputState& state = global::joystickInputStates[i];

        int present = glfwJoystickPresent(i);
        if (present == GLFW_FALSE) {
            state.isConnected = false;
            continue;
        }

        if (!state.isConnected) {
            // Joystick was added
            state.isConnected = true;
            state.name = sgct::Engine::getJoystickName(i);

            std::fill(state.axes.begin(), state.axes.end(), 0.f);
            std::fill(state.buttons.begin(), state.buttons.end(), JoystickAction::Idle);
        }

        const float* axes = sgct::Engine::getJoystickAxes(i, &state.nAxes);
        if (state.nAxes > JoystickInputState::MaxAxes) {
            LWARNING(fmt::format(
                "Joystick/Gamepad {} has {} axes, but only {} axes are supported. "
                "All excess axes are ignored",
                state.name, state.nAxes, JoystickInputState::MaxAxes
            ));
            state.nAxes = JoystickInputState::MaxAxes;
        }
        std::memcpy(state.axes.data(), axes, state.nAxes * sizeof(float));

        const unsigned char* buttons = sgct::Engine::getJoystickButtons(
            i,
            &state.nButtons
        );

        if (state.nButtons > JoystickInputState::MaxButtons) {
            LWARNING(fmt::format(
                "Joystick/Gamepad {} has {} buttons, but only {} buttons are "
                "supported. All excess buttons are ignored",
                state.name, state.nButtons, JoystickInputState::MaxButtons
            ));
            state.nButtons = JoystickInputState::MaxButtons;
        }

        for (int j = 0; j < state.nButtons; ++j) {
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

    LTRACE("main::mainPreSyncFunc(end)");
#ifdef OPENSPACE_HAS_VTUNE
    if (EnableDetailedVtune) {
        __itt_frame_end_v3(_vTune.preSync, nullptr);
    }
#endif // OPENSPACE_HAS_VTUNE
}



void mainPostSyncPreDrawFunc() {
#ifdef OPENSPACE_HAS_VTUNE
    if (EnableDetailedVtune) {
        __itt_frame_begin_v3(_vTune.postSyncPreDraw, nullptr);
    }
#endif // OPENSPACE_HAS_VTUNE
#ifdef OPENSPACE_HAS_NVTOOLS
    nvtxRangePush("postSyncPreDraw");
#endif // OPENSPACE_HAS_NVTOOLS
    LTRACE("main::postSynchronizationPreDraw(begin)");

    global::openSpaceEngine.postSynchronizationPreDraw();

#ifdef OPENVR_SUPPORT
    if (FirstOpenVRWindow) {
        // Update pose matrices for all tracked OpenVR devices once per frame
        sgct::SGCTOpenVR::updatePoses();
    }
#endif // OPENVR_SUPPORT

    LTRACE("main::postSynchronizationPreDraw(end)");

#ifdef OPENSPACE_HAS_NVTOOLS
    nvtxRangePop();
#endif // OPENSPACE_HAS_NVTOOLS
#ifdef OPENSPACE_HAS_VTUNE
    if (EnableDetailedVtune) {
        __itt_frame_end_v3(_vTune.postSyncPreDraw, nullptr);
    }
#endif // OPENSPACE_HAS_VTUNE
}



void mainRenderFunc() {
#ifdef OPENSPACE_HAS_VTUNE
    if (EnableDetailedVtune) {
        __itt_frame_begin_v3(_vTune.render, nullptr);
    }
#endif // OPENSPACE_HAS_VTUNE
#ifdef OPENSPACE_HAS_NVTOOLS
    nvtxRangePush("render");
#endif // OPENSPACE_HAS_NVTOOLS
    LTRACE("main::mainRenderFunc(begin)");

    glm::mat4 viewMatrix = SgctEngine->getCurrentViewMatrix() *
        glm::translate(glm::mat4(1.f), sgct::Engine::getDefaultUserPtr()->getPos());

    glm::mat4 projectionMatrix = SgctEngine->getCurrentProjectionMatrix();
#ifdef OPENVR_SUPPORT
    bool currentWindowIsHMD = FirstOpenVRWindow == SgctEngine->getCurrentWindowPtr();
    if (sgct::SGCTOpenVR::isHMDActive() && currentWindowIsHMD) {
        projectionMatrix = sgct::SGCTOpenVR::getHMDCurrentViewProjectionMatrix(
            SgctEngine->getCurrentFrustumMode()
        );
    }
#endif

    try {
        global::openSpaceEngine.render(
            SgctEngine->getModelMatrix(),
            viewMatrix,
            projectionMatrix
        );
    }
    catch (const ghoul::RuntimeError& e) {
        LERRORC(e.component, e.message);
    }

    LTRACE("main::mainRenderFunc(end)");
#ifdef OPENSPACE_HAS_NVTOOLS
    nvtxRangePop();
#endif // OPENSPACE_HAS_NVTOOLS
#ifdef OPENSPACE_HAS_VTUNE
    if (EnableDetailedVtune) {
        __itt_frame_end_v3(_vTune.render, nullptr);
    }
#endif // OPENSPACE_HAS_VTUNE
}



void mainDraw2DFunc() {
#ifdef OPENSPACE_HAS_VTUNE
    if (EnableDetailedVtune) {
        __itt_frame_begin_v3(_vTune.draw2D, nullptr);
    }
#endif // OPENSPACE_HAS_VTUNE
    LTRACE("main::mainDraw2DFunc(begin)");

    try {
        global::openSpaceEngine.drawOverlays();
    }
    catch (const ghoul::RuntimeError& e) {
        LERRORC(e.component, e.message);
    }

    // SGCT gets angry if we change this in our function
    glEnable(GL_BLEND);
    glDisable(GL_CULL_FACE);
    glDisable(GL_DEPTH_TEST);

    LTRACE("main::mainDraw2DFunc(end)");
#ifdef OPENSPACE_HAS_VTUNE
    if (EnableDetailedVtune) {
        __itt_frame_end_v3(_vTune.draw2D, nullptr);
    }
#endif // OPENSPACE_HAS_VTUNE
}



void mainPostDrawFunc() {
#ifdef OPENSPACE_HAS_VTUNE
    if (EnableDetailedVtune) {
        __itt_frame_begin_v3(_vTune.postDraw, nullptr);
    }
#endif // OPENSPACE_HAS_VTUNE
    LTRACE("main::mainPostDrawFunc(begin)");

#ifdef OPENVR_SUPPORT
    if (FirstOpenVRWindow) {
        // Copy the first OpenVR window to the HMD
        sgct::SGCTOpenVR::copyWindowToHMD(FirstOpenVRWindow);
    }
#endif // OPENVR_SUPPORT

    global::openSpaceEngine.postDraw();

#ifdef OPENSPACE_HAS_SPOUT
    for (const SpoutWindow& w : SpoutWindows) {
        sgct::SGCTWindow* window = SgctEngine->getWindowPtr(w.windowId);
        if (w.leftOrMain.initialized) {
            const GLuint texId = window->getFrameBufferTexture(sgct::Engine::LeftEye);
            glBindTexture(GL_TEXTURE_2D, texId);
            w.leftOrMain.handle->SendTexture(
                texId,
                GL_TEXTURE_2D,
                window->getXFramebufferResolution(),
                window->getYFramebufferResolution()
            );
        }

        if (w.right.initialized) {
            const GLuint texId = window->getFrameBufferTexture(sgct::Engine::RightEye);
            glBindTexture(GL_TEXTURE_2D, texId);
            w.right.handle->SendTexture(
                texId,
                GL_TEXTURE_2D,
                window->getXFramebufferResolution(),
                window->getYFramebufferResolution()
            );
        }
    }
    glBindTexture(GL_TEXTURE_2D, 0);
#endif // OPENSPACE_HAS_SPOUT

    LTRACE("main::mainPostDrawFunc(end)");
#ifdef OPENSPACE_HAS_VTUNE
    if (EnableDetailedVtune) {
        __itt_frame_end_v3(_vTune.postDraw, nullptr);
    }
#endif // OPENSPACE_HAS_VTUNE
}



void mainKeyboardCallback(int key, int, int action, int modifiers) {
#ifdef OPENSPACE_HAS_VTUNE
    if (EnableDetailedVtune) {
        __itt_frame_begin_v3(_vTune.keyboard, nullptr);
    }
#endif // OPENSPACE_HAS_VTUNE
    LTRACE("main::mainKeyboardCallback(begin)");

    const Key k = Key(key);
    const KeyModifier m = KeyModifier(modifiers);
    const KeyAction a = KeyAction(action);
    global::openSpaceEngine.keyboardCallback(k, m, a);

    LTRACE("main::mainKeyboardCallback(begin)");
#ifdef OPENSPACE_HAS_VTUNE
    if (EnableDetailedVtune) {
        __itt_frame_end_v3(_vTune.keyboard, nullptr);
    }
#endif // OPENSPACE_HAS_VTUNE
}



void mainMouseButtonCallback(int key, int action, int modifiers) {
#ifdef OPENSPACE_HAS_VTUNE
    if (EnableDetailedVtune) {
        __itt_frame_begin_v3(_vTune.mouseButton, nullptr);
    }
#endif // OPENSPACE_HAS_VTUNE
    LTRACE("main::mainMouseButtonCallback(begin)");

    const MouseButton k = MouseButton(key);
    const MouseAction a = MouseAction(action);
    const KeyModifier m = KeyModifier(modifiers);
    global::openSpaceEngine.mouseButtonCallback(k, a, m);

    LTRACE("main::mainMouseButtonCallback(end)");
#ifdef OPENSPACE_HAS_VTUNE
    if (EnableDetailedVtune) {
        __itt_frame_end_v3(_vTune.mouseButton, nullptr);
    }
#endif // OPENSPACE_HAS_VTUNE
}



void mainMousePosCallback(double x, double y) {
#ifdef OPENSPACE_HAS_VTUNE
    if (EnableDetailedVtune) {
        __itt_frame_begin_v3(_vTune.mousePos, nullptr);
    }
#endif // OPENSPACE_HAS_VTUNE

    global::openSpaceEngine.mousePositionCallback(x, y);

#ifdef OPENSPACE_HAS_VTUNE
    if (EnableDetailedVtune) {
        __itt_frame_end_v3(_vTune.mousePos, nullptr);
    }
#endif // OPENSPACE_HAS_VTUNE
}



void mainMouseScrollCallback(double posX, double posY) {
#ifdef OPENSPACE_HAS_VTUNE
    if (EnableDetailedVtune) {
        __itt_frame_begin_v3(_vTune.mouseScroll, nullptr);
    }
#endif // OPENSPACE_HAS_VTUNE
    LTRACE("main::mainMouseScrollCallback(begin");

    global::openSpaceEngine.mouseScrollWheelCallback(posX, posY);

    LTRACE("main::mainMouseScrollCallback(end)");
#ifdef OPENSPACE_HAS_VTUNE
    if (EnableDetailedVtune) {
        __itt_frame_end_v3(_vTune.mouseScroll, nullptr);
    }
#endif // OPENSPACE_HAS_VTUNE
}



void mainCharCallback(unsigned int codepoint, int modifiers) {
#ifdef OPENSPACE_HAS_VTUNE
    if (EnableDetailedVtune) {
        __itt_frame_begin_v3(_vTune.character, nullptr);
    }
#endif // OPENSPACE_HAS_VTUNE

    const KeyModifier m = KeyModifier(modifiers);
    global::openSpaceEngine.charCallback(codepoint, m);

#ifdef OPENSPACE_HAS_VTUNE
    if (EnableDetailedVtune) {
        __itt_frame_end_v3(_vTune.character, nullptr);
    }
#endif // OPENSPACE_HAS_VTUNE
}



void mainEncodeFun() {
#ifdef OPENSPACE_HAS_VTUNE
    if (EnableDetailedVtune) {
        __itt_frame_begin_v3(_vTune.encode, nullptr);
    }
#endif // OPENSPACE_HAS_VTUNE
    LTRACE("main::mainEncodeFun(begin)");

    std::vector<char> data = global::openSpaceEngine.encode();
    _synchronizationBuffer.setVal(std::move(data));
    sgct::SharedData::instance()->writeVector(&_synchronizationBuffer);

    LTRACE("main::mainEncodeFun(end)");
#ifdef OPENSPACE_HAS_VTUNE
    if (EnableDetailedVtune) {
        __itt_frame_end_v3(_vTune.encode, nullptr);
    }
#endif // OPENSPACE_HAS_VTUNE
}



void mainDecodeFun() {
#ifdef OPENSPACE_HAS_VTUNE
    if (EnableDetailedVtune) {
        __itt_frame_begin_v3(_vTune.decode, nullptr);
    }
#endif // OPENSPACE_HAS_VTUNE
    LTRACE("main::mainDecodeFun(begin)");

    sgct::SharedData::instance()->readVector(&_synchronizationBuffer);
    std::vector<char> data = _synchronizationBuffer.getVal();
    global::openSpaceEngine.decode(std::move(data));

    LTRACE("main::mainDecodeFun(end)");
#ifdef OPENSPACE_HAS_VTUNE
    if (EnableDetailedVtune) {
        __itt_frame_end_v3(_vTune.decode, nullptr);
    }
#endif // OPENSPACE_HAS_VTUNE
}



void mainLogCallback(const char* msg) {
    std::string message = msg;
    if (message.empty() || message == ".") {
        // We don't want the empty '.' message that SGCT sends while it is waiting for
        // connections from other network nodes
        return;
    }
    // Remove the trailing \n that is passed along
    LINFOC("SGCT", message.substr(0, message.size() - 1));
}

} // namespace


void setSgctDelegateFunctions() {
    WindowDelegate& sgctDelegate = global::windowDelegate;
    sgctDelegate.terminate = []() { sgct::Engine::instance()->terminate(); };
    sgctDelegate.setBarrier = [](bool enabled) {
        sgct::SGCTWindow::setBarrier(enabled);
    };
    sgctDelegate.setSynchronization = [](bool enabled) {
        sgct_core::ClusterManager::instance()->setUseIgnoreSync(enabled);
    };
    sgctDelegate.clearAllWindows = [](const glm::vec4& clearColor) {
        size_t n = sgct::Engine::instance()->getNumberOfWindows();
        for (size_t i = 0; i < n; ++i) {
            glClearColor(clearColor.r, clearColor.g, clearColor.b, clearColor.a);
            glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
            GLFWwindow* w = sgct::Engine::instance()->getWindowPtr(i)->getWindowHandle();
            glfwSwapBuffers(w);
        }
    };
    sgctDelegate.windowHasResized = []() {
        return sgct::Engine::instance()->getCurrentWindowPtr()->isWindowResized();
    };
    sgctDelegate.averageDeltaTime = []() { return sgct::Engine::instance()->getAvgDt(); };
    sgctDelegate.deltaTimeStandardDeviation = []() {
        return sgct::Engine::instance()->getDtStandardDeviation();
    };
    sgctDelegate.minDeltaTime = []() {
        return sgct::Engine::instance()->getMinDt();
    };
    sgctDelegate.maxDeltaTime = []() {
        return sgct::Engine::instance()->getMaxDt();
    };
    sgctDelegate.deltaTime = []() { return sgct::Engine::instance()->getDt(); };
    sgctDelegate.applicationTime = []() { return sgct::Engine::getTime(); };
    sgctDelegate.mousePosition = []() {
        int id = sgct::Engine::instance()->getCurrentWindowPtr()->getId();
        double posX, posY;
        sgct::Engine::getMousePos(id, &posX, &posY);
        return glm::vec2(posX, posY);
    };
    sgctDelegate.mouseButtons = [](int maxNumber) {
        int id = sgct::Engine::instance()->getCurrentWindowPtr()->getId();
        uint32_t result = 0;
        for (int i = 0; i < maxNumber; ++i) {
            bool button = (sgct::Engine::getMouseButton(id, i) != 0);
            if (button) {
                result |= (1 << i);
            }
        }
        return result;
    };
    sgctDelegate.currentWindowSize = []() {
        return glm::ivec2(
            sgct::Engine::instance()->getCurrentWindowPtr()->getXResolution(),
            sgct::Engine::instance()->getCurrentWindowPtr()->getYResolution());
    };
    sgctDelegate.currentSubwindowSize = []() {
        auto window = sgct::Engine::instance()->getCurrentWindowPtr();
        switch (window->getStereoMode()) {
            case sgct::SGCTWindow::Side_By_Side_Stereo:
            case sgct::SGCTWindow::Side_By_Side_Inverted_Stereo:
                return glm::ivec2(window->getXResolution() / 2, window->getYResolution());
            case sgct::SGCTWindow::Top_Bottom_Stereo:
            case sgct::SGCTWindow::Top_Bottom_Inverted_Stereo:
                return glm::ivec2(window->getXResolution(), window->getYResolution() / 2);
            default:
                return glm::ivec2(window->getXResolution(), window->getYResolution());
        }
    };
    sgctDelegate.currentWindowResolution = []() {
        int x, y;
        auto window = sgct::Engine::instance()->getCurrentWindowPtr();
        window->getFinalFBODimensions(x, y);
        return glm::ivec2(x, y);
    };
    sgctDelegate.currentDrawBufferResolution = []() {
        sgct_core::Viewport* viewport =
            sgct::Engine::instance()->getCurrentWindowPtr()->getViewport(0);
        if (viewport != nullptr) {
            if (viewport->hasSubViewports() && viewport->getNonLinearProjectionPtr()) {
                int res = viewport->getNonLinearProjectionPtr()->getCubemapResolution();
                return glm::ivec2(res, res);
            }
            else {
                int x, y;
                auto window = sgct::Engine::instance()->getCurrentWindowPtr();
                window->getFinalFBODimensions(x, y);
                return glm::ivec2(x, y);
            }
        }
        return glm::ivec2(-1, -1);
    };
    sgctDelegate.currentViewportSize = []() {
        sgct_core::Viewport* viewport =
            sgct::Engine::instance()->getCurrentWindowPtr()->getViewport(0);
        if (viewport != nullptr) {
            int x = 0;
            int y = 0;
            sgct::Engine::instance()->getCurrentViewportSize(x, y);
            return glm::ivec2(x, y);
        }
        return glm::ivec2(-1, -1);
    };
    sgctDelegate.dpiScaling = []() {
        return glm::vec2(
            sgct::Engine::instance()->getCurrentWindowPtr()->getXScale(),
            sgct::Engine::instance()->getCurrentWindowPtr()->getYScale()
        );
    };
    sgctDelegate.currentNumberOfAaSamples = []() {
        return sgct::Engine::instance()->getCurrentWindowPtr()->getNumberOfAASamples();
    };
    sgctDelegate.isRegularRendering = []() {
        sgct::SGCTWindow* w = sgct::Engine::instance()->getCurrentWindowPtr();
        ghoul_assert(
            w->getNumberOfViewports() > 0,
            "At least one viewport must exist at this time"
        );
        sgct_core::Viewport* vp = w->getViewport(0);
        sgct_core::NonLinearProjection* nlp = vp->getNonLinearProjectionPtr();
        return nlp == nullptr;
    };
    sgctDelegate.hasGuiWindow = []() {
        auto engine = sgct::Engine::instance();
        for (size_t i = 0; i < engine->getNumberOfWindows(); ++i) {
            if (engine->getWindowPtr(i)->checkIfTagExists("GUI")) {
                return true;
            }
        }
        return false;
    };
    sgctDelegate.isGuiWindow = []() {
        return sgct::Engine::instance()->getCurrentWindowPtr()->checkIfTagExists("GUI");
    };
    sgctDelegate.isMaster = []() { return sgct::Engine::instance()->isMaster(); };
    sgctDelegate.isUsingSwapGroups = []() {
        return sgct::SGCTWindow::isUsingSwapGroups();
    };
    sgctDelegate.isSwapGroupMaster = []() {
        return sgct::SGCTWindow::isSwapGroupMaster();
    };
    sgctDelegate.viewProjectionMatrix = []() {
        return sgct::Engine::instance()->getCurrentModelViewProjectionMatrix();
    };
    sgctDelegate.modelMatrix = []() {
        return sgct::Engine::instance()->getModelMatrix();
    };
    sgctDelegate.setNearFarClippingPlane = [](float nearPlane, float farPlane) {
        sgct::Engine::instance()->setNearAndFarClippingPlanes(nearPlane, farPlane);
    };
    sgctDelegate.setEyeSeparationDistance = [](float distance) {
        sgct::Engine::instance()->setEyeSeparation(distance);
    };
    sgctDelegate.viewportPixelCoordinates = []() {
        sgct::SGCTWindow* window = sgct::Engine::instance()->getCurrentWindowPtr();
        if (!window || !window->getCurrentViewport()) {
            return glm::ivec4(0, 0, 0, 0);
        }
        else {
            const int* data = sgct::Engine::instance()->getCurrentViewportPixelCoords();
            return glm::ivec4(data[0], data[2], data[1], data[3]);
        }
    };
    sgctDelegate.isExternalControlConnected = []() {
        return sgct::Engine::instance()->isExternalControlConnected();
    };
    sgctDelegate.sendMessageToExternalControl = [](const std::vector<char>& message) {
        sgct::Engine::instance()->sendMessageToExternalControl(
            message.data(),
            static_cast<int>(message.size())
        );
    };
    sgctDelegate.isSimpleRendering = []() {
        return (sgct::Engine::instance()->getCurrentRenderTarget() !=
                sgct::Engine::NonLinearBuffer);
    };
    sgctDelegate.isFisheyeRendering = []() {
        sgct::SGCTWindow* w = sgct::Engine::instance()->getCurrentWindowPtr();
        return dynamic_cast<sgct_core::FisheyeProjection*>(
            w->getViewport(0)->getNonLinearProjectionPtr()
        ) != nullptr;
    };
    sgctDelegate.takeScreenshot = [](bool applyWarping) {
        sgct::SGCTSettings::instance()->setCaptureFromBackBuffer(applyWarping);
        sgct::Engine::instance()->takeScreenshot();
        return sgct::Engine::instance()->getScreenShotNumber();
    };
    sgctDelegate.swapBuffer = []() {
        GLFWwindow* w = glfwGetCurrentContext();
        glfwSwapBuffers(w);
        glfwPollEvents();
    };
    sgctDelegate.nWindows = []() {
        return static_cast<int>(sgct::Engine::instance()->getNumberOfWindows());
    };
    sgctDelegate.currentWindowId = []() {
        return sgct::Engine::instance()->getCurrentWindowPtr()->getId();
    };
    sgctDelegate.openGLProcedureAddress = [](const char* func) {
        return glfwGetProcAddress(func);
    };
    sgctDelegate.getHorizFieldOfView = []() {
        return static_cast<double>(
            sgct::Engine::instance()->getWindowPtr(0)->getHorizFieldOfViewDegrees()
        );
    };
    sgctDelegate.setHorizFieldOfView = [](float hFovDeg) {
        sgct::SGCTWindow* w = sgct::Engine::instance()->getWindowPtr(0);
        w->setHorizFieldOfView(hFovDeg);
    };
    #ifdef WIN32
    sgctDelegate.getNativeWindowHandle = [](size_t windowIndex) -> void* {
        sgct::SGCTWindow* w = sgct::Engine::instance()->getWindowPtr(windowIndex);
        if(w) {
                HWND hWnd = glfwGetWin32Window(w->getWindowHandle());
                return reinterpret_cast<void*>(hWnd);
        }
        return nullptr;
    };
    #endif // WIN32
    sgctDelegate.frustumMode = []() {
        using FM = sgct_core::Frustum::FrustumMode;
        switch (sgct::Engine::instance()->getCurrentFrustumMode()) {
            case FM::MonoEye: return WindowDelegate::Frustum::Mono;
            case FM::StereoLeftEye: return WindowDelegate::Frustum::LeftEye;
            case FM::StereoRightEye: return WindowDelegate::Frustum::RightEye;
        }
    };
    sgctDelegate.swapGroupFrameNumber = []() {
        unsigned int fn = 0;
        sgct::Engine::instance()->getCurrentWindowPtr()->getSwapGroupFrameNumber(fn);
        return static_cast<uint64_t>(fn);
    };
}

int main(int argc, char** argv) {
#ifdef WIN32
    SetUnhandledExceptionFilter(generateMiniDump);
#endif // WIN32

#ifdef OPENSPACE_HAS_VTUNE
    if (EnableDetailedVtune) {
        _vTune.init = __itt_domain_create("init");
        _vTune.preSync = __itt_domain_create("preSync");
        _vTune.postSyncPreDraw = __itt_domain_create("postSyncPreDraw");
        _vTune.render = __itt_domain_create("render");
        _vTune.draw2D = __itt_domain_create("draw2D");
        _vTune.postDraw = __itt_domain_create("postDraw");
        _vTune.keyboard = __itt_domain_create("keyboard");
        _vTune.mouseButton = __itt_domain_create("mouseButton");
        _vTune.mousePos = __itt_domain_create("mousePos");
        _vTune.mouseScroll = __itt_domain_create("mouseScroll");
        _vTune.character = __itt_domain_create("character");
        _vTune.encode = __itt_domain_create("encode");
        _vTune.decode = __itt_domain_create("decode");
    }
#endif // OPENSPACE_HAS_VTUNE


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
            LogMgr.addLog(std::make_unique<ghoul::logging::VisualStudioOutputLog>());
        }
#endif // WIN32

    }

    ghoul::initialize();

    // Register the path of the executable,
    // to make it possible to find other files in the same directory.
    FileSys.registerPathToken(
        "${BIN}",
        ghoul::filesystem::File(absPath(argv[0])).directoryName(),
        ghoul::filesystem::FileSystem::Override::Yes
    );

    //
    // Parse commandline arguments
    //
    ghoul::cmdparser::CommandlineParser parser(
        std::string(argv[0]),
        ghoul::cmdparser::CommandlineParser::AllowUnknownCommands::Yes
    );

    CommandlineArguments commandlineArguments;
    parser.addCommand(std::make_unique<ghoul::cmdparser::SingleCommand<std::string>>(
        commandlineArguments.configurationName, "--file", "-f",
        "Provides the path to the OpenSpace configuration file. Only the '${TEMPORARY}' "
        "path token is available and any other path has to be specified relative to the "
        "current working directory."
    ));

    parser.addCommand(std::make_unique<ghoul::cmdparser::SingleCommand<std::string>>(
        commandlineArguments.configurationOverride, "--config", "-c",
        "Provides the ability to pass arbitrary Lua code to the application that will be "
        "evaluated after the configuration file has been loaded but before the other "
        "commandline arguments are triggered. This can be used to manipulate the "
        "configuration file without editing the file on disk, for example in a "
        "planetarium environment. Please not that the Lua script must not contain any - "
        "or they will be interpreted as a new command. Similar, in Bash, ${...} will be "
        "evaluated before it is passed to OpenSpace."
    ));

    // setCommandLine returns a reference to the vector that will be filled later
    const std::vector<std::string>& sgctArguments = parser.setCommandLine(
        { argv, argv + argc }
    );

    bool showHelp = parser.execute();
    if (showHelp) {
        parser.displayHelp(std::cout);
        exit(EXIT_SUCCESS);
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
        std::string configurationFilePath = commandlineArguments.configurationName;
        if (commandlineArguments.configurationName.empty()) {
            LDEBUG("Finding configuration");
            configurationFilePath = configuration::findConfiguration();
        }
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
        if (!commandlineArguments.configurationOverride.empty()) {
            LDEBUG("Executing Lua script passed through the commandline:");
            LDEBUG(commandlineArguments.configurationOverride);
            ghoul::lua::runScript(
                global::configuration.state,
                commandlineArguments.configurationOverride
            );
            parseLuaState(global::configuration);
        }

        // Determining SGCT configuration file
        LDEBUG("SGCT Configuration file: " + global::configuration.windowConfiguration);

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

    global::openSpaceEngine.registerPathTokens();

    // Prepend the outgoing sgctArguments with the program name
    // as well as the configuration file that sgct is supposed to use
    arguments.insert(arguments.begin(), argv[0]);
    arguments.insert(arguments.begin() + 1, "-config");
    arguments.insert(arguments.begin() + 2, absPath(windowConfiguration));

    // Need to set this before the creation of the sgct::Engine
    sgct::MessageHandler::instance()->setLogToConsole(false);
    sgct::MessageHandler::instance()->setShowTime(false);
    sgct::MessageHandler::instance()->setLogToCallback(true);
    sgct::MessageHandler::instance()->setLogCallback(mainLogCallback);

#ifdef __APPLE__
    glfwWindowHint(GLFW_STENCIL_BITS, 8);
#endif

    LDEBUG("Creating SGCT Engine");
    SgctEngine = new sgct::Engine(arguments);

    // Bind functions
    SgctEngine->setInitOGLFunction(mainInitFunc);
    SgctEngine->setPreSyncFunction(mainPreSyncFunc);
    SgctEngine->setPostSyncPreDrawFunction(mainPostSyncPreDrawFunc);
    SgctEngine->setDrawFunction(mainRenderFunc);
    SgctEngine->setDraw2DFunction(mainDraw2DFunc);
    SgctEngine->setPostDrawFunction(mainPostDrawFunc);
    SgctEngine->setKeyboardCallbackFunction(mainKeyboardCallback);
    SgctEngine->setMouseButtonCallbackFunction(mainMouseButtonCallback);
    SgctEngine->setMousePosCallbackFunction(mainMousePosCallback);
    SgctEngine->setMouseScrollCallbackFunction(mainMouseScrollCallback);
    SgctEngine->setCharCallbackFunction(mainCharCallback);

    // Disable the immediate exit of the application when the ESC key is pressed
    SgctEngine->setExitKey(SGCT_KEY_UNKNOWN);

    sgct::MessageHandler::instance()->setNotifyLevel(sgct::MessageHandler::NOTIFY_ALL);

    // Set encode and decode functions
    // NOTE: starts synchronizing before init functions
    sgct::SharedData::instance()->setEncodeFunction(mainEncodeFun);
    sgct::SharedData::instance()->setDecodeFunction(mainDecodeFun);

    // Try to open a window
    LDEBUG("Initialize SGCT Engine");
    std::map<std::pair<int, int>, sgct::Engine::RunMode> versionMapping = {
        { { 3, 3 }, sgct::Engine::RunMode::OpenGL_3_3_Core_Profile },
        { { 4, 0 }, sgct::Engine::RunMode::OpenGL_4_0_Core_Profile },
        { { 4, 1 }, sgct::Engine::RunMode::OpenGL_4_1_Core_Profile },
        { { 4, 2 }, sgct::Engine::RunMode::OpenGL_4_2_Core_Profile },
        { { 4, 3 }, sgct::Engine::RunMode::OpenGL_4_3_Core_Profile },
        { { 4, 4 }, sgct::Engine::RunMode::OpenGL_4_4_Core_Profile },
        { { 4, 5 }, sgct::Engine::RunMode::OpenGL_4_5_Core_Profile },
        { { 4, 6 }, sgct::Engine::RunMode::OpenGL_4_6_Core_Profile }
    };


    std::pair<int, int> version = supportedOpenGLVersion();
    LINFO(fmt::format("Detected OpenGL version: {}.{}", version.first, version.second));
    bool initSuccess = SgctEngine->init(versionMapping[version]);

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
    size_t n = sgct::Engine::instance()->getNumberOfWindows();
    for (size_t i = 0; i < n; ++i) {
        GLFWwindow* w = sgct::Engine::instance()->getWindowPtr(i)->getWindowHandle();
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


    // Do not print message if slaves are waiting for the master
    // Only timeout after 15 minutes
    SgctEngine->setSyncParameters(false, 15.f * 60.f);

    auto cleanup = [&](bool isInitialized) {
        if (isInitialized) {
            global::openSpaceEngine.deinitializeGL();
            global::openSpaceEngine.deinitialize();
        }

        // Clear function bindings to avoid crash after destroying the OpenSpace Engine
        sgct::MessageHandler::instance()->setLogToCallback(false);
        sgct::MessageHandler::instance()->setLogCallback(nullptr);

        LDEBUG("Destroying SGCT Engine");
        delete SgctEngine;

#ifdef OPENVR_SUPPORT
        // Clean up OpenVR
        sgct::SGCTOpenVR::shutdown();
#endif

#ifdef OPENSPACE_HAS_SPOUT
        for (SpoutWindow& w : SpoutWindows) {
            if (w.leftOrMain.handle) {
                w.leftOrMain.handle->ReleaseReceiver();
                w.leftOrMain.handle->Release();
            }
            if (w.right.handle) {
                w.right.handle->ReleaseReceiver();
                w.right.handle->Release();
            }
        }
#endif // OPENSPACE_HAS_SPOUT

        ghoul::deinitialize();
    };

    if (!initSuccess) {
        LFATAL("Initializing failed");
        cleanup(false);
        return EXIT_FAILURE;
    }

    LINFO("Starting rendering loop");
    SgctEngine->render();
    LINFO("Ending rendering loop");

    cleanup(true);

    exit(EXIT_SUCCESS);
}
