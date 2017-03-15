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

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/wrapper/sgctwindowwrapper.h>
#include <openspace/util/keys.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/opengl/ghoul_gl.h>

#include <sgct.h>

#ifdef OPENVR_SUPPORT
#include <SGCTOpenVR.h>
#endif // OPENVR_SUPPORT

#define DEVELOPER_MODE

namespace {
    
const char* _loggerCat = "main";

sgct::Engine* SgctEngine;
    
#ifdef OPENVR_SUPPORT
sgct::SGCTWindow* FirstOpenVRWindow = nullptr;
#endif

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

void mainInitFunc() {
    LTRACE("main::mainInitFunc(begin)");

    LDEBUG("Initializing OpenSpace Engine started");
    OsEng.initialize();
    LDEBUG("Initializing OpenSpace Engine finished");

    LDEBUG("Initializing OpenGL in OpenSpace Engine started");
    OsEng.initializeGL();
    LDEBUG("Initializing OpenGL in OpenSpace Engine finished");
    
    // Find if we have at least one OpenVR window
    // Save reference to first OpenVR window, which is the one we will copy to the HMD.
    for (size_t i = 0; i < SgctEngine->getNumberOfWindows(); ++i) {
        if (SgctEngine->getWindowPtr(i)->checkIfTagExists("OpenVR")) {
#ifdef OPENVR_SUPPORT
            FirstOpenVRWindow = SgctEngine->getWindowPtr(i);
            
            // If we have an OpenVRWindow, initialize OpenVR.
            sgct::SGCTOpenVR::initialize(
                SgctEngine->getNearClippingPlane(), _sgctEngine->getFarClippingPlane()
            );
#else
            LWARNING(
                "OpenVR was requested, but OpenSpace was compiled without VR support."
            );
#endif

            break;
        }
    }

    // Set the clear color for all non-linear projection viewports
    // @CLEANUP:  Why is this necessary?  We can set the clear color in the configuration
    // files --- abock
    size_t nWindows = SgctEngine->getNumberOfWindows();
    for (size_t i = 0; i < nWindows; ++i) {
        sgct::SGCTWindow* w = SgctEngine->getWindowPtr(i);
        size_t nViewports = w->getNumberOfViewports();
        for (size_t j = 0; j < nViewports; ++j) {
            sgct_core::Viewport* v = w->getViewport(j);
            ghoul_assert(v != nullptr, "Number of reported viewports was incorrect");
            sgct_core::NonLinearProjection* p = v->getNonLinearProjectionPtr();
            if (p)
                p->setClearColor(glm::vec4(0.f, 0.f, 0.f, 1.f));
        }
    }
    LTRACE("main::mainInitFunc(end)");
}

void mainPreSyncFunc() {
    LTRACE("main::mainPreSyncFunc(begin)");
    OsEng.setRunTime(sgct::Engine::getTime());
    OsEng.preSynchronization();
    LTRACE("main::mainPreSyncFunc(end)");
}

void mainPostSyncPreDrawFunc() {
    LTRACE("main::postSynchronizationPreDraw(begin)");
    OsEng.postSynchronizationPreDraw();

#ifdef OPENVR_SUPPORT
    if (FirstOpenVRWindow) {
        // Update pose matrices for all tracked OpenVR devices once per frame
        sgct::SGCTOpenVR::updatePoses();
    }
#endif

    LTRACE("main::postSynchronizationPreDraw(end)");
}

void mainRenderFunc() {
    LTRACE("main::mainRenderFunc(begin)");

    glm::mat4 viewMatrix =
        SgctEngine->getCurrentViewMatrix() *
        // User matrix
        glm::translate(
           glm::mat4(1.f),
           SgctEngine->getDefaultUserPtr()->getPos()
       )
    ;

    glm::mat4 projectionMatrix = SgctEngine->getCurrentProjectionMatrix();
#ifdef OPENVR_SUPPORT
    bool currentWindowIsHMD = FirstOpenVRWindow == _sgctEngine->getCurrentWindowPtr();
    if (sgct::SGCTOpenVR::isHMDActive() && currentWindowIsHMD) {
        projectionMatrix = sgct::SGCTOpenVR::getHMDCurrentViewProjectionMatrix(
            _sgctEngine->getCurrentFrustumMode()
        );
    }
#endif

    if (SgctEngine->isMaster()) {
        OsEng.render(viewMatrix, projectionMatrix);
    }
    else {
        glm::mat4 sceneMatrix = SgctEngine->getModelMatrix();
        OsEng.render(viewMatrix * sceneMatrix, projectionMatrix);
    }
    LTRACE("main::mainRenderFunc(end)");
}

void mainPostDrawFunc() {
    LTRACE("main::mainPostDrawFunc(begin)");

#ifdef OPENVR_SUPPORT
    if (FirstOpenVRWindow) {
        // Copy the first OpenVR window to the HMD
        sgct::SGCTOpenVR::copyWindowToHMD(FirstOpenVRWindow);
    }
#endif

    OsEng.postDraw();
    LTRACE("main::mainPostDrawFunc(end)");
}

void mainExternalControlCallback(const char* receivedChars, int size) {
    LTRACE("main::mainExternalControlCallback(begin)");
    if (SgctEngine->isMaster()) {
        OsEng.externalControlCallback(receivedChars, size, 0);
    }
    LTRACE("main::mainExternalControlCallback(end)");
}

void mainKeyboardCallback(int key, int, int action, int mods) {
    LTRACE("main::mainKeyboardCallback(begin)");
    if (SgctEngine->isMaster()) {
        OsEng.keyboardCallback(
            openspace::Key(key),
            openspace::KeyModifier(mods),
            openspace::KeyAction(action)
        );
    }
    LTRACE("main::mainKeyboardCallback(begin)");
}

void mainMouseButtonCallback(int key, int action) {
    LTRACE("main::mainMouseButtonCallback(begin)");
    if (SgctEngine->isMaster()) {
        OsEng.mouseButtonCallback(
            openspace::MouseButton(key),
            openspace::MouseAction(action)
        );
    }
    LTRACE("main::mainMouseButtonCallback(end)");
}

void mainMousePosCallback(double x, double y) {
    if (SgctEngine->isMaster()) {
        OsEng.mousePositionCallback(x, y);
    }
}

void mainMouseScrollCallback(double posX, double posY) {
    LTRACE("main::mainMouseScrollCallback(begin");
    if (SgctEngine->isMaster()) {
        OsEng.mouseScrollWheelCallback(posY);
    }
    LTRACE("main::mainMouseScrollCallback(end)");
}

void mainCharCallback(unsigned int codepoint, int mods) {
    if (SgctEngine->isMaster()) {
        OsEng.charCallback(codepoint, openspace::KeyModifier(mods));
    }
}

void mainEncodeFun() {
    LTRACE("main::mainEncodeFun(begin)");
    OsEng.encode();
    LTRACE("main::mainEncodeFun(end)");
}

void mainDecodeFun() {
    LTRACE("main::mainDecodeFun(begin)");
    OsEng.decode();
    LTRACE("main::mainDecodeFun(end)");
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

int main_main(int argc, char** argv) {
    std::pair<int, int> glVersion = supportedOpenGLVersion();
    
    // Create the OpenSpace engine and get arguments for the SGCT engine
    // @CLEANUP:  Replace the return valua with throwing an exception --abock
    std::vector<std::string> sgctArguments;
    bool requestQuit = false;
    openspace::OpenSpaceEngine::create(
        argc, argv,
        std::make_unique<openspace::SGCTWindowWrapper>(),
        sgctArguments,
        requestQuit
    );
    
    if (requestQuit) {
        return EXIT_SUCCESS;
    }
    
    LINFO("Detected OpenGL version: " << glVersion.first << "." << glVersion.second);
    
    // Create sgct engine c arguments
    int newArgc = static_cast<int>(sgctArguments.size());
    
    char** newArgv = new char*[newArgc];
    for (int i = 0; i < newArgc; ++i) {
        newArgv[i] = const_cast<char*>(sgctArguments.at(i).c_str());
    }
    
    // Need to set this before the creation of the sgct::Engine
    sgct::MessageHandler::instance()->setLogToConsole(false);
    sgct::MessageHandler::instance()->setShowTime(false);
    sgct::MessageHandler::instance()->setLogToCallback(true);
    sgct::MessageHandler::instance()->setLogCallback(mainLogCallback);
    
#ifdef __APPLE__
    glfwWindowHint(GLFW_STENCIL_BITS, 8);
#endif
    
    LDEBUG("Creating SGCT Engine");
    SgctEngine = new sgct::Engine(newArgc, newArgv);
    
    // Deallocate sgct c arguments
    delete[] newArgv;
    
    // Bind functions
    SgctEngine->setInitOGLFunction(mainInitFunc);
    SgctEngine->setPreSyncFunction(mainPreSyncFunc);
    SgctEngine->setPostSyncPreDrawFunction(mainPostSyncPreDrawFunc);
    SgctEngine->setDrawFunction(mainRenderFunc);
    SgctEngine->setPostDrawFunction(mainPostDrawFunc);
    SgctEngine->setKeyboardCallbackFunction(mainKeyboardCallback);
    SgctEngine->setMouseButtonCallbackFunction(mainMouseButtonCallback);
    SgctEngine->setMousePosCallbackFunction(mainMousePosCallback);
    SgctEngine->setMouseScrollCallbackFunction(mainMouseScrollCallback);
    SgctEngine->setExternalControlCallback(mainExternalControlCallback);
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
        { { 4, 5 }, sgct::Engine::RunMode::OpenGL_4_5_Core_Profile }
    };
    ghoul_assert(
        versionMapping.find(glVersion) != versionMapping.end(),
        "Unknown OpenGL version. Missing statement in version mapping map"
    );
    
    auto cleanup = [&](){
        OsEng.deinitialize();
        
        // Clear function bindings to avoid crash after destroying the OpenSpace Engine
        sgct::MessageHandler::instance()->setLogToCallback(false);
        sgct::MessageHandler::instance()->setLogCallback(nullptr);
        
        LDEBUG("Destroying OpenSpaceEngine");
        openspace::OpenSpaceEngine::destroy();
        
        LDEBUG("Destroying SGCT Engine");
        delete SgctEngine;
    };
    
    bool initSuccess = SgctEngine->init(versionMapping[glVersion]);
    
    if (!initSuccess) {
        LFATAL("Initializing failed");
        cleanup();
        return EXIT_FAILURE;
    }
    
    // Main loop
    LDEBUG("Starting rendering loop");
    SgctEngine->render();
    LDEBUG("Ending rendering loop");
    
    cleanup();
    
#ifdef OPENVR_SUPPORT
    // Clean up OpenVR
    sgct::SGCTOpenVR::shutdown();
#endif
    
    // Exit program
    exit(EXIT_SUCCESS); 
}
    
} // namespace

int main(int argc, char** argv) {
    // If we are working as a developer, we don't want to catch the exceptions in order to
    // find the locations where the exceptions are raised.
    // If we are not in developer mode, we want to catch and at least log the error before
    // dying
#ifdef DEVELOPER_MODE
    return main_main(argc, argv);
#else
    // We wrap the actual main function in a try catch block so that we can get and print
    // some additional information in case an exception is raised
    try {
        return main_main(argc, argv);
    }
    catch (const ghoul::RuntimeError& e) {
        // Write out all of the information about the exception and flush the logs
        LFATALC(e.component, e.message);
        LogMgr.flushLogs();
        return EXIT_FAILURE;
    }
    catch (const ghoul::AssertionException& e) {
        // We don't want to catch the assertion exception as we won't be able to add a
        // breakpoint for debugging
        LFATALC("Assertion failed", e.what());
        throw;
    } catch (const std::exception& e) {
        LFATALC("Exception", e.what());
        LogMgr.flushLogs();
        return EXIT_FAILURE;
    }
    catch (...) {
        LFATALC("Exception", "Unknown exception");
        LogMgr.flushLogs();
        return EXIT_FAILURE;
    }
#endif // DEVELOPER_MODE
}
