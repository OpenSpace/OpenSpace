/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2015                                                               *
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
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logging>
#include <openspace/rendering/renderengine.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <sgct.h>

sgct::Engine* _sgctEngine;

// function pointer declarations
void mainInitFunc();
void mainPreSyncFunc();
void mainPostSyncPreDrawFunc();
void mainRenderFunc();
void mainPostDrawFunc();
void mainKeyboardCallback(int key, int action);
void mainCharCallback(unsigned int codepoint);
void mainMouseButtonCallback(int key, int action);
void mainMousePosCallback(double x, double y);
void mainMouseScrollCallback(double posX, double posY);
void mainEncodeFun();
void mainDecodeFun();
void mainExternalControlCallback(const char * receivedChars, int size);
void mainLogCallback(const char* msg);

std::pair<int, int> supportedOpenGLVersion () {
    glfwInit();

    //On OS X we need to explicitly set the version and specify that we are using CORE profile
    //to be able to use glGetIntegerv(GL_MAJOR_VERSION, &major) and glGetIntegerv(GL_MINOR_VERSION, &minor)
    //explicitly setting to OGL 3.3 CORE works since all Mac's now support at least 3.3
#ifdef __APPLE__
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
#endif
    
    glfwWindowHint(GLFW_VISIBLE, GL_FALSE);
    GLFWwindow* offscreen = glfwCreateWindow(128, 128, "", nullptr, nullptr);
    glfwMakeContextCurrent(offscreen);

    int major, minor;
    glGetIntegerv(GL_MAJOR_VERSION, &major);
    glGetIntegerv(GL_MINOR_VERSION, &minor);

    glfwDestroyWindow(offscreen);
    glfwWindowHint(GLFW_VISIBLE, GL_TRUE);
    return { major, minor };
}

#include <ghoul/lua/lua_helper.h>

namespace {
    const std::string _loggerCat = "main";
}

int main(int argc, char** argv) {
    auto glVersion = supportedOpenGLVersion();
    
    // create the OpenSpace engine and get arguments for the sgct engine
    std::vector<std::string> sgctArguments;
    const bool success = openspace::OpenSpaceEngine::create(
        argc, argv,
        sgctArguments
    );
    if (!success)
        return EXIT_FAILURE;
    
    LINFO("Detected OpenGL version: " << glVersion.first << "." << glVersion.second);

    // create sgct engine c arguments
    int newArgc = static_cast<int>(sgctArguments.size());
    char** newArgv = new char*[newArgc];
    for (int i = 0; i < newArgc; ++i)
        newArgv[i] = const_cast<char*>(sgctArguments.at(i).c_str());

    // Need to set this before the creation of the sgct::Engine
    sgct::MessageHandler::instance()->setLogToConsole(false);
    sgct::MessageHandler::instance()->setShowTime(false);
    sgct::MessageHandler::instance()->setLogToCallback(true);
    sgct::MessageHandler::instance()->setLogCallback(mainLogCallback);

#ifdef __APPLE__
    glfwWindowHint(GLFW_STENCIL_BITS, 8);
#endif
    
    LDEBUG("Creating SGCT Engine");
    _sgctEngine = new sgct::Engine(newArgc, newArgv);

    // deallocate sgct c arguments
    delete[] newArgv;

    // Bind functions
    _sgctEngine->setInitOGLFunction(mainInitFunc);
    _sgctEngine->setPreSyncFunction(mainPreSyncFunc);
    _sgctEngine->setPostSyncPreDrawFunction(mainPostSyncPreDrawFunc);
    _sgctEngine->setDrawFunction(mainRenderFunc);
    _sgctEngine->setPostDrawFunction(mainPostDrawFunc);
    _sgctEngine->setKeyboardCallbackFunction(mainKeyboardCallback);
    _sgctEngine->setMouseButtonCallbackFunction(mainMouseButtonCallback);
    _sgctEngine->setMousePosCallbackFunction(mainMousePosCallback);
    _sgctEngine->setMouseScrollCallbackFunction(mainMouseScrollCallback);
    _sgctEngine->setExternalControlCallback(mainExternalControlCallback);
    _sgctEngine->setCharCallbackFunction(mainCharCallback);

    // set encode and decode functions
    // NOTE: starts synchronizing before init functions
    sgct::SharedData::instance()->setEncodeFunction(mainEncodeFun);
    sgct::SharedData::instance()->setDecodeFunction(mainDecodeFun);

    // try to open a window
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
    if (versionMapping.find(glVersion) == versionMapping.end()) {
        LFATAL("Requested OpenGL version " << glVersion.first << "." << glVersion.second << " not supported");
        return EXIT_FAILURE;
    }
    sgct::Engine::RunMode rm = versionMapping[glVersion];
    const bool initSuccess = _sgctEngine->init(rm);

    if (!initSuccess) {
        LFATAL("Initializing failed");
        // could not open a window, deallocates and exits
        delete _sgctEngine;
        openspace::OpenSpaceEngine::destroy();
        return EXIT_FAILURE;
    }

    // Main loop
    LDEBUG("Starting rendering loop");
    _sgctEngine->render();

	//clear function bindings to avoid crash after destroying the OpenSpace Engine
	sgct::MessageHandler::instance()->setLogToCallback(false);
	sgct::MessageHandler::instance()->setLogCallback(nullptr);

    LDEBUG("Destroying OpenSpaceEngine");
    openspace::OpenSpaceEngine::destroy();

    // Clean up (de-allocate)
    LDEBUG("Destroying SGCT Engine");
    delete _sgctEngine;

    // Exit program
    exit(EXIT_SUCCESS); 
}

void mainInitFunc() {
    //is this node the master?	(must be set after call to _sgctEngine->init())
    OsEng.setMaster(_sgctEngine->isMaster());
    
    bool success = OsEng.initialize();
    if (success)
        success = OsEng.initializeGL();

    if (!success) {
        LFATAL("Initializing OpenSpaceEngine failed");
        std::cout << "Press any key to continue...";
        std::cin.ignore(100);
        exit(EXIT_FAILURE);
    }
    
    // Set the clear color for all non-linear projection viewports
    size_t nWindows = _sgctEngine->getNumberOfWindows();
    for (size_t i = 0; i < nWindows; ++i) {
        sgct::SGCTWindow* w = _sgctEngine->getWindowPtr(i);
        size_t nViewports = nViewports = w->getNumberOfViewports();
        for (size_t j = 0; j < nViewports; ++j) {
            sgct_core::Viewport* v = w->getViewport(j);
            ghoul_assert(v != nullptr, "Number of reported viewports was incorrect");
            sgct_core::NonLinearProjection* p = v->getNonLinearProjectionPtr();
            if (p)
                p->setClearColor(glm::vec4(0.f, 0.f, 0.f, 1.f));
        }
    }

}

void mainPreSyncFunc() {
    OsEng.setRunTime(sgct::Engine::getTime());
    OsEng.preSynchronization();
}

void mainPostSyncPreDrawFunc() {
    OsEng.postSynchronizationPreDraw();
}

void mainRenderFunc() {
    using glm::mat4;
    using glm::translate;
    //not the most efficient, but for clarity @JK
    
    mat4 userMatrix = translate(mat4(1.f), _sgctEngine->getDefaultUserPtr()->getPos());
    mat4 sceneMatrix = _sgctEngine->getModelMatrix();
    mat4 viewMatrix = _sgctEngine->getCurrentViewMatrix() * userMatrix;
    
    //dont shift nav-direction on master, makes it very tricky to navigate @JK
    if (!OsEng.ref().isMaster())
        viewMatrix = viewMatrix * sceneMatrix;

    mat4 projectionMatrix = _sgctEngine->getCurrentProjectionMatrix();
    OsEng.render(projectionMatrix, viewMatrix);
}

void mainPostDrawFunc() {
    OsEng.postDraw();
}

void mainExternalControlCallback(const char* receivedChars, int size) {
    if (OsEng.isMaster())
        OsEng.externalControlCallback(receivedChars, size, 0);
}

void mainKeyboardCallback(int key, int action) {
    if (OsEng.isMaster())
        OsEng.keyboardCallback(key, action);
}

void mainMouseButtonCallback(int key, int action) {
    if (OsEng.isMaster())
        OsEng.mouseButtonCallback(key, action);
}

void mainMousePosCallback(double x, double y) {
    if (OsEng.isMaster())
        OsEng.mousePositionCallback(x, y);
}

void mainMouseScrollCallback(double posX, double posY) {
    if (OsEng.isMaster())
        OsEng.mouseScrollWheelCallback(posY);
}

void mainCharCallback(unsigned int codepoint) {
    if (OsEng.isMaster())
        OsEng.charCallback(codepoint);
}

void mainEncodeFun() {
    OsEng.encode();
}

void mainDecodeFun() {
    OsEng.decode();
}

void mainLogCallback(const char* msg){
    std::string message = msg;
    if (message == ".")
        // We don't want the empty '.' message that SGCT sends while it is waiting for
        // connections from other network nodes
        return;
    // Remove the trailing \n that is passed along
    LINFOC("SGCT", message.substr(0, std::max<size_t>(message.size() - 1, 0)));
}
