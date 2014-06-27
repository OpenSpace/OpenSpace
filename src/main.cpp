/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014                                                                    *
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


// open space includes
#include <openspace/engine/openspaceengine.h>
#include <openspace/interface/interface.h>

// sgct includes
#include <sgct.h>

sgct::Engine* _sgctEngine;
openspace::Interface* _interface;

// function pointer declarations
void mainInitFunc(void);
void mainPreSyncFunc(void);
void mainPostSyncPreDrawFunc(void);
void mainRenderFunc(void);
void mainPostDrawFunc(void);
void mainKeyboardCallback(int key, int action);
void mainMouseButtonCallback(int key, int action);
void mainMousePosCallback(double x, double y);
void mainMouseScrollCallback(double posX, double posY);
void mainEncodeFun();
void mainDecodeFun();
void mainExternalControlCallback(const char * receivedChars, int size, int clientId);

int main(int argc, char** argv)
{
    // create the OpenSpace engine and get arguments for the sgct engine
    std::vector<std::string> sgctArguments;
    openspace::OpenSpaceEngine::create(argc, argv, sgctArguments);

    // create sgct engine c arguments
    int newArgc = static_cast<int>(sgctArguments.size());
    char** newArgv = new char* [newArgc];
    for (int i = 0; i < newArgc; ++i) {
        // newArgv[i] = new char[sgctArguments.at(i).length()];
        // std::strcpy(newArgv[i], sgctArguments.at(i).c_str());
        newArgv[i] = const_cast<char*>(sgctArguments.at(i).c_str());
    }

    _sgctEngine = new sgct::Engine(newArgc, newArgv);

    // deallocate sgct c arguments
    for (int i = 0; i < newArgc; ++i) {
        // delete newArgv[i];
    }
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

    // set encode and decode functions
    // NOTE: starts synchronizing before init functions
    sgct::SharedData::instance()->setEncodeFunction(mainEncodeFun);
    sgct::SharedData::instance()->setDecodeFunction(mainDecodeFun);

    // init the interface which will handle callbacks from an external gui
    _interface = new openspace::Interface(&OsEng);

    // try to open a window
    if (!_sgctEngine->init(sgct::Engine::OpenGL_4_0_Core_Profile)) {
        // could not open a window, deallocates and exits
        delete _sgctEngine;
        openspace::OpenSpaceEngine::destroy();
        return EXIT_FAILURE;
    }

    // Main loop
    _sgctEngine->render();

    // Clean up (de-allocate)
    delete _sgctEngine;

    // Exit program
    exit(EXIT_SUCCESS);
}

void mainExternalControlCallback(const char* receivedChars, int size, int clientId)
{
    if (_sgctEngine->isMaster())
        _interface->callback(receivedChars);
}

void mainInitFunc(void)
{
    OsEng.initialize();
    OsEng.initializeGL();
}

void mainPreSyncFunc(void)
{
    OsEng.preSynchronization();
}

void mainPostSyncPreDrawFunc(void)
{
    OsEng.postSynchronizationPreDraw();
}

void mainRenderFunc(void)
{
    OsEng.render();
}

void mainPostDrawFunc(void)
{
    OsEng.postDraw();
}

void mainKeyboardCallback(int key, int action)
{
    if (_sgctEngine->isMaster())
        OsEng.keyboardCallback(key, action);
}

void mainMouseButtonCallback(int key, int action)
{
    if (_sgctEngine->isMaster())
        OsEng.mouseButtonCallback(key, action);
}

void mainMousePosCallback(double x, double y)
{
    // TODO use float instead
    if (_sgctEngine->isMaster())
        OsEng.mousePositionCallback(static_cast<int>(x), static_cast<int>(y));
}

void mainMouseScrollCallback(double posX, double posY)
{
    // TODO use float instead
    if (_sgctEngine->isMaster())
        OsEng.mouseScrollWheelCallback(static_cast<int>(posY));
}

void mainEncodeFun()
{
    OsEng.encode();
}

void mainDecodeFun()
{
    OsEng.decode();
}
