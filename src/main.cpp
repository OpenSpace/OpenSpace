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
#include "openspaceengine.h"

// sgct includes
#include "sgct.h"

sgct::Engine* _sgctEngine;

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

int main(int argc, char **argv) {
    int newArgc;
    char** newArgv;
    openspace::OpenSpaceEngine::create(argc, argv, newArgc, newArgv);
    _sgctEngine = new sgct::Engine(newArgc, newArgv);
    delete[] newArgv;
 	
    // Bind functions
	_sgctEngine->setInitOGLFunction( mainInitFunc );
	_sgctEngine->setPreSyncFunction( mainPreSyncFunc );
	_sgctEngine->setPostSyncPreDrawFunction( mainPostSyncPreDrawFunc );
	_sgctEngine->setDrawFunction( mainRenderFunc );
	_sgctEngine->setPostDrawFunction( mainPostDrawFunc );
	_sgctEngine->setKeyboardCallbackFunction( mainKeyboardCallback );
	_sgctEngine->setMouseButtonCallbackFunction( mainMouseButtonCallback );
	_sgctEngine->setMousePosCallbackFunction( mainMousePosCallback );
	_sgctEngine->setMouseScrollCallbackFunction( mainMouseScrollCallback );

	// set encode and decode functions
	// NOTE: starts synchronizing before init functions
	sgct::SharedData::instance()->setEncodeFunction(mainEncodeFun);
	sgct::SharedData::instance()->setDecodeFunction(mainDecodeFun);

	// try to open a window
	if( ! _sgctEngine->init(sgct::Engine::OpenGL_4_0_Core_Profile)) {
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
	exit( EXIT_SUCCESS );
}


void mainInitFunc(void) {
    OsEng.initialize();
    OsEng.initializeGL();
}

void mainPreSyncFunc(void) {
    OsEng.preSynchronization();
}

void mainPostSyncPreDrawFunc(void) {
    OsEng.postSynchronizationPreDraw();
}

void mainRenderFunc(void) {
    OsEng.render();
}

void mainPostDrawFunc(void) {
    OsEng.postDraw();
}

void mainKeyboardCallback(int key, int action) {
	if (_sgctEngine->isMaster())
        OsEng.keyboardCallback(key, action);
}

void mainMouseButtonCallback(int key, int action) {
	if (_sgctEngine->isMaster())
        OsEng.mouseButtonCallback(key, action);
}

void mainMousePosCallback(double x, double y) {
    // TODO use float instead
	if (_sgctEngine->isMaster())
        OsEng.mousePositionCallback(static_cast<int>(x), static_cast<int>(y));
}

void mainMouseScrollCallback(double pos, double /*pos2*/) {
    // TODO use float instead
	if (_sgctEngine->isMaster())
        OsEng.mouseScrollWheelCallback(static_cast<int>(pos));
}

void mainEncodeFun() {
	OsEng.encode();
}

void mainDecodeFun() {
	OsEng.decode();
}


