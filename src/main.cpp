
// open space includes
#include "renderengine.h"

// sgct includes
#include "sgct.h"

// std includes
#include <cstdlib>
#include <cstdio>

#include <ghoul/filesystem/filesystem.h>

// graphics and openspace engines
sgct::Engine * gEngine;
openspace::RenderEngine *rEngine;

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

/**
* The main function, only purpose is to connect the
* OpenSpace rendering engine with SGCT.
*/
int main(int argc, char **argv) {
    ghoul::filesystem::FileSystem::initialize();
    FileSys.registerPathToken("${BASE_PATH}", "../..");

    char* cmd = "-config";
    const std::string pathStr = p("${BASE_PATH}/config/single.xml");
    char* path = _strdup(pathStr.c_str());
    char** newArgv = new char*[3];
    int newArgc = 3;
    newArgv[0] = argv[0];
    newArgv[1] = cmd;
    newArgv[2] = path;

	// allocate sgct- and openspace engine objects
	gEngine = new sgct::Engine( newArgc, newArgv );
	rEngine = new openspace::RenderEngine(argc, argv);
	
    free(path);
    delete[] newArgv;
 	
    // Bind functions
	gEngine->setInitOGLFunction( mainInitFunc );
	gEngine->setPreSyncFunction( mainPreSyncFunc );
	gEngine->setPostSyncPreDrawFunction( mainPostSyncPreDrawFunc );
	gEngine->setDrawFunction( mainRenderFunc );
	gEngine->setPostDrawFunction( mainPostDrawFunc );
	gEngine->setKeyboardCallbackFunction( mainKeyboardCallback );
	gEngine->setMouseButtonCallbackFunction( mainMouseButtonCallback );
	gEngine->setMousePosCallbackFunction( mainMousePosCallback );
	gEngine->setMouseScrollCallbackFunction( mainMouseScrollCallback );

	// set encode and decode functions
	// NOTE: starts synchronizing before init functions
	sgct::SharedData::instance()->setEncodeFunction(mainEncodeFun);
	sgct::SharedData::instance()->setDecodeFunction(mainDecodeFun);

	// try to open a window
	if( ! gEngine->init(sgct::Engine::OpenGL_4_4_Core_Profile)) {

		// could not open a window, deallocates and exits
		delete gEngine;
		delete rEngine;
		return EXIT_FAILURE;
	}
 
	// Main loop
	gEngine->render();
 
	// Clean up (de-allocate)
	delete gEngine;
	delete rEngine;
	
	// Exit program
	exit( EXIT_SUCCESS );
}


void mainInitFunc(void) {
    rEngine->init();
}

void mainPreSyncFunc(void) {
	rEngine->preSync();
}

void mainPostSyncPreDrawFunc(void) {
	rEngine->postSyncPreDraw();
}

void mainRenderFunc(void) {
	rEngine->render();
}

void mainPostDrawFunc(void) {
	rEngine->postDraw();
}

void mainKeyboardCallback(int key, int action) {
	if (gEngine->isMaster())
	{
		rEngine->keyboardCallback(key, action);
	}
}

void mainMouseButtonCallback(int key, int action) {
	if (gEngine->isMaster())
	{
		rEngine->mouseButtonCallback(key, action);
	}
}

void mainMousePosCallback(double x, double y) {
	if (gEngine->isMaster())
	{
		rEngine->mousePosCallback(static_cast<int>(x), static_cast<int>(y));
	}
}

void mainMouseScrollCallback(double pos, double /*pos2*/) {
	if (gEngine->isMaster())
	{
		rEngine->mouseScrollCallback(static_cast<int>(pos));
	}
}

void mainEncodeFun() {
	rEngine->encode();
}

void mainDecodeFun() {
	rEngine->decode();
}


