
// open space includes
#include "renderengine.h"

// sgct includes
#include "sgct.h"

// std includes
#include <cstdlib>
#include <cstdio>

#include <ghoul/filesystem/filesystem.h>

#include <lua.hpp>
#include <lstate.h>
#include <chrono>

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

void PrintTable(lua_State *L)
{
    lua_pushnil(L);

    while(lua_next(L, -2) != 0)
    {
        if(lua_isstring(L, -1))
            printf("%s = %s\n", lua_tostring(L, -2), lua_tostring(L, -1));
        else if(lua_isnumber(L, -1))
            printf("%s = %d\n", lua_tostring(L, -2), lua_tonumber(L, -1));
        else if(lua_istable(L, -1))
            PrintTable(L);

        lua_pop(L, 1);
    }
}

static void stackDump (lua_State *L) {
    int i;
    int top = lua_gettop(L);
    for (i = 1; i <= top; i++) {  /* repeat for each level */
        int t = lua_type(L, i);
        switch (t) {

        case LUA_TSTRING:  /* strings */
            printf("`%s'", lua_tostring(L, i));
            break;

        case LUA_TBOOLEAN:  /* booleans */
            printf(lua_toboolean(L, i) ? "true" : "false");
            break;

        case LUA_TNUMBER:  /* numbers */
            printf("%g", lua_tonumber(L, i));
            break;

        case LUA_TTABLE:
            PrintTable(L);
            break;
        default:  /* other values */
            printf("%s", lua_typename(L, t));
            break;

        }
        printf("  ");  /* put a separator */
    }
    printf("\n");  /* end the listing */
}

int main(int argc, char **argv) {
    ghoul::filesystem::FileSystem::initialize();
    //FileSys.registerPathToken("${BASE_PATH}", "../../..");
    FileSys.registerPathToken("${BASE_PATH}", "../..");
    FileSys.registerPathToken("${SCRIPTS}", "${BASE_PATH}/scripts");

    LARGE_INTEGER t1;
    QueryPerformanceCounter(&t1);
    lua_State* l = luaL_newstate();
    LARGE_INTEGER t2;
    QueryPerformanceCounter(&t2);

    luaL_openlibs(l);
    LARGE_INTEGER t3;
    QueryPerformanceCounter(&t3);
    if (luaL_loadfile(l, p("${SCRIPTS}/script.lua").c_str())) {
        std::cerr << lua_tostring(l, -1) << std::endl;
        return EXIT_SUCCESS;
    }
    LARGE_INTEGER t4;
    QueryPerformanceCounter(&t4);

    if (lua_pcall(l,0, LUA_MULTRET, 0)) {
        std::cerr << lua_tostring(l, -1) << std::endl;
        return EXIT_SUCCESS;
    }
    LARGE_INTEGER t5;
    QueryPerformanceCounter(&t5);

    stackDump(l);

    lua_getglobal(l, "t");
    std::cout << lua_istable(l, -1) << std::endl;

    stackDump(l);
    
    
    
    lua_close(l);
    LARGE_INTEGER t6;
    QueryPerformanceCounter(&t6);




    // --------
    LARGE_INTEGER freq;
    QueryPerformanceFrequency(&freq);

    std::cout << "State: " << ((t2.QuadPart - t1.QuadPart) / double(freq.QuadPart)) * 1000  << std::endl;
    std::cout << "Libs : " << ((t3.QuadPart - t2.QuadPart) / double(freq.QuadPart)) * 1000 << std::endl;
    std::cout << "Load : " << ((t4.QuadPart - t3.QuadPart) / double(freq.QuadPart)) * 1000 << std::endl;
    std::cout << "Exec : " << ((t5.QuadPart - t4.QuadPart) / double(freq.QuadPart)) * 1000 << std::endl;
    std::cout << "Close: " << ((t6.QuadPart - t5.QuadPart) / double(freq.QuadPart)) * 1000 << std::endl;

    
    exit(EXIT_SUCCESS);

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
	if( ! gEngine->init(sgct::Engine::OpenGL_4_0_Core_Profile)) {

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


