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

#include <openspace/scripting/scriptengine.h>

#include <openspace/scripting/scriptfunctions.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/crc32.h>

#include <ghoul/lua/lua_helper.h>
#include <fstream>

namespace openspace {
namespace scripting {

namespace {
    const std::string _loggerCat = "ScriptEngine";
    
    const std::string _openspaceLibraryName = "openspace";
    const std::string _luaGlobalNamespace = "_G";
    const std::string _printFunctionName = "print";
    const lua_CFunction _printFunctionReplacement = printInfo;
    
    const int _setTableOffset = -3; // -1 (top) -1 (first argument) -1 (second argument)
}

ScriptEngine::ScriptEngine()
    : _state(nullptr)
{
}

bool ScriptEngine::initialize() {
    _state = luaL_newstate();
    LDEBUG("Creating Lua state");
    if (_state == nullptr) {
        LFATAL("Error creating new Lua state: Memory allocation error");
        return false;
    }
    LDEBUG("Open Lua libraries");
    luaL_openlibs(_state);
    
    LDEBUG("Add OpenSpace modules");
    
    LDEBUG("Create openspace base library");
    lua_newtable(_state);
    lua_setglobal(_state, _openspaceLibraryName.c_str());
    
    LDEBUG("Adding base functions");
    addBaseLibrary();
    
    LDEBUG("Remap print function");
    remapPrintFunction();
    
    return true;
}

void ScriptEngine::deinitialize() {
    lua_close(_state);
    _state = nullptr;
}

bool ScriptEngine::addLibrary(const ScriptEngine::LuaLibrary& library) {
    assert(_state);
    if (library.functions.empty()) {
        LERROR("Lua library '" << library.name << "' does not have any functions");
        return false;
    }

	//ghoul::lua::logStack(_state);
    lua_getglobal(_state, _openspaceLibraryName.c_str());
	//ghoul::lua::logStack(_state);
    if (library.name.empty()) {
		//ghoul::lua::logStack(_state);
        addLibraryFunctions(library, true);
		//ghoul::lua::logStack(_state);
		lua_pop(_state, 1);
		//ghoul::lua::logStack(_state);
    }
    else {
        const bool allowed = isLibraryNameAllowed(library.name);
        if (!allowed)
            return false;
        
		//ghoul::lua::logStack(_state);
		
        lua_pushstring(_state, library.name.c_str());
		//ghoul::lua::logStack(_state);
        lua_newtable(_state);
		//ghoul::lua::logStack(_state);
        addLibraryFunctions(library, false);
        lua_settable(_state, _setTableOffset);
        //ghoul::lua::logStack(_state);

        _registeredLibraries.insert(ghoul::hashCRC32(library.name));
    }

    return true;
}

bool ScriptEngine::runScript(const std::string& script) {
    if (script.empty())
        return false;
    int status = luaL_loadstring(_state, script.c_str());
    if (status != LUA_OK) {
        LERROR("Error loading script: '" << lua_tostring(_state, -1) << "'");
        return false;
    }
    
    LDEBUG("Executing script");
    if (lua_pcall(_state, 0, LUA_MULTRET, 0)) {
        LERROR("Error executing script: " << lua_tostring(_state, -1));
        return false;
    }
    
    return true;
}

bool ScriptEngine::runScriptFile(const std::string& filename) {
    if (filename.empty()) {
        LWARNING("Filename was empty");
        return false;
    }
    if (!FileSys.fileExists(filename)) {
        LERROR("Script with name '" << filename << "' did not exist");
        return false;
    }
    std::ifstream file(filename.c_str());
    if (file.bad()) {
        LERROR("Error opening file '" << filename << "'");
        return false;
    }
    
    // Read the contents of the script
    std::string script((std::istreambuf_iterator<char>(file)),
                    std::istreambuf_iterator<char>());
    
    const bool runSuccess = runScript(script);
    return runSuccess;
}

bool ScriptEngine::hasLibrary(const std::string& name)
{
    const unsigned int hash = ghoul::hashCRC32(name);
    return (_registeredLibraries.find(hash) != _registeredLibraries.end());
}

bool ScriptEngine::isLibraryNameAllowed(const std::string& name)
{
    bool result = false;
    lua_getglobal(_state, _openspaceLibraryName.c_str());
    const bool hasOpenSpaceLibrary = lua_istable(_state, -1);
    if (!hasOpenSpaceLibrary) {
        LFATAL("OpenSpace library was not created in initialize method");
        return false;
    }
    lua_getfield(_state, -1, name.c_str());
    const int type = lua_type(_state, -1);
    switch (type) {
        case LUA_TNONE:
        case LUA_TNIL:
            result = true;
            break;
        case LUA_TBOOLEAN:
            LERROR("Library name '" << name << "' specifies a boolean");
            break;
        case LUA_TLIGHTUSERDATA:
            LERROR("Library name '" << name << "' specifies a light user data");
            break;
        case LUA_TNUMBER:
            LERROR("Library name '" << name << "' specifies a number");
            break;
        case LUA_TSTRING:
            LERROR("Library name '" << name << "' specifies a string");
            break;
        case LUA_TTABLE: {
            if (hasLibrary(name))
                LERROR("Library with name '" << name << "' has been registered before");
            else
                LERROR("Library name '" << name << "' specifies a table");
            break;
        }
        case LUA_TFUNCTION:
            LERROR("Library name '" << name << "' specifies a function");
            break;
        case LUA_TUSERDATA:
            LERROR("Library name '" << name << "' specifies a user data");
            break;
        case LUA_TTHREAD:
            LERROR("Library name '" << name << "' specifies a thread");
            break;
    }

    lua_pop(_state, 2);
    return result;
}

void ScriptEngine::addLibraryFunctions(const LuaLibrary& library, bool replace)
{
    for (std::pair<std::string, lua_CFunction> p : library.functions) {
        if (!replace) {
			ghoul::lua::logStack(_state);
            lua_getfield(_state, -1, p.first.c_str());
			ghoul::lua::logStack(_state);
            const bool isNil = lua_isnil(_state, -1);
            if (!isNil) {
                LERROR("Function name '" << p.first << "' was already assigned");
                return;
            }
			lua_pop(_state, 1);
        }
		//ghoul::lua::logStack(_state);
        lua_pushstring(_state, p.first.c_str());
		//ghoul::lua::logStack(_state);
        lua_pushcfunction(_state, p.second);
		//ghoul::lua::logStack(_state);
        lua_settable(_state, _setTableOffset);
		//ghoul::lua::logStack(_state);
    }
}
    
void ScriptEngine::addBaseLibrary() {
    LuaLibrary lib = {
        "",
        {
            { "printDebug", &printDebug },
            { "printInfo", &printInfo },
            { "printWarning", &printWarning },
            { "printError", &printError },
            { "printFatal", &printFatal }
        }
    };
    addLibrary(lib);
}
    
void ScriptEngine::remapPrintFunction() {
	//ghoul::lua::logStack(_state);
 //   lua_getglobal(_state, _luaGlobalNamespace.c_str());
	//ghoul::lua::logStack(_state);
 //   lua_pushstring(_state, _printFunctionName.c_str());
	//ghoul::lua::logStack(_state);
 //   lua_pushcfunction(_state, _printFunctionReplacement);
	//ghoul::lua::logStack(_state);
 //   lua_settable(_state, _setTableOffset);
	//ghoul::lua::logStack(_state);
}
    
} // namespace scripting
} // namespace openspace
