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

#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h>

#include <ghoul/lua/lua_helper.h>
#include <fstream>

namespace openspace {

namespace luascriptfunctions {

	int printInternal(ghoul::logging::LogManager::LogLevel level, lua_State* L) {
		using ghoul::lua::luaTypeToString;
		const std::string _loggerCat = "print";

		int nArguments = lua_gettop(L);
		if (nArguments != 1)
			return luaL_error(L, "Expected %i arguments, got %i", 1, nArguments);

		const int type = lua_type(L, -1);
		switch (type) {
			case LUA_TNONE:
			case LUA_TLIGHTUSERDATA:
			case LUA_TTABLE:
			case LUA_TFUNCTION:
			case LUA_TUSERDATA:
			case LUA_TTHREAD:
				LOG(level, "Function parameter was of type '" <<
					 luaTypeToString(type) << "'");
			case LUA_TNIL:
				break;
			case LUA_TBOOLEAN:
				LOG(level, lua_toboolean(L, -1));
				break;
			case LUA_TNUMBER:
				LOG(level, lua_tonumber(L, -1));
				break;
			case LUA_TSTRING:
				LOG(level, lua_tostring(L, -1));
				break;
		}
		return 0;
	}

	/**
	 * \ingroup LuaScripts
	 * printDebug(*):
	 * Logs the passed value to the installed LogManager with a LogLevel of 'Debug'.
	 * For Boolean, numbers, and strings, the internal values are printed, for all other
	 * types, the type is printed instead
	 */
	int printDebug(lua_State* L) {
		return printInternal(ghoul::logging::LogManager::LogLevel::Debug, L);
	}

	/**
	 * \ingroup LuaScripts
	 * printInfo(*):
	 * Logs the passed value to the installed LogManager with a LogLevel of 'Info'.
	 * For Boolean, numbers, and strings, the internal values are printed, for all other
	 * types, the type is printed instead
	 */
	int printInfo(lua_State* L) {
		return printInternal(ghoul::logging::LogManager::LogLevel::Info, L);
	}

	/**
	 * \ingroup LuaScripts
	 * printWarning(*):
	 * Logs the passed value to the installed LogManager with a LogLevel of 'Warning'.
	 * For Boolean, numbers, and strings, the internal values are printed, for all other
	 * types, the type is printed instead
	 */
	int printWarning(lua_State* L) {
		return printInternal(ghoul::logging::LogManager::LogLevel::Warning, L);
	}

	/**
	 * \ingroup LuaScripts
	 * printError(*):
	 * Logs the passed value to the installed LogManager with a LogLevel of 'Error'.
	 * For Boolean, numbers, and strings, the internal values are printed, for all other
	 * types, the type is printed instead
	 */
	int printError(lua_State* L) {
		return printInternal(ghoul::logging::LogManager::LogLevel::Error, L);
	}

	/**
	 * \ingroup LuaScripts
	 * printFatal(*):
	 * Logs the passed value to the installed LogManager with a LogLevel of 'Fatal'.
	 * For Boolean, numbers, and strings, the internal values are printed, for all other
	 * types, the type is printed instead
	 */
	int printFatal(lua_State* L) {
		return printInternal(ghoul::logging::LogManager::LogLevel::Fatal, L);
	}

	/**
	 * \ingroup LuaScripts
	 * absPath(string):
	 * Passes the argument to FileSystem::absolutePath, which resolves occuring path
	 * tokens and returns the absolute path.
	 */
	int absolutePath(lua_State* L) {
		int nArguments = lua_gettop(L);
		if (nArguments != 1)
			return luaL_error(L, "Expected %d arguments, got %d", 1, nArguments);

		std::string path = luaL_checkstring(L, -1);
		path = absPath(path);
		lua_pushstring(L, path.c_str());
		return 1;
	}

	/**
	 * \ingroup LuaScripts
	 * setPathToken(string, string):
	 * Registers the path token provided by the first argument to the path in the second
	 * argument. If the path token already exists, it will be silently overridden.
	 */
	int setPathToken(lua_State* L) {
		int nArguments = lua_gettop(L);
		if (nArguments != 2)
			return luaL_error(L, "Expected %i arguments, got %i", 2, nArguments);

		std::string pathToken = luaL_checkstring(L, -1);
		std::string path = luaL_checkstring(L, -2);
		FileSys.registerPathToken(pathToken, path, true);
		return 0;
	}

} // namespace luascriptfunctions

namespace scripting {

namespace {
    const std::string _loggerCat = "ScriptEngine";
    
    const std::string _openspaceLibraryName = "openspace";
    const std::string _luaGlobalNamespace = "_G";
    const std::string _printFunctionName = "print";
    const lua_CFunction _printFunctionReplacement = luascriptfunctions::printInfo;
    
    const int _setTableOffset = -3; // -1 (top) -1 (first argument) -1 (second argument)

}

bool ScriptEngine::LuaLibrary::operator<(const LuaLibrary& rhs) const {
	return name < rhs.name;
}

ScriptEngine::ScriptEngine()
    : _state(nullptr)
{
}

bool ScriptEngine::initialize() {
    LDEBUG("Adding base functions");
    addBaseLibrary();

    _state = luaL_newstate();
    LDEBUG("Creating Lua state");
    if (_state == nullptr) {
        LFATAL("Error creating new Lua state: Memory allocation error");
        return false;
    }

    LDEBUG("Open Lua libraries");
    luaL_openlibs(_state);

	initializeLuaState(_state);
    
    LDEBUG("Remap print function");
    remapPrintFunction();
    
    return true;
}

void ScriptEngine::deinitialize() {
    lua_close(_state);
    _state = nullptr;
}

void ScriptEngine::addLibrary(const ScriptEngine::LuaLibrary& library) {
	_registeredLibraries.insert(library);
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

	int status = luaL_loadfile(_state, filename.c_str());
	if (status != LUA_OK) {
        LERROR("Error loading script: '" << lua_tostring(_state, -1) << "'");
        return false;
    }

	LDEBUG("Executing script '" << filename << "'");
    if (lua_pcall(_state, 0, LUA_MULTRET, 0)) {
        LERROR("Error executing script: " << lua_tostring(_state, -1));
        return false;
    }
    
    return true;
}

bool ScriptEngine::hasLibrary(const std::string& name)
{
	for (auto it = _registeredLibraries.begin(); it != _registeredLibraries.end(); ++it)
		if (it->name == name)
			return true;
	return false;
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

void ScriptEngine::addLibraryFunctions(lua_State* state, const LuaLibrary& library, bool replace) {
	assert(state);
    for (LuaLibrary::Function p : library.functions) {
        if (!replace) {
			//ghoul::lua::logStack(_state);
            lua_getfield(state, -1, p.name.c_str());
			//ghoul::lua::logStack(_state);
            const bool isNil = lua_isnil(state, -1);
            if (!isNil) {
                LERROR("Function name '" << p.name << "' was already assigned");
                return;
            }
			lua_pop(state, 1);
        }
		//ghoul::lua::logStack(_state);
        lua_pushstring(state, p.name.c_str());
		//ghoul::lua::logStack(_state);
        lua_pushcfunction(state, p.function);
		//ghoul::lua::logStack(_state);
        lua_settable(state, _setTableOffset);
		//ghoul::lua::logStack(_state);
    }
}
    
void ScriptEngine::addBaseLibrary() {
    LuaLibrary lib = {
        "",
        {
            {
				"printDebug",
				&luascriptfunctions::printDebug,
				"printDebug(*): Logs the passed value to the installed LogManager with a "
				"LogLevel of 'Debug'"
			},
			{
				"printInfo",
				&luascriptfunctions::printInfo,
				"printInfo(*): Logs the passed value to the installed LogManager with a "
				" LogLevel of 'Info'"
			},
			{
				"printWarning",
				&luascriptfunctions::printWarning,
				"printWarning(*): Logs the passed value to the installed LogManager with "
				"a LogLevel of 'Warning'"
			},
			{
				"printError",
				&luascriptfunctions::printError,
				"printError(*): Logs the passed value to the installed LogManager with a "
				"LogLevel of 'Error'"
			},
			{
				"printFatal",
				&luascriptfunctions::printFatal,
				"printFatal(*): Logs the passed value to the installed LogManager with a "
				"LogLevel of 'Fatal'"
			},
			{
				"absPath",
				&luascriptfunctions::absolutePath,
				"absPath(string): Returns the absolute path to the passed path, resolving"
				" path tokens as well as resolving relative paths"
			},
			{
				"setPathToken",
				&luascriptfunctions::setPathToken,
				"setPathToken(string, string): Registers a new path token provided by the"
				" first argument to the path provided in the second argument"
			}
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

void ScriptEngine::initializeLuaState(lua_State* state) {
    LDEBUG("Create openspace base library");
    lua_newtable(_state);
    lua_setglobal(_state, _openspaceLibraryName.c_str());

	LDEBUG("Add OpenSpace modules");
	for (auto lib : _registeredLibraries)
		registerLuaLibrary(state, lib);
}

bool ScriptEngine::registerLuaLibrary(lua_State* state, const LuaLibrary& library) {
	assert(state);
    if (library.functions.empty()) {
        LERROR("Lua library '" << library.name << "' does not have any functions");
        return false;
    }

	//ghoul::lua::logStack(_state);
    lua_getglobal(state, _openspaceLibraryName.c_str());
	//ghoul::lua::logStack(_state);
    if (library.name.empty()) {
		//ghoul::lua::logStack(_state);
        addLibraryFunctions(state, library, true);
		//ghoul::lua::logStack(_state);
		lua_pop(state, 1);
		//ghoul::lua::logStack(_state);
    }
    else {
        const bool allowed = isLibraryNameAllowed(library.name);
        if (!allowed)
            return false;
        
		//ghoul::lua::logStack(_state);
		
        lua_pushstring(state, library.name.c_str());
		//ghoul::lua::logStack(_state);
        lua_newtable(state);
		//ghoul::lua::logStack(_state);
        addLibraryFunctions(state, library, false);
        lua_settable(state, _setTableOffset);
        //ghoul::lua::logStack(_state);

        _registeredLibraries.insert(library);
    }
    return true;
}


} // namespace scripting
} // namespace openspace
