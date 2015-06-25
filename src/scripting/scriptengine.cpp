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

#include <openspace/scripting/scriptengine.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <openspace/util/syncbuffer.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/network/parallelconnection.h>
#include <ghoul/lua/lua_helper.h>
#include <fstream>
#include <iomanip>

#include "scriptengine_lua.inl"

namespace openspace {

namespace scripting {

namespace {
    const std::string _loggerCat = "ScriptEngine";
    
    const std::string _openspaceLibraryName = "openspace";
    const std::string _luaGlobalNamespace = "_G";
    const std::string _printFunctionName = "print";
    //const lua_CFunction _printFunctionReplacement = luascriptfunctions::printInfo;
    
    const int _setTableOffset = -3; // -1 (top) -1 (first argument) -1 (second argument)

}

bool ScriptEngine::LuaLibrary::operator<(const LuaLibrary& rhs) const {
	return name < rhs.name;
}

ScriptEngine::ScriptEngine()
    : _state(nullptr)
{
}

ScriptEngine::~ScriptEngine() {
	//deinitialize();
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
    if (_state) {
        lua_close(_state);
        _state = nullptr;
    }
}

void ScriptEngine::addLibrary(LuaLibrary library) {
	auto sortFunc = [](const LuaLibrary::Function& lhs, const LuaLibrary::Function& rhs)
	{
		return lhs.name < rhs.name;
	};

	// do we have a library with the same name as the incoming one
	auto it = std::find_if(_registeredLibraries.begin(), _registeredLibraries.end(),
		[&library](const LuaLibrary& lib) { return lib.name == library.name; });

	if (it == _registeredLibraries.end()) {
		// If not, we can add it after we sorted it
		std::sort(library.functions.begin(), library.functions.end(), sortFunc);
		_registeredLibraries.insert(std::move(library));
	}
	else {
		// otherwise, we merge the libraries

		LuaLibrary merged = *it;
		for (const LuaLibrary::Function& fun : library.functions) {
			auto it = std::find_if(merged.functions.begin(), merged.functions.end(),
				[&fun](const LuaLibrary::Function& function) {
					return fun.name == function.name;
			});
			if (it != merged.functions.end()) {
				// the function with the desired name is already present, but we don't
				// want to overwrite it
				LERROR("Lua function '" << fun.name << "' in library '" << library.name <<
					"' has been defined twice");
				return;
			}
			else
				merged.functions.push_back(fun);
		}

		_registeredLibraries.erase(it);

		// Sort the merged library before inserting it
		std::sort(merged.functions.begin(), merged.functions.end(), sortFunc);
		_registeredLibraries.insert(std::move(merged));
	}
}

bool ScriptEngine::runScript(const std::string& script) {
	if (script.empty()){
		LWARNING("Script was empty");
		return false;
	}
    
    int status = luaL_loadstring(_state, script.c_str());
    if (status != LUA_OK) {
        LERROR("Error loading script: '" << lua_tostring(_state, -1) << "'");
        return false;
    }
    
    //LDEBUG("Executing script");
	//LINFO(script);
    if (lua_pcall(_state, 0, LUA_MULTRET, 0)) {
        LERROR("Error executing script: " << lua_tostring(_state, -1));
        return false;
    }
    
    //if we're currently hosting the parallel session, find out if script should be synchronized.
    if(OsEng.parallelConnection()->isHost()){
        
        //"deconstruct the script to find library and function name
        //assuming a script looks like: "openspace.library.function()"
        //or openspace.funcion()
        std::string sub;
        std::string lib;
        std::string func;
        //find first "."
        std::size_t pos = script.find(".");
        if(pos != std::string::npos){
            //strip "openspace."
            sub = script.substr(pos + 1, script.size());
            pos = sub.find(".");
            //one more "." was found, we have a library name
            if(pos != std::string::npos){
                //assing library name
                lib = sub.substr(0, pos);
                //strip "library."
                sub = sub.substr(pos + 1, sub.size());
                
                pos = sub.find("(");
                if(pos != std::string::npos && pos > 0){
                    //strip the () and we're left with function name
                    func = sub.substr(0, pos);
                }
            }
            else{
                //no more "." was found, we have the case of "openspace.funcion()"
                pos = sub.find("(");
                if(pos != std::string::npos && pos > 0){
                    //strip the () and we're left with function name
                    func = sub.substr(0, pos);
                }
            }
        }
        
        LuaLibrary *library = nullptr;
        std::set<LuaLibrary>::const_iterator libit;
        for(libit = _registeredLibraries.cbegin();
            libit != _registeredLibraries.cend();
            ++libit){
            if(libit->name.compare(lib) == 0){
                break;
            }
        }
        
        std::vector<scripting::ScriptEngine::LuaLibrary::Function>::const_iterator funcit;
        //library was found
        if(libit != _registeredLibraries.cend()){
            for( funcit = libit->functions.cbegin();
                funcit != libit->functions.cend();
                ++funcit){
                //function was found!
                if(funcit->name.compare(func) == 0){
                    //is the function of a type that should be shared via parallel connection?
                    if(funcit->parallelShared ){
                        OsEng.parallelConnection()->sendScript(script);
                    }
                }
            }
        }
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
	auto it = std::find_if(
		_registeredLibraries.begin(),
		_registeredLibraries.end(),
		[name](const LuaLibrary& it) { return it.name == name; }
	);

	return (it != _registeredLibraries.end());


	//for (const LuaLibrary& it : _registeredLibraries) {
	//	if (it.name == name)
	//		return true;
	//}
	//return false;
}

bool ScriptEngine::isLibraryNameAllowed(lua_State* state, const std::string& name) {
    bool result = false;
    lua_getglobal(state, _openspaceLibraryName.c_str());
    const bool hasOpenSpaceLibrary = lua_istable(state, -1);
    if (!hasOpenSpaceLibrary) {
        LFATAL("OpenSpace library was not created in initialize method");
        return false;
    }
    lua_getfield(state, -1, name.c_str());
    const int type = lua_type(state, -1);
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

    lua_pop(state, 2);
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
				"*",
				"Logs the passed value to the installed LogManager with a "
				"LogLevel of 'Debug'"
			},
			{
				"printInfo",
				&luascriptfunctions::printInfo,
				"*",
				"Logs the passed value to the installed LogManager with a "
				" LogLevel of 'Info'"
			},
			{
				"printWarning",
				&luascriptfunctions::printWarning,
				"*",
				"Logs the passed value to the installed LogManager with "
				"a LogLevel of 'Warning'"
			},
			{
				"printError",
				&luascriptfunctions::printError,
				"*",
				"Logs the passed value to the installed LogManager with a "
				"LogLevel of 'Error'"
			},
			{
				"printFatal",
				&luascriptfunctions::printFatal,
				"*",
				"Logs the passed value to the installed LogManager with a "
				"LogLevel of 'Fatal'"
			},
			{
				"absPath",
				&luascriptfunctions::absolutePath,
				"string",
				"Returns the absolute path to the passed path, resolving"
				" path tokens as well as resolving relative paths"
			},
			{
				"setPathToken",
				&luascriptfunctions::setPathToken,
				"string, string",
				"Registers a new path token provided by the"
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
    lua_newtable(state);
    lua_setglobal(state, _openspaceLibraryName.c_str());

	LDEBUG("Add OpenSpace modules");
	for (const LuaLibrary& lib : _registeredLibraries)
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
        const bool allowed = isLibraryNameAllowed(state, library.name);
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

        //_registeredLibraries.insert(library);
		//_registeredLibraries.push_back(library);
    }
    return true;
}

std::vector<std::string> ScriptEngine::allLuaFunctions() const {
    std::vector<std::string> result;

    for (const LuaLibrary& library : _registeredLibraries) {
        for (const LuaLibrary::Function& function : library.functions) {
            std::string total = "openspace.";
            if (!library.name.empty())
                total += library.name + ".";
            total += function.name;
            result.push_back(std::move(total));
        }
    }

    return result;
}

bool ScriptEngine::writeDocumentation(const std::string& filename, const std::string& type) const {
	if (type == "text") {
		// The additional space between the longest function name and the descriptions
		LDEBUG("Writing Lua documentation of type '" << type <<
			"' to file '" << filename << "'");
		std::ofstream file(filename);
		if (!file.good()) {
			LERROR("Could not open file '" << filename << "' for writing documentation");
			return false;
		}

		auto concatenate = [](std::string library, std::string function) {
			std::string total = "openspace.";
			if (!library.empty())
				total += std::move(library) + ".";
			total += std::move(function);
			return total;
		};

		// Settings
		const unsigned int lineWidth = 80;
		static const std::string whitespace = " \t";
		static const std::string padding = "    ";
		const bool commandListArguments = true;

		file << "Available commands:\n";
		// Now write out the functions
		for (const LuaLibrary& library : _registeredLibraries) {
			for (const LuaLibrary::Function& function : library.functions) {

				std::string functionName = concatenate(library.name, function.name);
				file << padding << functionName;
				if (commandListArguments)
					file << "(" << function.argumentText << ")";
				file << std::endl;
			}
		}
		file << std::endl;

		// Now write out the functions definitions
		for (const LuaLibrary& library : _registeredLibraries) {
			for (const LuaLibrary::Function& function : library.functions) {

				std::string functionName = concatenate(library.name, function.name);
				file << functionName << "(" << function.argumentText << "):" << std::endl;

				std::string remainingHelptext = function.helpText;

				// @CLEANUP This needs to become a bit prettier ---abock
				while (!remainingHelptext.empty()) {
					const auto length = remainingHelptext.length();
					const auto paddingLength = padding.length();
					if ((length + paddingLength) > lineWidth) {
						auto lastSpace = remainingHelptext.find_last_of(whitespace, lineWidth - 1 - paddingLength);
						if (lastSpace == remainingHelptext.npos)
							lastSpace = lineWidth;
						file << padding << remainingHelptext.substr(0, lastSpace) << std::endl;
						auto firstNotSpace = remainingHelptext.find_first_not_of(whitespace, lastSpace);
						if (firstNotSpace == remainingHelptext.npos)
							firstNotSpace = lastSpace;
						remainingHelptext = remainingHelptext.substr(firstNotSpace);
					}
					else {
						file << padding << remainingHelptext << std::endl;
						remainingHelptext = "";
					}
				}
				file << std::endl;
			}
		}
		return true;
	}
	else {
		LERROR("Undefined type '" << type << "' for Lua documentation");
		return false;
	}
}

void ScriptEngine::serialize(SyncBuffer* syncBuffer){
	syncBuffer->encode(_currentSyncedScript);
	_currentSyncedScript.clear();
}

void ScriptEngine::deserialize(SyncBuffer* syncBuffer){
	syncBuffer->decode(_currentSyncedScript);

	if (!_currentSyncedScript.empty()){
		_mutex.lock();
		_receivedScripts.push_back(_currentSyncedScript);
		_mutex.unlock();
	}
}

void ScriptEngine::postSynchronizationPreDraw(){
	_mutex.lock();
	while(!_receivedScripts.empty()){
		runScript(_receivedScripts.back());
		_receivedScripts.pop_back();
	}
	_mutex.unlock();
}

void ScriptEngine::preSynchronization(){
	
	_mutex.lock();
	
	if (!_queuedScripts.empty()){
		_currentSyncedScript = _queuedScripts.back();
		_queuedScripts.pop_back();
		
		//Not really a received script but the master also needs to run the script...
		_receivedScripts.push_back(_currentSyncedScript);
	}
	
	_mutex.unlock();
}

void ScriptEngine::queueScript(const std::string &script){
	if (script.empty())
		return;
    
	_mutex.lock();

	_queuedScripts.insert(_queuedScripts.begin(), script);

	_mutex.unlock();
}

} // namespace scripting
} // namespace openspace
