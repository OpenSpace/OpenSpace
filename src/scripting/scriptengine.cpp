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

#include <openspace/scripting/scriptengine.h>
#include <openspace/openspace.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/lua/lua_helper.h>
#include <ghoul/misc/exception.h>

#include <openspace/engine/configurationmanager.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/network/parallelconnection.h>
#include <openspace/util/syncbuffer.h>

#include <fstream>
#include <iomanip>

#include "scriptengine_lua.inl"

namespace {
    const std::string _loggerCat = "ScriptEngine";

    const char* LuaGlobalNamespace = "_G";
    const char* PrintFunctionName = "print";
    //const lua_CFunction _printFunctionReplacement = luascriptfunctions::printInfo;

    const int TableOffset = -3; // -1 (top) -1 (first argument) -1 (second argument)

    const char* MainTemplateFilename = "${OPENSPACE_DATA}/web/luascripting/main.hbs";
    const char* ScriptingTemplateFilename = "${OPENSPACE_DATA}/web/luascripting/scripting.hbs";
    const char* JsFilename = "${OPENSPACE_DATA}/web/luascripting/script.js";
} // namespace

namespace openspace {
namespace scripting {

std::string ScriptEngine::OpenSpaceLibraryName = "openspace";

ScriptEngine::ScriptEngine()
    : DocumentationGenerator(
        "Script Documentation",
        "scripting",
        {
            { "mainTemplate", MainTemplateFilename },
            { "scriptingTemplate", ScriptingTemplateFilename }
        },
        JsFilename
    )
{}

void ScriptEngine::initialize() {
    LDEBUG("Adding base library");
    addBaseLibrary();
    LDEBUG("Initializing Lua state");
    initializeLuaState(_state);
    LDEBUG("Remapping Print functions");
    remapPrintFunction();
}

void ScriptEngine::deinitialize() {}

void ScriptEngine::initializeLuaState(lua_State* state) {
    LDEBUG("Create openspace base library");
    lua_newtable(state);
    lua_setglobal(state, OpenSpaceLibraryName.c_str());
    
    LDEBUG("Add OpenSpace modules");
    for (const LuaLibrary& lib : _registeredLibraries) {
        registerLuaLibrary(state, lib);
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
            auto itf = std::find_if(
                merged.functions.begin(),
                merged.functions.end(),
                [&fun](const LuaLibrary::Function& function) {
                    return fun.name == function.name;
                }
            );
            if (itf != merged.functions.end()) {
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

bool ScriptEngine::hasLibrary(const std::string& name) {
    auto it = std::find_if(
        _registeredLibraries.begin(),
        _registeredLibraries.end(),
        [name](const LuaLibrary& it) { return it.name == name; }
    );
    return (it != _registeredLibraries.end());
}

bool ScriptEngine::runScript(const std::string& script) {
    if (script.empty()){
        LWARNING("Script was empty");
        return false;
    }

    if (_logScripts) {
        // Write command to log before it's executed
        writeLog(script);
    }

    try {
        ghoul::lua::runScript(_state, script);
    }
    catch (const ghoul::lua::LuaLoadingException& e) {
        LERRORC(e.component, e.message);
        return false;
    }
    catch (const ghoul::lua::LuaExecutionException& e) {
        LERRORC(e.component, e.message);
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
    
    try {
        ghoul::lua::runScriptFile(_state, filename);
    }
    catch (const ghoul::lua::LuaLoadingException& e) {
        LERRORC(e.component, e.message);
        return false;
    }
    catch (const ghoul::lua::LuaExecutionException& e) {
        LERRORC(e.component, e.message);
        return false;
    }
    
    return true;
}

/*bool ScriptEngine::shouldScriptBeSent(const std::string& library, const std::string& function) {
    std::set<LuaLibrary>::const_iterator libit;
    for (libit = _registeredLibraries.cbegin();
         libit != _registeredLibraries.cend();
         ++libit){
        if (libit->name.compare(library) == 0){
            break;
        }
    }
    
    std::vector<scripting::LuaLibrary::Function>::const_iterator funcit;
    //library was found
    if (libit != _registeredLibraries.cend()){
        for (funcit = libit->functions.cbegin();
             funcit != libit->functions.cend();
             ++funcit){
            //function was found!
            if (funcit->name.compare(function) == 0){
                //is the function of a type that should be shared via parallel connection?
                return funcit->parallelShared;
            }
        }
    }
    
    return false;
}*/
    
/*void ScriptEngine::cacheScript(const std::string &library, const std::string &function, const std::string &script){
    _cachedScriptsMutex.lock();
    _cachedScripts[library][function] = script;
    _cachedScriptsMutex.unlock();
}
    
std::vector<std::string> ScriptEngine::cachedScripts(){
    _cachedScriptsMutex.lock();
    
    std::vector<std::string> retVal;
    std::map<std::string, std::map<std::string, std::string>>::const_iterator outerIt;
    std::map<std::string, std::string>::const_iterator innerIt;
    for(outerIt = _cachedScripts.cbegin();
        outerIt != _cachedScripts.cend();
        ++outerIt){
        for(innerIt = outerIt->second.cbegin();
            innerIt != outerIt->second.cend();
            ++innerIt){
            retVal.push_back(innerIt->second);
        }
    }
    
    _cachedScriptsMutex.unlock();

    return retVal;
}*/
    
/*
bool ScriptEngine::parseLibraryAndFunctionNames(std::string &library, std::string &function, const std::string &script){
    
    //"deconstruct the script to find library and function name
    //assuming a script looks like: "openspace.library.function()"
    //or openspace.funcion()
    std::string sub;
    library.clear();
    function.clear();
    //find first "."
    std::size_t pos = script.find(".");
    
    if (pos != std::string::npos){
        //strip "openspace."
        sub = script.substr(pos + 1, script.size());
        pos = sub.find(".");
        //one more "." was found, if the "." comes before first "(" we have a library name
        if (pos != std::string::npos && pos < sub.find("(")){
            //assing library name
            library = sub.substr(0, pos);
            //strip "library."
            sub = sub.substr(pos + 1, sub.size());

            pos = sub.find("(");
            if (pos != std::string::npos && pos > 0){
                //strip the () and we're left with function name
                function = sub.substr(0, pos);
            }
        }
        else{
            //no more "." was found, we have the case of "openspace.funcion()"
            pos = sub.find("(");
            if (pos != std::string::npos && pos > 0){
                //strip the () and we're left with function name
                function = sub.substr(0, pos);
            }
        }
    }

    //if we found a function all is good
    return !function.empty();
}
*/
bool ScriptEngine::isLibraryNameAllowed(lua_State* state, const std::string& name) {
    bool result = false;
    lua_getglobal(state, OpenSpaceLibraryName.c_str());
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
        lua_settable(state, TableOffset);
        //ghoul::lua::logStack(_state);
    }
}
    
void ScriptEngine::addBaseLibrary() {
    LuaLibrary lib = {
        "",
        {
            {
                "printTrace",
                &luascriptfunctions::printTrace,
                "*",
                "Logs the passed value to the installed LogManager with a LogLevel of "
                "'Trace'"
            },
            {
                "printDebug",
                &luascriptfunctions::printDebug,
                "*",
                "Logs the passed value to the installed LogManager with a LogLevel of "
                "'Debug'"
            },
            {
                "printInfo",
                &luascriptfunctions::printInfo,
                "*",
                "Logs the passed value to the installed LogManager with a LogLevel of "
                "'Info'"
            },
            {
                "printWarning",
                &luascriptfunctions::printWarning,
                "*",
                "Logs the passed value to the installed LogManager with a LogLevel of "
                "'Warning'"
            },
            {
                "printError",
                &luascriptfunctions::printError,
                "*",
                "Logs the passed value to the installed LogManager with a LogLevel of "
                "'Error'"
            },
            {
                "printFatal",
                &luascriptfunctions::printFatal,
                "*",
                "Logs the passed value to the installed LogManager with a LogLevel of "
                "'Fatal'"
            },
            {
                "absPath",
                &luascriptfunctions::absolutePath,
                "string",
                "Returns the absolute path to the passed path, resolving path tokens as "
                "well as resolving relative paths"
            },
            {
                "setPathToken",
                &luascriptfunctions::setPathToken,
                "string, string",
                "Registers a new path token provided by the first argument to the path "
                "provided in the second argument"
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

bool ScriptEngine::registerLuaLibrary(lua_State* state, const LuaLibrary& library) {
    ghoul_assert(state, "State must not be nullptr");

    if (library.functions.empty()) {
        LERROR("Lua library '" << library.name << "' does not have any functions");
        return false;
    }

    //ghoul::lua::logStack(_state);
    lua_getglobal(state, OpenSpaceLibraryName.c_str());
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
        if (!allowed) {
            return false;
        }
        
        //ghoul::lua::logStack(_state);
        
        lua_pushstring(state, library.name.c_str());
        //ghoul::lua::logStack(_state);
        lua_newtable(state);
        //ghoul::lua::logStack(_state);
        addLibraryFunctions(state, library, false);
        lua_settable(state, TableOffset);
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

std::string ScriptEngine::generateJson() const {
    // Create JSON
    std::stringstream json;
    json << "[";

    bool first = true;
    for (const LuaLibrary& l : _registeredLibraries) {
        if (!first) {
            json << ",";
        }
        first = false;

        json << "{";
        json << "\"library\": \"" << l.name << "\",";
        json << "\"functions\": [";

        for (const LuaLibrary::Function& f : l.functions) {
            json << "{";
            json << "\"name\": \"" << f.name << "\", ";
            json << "\"arguments\": \"" << f.argumentText << "\", ";
            json << "\"help\": \"" << f.helpText << "\"";
            json << "}";
            if (&f != &l.functions.back()) {
                json << ",";
            }
        }
        json << "]}";

    }
    json << "]";

    std::string jsonString = "";
    for (const char& c : json.str()) {
        if (c == '\'') {
            jsonString += "\\'";
        }
        else {
            jsonString += c;
        }
    }

    return jsonString;
}

bool ScriptEngine::writeLog(const std::string& script) {
    const std::string KeyScriptLogType =
        ConfigurationManager::KeyScriptLog + '.' + ConfigurationManager::PartType;
    const std::string KeyScriptLogFile =
        ConfigurationManager::KeyScriptLog + '.' + ConfigurationManager::PartFile;

    // Check that logging is enabled and initialize if necessary
    if (!_logFileExists) {
        // If a ScriptLogFile was specified, generate it now
        const bool hasType = OsEng.configurationManager()
            .hasKey(KeyScriptLogType);
        const bool hasFile = OsEng.configurationManager()
            .hasKey(KeyScriptLogFile);
        if (hasType && hasFile) {
            OsEng.configurationManager()
                .getValue(KeyScriptLogType, _logType);
            OsEng.configurationManager()
                .getValue(KeyScriptLogFile, _logFilename);

            _logFilename = absPath(_logFilename);
            _logFileExists = true;

            LDEBUG("Using script log of type '" << _logType <<
                   "' to file '" << _logFilename << "'");

            // Test file and clear previous input
            std::ofstream file(_logFilename, std::ofstream::out | std::ofstream::trunc);

            if (!file.good()) {
                LERROR("Could not open file '" << _logFilename
                       << "' for logging scripts");

                return false;
            }
        } else {
            LDEBUG("No script log specified in 'openspace.cfg.' To log, set '"
                   << KeyScriptLogType << " and "
                   << KeyScriptLogFile
                   << " in configuration table.");
            _logScripts = false;
            return false;
        }
    }

    if (_logType == "text") {
        // Simple text output to logfile
        std::ofstream file(_logFilename, std::ofstream::app);
        if (!file.good()) {
            LERROR("Could not open file '" << _logFilename << "' for logging scripts");
            return false;
        }

        file << script << std::endl;
        file.close();
    }
    else {
        LERROR("Undefined type '" << _logType << "' for script documentation");
        _logScripts = false;
        return false;
    }

    return true;
}

void ScriptEngine::presync(bool isMaster) {
    if (!isMaster) return;

    _mutex.lock();

    if (!_queuedScripts.empty()) {
        _currentSyncedScript = _queuedScripts.back().first;
        bool remoteScripting = _queuedScripts.back().second;


        //Not really a received script but the master also needs to run the script...
        _receivedScripts.push_back(_currentSyncedScript);
        _queuedScripts.pop_back();

        if (OsEng.parallelConnection().isHost() && remoteScripting) {
            OsEng.parallelConnection().sendScript(_currentSyncedScript);
        }

    }

    _mutex.unlock();

}

void ScriptEngine::encode(SyncBuffer* syncBuffer) {
    syncBuffer->encode(_currentSyncedScript);
    _currentSyncedScript.clear();
}

void ScriptEngine::decode(SyncBuffer* syncBuffer) {
    syncBuffer->decode(_currentSyncedScript);

    if (!_currentSyncedScript.empty()) {
        _mutex.lock();
        _receivedScripts.push_back(_currentSyncedScript);
        _mutex.unlock();
    }
}

void ScriptEngine::postsync(bool) {
    std::vector<std::string> scripts;

    _mutex.lock();
    scripts.assign(_receivedScripts.begin(), _receivedScripts.end());
    _receivedScripts.clear();
    _mutex.unlock();

    while (!scripts.empty()) {
        try {
            runScript(scripts.back());
        }
        catch (const ghoul::RuntimeError& e) {
            LERRORC(e.component, e.message);
        }
        scripts.pop_back();
    }
}

void ScriptEngine::queueScript(const std::string &script, ScriptEngine::RemoteScripting remoteScripting){
    if (script.empty())
        return;
    
    _mutex.lock();

    _queuedScripts.insert(_queuedScripts.begin(), std::make_pair(script, remoteScripting));

    _mutex.unlock();
}

} // namespace scripting
} // namespace openspace
