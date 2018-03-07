/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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
    constexpr const char* _loggerCat = "ScriptEngine";

    constexpr int TableOffset = -3; // -1 (top) -1 (first argument) -1 (second argument)
} // namespace

namespace openspace::scripting {

std::string ScriptEngine::OpenSpaceLibraryName = "openspace";

ScriptEngine::ScriptEngine()
    : DocumentationGenerator(
        "Script Documentation",
        "scripting",
        {
            {
                "mainTemplate",
                "${WEB}/luascripting/main.hbs"
            },
            {
                "scriptingTemplate",
                "${WEB}/luascripting/scripting.hbs"
            }
        },
        "${WEB}/luascripting/script.js"
    )
{}

void ScriptEngine::initialize() {
    LDEBUG("Adding base library");
    addBaseLibrary();
    LDEBUG("Initializing Lua state");
    initializeLuaState(_state);
}

void ScriptEngine::deinitialize() {
    _registeredLibraries.clear();
}

void ScriptEngine::initializeLuaState(lua_State* state) {
    LDEBUG("Create openspace base library");
    int top = lua_gettop(state);

    lua_newtable(state);
    lua_setglobal(state, OpenSpaceLibraryName.c_str());

    LDEBUG("Add OpenSpace modules");
    for (LuaLibrary& lib : _registeredLibraries) {
        registerLuaLibrary(state, lib);
    }

    lua_settop(state, top);
}

ghoul::lua::LuaState * ScriptEngine::luaState() {
    return &_state;
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
        _registeredLibraries.push_back(std::move(library));
        std::sort(_registeredLibraries.begin(), _registeredLibraries.end());
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
                LERROR(fmt::format(
                    "Lua function '{}' in library '{}' has been defined twice",
                    fun.name,
                    library.name
                ));
                return;
            }
            else {
                merged.functions.push_back(fun);
            }
        }

        for (const std::string& script : library.scripts) {
            merged.scripts.push_back(script);
        }

        _registeredLibraries.erase(it);

        // Sort the merged library before inserting it
        std::sort(merged.functions.begin(), merged.functions.end(), sortFunc);
        _registeredLibraries.push_back(std::move(merged));
        std::sort(_registeredLibraries.begin(), _registeredLibraries.end());
    }
}

bool ScriptEngine::hasLibrary(const std::string& name) {
    auto it = std::find_if(
        _registeredLibraries.begin(),
        _registeredLibraries.end(),
        [name](const LuaLibrary& i) { return i.name == name; }
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
        LERROR(fmt::format("Script with name '{}' did not exist", filename));
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
            LERROR(fmt::format("Library name '{}' specifies a boolean", name));
            break;
        case LUA_TLIGHTUSERDATA:
            LERROR(fmt::format("Library name '{}' specifies a light user data", name));
            break;
        case LUA_TNUMBER:
            LERROR(fmt::format("Library name '{}' specifies a number", name));
            break;
        case LUA_TSTRING:
            LERROR(fmt::format("Library name '{}' specifies a string", name));
            break;
        case LUA_TTABLE: {
            if (hasLibrary(name))
                LERROR(fmt::format(
                    "Library with name '{}' has been registered before", name
                ));
            else
                LERROR(fmt::format("Library name '{}' specifies a table", name));
            break;
        }
        case LUA_TFUNCTION:
            LERROR(fmt::format("Library name '{}' specifies a function", name));
            break;
        case LUA_TUSERDATA:
            LERROR(fmt::format("Library name '{}' specifies a user data", name));
            break;
        case LUA_TTHREAD:
            LERROR(fmt::format("Library name '{}' specifies a thread", name));
            break;
    }

    lua_pop(state, 2);
    return result;
}

void ScriptEngine::addLibraryFunctions(lua_State* state, LuaLibrary& library,
                                       bool replace)
{
    ghoul_assert(state, "State must not be nullptr");
    for (LuaLibrary::Function p : library.functions) {
        if (!replace) {
            lua_getfield(state, -1, p.name.c_str());
            const bool isNil = lua_isnil(state, -1);
            if (!isNil) {
                LERROR(fmt::format("Function name '{}' was already assigned", p.name));
                return;
            }
            lua_pop(state, 1);
        }
        lua_pushstring(state, p.name.c_str());
        for (void* d : p.userdata) {
            lua_pushlightuserdata(state, d);
        }
        lua_pushcclosure(state, p.function, static_cast<int>(p.userdata.size()));
        lua_settable(state, TableOffset);
    }

    for (const std::string& script : library.scripts) {
        // First we run the script to set its values in the current state
        ghoul::lua::runScriptFile(state, script);

        library.documentations.clear();

        // Then, we extract the documentation information from the file
        lua_pushstring(state, "documentation");
        lua_gettable(state, -2);
        if (lua_isnil(state, -1)) {
            LERROR(fmt::format(
                "Module '{}' did not provide a documentation in script file '{}'",
                library.name,
                script
            ));
        }
        else {
            lua_pushnil(state);
            while (lua_next(state, -2)) {
                lua_pushstring(state, "Name");
                lua_gettable(state, -2);
                const std::string name = lua_tostring(state, -1);
                lua_pop(state, 1);

                lua_pushstring(state, "Arguments");
                lua_gettable(state, -2);
                const std::string arguments = lua_tostring(state, -1);
                lua_pop(state, 1);

                lua_pushstring(state, "Documentation");
                lua_gettable(state, -2);
                const std::string documentation = lua_tostring(state, -1);
                lua_pop(state, 1);

                lua_pop(state, 1);

                library.documentations.push_back({ name, arguments, documentation });
            }
        }
        lua_pop(state, 1);
    }
}

void ScriptEngine::addBaseLibrary() {
LuaLibrary lib = {
    "",
    {
        {
            "printTrace",
            &luascriptfunctions::printTrace,
            {},
            "*",
            "Logs the passed value to the installed LogManager with a LogLevel of "
            "'Trace'"
        },
        {
            "printDebug",
            &luascriptfunctions::printDebug,
            {},
            "*",
            "Logs the passed value to the installed LogManager with a LogLevel of "
            "'Debug'"
        },
        {
            "printInfo",
            &luascriptfunctions::printInfo,
            {},
            "*",
            "Logs the passed value to the installed LogManager with a LogLevel of "
            "'Info'"
        },
        {
            "printWarning",
            &luascriptfunctions::printWarning,
            {},
            "*",
            "Logs the passed value to the installed LogManager with a LogLevel of "
            "'Warning'"
        },
        {
            "printError",
            &luascriptfunctions::printError,
            {},
            "*",
            "Logs the passed value to the installed LogManager with a LogLevel of "
            "'Error'"
        },
        {
            "printFatal",
            &luascriptfunctions::printFatal,
            {},
            "*",
            "Logs the passed value to the installed LogManager with a LogLevel of "
            "'Fatal'"
        },
        {
            "absPath",
            &luascriptfunctions::absolutePath,
            {},
            "string",
            "Returns the absolute path to the passed path, resolving path tokens as "
            "well as resolving relative paths"
        },
        {
            "fileExists",
            &luascriptfunctions::fileExists,
            {},
            "string",
            "Checks whether the provided file exists."
        },
        {
            "directoryExists",
            &luascriptfunctions::directoryExists,
            {},
            "string",
            "Chckes whether the provided directory exists."
        },
        {
            "setPathToken",
            &luascriptfunctions::setPathToken,
            {},
            "string, string",
            "Registers a new path token provided by the first argument to the path "
            "provided in the second argument"
        },
        {
            "walkDirectory",
            &luascriptfunctions::walkDirectory,
            {},
            "string [bool, bool]",
            "Walks a directory and returns all contents (files and directories) of "
            "the directory as absolute paths. The first argument is the path of the "
            "directory that should be walked, the second argument determines if the "
            "walk is recursive and will continue in contained directories. The third "
            "argument determines whether the table that is returned is sorted."
        },
        {
            "walkDirectoryFiles",
            &luascriptfunctions::walkDirectoryFiles,
            {},
            "string [bool, bool]",
            "Walks a directory and returns the files of the directory as absolute "
            "paths. The first argument is the path of the directory that should be "
            "walked, the second argument determines if the walk is recursive and "
            "will continue in contained directories. The third argument determines "
            "whether the table that is returned is sorted."
        },
        {
            "walkDirectoryFolder",
            &luascriptfunctions::walkDirectoryFolder,
            {},
            "string [bool, bool]",
            "Walks a directory and returns the subfolders of the directory as "
            "absolute paths. The first argument is the path of the directory that "
            "should be walked, the second argument determines if the walk is "
            "recursive and will continue in contained directories. The third "
            "argument determines whether the table that is returned is sorted."
        },
        {
            "directoryForPath",
            &luascriptfunctions::directoryForPath,
            {},
            "string",
            "This function extracts the directory part of the passed path. For example, "
            "if the parameter is 'C:/OpenSpace/foobar/foo.txt', this function returns "
            "'C:/OpenSpace/foobar'."
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

bool ScriptEngine::registerLuaLibrary(lua_State* state, LuaLibrary& library) {
    ghoul_assert(state, "State must not be nullptr");
    int top = lua_gettop(state);

    lua_getglobal(state, OpenSpaceLibraryName.c_str());
    if (library.name.empty()) {
        addLibraryFunctions(state, library, true);
        lua_pop(state, 1);
    }
    else {
        const bool allowed = isLibraryNameAllowed(state, library.name);
        if (!allowed) {
            lua_settop(state, top);
            return false;
        }

        // We need to first create the table and then retrieve it as the table will
        // probably be used by scripts already

        // Add the table
        lua_pushstring(state, library.name.c_str());
        lua_newtable(state);
        lua_settable(state, TableOffset);

        // Retrieve the table
        lua_pushstring(state, library.name.c_str());
        lua_gettable(state, -2);

        // Add the library functions into the table
        addLibraryFunctions(state, library, false);

        // Pop the table
        lua_pop(state, 1);
    }

    lua_settop(state, top);
    return true;
}

std::vector<std::string> ScriptEngine::allLuaFunctions() const {
    std::vector<std::string> result;

    for (const LuaLibrary& library : _registeredLibraries) {
        for (const LuaLibrary::Function& function : library.functions) {
            std::string total = "openspace.";
            if (!library.name.empty()) {
                total += library.name + ".";
            }
            total += function.name;
            result.push_back(std::move(total));
        }

        for (const LuaLibrary::Documentation& doc : library.documentations) {
            std::string total = "openspace.";
            if (!library.name.empty()) {
                total += library.name + ".";
            }
            total += doc.name;
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
            json << "\"arguments\": \"" << escapedJson(f.argumentText) << "\", ";
            json << "\"help\": \"" << escapedJson(f.helpText) << "\"";
            json << "}";
            if (&f != &l.functions.back() || !l.documentations.empty()) {
                json << ",";
            }
        }

        for (const LuaLibrary::Documentation& doc : l.documentations) {
            json << "{";
            json << "\"name\": \"" << doc.name << "\", ";
            json << "\"arguments\": \"" << escapedJson(doc.parameter) << "\", ";
            json << "\"help\": \"" << escapedJson(doc.description) << "\"";
            json << "}";
            if (&doc != &l.documentations.back()) {
                json << ",";
            }
        }


        json << "]}";

    }
    json << "]";

    return json.str();
}

bool ScriptEngine::writeLog(const std::string& script) {
    // Check that logging is enabled and initialize if necessary
    if (!_logFileExists) {
        // If a ScriptLogFile was specified, generate it now
        const bool hasFile = OsEng.configurationManager().hasKey(
            ConfigurationManager::KeyScriptLog
        );
        if (hasFile) {
            OsEng.configurationManager().getValue(
                ConfigurationManager::KeyScriptLog,
                _logFilename
            );

            _logFilename = absPath(_logFilename);
            _logFileExists = true;

            LDEBUG(fmt::format(
                "Using script log of type '{}' to file '{}'", _logType, _logFilename
            ));

            // Test file and clear previous input
            std::ofstream file(_logFilename, std::ofstream::out | std::ofstream::trunc);

            if (!file.good()) {
                LERROR(fmt::format(
                    "Could not open file '{}' for logging scripts", _logFilename
                ));

                return false;
            }
        } else {
            _logScripts = false;
            return false;
        }
    }

    // Simple text output to logfile
    std::ofstream file(_logFilename, std::ofstream::app);
    if (!file.good()) {
        LERROR(fmt::format("Could not open file '{}' for logging scripts", _logFilename));
        return false;
    }

    file << script << std::endl;
    file.close();

    return true;
}

void ScriptEngine::presync(bool isMaster) {
    if (!isMaster) {
        return;
    }

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

void ScriptEngine::queueScript(const std::string &script,
                               ScriptEngine::RemoteScripting remoteScripting)
{
    if (script.empty()) {
        return;
    }

    _mutex.lock();
    _queuedScripts.insert(
        _queuedScripts.begin(),
        std::make_pair(script, remoteScripting)
    );
    _mutex.unlock();
}

} // namespace openspace::scripting
