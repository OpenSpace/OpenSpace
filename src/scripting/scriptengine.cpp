/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <openspace/engine/configuration.h>
#include <openspace/engine/globals.h>
#include <openspace/interaction/sessionrecording.h>
#include <openspace/network/parallelpeer.h>
#include <openspace/util/json_helper.h>
#include <openspace/util/syncbuffer.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/lua/lua_helper.h>
#include <ghoul/misc/profiling.h>
#include <filesystem>
#include <fstream>

#include "scriptengine_lua.inl"

namespace {
    constexpr const char* _loggerCat = "ScriptEngine";

    constexpr const int TableOffset = -3; // top-first argument-second argument

    std::vector<std::string> luaFunctions(const openspace::scripting::LuaLibrary& library,
                                          std::string prefix)
    {
        using namespace openspace::scripting;

        std::vector<std::string> result;

        std::string total = prefix;
        if (!library.name.empty()) {
            total += library.name + ".";
        }

        for (const LuaLibrary::Function& function : library.functions) {
            result.push_back(total + function.name);
        }

        for (const LuaLibrary& sl : library.subLibraries) {
            std::vector<std::string> r = luaFunctions(sl, total);
            result.insert(result.end(), r.begin(), r.end());
        }

        for (const LuaLibrary::Documentation& doc : library.documentations) {
            result.push_back(total + doc.name);
        }

        return result;
    }

    void toJson(const openspace::scripting::LuaLibrary& library, std::stringstream& json)
    {
        constexpr const char* replStr = R"("{}": "{}", )";
        constexpr const char* replStr2 = R"("{}": "{}")";

        using namespace openspace;
        using namespace openspace::scripting;

        json << "{";
        json << fmt::format(replStr, "library", library.name);
        json << "\"functions\": [";

        for (const LuaLibrary::Function& f : library.functions) {
            json << "{";
            json << fmt::format(replStr, "name", f.name);
            json << fmt::format(replStr, "arguments", escapedJson(f.argumentText));
            json << fmt::format(replStr2, "help", escapedJson(f.helpText));
            json << "}";
            if (&f != &library.functions.back() || !library.documentations.empty()) {
                json << ",";
            }
        }

        for (const LuaLibrary::Documentation& doc : library.documentations) {
            json << "{";
            json << fmt::format(replStr, "name", doc.name);
            json << fmt::format(replStr, "arguments", escapedJson(doc.parameter));
            json << fmt::format(replStr2, "help", escapedJson(doc.description));
            json << "}";
            if (&doc != &library.documentations.back()) {
                json << ",";
            }
        }

        json << "],";

        json << "\"subLibraries\": [";
        for (const LuaLibrary& sl : library.subLibraries) {
            toJson(sl, json);
            if (&sl != &library.subLibraries.back()) {
                json << ",";
            }
        }
        json << "]}";
    }
} // namespace

namespace openspace::scripting {

ScriptEngine::ScriptEngine()
    : DocumentationGenerator(
        "Script Documentation",
        "scripting",
        {
            { "scriptingTemplate","${WEB}/documentation/scripting.hbs" },
        }
    )
{
    //tracy::LuaRegister(_state);
}

void ScriptEngine::initialize() {
    ZoneScoped

    LDEBUG("Adding base library");
    addBaseLibrary();

    LDEBUG("Initializing Lua state");
    initializeLuaState(_state);
}

void ScriptEngine::deinitialize() {
    ZoneScoped

    _registeredLibraries.clear();
}

void ScriptEngine::initializeLuaState(lua_State* state) {
    ZoneScoped

    LDEBUG("Create openspace base library");
    const int top = lua_gettop(state);

    lua_newtable(state);
    lua_setglobal(state, OpenSpaceLibraryName);

    LDEBUG("Add OpenSpace modules");
    for (LuaLibrary& lib : _registeredLibraries) {
        registerLuaLibrary(state, lib);
    }

    lua_settop(state, top);
}

ghoul::lua::LuaState* ScriptEngine::luaState() {
    return &_state;
}

void ScriptEngine::addLibrary(LuaLibrary library) {
    ZoneScoped

    auto sortFunc = [](const LuaLibrary::Function& lhs, const LuaLibrary::Function& rhs) {
        return lhs.name < rhs.name;
    };

    // do we have a library with the same name as the incoming one
    const auto it = std::find_if(
        _registeredLibraries.begin(),
        _registeredLibraries.end(),
        [&library](const LuaLibrary& lib) { return lib.name == library.name; }
    );

    if (it != _registeredLibraries.end()) {
        // if we found the library, we want to merge them
        LuaLibrary cpy = *it;
        cpy.merge(std::move(library));
        _registeredLibraries.erase(it);
        library = cpy;
    }

    // If not, we can add it after we sorted it
    std::sort(library.functions.begin(), library.functions.end(), sortFunc);
    _registeredLibraries.push_back(std::move(library));
    std::sort(_registeredLibraries.begin(), _registeredLibraries.end());
}

bool ScriptEngine::hasLibrary(const std::string& name) {
    const auto it = std::find_if(
        _registeredLibraries.begin(),
        _registeredLibraries.end(),
        [name](const LuaLibrary& i) { return i.name == name; }
    );
    return (it != _registeredLibraries.end());
}

bool ScriptEngine::runScript(const std::string& script, ScriptCallback callback) {
    ZoneScoped

    if (script.empty()) {
        LWARNING("Script was empty");
        return false;
    }

    if (_logScripts) {
        // Write command to log before it's executed
        writeLog(script);
    }

    try {
        if (callback) {
            ghoul::Dictionary returnValue =
                ghoul::lua::loadArrayDictionaryFromString(script, _state);
            callback(returnValue);
        }
        else {
            ghoul::lua::runScript(_state, script);
        }
    }
    catch (const ghoul::lua::LuaLoadingException& e) {
        LERRORC(e.component, e.message);
        if (callback) {
            callback(ghoul::Dictionary());
        }
        return false;
    }
    catch (const ghoul::lua::LuaExecutionException& e) {
        LERRORC(e.component, e.message);
        if (callback) {
            callback(ghoul::Dictionary());
        }
        return false;
    }
    catch (const ghoul::RuntimeError& e) {
        LERRORC(e.component, e.message);
        if (callback) {
            callback(ghoul::Dictionary());
        }
        return false;
    }

    return true;
}

bool ScriptEngine::runScriptFile(const std::filesystem::path& filename) {
    ZoneScoped

    if (!std::filesystem::is_regular_file(filename)) {
        LERROR(fmt::format("Script with name {} did not exist", filename));
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
    lua_getglobal(state, OpenSpaceLibraryName);
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
            if (hasLibrary(name)) {
                LERROR(fmt::format(
                    "Library with name '{}' has been registered before", name
                ));
            }
            else {
                LERROR(fmt::format("Library name '{}' specifies a table", name));
            }
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
                                       Replace replace)
{
    ZoneScoped

    ghoul_assert(state, "State must not be nullptr");
    for (const LuaLibrary::Function& p : library.functions) {
        if (!replace) {
            lua_getfield(state, -1, p.name.c_str());
            const bool isNil = lua_isnil(state, -1);
            if (!isNil) {
                LERROR(fmt::format("Function name '{}' was already assigned", p.name));
                return;
            }
            lua_pop(state, 1);
        }
        ghoul::lua::push(state, p.name);
        lua_pushcfunction(state, p.function);
        lua_settable(state, TableOffset);
    }

    for (LuaLibrary& sl : library.subLibraries) {
        ghoul::lua::push(state, sl.name);
        lua_newtable(state);
        lua_settable(state, TableOffset);

        // Retrieve the table
        ghoul::lua::push(state, sl.name);
        lua_gettable(state, -2);

        // Add the library functions into the table
        addLibraryFunctions(state, sl, Replace::No);

        // Pop the table
        lua_pop(state, 1);
    }

    for (const std::filesystem::path& script : library.scripts) {
        // First we run the script to set its values in the current state
        ghoul::lua::runScriptFile(state, script.string());

        library.documentations.clear();

        // Then, we extract the documentation information from the file
        ghoul::lua::push(state, "documentation");
        lua_gettable(state, -2);
        if (lua_isnil(state, -1)) {
            LERROR(fmt::format(
                "Module '{}' did not provide a documentation in script file {}",
                library.name, script
            ));
        }
        else {
            lua_pushnil(state);
            while (lua_next(state, -2)) {
                ghoul::lua::push(state, "Name");
                lua_gettable(state, -2);
                const std::string name = lua_tostring(state, -1);
                lua_pop(state, 1);

                ghoul::lua::push(state, "Arguments");
                lua_gettable(state, -2);
                const std::string arguments = lua_tostring(state, -1);
                lua_pop(state, 1);

                ghoul::lua::push(state, "Documentation");
                lua_gettable(state, -2);
                const std::string documentation = lua_tostring(state, -1);
                lua_pop(state, 2);

                library.documentations.push_back({ name, arguments, documentation });
            }
        }
        lua_pop(state, 1);
    }
}

void ScriptEngine::addBaseLibrary() {
    ZoneScoped

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
                "fileExists",
                &luascriptfunctions::fileExists,
                "string",
                "Checks whether the provided file exists."
            },
            {
                "readFile",
                &luascriptfunctions::readFile,
                "string",
                "Reads a file from disk and return its contents"
            },
            {
                "directoryExists",
                &luascriptfunctions::directoryExists,
                "string",
                "Chckes whether the provided directory exists."
            },
            {
                "setPathToken",
                &luascriptfunctions::setPathToken,
                "string, string",
                "Registers a new path token provided by the first argument to the path "
                "provided in the second argument"
            },
            {
                "walkDirectory",
                &luascriptfunctions::walkDirectory,
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
                "string",
                "This function extracts the directory part of the passed path. For "
                "example, if the parameter is 'C:/OpenSpace/foobar/foo.txt', this "
                "function returns 'C:/OpenSpace/foobar'."
            },
            {
                "unzipFile",
                &luascriptfunctions::unzipFile,
                "string, string [bool]",
                "This function extracts the contents of a zip file. The first "
                "argument is the path to the zip file. The second argument is the "
                "directory where to put the extracted files. If the third argument is "
                "true, then the compressed file will be deleted after the decompression "
                 "is finished."
            },
            {
                "saveLastChangeToProfile",
                &luascriptfunctions::saveLastChangeToProfile,
                "",
                "This function saves the last script log action into the profile."
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
    ZoneScoped

    ghoul_assert(state, "State must not be nullptr");
    const int top = lua_gettop(state);

    lua_getglobal(state, OpenSpaceLibraryName);
    if (library.name.empty()) {
        addLibraryFunctions(state, library, Replace::Yes);
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
        ghoul::lua::push(state, library.name);
        lua_newtable(state);
        lua_settable(state, TableOffset);

        // Retrieve the table
        ghoul::lua::push(state, library.name);
        lua_gettable(state, -2);

        // Add the library functions into the table
        addLibraryFunctions(state, library, Replace::No);

        // Pop the table
        lua_pop(state, 1);
    }

    lua_settop(state, top);
    return true;
}

std::vector<std::string> ScriptEngine::allLuaFunctions() const {
    ZoneScoped

    std::vector<std::string> result;
    for (const LuaLibrary& library : _registeredLibraries) {
        std::vector<std::string> r = luaFunctions(library, "openspace.");
        result.insert(result.end(), r.begin(), r.end());
    }
    return result;
}

std::string ScriptEngine::generateJson() const {
    ZoneScoped

    // Create JSON
    std::stringstream json;
    json << "[";

    bool first = true;
    for (const LuaLibrary& l : _registeredLibraries) {
        if (!first) {
            json << ",";
        }
        first = false;

        toJson(l, json);
    }
    json << "]";

    return json.str();
}

bool ScriptEngine::writeLog(const std::string& script) {
    ZoneScoped

    // Check that logging is enabled and initialize if necessary
    if (!_logFileExists) {
        // If a ScriptLogFile was specified, generate it now
        if (!global::configuration->scriptLog.empty()) {
            _logFilename = absPath(global::configuration->scriptLog).string();
            _logFileExists = true;

            LDEBUG(fmt::format(
                "Using script log file {}", std::filesystem::path(_logFilename)
            ));

            // Test file and clear previous input
            std::ofstream file(_logFilename, std::ofstream::out | std::ofstream::trunc);

            if (!file.good()) {
                LERROR(fmt::format(
                    "Could not open file {} for logging scripts",
                    std::filesystem::path(_logFilename)
                ));

                return false;
            }
        }
        else {
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

void ScriptEngine::preSync(bool isMaster) {
    ZoneScoped

    if (!isMaster) {
        return;
    }

    std::lock_guard guard(_slaveScriptsMutex);
    while (!_incomingScripts.empty()) {
        QueueItem item = std::move(_incomingScripts.front());
        _incomingScripts.pop();

        _scriptsToSync.push_back(item.script);
        const bool remoteScripting = item.remoteScripting;

        // Not really a received script but the master also needs to run the script...
        _masterScriptQueue.push(item);

        if (global::parallelPeer->isHost() && remoteScripting) {
            global::parallelPeer->sendScript(item.script);
        }
        if (global::sessionRecording->isRecording()) {
            global::sessionRecording->saveScriptKeyframeToTimeline(item.script);
        }
    }
}

void ScriptEngine::encode(SyncBuffer* syncBuffer) {
    ZoneScoped

    size_t nScripts = _scriptsToSync.size();
    syncBuffer->encode(nScripts);
    for (const std::string& s : _scriptsToSync) {
        syncBuffer->encode(s);
    }
    _scriptsToSync.clear();
}

void ScriptEngine::decode(SyncBuffer* syncBuffer) {
    ZoneScoped

    std::lock_guard guard(_slaveScriptsMutex);
    size_t nScripts;
    syncBuffer->decode(nScripts);

    for (size_t i = 0; i < nScripts; ++i) {
        std::string script;
        syncBuffer->decode(script);
        _slaveScriptQueue.push(std::move(script));
    }
}

void ScriptEngine::postSync(bool isMaster) {
    ZoneScoped

    if (isMaster) {
        while (!_masterScriptQueue.empty()) {
            std::string script = std::move(_masterScriptQueue.front().script);
            ScriptCallback callback = std::move(_masterScriptQueue.front().callback);
            _masterScriptQueue.pop();
            try {
                runScript(script, callback);
            }
            catch (const ghoul::RuntimeError& e) {
                LERRORC(e.component, e.message);
                continue;
            }
        }
    }
    else {
        std::lock_guard guard(_slaveScriptsMutex);
        while (!_slaveScriptQueue.empty()) {
            try {
                runScript(_slaveScriptQueue.front());
                _slaveScriptQueue.pop();
            }
            catch (const ghoul::RuntimeError& e) {
                LERRORC(e.component, e.message);
            }
        }
    }
}

void ScriptEngine::queueScript(const std::string& script,
                               ScriptEngine::RemoteScripting remoteScripting,
                               ScriptCallback callback)
{
    ZoneScoped

    if (!script.empty()) {
        _incomingScripts.push({ script, remoteScripting, callback });
    }
}

} // namespace openspace::scripting
