/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <openspace/documentation/documentation.h>
#include <openspace/engine/configuration.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/interaction/sessionrecording.h>
#include <openspace/network/parallelpeer.h>
#include <openspace/util/syncbuffer.h>
#include <openspace/documentation/documentation.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/lua/lua_helper.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/ext/assimp/contrib/zip/src/zip.h>
#include <filesystem>
#include <fstream>
#include "scriptengine_lua.inl"

namespace {
    constexpr std::string_view _loggerCat = "ScriptEngine";

    constexpr int TableOffset = -3; // top-first argument-second argument

    struct [[codegen::Dictionary(Documentation)]] Parameters {
        std::string name;
        std::vector<std::vector<std::string>> arguments;
        std::optional<std::string> returnValue [[codegen::key("Return")]];
        std::optional<std::string> documentation;
    };

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

        for (const LuaLibrary::Function& function : library.documentations) {
            result.push_back(total + function.name);
        }

        return result;
    }
#include "scriptengine_codegen.cpp"
} // namespace

namespace openspace::scripting {

ScriptEngine::ScriptEngine(bool sandboxedLua)
    : _state(ghoul::lua::LuaState::Sandboxed(sandboxedLua))
{}

void ScriptEngine::initialize() {
    ZoneScoped;

    LDEBUG("Adding base library");
    addBaseLibrary();

    LDEBUG("Initializing Lua state");
    initializeLuaState(_state);
}

void ScriptEngine::deinitialize() {
    ZoneScoped;

    _registeredLibraries.clear();
    for (const RepeatedScriptInfo& info : _repeatedScripts) {
        if (info.postScript.empty()) {
            queueScript(info.postScript);
        }
    }
    _repeatedScripts.clear();
}

void ScriptEngine::initializeLuaState(lua_State* state) {
    ZoneScoped;

    LDEBUG("Create openspace base library");
    const int top = lua_gettop(state);

    lua_newtable(state);
    lua_setglobal(state, OpenSpaceLibraryName.data());

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
    ZoneScoped;

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

bool ScriptEngine::runScript(const Script& script) {
    ZoneScoped;
    ZoneText(script.code.c_str(), script.code.size());

    ghoul_assert(!script.code.empty(), "Script must not be empty");
    ghoul_assert(
        !(script.addToLog && ghoul::lua::isScriptBinary(script.code)),
        "Shouldn't try to add a script to a log that is a binary blob"
    );

    // Binary scripts should never be logged
    if (_logScripts && !ghoul::lua::isScriptBinary(script.code)) {
        if (script.addToLog && !global::configuration->verboseScriptLog) {
            writeLog(script.code);
        }
        else if (global::configuration->verboseScriptLog) {
            // Even if the script doesn't want to get logged, it will get anyway if
            // verbose logging is enabled
            writeLog(std::format(
                "{}{}{}{}",
                script.addToLog ? "" : "--[[generated]]",
                script.synchronized ? "--[[sync]]" : "--[[!sync]]",
                script.sendToRemote ? "--[[sendRemote]]" : "--[[!sendRemote]]",
                script.code
            ));
        }
    }

    try {
        if (script.callback) {
            ghoul::Dictionary returnValue =
                ghoul::lua::loadArrayDictionaryFromString(script.code, _state);
            script.callback(std::move(returnValue));
        }
        else {
            ghoul::lua::runScript(_state, script.code);
        }
    }
    catch (const ghoul::lua::LuaLoadingException& e) {
        LERRORC(e.component, e.message);
        if (script.callback) {
            script.callback(ghoul::Dictionary());
        }
        return false;
    }
    catch (const ghoul::lua::LuaExecutionException& e) {
        LERRORC(e.component, e.message);
        if (script.callback) {
            script.callback(ghoul::Dictionary());
        }
        return false;
    }
    catch (const documentation::SpecificationError& e) {
        LERRORC(e.component, e.message);
        documentation::logError(e, e.component);
        if (script.callback) {
            script.callback(ghoul::Dictionary());
        }
        return false;
    }
    catch (const ghoul::RuntimeError& e) {
        LERRORC(e.component, e.message);
        if (script.callback) {
            script.callback(ghoul::Dictionary());
        }
        return false;
    }

    return true;
}

bool ScriptEngine::isLibraryNameAllowed(lua_State* state, const std::string& name) {
    bool result = false;
    lua_getglobal(state, OpenSpaceLibraryName.data());
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
            LERROR(std::format("Library name '{}' specifies a boolean", name));
            break;
        case LUA_TLIGHTUSERDATA:
            LERROR(std::format("Library name '{}' specifies a light user data", name));
            break;
        case LUA_TNUMBER:
            LERROR(std::format("Library name '{}' specifies a number", name));
            break;
        case LUA_TSTRING:
            LERROR(std::format("Library name '{}' specifies a string", name));
            break;
        case LUA_TTABLE:
            if (hasLibrary(name)) {
                LERROR(std::format(
                    "Library with name '{}' has been registered before", name
                ));
            }
            else {
                LERROR(std::format("Library name '{}' specifies a table", name));
            }
            break;
        case LUA_TFUNCTION:
            LERROR(std::format("Library name '{}' specifies a function", name));
            break;
        case LUA_TUSERDATA:
            LERROR(std::format("Library name '{}' specifies a user data", name));
            break;
        case LUA_TTHREAD:
            LERROR(std::format("Library name '{}' specifies a thread", name));
            break;
    }

    lua_pop(state, 2);
    return result;
}

void ScriptEngine::addLibraryFunctions(lua_State* state, LuaLibrary& library,
                                       Replace replace)
{
    ZoneScoped;

    ghoul_assert(state, "State must not be nullptr");
    for (const LuaLibrary::Function& p : library.functions) {
        if (!replace) {
            lua_getfield(state, -1, p.name.c_str());
            const bool isNil = lua_isnil(state, -1);
            if (!isNil) {
                LERROR(std::format("Function name '{}' was already assigned", p.name));
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

    library.documentations.clear();
    for (const std::filesystem::path& script : library.scripts) {
        // First we run the script to set its values in the current state
        ghoul::lua::runScriptFile(state, script);


        // Then, we extract the documentation information from the file
        ghoul::lua::push(state, "documentation");
        lua_gettable(state, -2);
        if (lua_isnil(state, -1)) {
            LERROR(std::format(
                "Module '{}' did not provide a documentation in script file {}",
                library.name, script
            ));
            lua_pop(state, 1);
            continue;
        }

        lua_pushnil(state);
        while (lua_next(state, -2)) {
            ghoul::Dictionary d = ghoul::lua::luaDictionaryFromState(state);
            try {
                const Parameters p = codegen::bake<Parameters>(d);

                LuaLibrary::Function func;
                func.name = p.name;
                for (const std::vector<std::string>& pair : p.arguments)
                {
                    LuaLibrary::Function::Argument arg;
                    arg.name = pair[0];
                    arg.type = pair[1];
                    func.arguments.push_back(arg);
                }
                func.returnType = p.returnValue.value_or(func.returnType);
                func.helpText = p.documentation.value_or(func.helpText);
                library.documentations.push_back(std::move(func));
            }
            catch (const documentation::SpecificationError& e) {
                logError(e);
            }
            lua_pop(state, 1);
        }
        lua_pop(state, 1);
    }
}

bool ScriptEngine::registerLuaLibrary(lua_State* state, LuaLibrary& library) {
    ZoneScoped;

    ghoul_assert(state, "State must not be nullptr");
    const int top = lua_gettop(state);

    lua_getglobal(state, OpenSpaceLibraryName.data());
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
    ZoneScoped;

    std::vector<std::string> result;
    for (const LuaLibrary& library : _registeredLibraries) {
        std::vector<std::string> r = luaFunctions(library, "openspace.");
        result.insert(result.end(), r.begin(), r.end());
    }
    return result;
}

const std::vector<LuaLibrary>& ScriptEngine::allLuaLibraries() const {
    return _registeredLibraries;
}

void ScriptEngine::writeLog(const std::string& script) {
    ZoneScoped;

    // Check that logging is enabled and initialize if necessary
    if (!_logFileExists) {
        // If a ScriptLogFile was specified, generate it now
        if (!global::configuration->scriptLog.empty()) {
            _logFilename = absPath(global::configuration->scriptLog);
            _logFileExists = true;

            LDEBUG(std::format("Using script log file '{}'", _logFilename));

            // Test file and clear previous input
            std::ofstream file(_logFilename, std::ofstream::out | std::ofstream::trunc);

            if (!file.good()) {
                LERROR(std::format(
                    "Could not open file '{}' for logging scripts", _logFilename
                ));
                return;
            }
        }
        else {
            _logScripts = false;
            return;
        }
    }

    // Simple text output to logfile
    std::ofstream file(_logFilename, std::ofstream::app);
    if (!file.good()) {
        LERROR(std::format("Could not open file '{}' for logging scripts", _logFilename));
        return;
    }

    file << script << '\n';
}

void ScriptEngine::preSync(bool isMaster) {
    ZoneScoped;

    std::lock_guard guard(_clientScriptsMutex);
    if (isMaster) {
        while (!_incomingScripts.empty()) {
            Script item = std::move(_incomingScripts.front());
            _incomingScripts.pop();

            // Not really a received script but the master also needs to run the script...
            _masterScriptQueue.push(item);

            if (global::sessionRecording->isRecording()) {
                global::sessionRecording->saveScriptKeyframeToTimeline(item.code);
            }

            // Sync out to other nodes (cluster)
            if (!item.synchronized) {
                continue;
            }
            _scriptsToSync.push_back(item.code);

            // Send to other peers (parallel connection)
            if (global::parallelPeer->isHost() && item.sendToRemote) {
                global::parallelPeer->sendScript(item.code);
            }
        }
    }
    else {
        while (!_incomingScripts.empty()) {
            Script item = std::move(_incomingScripts.front());
            _incomingScripts.pop();
            _clientScriptQueue.push(item.code);
        }
    }
}

void ScriptEngine::encode(SyncBuffer* syncBuffer) {
    ZoneScoped;

    size_t nScripts = _scriptsToSync.size();
    syncBuffer->encode(nScripts);
    for (const std::string& s : _scriptsToSync) {
        syncBuffer->encode(s);
    }
    _scriptsToSync.clear();
}

void ScriptEngine::decode(SyncBuffer* syncBuffer) {
    ZoneScoped;

    std::lock_guard guard(_clientScriptsMutex);
    size_t nScripts;
    syncBuffer->decode(nScripts);

    for (size_t i = 0; i < nScripts; ++i) {
        std::string script;
        syncBuffer->decode(script);
        _clientScriptQueue.push(std::move(script));
    }
}

void ScriptEngine::postSync(bool isMaster) {
    ZoneScoped;

    if (isMaster) {
        while (!_masterScriptQueue.empty()) {
            Script script = std::move(_masterScriptQueue.front());
            _masterScriptQueue.pop();
            try {
                runScript(script);
            }
            catch (const ghoul::RuntimeError& e) {
                LERRORC(e.component, e.message);
                continue;
            }
        }
    }
    else {
        std::lock_guard guard(_clientScriptsMutex);
        while (!_clientScriptQueue.empty()) {
            try {
                runScript({ _clientScriptQueue.front() });
                _clientScriptQueue.pop();
            }
            catch (const ghoul::RuntimeError& e) {
                LERRORC(e.component, e.message);
            }
        }
    }

    double now =
        global::sessionRecording->isSavingFramesDuringPlayback() ?
        global::sessionRecording->currentApplicationInterpolationTime() :
        global::windowDelegate->applicationTime();
    for (RepeatedScriptInfo& info : _repeatedScripts) {
        if (now - info.lastRun >= info.timeout) {
            runScript({ info.script });
            info.lastRun = now;
        }
    }
}

void ScriptEngine::queueScript(Script script) {
    ZoneScoped;

    if (script.code.empty()) {
        return;
    }
    _incomingScripts.push(std::move(script));
}

void ScriptEngine::queueScript(std::string script) {
    queueScript({ .code = std::move(script) });
}

void ScriptEngine::registerRepeatedScript(std::string identifier, std::string script,
                                          double timeout, std::string preScript,
                                          std::string postScript)
{
    auto it = std::find_if(
        _repeatedScripts.begin(),
        _repeatedScripts.end(),
        [&identifier](const RepeatedScriptInfo& info) {
            return info.identifier == identifier;
        }
    );
    if (it != _repeatedScripts.end()) {
        throw ghoul::RuntimeError(
            std::format("Script with identifier '{}' already registered", identifier),
            "ScriptEngine"
        );
    }

    if (!preScript.empty()) {
        runScript({ std::move(preScript) });
    }
    _repeatedScripts.emplace_back(
        std::move(script),
        std::move(postScript),
        std::move(identifier),
        timeout
    );
}

void ScriptEngine::removeRepeatedScript(std::string_view identifier) {
    auto it = std::find_if(
        _repeatedScripts.begin(),
        _repeatedScripts.end(),
        [&identifier](const RepeatedScriptInfo& info) {
            return info.identifier == identifier;
        }
    );
    if (it != _repeatedScripts.end()) {
        if (!it->postScript.empty()) {
            queueScript(it->postScript);
        }

        _repeatedScripts.erase(it);
    }
    else {
        LERROR(std::format("Could not find script with identifier '{}'", identifier));
    }
}

void ScriptEngine::addBaseLibrary() {
    ZoneScoped;

    LuaLibrary lib = {
        .name = "",
        .functions = {
            {
                "printTrace",
                &luascriptfunctions::printTrace,
                { { "", "*" } },
                "",
                "Logs the passed value to the installed LogManager with a LogLevel of "
                "'Trace'. For Boolean, numbers, and strings, the internal values are "
                "printed, for all other types, the type is printed instead",
                {}
            },
            {
                "printDebug",
                &luascriptfunctions::printDebug,
                { { "", "*" } },
                "",
                "Logs the passed value to the installed LogManager with a LogLevel of "
                "'Debug'. For Boolean, numbers, and strings, the internal values are "
                "printed, for all other types, the type is printed instead",
                {}
            },
            {
                "printInfo",
                &luascriptfunctions::printInfo,
                { { "", "*" } },
                "",
                "Logs the passed value to the installed LogManager with a LogLevel of "
                "'Info'. For Boolean, numbers, and strings, the internal values are "
                "printed, for all other types, the type is printed instead",
                {}
            },
            {
                "printWarning",
                &luascriptfunctions::printWarning,
                { { "", "*" } },
                "",
                "Logs the passed value to the installed LogManager with a LogLevel of "
                "'Warning'. For Boolean, numbers, and strings, the internal values are "
                "printed, for all other types, the type is printed instead",
                {}
            },
            {
                "printError",
                &luascriptfunctions::printError,
                { { "", "*" } },
                "",
                "Logs the passed value to the installed LogManager with a LogLevel of "
                "'Error'. For Boolean, numbers, and strings, the internal values are "
                "printed, for all other types, the type is printed instead",
                {}
            },
            {
                "printFatal",
                &luascriptfunctions::printFatal,
                { { "", "*" } },
                "",
                "Logs the passed value to the installed LogManager with a LogLevel of "
                "'Fatal'. For Boolean, numbers, and strings, the internal values are "
                "printed, for all other types, the type is printed instead",
                {}
            },
            codegen::lua::AbsolutePath,
            codegen::lua::SetPathToken,
            codegen::lua::FileExists,
            codegen::lua::ReadFile,
            codegen::lua::ReadFileLines,
            codegen::lua::DirectoryExists,
            codegen::lua::CreateDirectory,
            codegen::lua::WalkDirectory,
            codegen::lua::WalkDirectoryFiles,
            codegen::lua::WalkDirectoryFolders,
            codegen::lua::DirectoryForPath,
            codegen::lua::UnzipFile,
            codegen::lua::RegisterRepeatedScript,
            codegen::lua::RemoveRepeatedScript
        }
    };
    addLibrary(lib);
}

} // namespace openspace::scripting
