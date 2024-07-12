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

#ifndef __OPENSPACE_CORE___SCRIPTENGINE___H__
#define __OPENSPACE_CORE___SCRIPTENGINE___H__

#include <openspace/util/syncable.h>
#include <openspace/scripting/lualibrary.h>
#include <ghoul/lua/luastate.h>
#include <ghoul/misc/boolean.h>
#include <filesystem>
#include <mutex>
#include <optional>
#include <queue>
#include <functional>

namespace openspace { class SyncBuffer; }

namespace openspace::scripting {

/**
 * The ScriptEngine is responsible for handling the execution of custom Lua functions and
 * executing scripts (#runScript and #runScriptFile). Before usage, it has to be
 * #initialize%d and #deinitialize%d. New ScriptEngine::Library%s consisting of
 * Library::Function%s have to be added which can then be called using the
 * `openspace` namespace prefix in Lua. The same functions can be exposed to other Lua
 * states by passing them to the #initializeLuaState method.
 */
class ScriptEngine : public Syncable {
public:
    using ScriptCallback = std::function<void(ghoul::Dictionary)>;
    BooleanType(ShouldBeSynchronized);
    BooleanType(ShouldSendToRemote);

    struct QueueItem {
        std::string script;
        ShouldBeSynchronized shouldBeSynchronized;
        ShouldSendToRemote shouldSendToRemote;
        ScriptCallback callback;
    };

    static constexpr std::string_view OpenSpaceLibraryName = "openspace";

    ScriptEngine();

    /**
     * Initializes the internal Lua state and registers a common set of library functions.
     *
     * \throw LuaRuntimeException If the creation of the new Lua state fails
     */
    void initialize();

    /**
     * Cleans the internal Lua state and leaves the ScriptEngine in a state to be newly
     * initialize%d.
     */
    void deinitialize();

    void initializeLuaState(lua_State* state);
    ghoul::lua::LuaState* luaState();

    void addLibrary(LuaLibrary library);
    bool hasLibrary(const std::string& name);

    bool runScript(const std::string& script,
        const ScriptCallback& callback = ScriptCallback());
    bool runScriptFile(const std::filesystem::path& filename);

    virtual void preSync(bool isMaster) override;
    virtual void encode(SyncBuffer* syncBuffer) override;
    virtual void decode(SyncBuffer* syncBuffer) override;
    virtual void postSync(bool isMaster) override;

    void queueScript(std::string script, ShouldBeSynchronized shouldBeSynchronized,
        ShouldSendToRemote shouldSendToRemote,
        ScriptCallback callback = ScriptCallback());

    std::vector<std::string> allLuaFunctions() const;
    const std::vector<LuaLibrary>& allLuaLibraries() const;

private:
    BooleanType(Replace);

    void writeLog(const std::string& script);

    bool registerLuaLibrary(lua_State* state, LuaLibrary& library);
    void addLibraryFunctions(lua_State* state, LuaLibrary& library, Replace replace);

    bool isLibraryNameAllowed(lua_State* state, const std::string& name);

    void addBaseLibrary();

    ghoul::lua::LuaState _state;
    std::vector<LuaLibrary> _registeredLibraries;

    std::queue<QueueItem> _incomingScripts;

    // Client scripts are mutex protected since decode and rendering may happen
    // asynchronously
    std::mutex _clientScriptsMutex;
    std::queue<std::string> _clientScriptQueue;
    std::queue<QueueItem> _masterScriptQueue;

    std::vector<std::string> _scriptsToSync;

    // Logging variables
    bool _logFileExists = false;
    bool _logScripts = true;
    std::filesystem::path _logFilename;
};

} // namespace openspace::scripting

#endif // __OPENSPACE_CORE___SCRIPTENGINE___H__
