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

#ifndef __OPENSPACE_CORE___SCRIPTENGINE___H__
#define __OPENSPACE_CORE___SCRIPTENGINE___H__

#include <openspace/documentation/documentationgenerator.h>
#include <openspace/util/syncdata.h>

#include <openspace/scripting/lualibrary.h>

#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/lua/luastate.h>
#include <ghoul/misc/boolean.h>

#include <map>
#include <memory>
#include <set>

namespace openspace {

class SyncBuffer;

namespace scripting {

/**
 * The ScriptEngine is responsible for handling the execution of custom Lua functions and
 * executing scripts (#runScript and #runScriptFile). Before usage, it has to be
 * #initialize%d and #deinitialize%d. New ScriptEngine::Library%s consisting of
 * ScriptEngine::Library::Function%s have to be added which can then be called using the
 * <code>openspace</code> namespac prefix in Lua. The same functions can be exposed to
 * other Lua states by passing them to the #initializeLuaState method.
 */
class ScriptEngine : public Syncable, public DocumentationGenerator {
public:
    using RemoteScripting = ghoul::Boolean;

    ScriptEngine();

    /**
     * Initializes the internal Lua state and registers a common set of library functions
     * \throw LuaRuntimeException If the creation of the new Lua state fails
     */
    void initialize();
    
    /**
     * Cleans the internal Lua state and leaves the ScriptEngine in a state to be newly
     * initialize%d.
     */
    void deinitialize();
    
    void initializeLuaState(lua_State* state);

    void addLibrary(LuaLibrary library);
    bool hasLibrary(const std::string& name);
    
    bool runScript(const std::string& script);
    bool runScriptFile(const std::string& filename);

    bool writeLog(const std::string& script);

    virtual void presync(bool isMaster);
    virtual void encode(SyncBuffer* syncBuffer);
    virtual void decode(SyncBuffer* syncBuffer);
    virtual void postsync(bool isMaster);

    void queueScript(const std::string &script, RemoteScripting remoteScripting);

    void setLogFile(const std::string& filename, const std::string& type);

    std::vector<std::string> cachedScripts();

    std::vector<std::string> allLuaFunctions() const;
    
    //parallel functions
    //bool parseLibraryAndFunctionNames(std::string &library, std::string &function, const std::string &script);
    //bool shouldScriptBeSent(const std::string &library, const std::string &function);
    //void cacheScript(const std::string &library, const std::string &function, const std::string &script);
    
    static std::string OpenSpaceLibraryName;
    
private:
    bool registerLuaLibrary(lua_State* state, const LuaLibrary& library);
    void addLibraryFunctions(lua_State* state, const LuaLibrary& library, bool replace);

    bool isLibraryNameAllowed(lua_State* state, const std::string& name);
    
    void addBaseLibrary();
    void remapPrintFunction();
    
    std::string generateJson() const override;

    ghoul::lua::LuaState _state;
    std::set<LuaLibrary> _registeredLibraries;
    
    //sync variables
    std::mutex _mutex;
    std::vector<std::pair<std::string, bool>> _queuedScripts;
    std::vector<std::string> _receivedScripts;
    std::string _currentSyncedScript;
    
    //parallel variables
    //std::map<std::string, std::map<std::string, std::string>> _cachedScripts;
    //std::mutex _cachedScriptsMutex;

    //logging variables
    bool _logFileExists = false;
    bool _logScripts = true;
    std::string _logType;
    std::string _logFilename;
};

} // namespace scripting
} // namespace openspace

#endif // __OPENSPACE_CORE___SCRIPTENGINE___H__
