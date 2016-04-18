/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#ifndef __SCRIPTENGINE_H__
#define __SCRIPTENGINE_H__

#include <ghoul/lua/ghoul_lua.h>

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
class ScriptEngine {
public:
    /**
     * This structure represents a Lua library, itself consisting of a unique #name and
     * an arbitrary number of #functions
     */
    struct LuaLibrary {
        /**
         * This structure represents a Lua function with its #name, #function pointer
         * #argumentText describing the arguments this function takes, the #helpText
         * describing the function, and whether it should be shared in a parallel
         * connection (#parallelShared)
         */
        struct Function {
            /// The name of the function
            std::string name;
            /// The function pointer that is executed if the function is called
            lua_CFunction function;
            /// A text describing the arugments to this function
            std::string argumentText;
            /// A help text describing what the function does/
            std::string helpText;
            /// If <code>true</code>, this function will be shared with other parallel
            /// connections
            bool parallelShared;
        };
        /// The name of the library
        std::string name;
        /// The list of all functions for this library
        std::vector<Function> functions;

        /// Comparison function that compares two LuaLibrary%s name
        bool operator<(const LuaLibrary& rhs) const;
    };

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

    bool writeDocumentation(const std::string& filename, const std::string& type) const;

    void serialize(SyncBuffer* syncBuffer);

    void deserialize(SyncBuffer* syncBuffer);

    void postSynchronizationPreDraw();

    void preSynchronization();

    void queueScript(const std::string &script);
    
    std::vector<std::string> cachedScripts();

    std::vector<std::string> allLuaFunctions() const;
    
    //parallel functions
    bool parseLibraryAndFunctionNames(std::string &library, std::string &function, const std::string &script);
    bool shouldScriptBeSent(const std::string &library, const std::string &function);
    void cacheScript(const std::string &library, const std::string &function, const std::string &script);
    
private:
    bool registerLuaLibrary(lua_State* state, const LuaLibrary& library);
    void addLibraryFunctions(lua_State* state, const LuaLibrary& library, bool replace);

    bool isLibraryNameAllowed(lua_State* state, const std::string& name);
    
    void addBaseLibrary();
    void remapPrintFunction();
    
    lua_State* _state = nullptr;
    std::set<LuaLibrary> _registeredLibraries;
    
    //sync variables
    std::mutex _mutex;
    std::vector<std::string> _queuedScripts;
    std::vector<std::string> _receivedScripts;
    std::string _currentSyncedScript;
    
    //parallel variables
    std::map<std::string, std::map<std::string, std::string>> _cachedScripts;
    std::mutex _cachedScriptsMutex;
};

} // namespace scripting
} // namespace openspace

#endif // __SCRIPTENGINE_H__
