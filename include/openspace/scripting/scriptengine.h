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

#ifndef __SCRIPTENGINE_H__
#define __SCRIPTENGINE_H__

#include <ghoul/lua/ghoul_lua.h>

#include <set>

/**
 * \defgroup LuaScripts Lua Scripts
 */

namespace openspace {
namespace scripting {

class ScriptEngine {
public:
    struct LuaLibrary {
		struct Function {
			std::string name;
			lua_CFunction function;
			std::string helpText;
		};
        std::string name;
		std::vector<Function> functions;

		bool operator<(const LuaLibrary& rhs) const;
    };
    
    ScriptEngine();

    bool initialize();
    void deinitialize();
    
	void initializeLuaState(lua_State* state);

	void addLibrary(const LuaLibrary& library);
    bool hasLibrary(const std::string& name);
    
    bool runScript(const std::string& script);
    bool runScriptFile(const std::string& filename);

    
private:
	bool registerLuaLibrary(lua_State* state, const LuaLibrary& library);
    void addLibraryFunctions(lua_State* state, const LuaLibrary& library, bool replace);

    bool isLibraryNameAllowed(const std::string& name);
    
    void addBaseLibrary();
    void remapPrintFunction();
    
    lua_State* _state;
    std::set<LuaLibrary> _registeredLibraries;
};
  
} // namespace scripting
} // namespace openspace

#endif // __SCRIPTENGINE_H__
