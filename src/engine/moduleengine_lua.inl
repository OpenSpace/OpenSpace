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

#include <openspace/engine/openspaceengine.h>
#include <ghoul/lua/ghoul_lua.h>

namespace openspace {
namespace luascriptfunctions {

/**
 * \ingroup LuaScripts
 * isLoaded(string):
 * Checks whether the passed OpenSpaceModule is loaded or not
 */
int isLoaded(lua_State* L) {
    int nArguments = lua_gettop(L);
    if (nArguments != 1)
        return luaL_error(L, "Expected %i arguments, got %i", 1, nArguments);

    const int type = lua_type(L, -1);
    if (type != LUA_TSTRING)
        return luaL_error(L, "Expected argument of type 'string'");
    std::string moduleName = lua_tostring(L, -1);

    std::vector<OpenSpaceModule*> modules = OsEng.moduleEngine().modules();
    
    auto it = std::find_if(
        modules.begin(),
        modules.end(),
        [moduleName](OpenSpaceModule* module) {
            return module->name() == moduleName;
        }
    );

    if (it != modules.end())
        lua_pushboolean(L, 1);
    else
        lua_pushboolean(L, 0);

    return 1;
}

} // namespace luascriptfunctions
} // namespace openspace
