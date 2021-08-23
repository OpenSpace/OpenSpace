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

#include <openspace/engine/globals.h>
#include <ghoul/lua/ghoul_lua.h>

namespace openspace::luascriptfunctions {

/**
 * \ingroup LuaScripts
 * isLoaded(string):
 * Checks whether the passed OpenSpaceModule is loaded or not
 */
int isLoaded(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::isLoaded");
    const std::string name = ghoul::lua::value<std::string>(L);

    const std::vector<OpenSpaceModule*>& modules = global::moduleEngine->modules();
    const auto it = std::find_if(
        modules.cbegin(), modules.cend(),
        [name](OpenSpaceModule* module) { return module->identifier() == name; }
    );

    ghoul::lua::push(L, it != modules.cend());
    return 1;
}

} // namespace openspace::luascriptfunctions
