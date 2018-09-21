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

#include <iostream>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>

namespace openspace::luascriptfunctions {

/**
* \ingroup LuaScripts
* loadItem(string):
* Load a data item
*/
int loadItem(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::loadItem");

    using ghoul::lua::errorLocation;

    const std::string& path = ghoul::lua::value<std::string>(L, 1, ghoul::lua::PopValue::Yes);

    /** handle path to check item type or handle it in different function **/

    global::moduleEngine.module<DataLoaderModule>()->loadDataItem(path);

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

/**
* \ingroup LuaScripts
* uploadItem(string):
* Upoad a data item. Send a ghoul dictionary formatted in Lua as a string.
*/
int uploadItem(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::loadItem");

    using ghoul::lua::errorLocation;

    const std::string& dictionaryString = ghoul::lua::value<std::string>(L, 1, ghoul::lua::PopValue::Yes);
    global::moduleEngine.module<DataLoaderModule>()->uploadDataItem(dictionaryString);

    ghoul_assert(lua_gettop(L) == 0, "Incorrect number of items left on stack");
    return 0;
}

} // namespace openspace::luascriptfunctions 
